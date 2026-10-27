//! 06 — Relay: WebSocket-Endpunkt `/ws` fuer PCM- und Opus-Peers.
//!
//! `GET /ws?room=R&transport=pcm|opus&lang=de`. Defaults: `room=default-room`,
//! `transport=pcm`, `lang` ueber `languages::canonical` mit `DEFAULT_LANG`.
//! Ein evtl. mitgeschicktes `target` wird IGNORIERT: die Zielsprache ergibt
//! sich automatisch aus der Sprache des Partners (Nutzer-Feedback).
//!
//! Jeder Peer erfaehrt seinen Partner ueber eine `watch`-Verbindung aus dem
//! Raumbuch und bekommt ein persoenliches `partner`-Event
//! (`{"type":"partner","partner":{lang,transport}}` bzw. `partner:null`).
//!
//! Intern transportiert der Raum immer PCM 24 kHz (`Out::Pcm24`) oder
//! Untertitel-Text (`Out::Text`). Jeder Peer entscheidet selbst, wie er
//! ankommendes Audio ausliefert:
//! - PCM-Peer: `Out::Pcm24` roh als Binary; Binaer rein → Broadcast an Partner.
//! - Opus-Peer: `Out::Pcm24` durch eigenen `DownlinkEncoder` (je Paket ein
//!   Binary); Binaer rein = Opus-Paket → `UplinkDecoder` → Gemini-Sitzung,
//!   deren Audio-Antworten als `Out::Pcm24` an den Partner gehen. Die
//!   Gemini-Sitzung startet ERST, wenn ein Partner da ist (Ziel =
//!   Partner-Sprache); ohne Partner kommt `waiting` statt `ready`.
//!
//! Der API-Key wird NIE geloggt und nie in einer Fehlermeldung durchgereicht.

use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};
use std::time::Duration;

use axum::{
    extract::{
        Query, State,
        ws::{Message, Utf8Bytes, WebSocket, WebSocketUpgrade},
    },
    response::IntoResponse,
};
use futures_util::{
    SinkExt, StreamExt,
    stream::{SplitSink, SplitStream},
};
use tokio::sync::mpsc;

use crate::codec::{DownlinkEncoder, UplinkDecoder, i16_to_le};
use crate::config::Transport;
use crate::gemini::{self, Event};
use crate::languages::{self, DEFAULT_LANG};
use crate::state::{Out, PEER_BUFFER, PartnerRx, PeerInfo, Rooms};

/// Fortlaufende Peer-Id (nur fuer Logging und Broadcast-Ausschluss).
static NEXT_PEER: AtomicU64 = AtomicU64::new(1);

/// Standard-Raumname, wenn `?room=` fehlt oder leer ist.
const DEFAULT_ROOM: &str = "default-room";
/// Wartezeit auf die erste `hello`-Nachricht eines Opus-Peers.
const HELLO_TIMEOUT: Duration = Duration::from_secs(5);

/// Geteilter Serverzustand fuer alle Routen.
#[derive(Clone)]
pub struct AppState {
    /// Raumbuch (max. 2 Peers je Raum).
    pub rooms: Arc<Rooms>,
    /// Gemini-Endpunkt (Tests: Fake `ws://127.0.0.1:…`).
    pub gemini_url: String,
    /// Optionaler Server-Key (Opus-Peers ohne eigenen Key nutzen ihn).
    pub server_key: Option<String>,
}

/// Query-Parameter des `/ws`-Endpunkts. `target` wird NICHT mehr gelesen
/// (Zielsprache folgt automatisch aus dem Partner).
#[derive(Debug, serde::Deserialize, Default)]
pub struct WsParams {
    pub room: Option<String>,
    pub transport: Option<String>,
    pub lang: Option<String>,
}

/// Upgrade-Handler: Parameter aufloesen, dann `peer_loop`.
pub async fn ws_handler(
    ws: WebSocketUpgrade,
    Query(p): Query<WsParams>,
    State(state): State<AppState>,
) -> impl IntoResponse {
    let room = p
        .room
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| DEFAULT_ROOM.to_owned());
    let transport = Transport::parse(p.transport.as_deref().unwrap_or(""));
    let lang = languages::canonical(p.lang.as_deref().unwrap_or(""), DEFAULT_LANG).to_owned();
    let info = PeerInfo { lang, transport };
    ws.on_upgrade(move |sock| peer_loop(sock, state, room, info))
}

/// Hilfsfunktion: Text-Frame bauen.
fn text(msg: impl Into<String>) -> Message {
    Message::Text(Utf8Bytes::from(msg.into()))
}

/// `partner`-Event als JSON fuer eine Partner-Info (oder `null`).
fn partner_json(partner: &Option<PeerInfo>) -> String {
    serde_json::json!({ "type": "partner", "partner": partner }).to_string()
}

/// Ein Peer von Beitritt bis Austritt.
async fn peer_loop(sock: WebSocket, state: AppState, room: String, info: PeerInfo) {
    let id = NEXT_PEER.fetch_add(1, Ordering::Relaxed);
    let transport = info.transport;
    let own_lang = info.lang.clone();
    let (tx, rx) = mpsc::channel::<Out>(PEER_BUFFER);

    let Some(partner_rx) = state.rooms.join(&room, id, tx, info).await else {
        tracing::warn!(room = %room, peer = id, "abgewiesen (raum voll)");
        let mut sock = sock;
        let _ = sock
            .send(text(r#"{"type":"error","msg":"raum voll"}"#))
            .await;
        let _ = sock.close().await;
        return;
    };
    tracing::info!(room = %room, peer = id, ?transport, "beigetreten");

    let (send, recv) = sock.split();

    match transport {
        Transport::Pcm => run_pcm(state.clone(), &room, id, send, recv, rx, partner_rx).await,
        Transport::Opus => {
            run_opus(
                state.clone(),
                &room,
                id,
                &own_lang,
                send,
                recv,
                rx,
                partner_rx,
            )
            .await;
        }
    }

    state.rooms.leave(&room, id).await;
    tracing::info!(room = %room, peer = id, "verlassen");
}

/// Sende-Task eines PCM-Peers: `Out` → WS. Audio roh als Binary.
fn spawn_pcm_sender(
    mut send: SplitSink<WebSocket, Message>,
    mut rx: mpsc::Receiver<Out>,
) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        while let Some(out) = rx.recv().await {
            let msg = match out {
                Out::Pcm24(b) => Message::Binary(b.into()),
                Out::Text(t) => text(t),
            };
            if send.send(msg).await.is_err() {
                break;
            }
        }
    })
}

/// PCM-Peer: Binary → Broadcast an Partner; Untertitel-Text weiterreichen;
/// Partner-Wechsel → persoenliches `partner`-Event.
#[allow(clippy::too_many_arguments)]
async fn run_pcm(
    state: AppState,
    room: &str,
    id: u64,
    send: SplitSink<WebSocket, Message>,
    mut recv: SplitStream<WebSocket>,
    rx: mpsc::Receiver<Out>,
    mut partner_rx: PartnerRx,
) {
    let sender = spawn_pcm_sender(send, rx);

    // Initialen Partner-Zustand sofort melden.
    let start = partner_rx.borrow_and_update().clone();
    send_partner(&state.rooms, room, id, &start).await;

    loop {
        tokio::select! {
            changed = partner_rx.changed() => {
                if changed.is_err() { break; }
                let p = partner_rx.borrow_and_update().clone();
                send_partner(&state.rooms, room, id, &p).await;
            }
            msg = recv.next() => {
                match msg {
                    Some(Ok(Message::Binary(b))) => {
                        let n = state
                            .rooms
                            .broadcast(room, id, Out::Pcm24(b.to_vec()))
                            .await;
                        tracing::debug!(room = %room, peer = id, bytes = b.len(), beliefert = n, "pcm-frame verteilt");
                    }
                    Some(Ok(Message::Text(t))) => {
                        if let Some(fwd) = partner_subtitle(&t) {
                            state.rooms.broadcast(room, id, Out::Text(fwd)).await;
                        }
                    }
                    Some(Ok(Message::Close(_))) | None => break,
                    Some(Err(e)) => {
                        tracing::debug!(room = %room, peer = id, fehler = %e, "pcm-lesefehler");
                        break;
                    }
                    _ => {}
                }
            }
        }
    }
    sender.abort();
}

/// Persoenliches `partner`-Event an genau diesen Peer senden.
async fn send_partner(rooms: &Rooms, room: &str, id: u64, partner: &Option<PeerInfo>) {
    rooms
        .send_to(room, id, Out::Text(partner_json(partner)))
        .await;
}

/// Browser-Untertitel `{"type":"text","dir":"in|out","text":..}` zu einer
/// Partner-Nachricht `{"type":"text","who":"partner",..}` umschreiben.
/// Andere Texte → `None` (ignorieren).
fn partner_subtitle(txt: &str) -> Option<String> {
    let v: serde_json::Value = serde_json::from_str(txt).ok()?;
    if v.get("type").and_then(|t| t.as_str()) != Some("text") {
        return None;
    }
    let dir = v.get("dir").and_then(|d| d.as_str())?;
    let text = v.get("text").and_then(|t| t.as_str())?;
    Some(
        serde_json::json!({
            "type": "text", "who": "partner", "dir": dir, "text": text
        })
        .to_string(),
    )
}

/// Sende-Task eines Opus-Peers: `Out::Pcm24` durch eigenen `DownlinkEncoder`
/// (je Paket ein Binary), `Out::Text` roh als Text.
fn spawn_opus_sender(
    mut send: SplitSink<WebSocket, Message>,
    mut rx: mpsc::Receiver<Out>,
) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        let mut enc = match DownlinkEncoder::new() {
            Ok(e) => e,
            Err(e) => {
                tracing::error!(fehler = %e, "downlink-encoder");
                return;
            }
        };
        while let Some(out) = rx.recv().await {
            match out {
                Out::Pcm24(b) => {
                    for pkt in enc.push_pcm24(&b) {
                        if send.send(Message::Binary(pkt.into())).await.is_err() {
                            return;
                        }
                    }
                }
                Out::Text(t) => {
                    if send.send(text(t)).await.is_err() {
                        return;
                    }
                }
            }
        }
    })
}

/// Erste Textnachricht `{"type":"hello","key":".."}` lesen (max. 5 s) und
/// den Key herausziehen. `None` bei Timeout/kaputter Nachricht/ohne Key.
async fn read_hello_key(recv: &mut SplitStream<WebSocket>) -> Option<String> {
    let fut = async {
        while let Some(Ok(msg)) = recv.next().await {
            match msg {
                Message::Text(t) => {
                    let v: serde_json::Value = serde_json::from_str(&t).ok()?;
                    if v.get("type").and_then(|x| x.as_str()) == Some("hello") {
                        return v
                            .get("key")
                            .and_then(|k| k.as_str())
                            .map(str::to_owned)
                            .filter(|k| !k.trim().is_empty());
                    }
                }
                Message::Close(_) => return None,
                _ => {}
            }
        }
        None
    };
    tokio::time::timeout(HELLO_TIMEOUT, fut)
        .await
        .ok()
        .flatten()
}

/// Eine laufende Gemini-Sitzung eines Opus-Peers samt Ziel-Sprache.
struct ActiveSession {
    session: gemini::Session,
    /// Zielsprache dieser Sitzung (= Partner-Sprache). Aendert sich der
    /// Partner, muss die Sitzung neu aufgebaut werden.
    target: String,
}

/// Opus-Peer: hello → Key, dann partner-gesteuerte Gemini-Sitzung.
///
/// - Kein Partner → Sitzung beenden, `{"type":"waiting"}` senden; eingehendes
///   Audio wird verworfen (keine Sitzung).
/// - Partner da → Sitzung mit Ziel = Partner-Sprache starten; `ready` erst
///   nach `setupComplete`. Eigene Sprache == Partner-Sprache → `echo = true`.
/// - Partner wechselt (andere Sprache) → alte Sitzung beenden, neue starten.
#[allow(clippy::too_many_arguments)]
async fn run_opus(
    state: AppState,
    room: &str,
    id: u64,
    own_lang: &str,
    send: SplitSink<WebSocket, Message>,
    mut recv: SplitStream<WebSocket>,
    rx: mpsc::Receiver<Out>,
    mut partner_rx: PartnerRx,
) {
    let sender = spawn_opus_sender(send, rx);

    // Partner-Event sofort melden (unabhaengig von hello/Gemini), damit das
    // Frontend „Partner spricht: …" bzw. „Warte auf Partner…" sofort zeigt.
    let start_partner = partner_rx.borrow_and_update().clone();
    send_partner(&state.rooms, room, id, &start_partner).await;

    // Key bestimmen: hello-Key, sonst Server-Key; keiner → Fehler + Ende.
    let hello_key = read_hello_key(&mut recv).await;
    let key = match hello_key.or_else(|| state.server_key.clone()) {
        Some(k) => k,
        None => {
            state
                .rooms
                .send_to(
                    room,
                    id,
                    Out::Text(r#"{"type":"error","msg":"kein api-key"}"#.to_owned()),
                )
                .await;
            sender.abort();
            return;
        }
    };

    let mut dec = match UplinkDecoder::new() {
        Ok(d) => d,
        Err(e) => {
            tracing::error!(fehler = %e, "uplink-decoder");
            state
                .rooms
                .send_to(
                    room,
                    id,
                    Out::Text(r#"{"type":"error","msg":"decoder"}"#.to_owned()),
                )
                .await;
            sender.abort();
            return;
        }
    };

    // Aktuelle Sitzung (anfangs keine) und ihr Event-Kanal.
    let mut active: Option<ActiveSession> = None;
    let (ev_tx, mut ev_rx) = mpsc::channel::<Event>(64);

    // Startzustand anhand des aktuellen Partners herstellen (kann sich
    // waehrend der hello-Wartezeit geaendert haben).
    let current_partner = partner_rx.borrow_and_update().clone();
    apply_partner(
        &state,
        room,
        id,
        own_lang,
        &key,
        &ev_tx,
        &mut active,
        &current_partner,
    )
    .await;

    loop {
        tokio::select! {
            // Partner-Wechsel (Join/Leave/Sprachwechsel).
            changed = partner_rx.changed() => {
                if changed.is_err() { break; }
                let p = partner_rx.borrow_and_update().clone();
                apply_partner(&state, room, id, own_lang, &key, &ev_tx, &mut active, &p).await;
            }
            // Events aus der laufenden Gemini-Sitzung.
            ev = ev_rx.recv() => {
                match ev {
                    Some(Event::Ready) => {
                        state.rooms.send_to(room, id, Out::Text(r#"{"type":"ready"}"#.to_owned())).await;
                    }
                    Some(Event::Audio(pcm24)) => {
                        state.rooms.broadcast(room, id, Out::Pcm24(pcm24)).await;
                    }
                    Some(Event::InText(t)) => {
                        send_subtitle(&state.rooms, room, id, "in", &t).await;
                    }
                    Some(Event::OutText(t)) => {
                        send_subtitle(&state.rooms, room, id, "out", &t).await;
                    }
                    Some(Event::Closed(grund)) => {
                        tracing::debug!(room = %room, peer = id, %grund, "gemini geschlossen");
                        // Nur melden, wenn wir noch eine Sitzung erwarten.
                        if active.is_some() {
                            state.rooms.send_to(room, id, Out::Text(r#"{"type":"error","msg":"gemini verbindung beendet"}"#.to_owned())).await;
                        }
                    }
                    None => {}
                }
            }
            // Frames vom Opus-Peer.
            msg = recv.next() => {
                match msg {
                    Some(Ok(Message::Binary(b))) => {
                        match dec.decode(&b) {
                            Ok(samples) => {
                                // Ohne laufende Sitzung (kein Partner): Audio verwerfen.
                                if let Some(a) = active.as_ref() {
                                    let pcm16 = i16_to_le(&samples);
                                    if a.session.tx.try_send(pcm16).is_err() {
                                        tracing::debug!(room = %room, peer = id, "gemini-eingang voll/zu");
                                    }
                                }
                            }
                            Err(e) => {
                                tracing::debug!(room = %room, peer = id, fehler = %e, "opus-paket verworfen");
                            }
                        }
                    }
                    Some(Ok(Message::Text(_))) => {}
                    Some(Ok(Message::Close(_))) | None => break,
                    Some(Ok(_)) => {}
                    Some(Err(e)) => {
                        tracing::debug!(room = %room, peer = id, fehler = %e, "opus-lesefehler");
                        break;
                    }
                }
            }
        }
    }

    sender.abort();
}

/// Auf einen (neuen) Partner reagieren: Sitzung passend oeffnen/schliessen.
///
/// - Kein Partner → Sitzung schliessen, `waiting` senden.
/// - Partner mit Sprache L:
///   - laeuft schon eine Sitzung mit Ziel L → nichts tun.
///   - sonst alte Sitzung schliessen, neue mit Ziel L starten (echo, wenn
///     eigene Sprache == L).
#[allow(clippy::too_many_arguments)]
async fn apply_partner(
    state: &AppState,
    room: &str,
    id: u64,
    own_lang: &str,
    key: &str,
    ev_tx: &mpsc::Sender<Event>,
    active: &mut Option<ActiveSession>,
    partner: &Option<PeerInfo>,
) {
    // Dem Opus-Peer immer sein persoenliches partner-Event schicken, damit das
    // Frontend „Partner spricht: …" bzw. „Warte auf Partner…" anzeigen kann.
    send_partner(&state.rooms, room, id, partner).await;
    match partner {
        None => {
            if active.take().is_some() {
                tracing::info!(room = %room, peer = id, "partner weg → sitzung beendet");
            }
            state
                .rooms
                .send_to(room, id, Out::Text(r#"{"type":"waiting"}"#.to_owned()))
                .await;
        }
        Some(p) => {
            // Passt die laufende Sitzung schon zum Ziel? Dann nichts tun.
            if active.as_ref().map(|a| a.target.as_str()) == Some(p.lang.as_str()) {
                return;
            }
            // Alte Sitzung (falls vorhanden) verwerfen.
            active.take();
            let echo = own_lang.eq_ignore_ascii_case(&p.lang);
            match gemini::connect(&state.gemini_url, key, &p.lang, echo, ev_tx.clone()).await {
                Ok(session) => {
                    tracing::info!(room = %room, peer = id, ziel = %p.lang, echo, "gemini-sitzung gestartet");
                    *active = Some(ActiveSession {
                        session,
                        target: p.lang.clone(),
                    });
                }
                Err(e) => {
                    tracing::warn!(room = %room, peer = id, fehler = %e, "gemini-connect fehlgeschlagen");
                    state
                        .rooms
                        .send_to(
                            room,
                            id,
                            Out::Text(format!(
                                r#"{{"type":"error","msg":"gemini: {}"}}"#,
                                json_escape(&e)
                            )),
                        )
                        .await;
                }
            }
        }
    }
}

/// Untertitel aus Gemini an diesen Peer (`who:"me"`) UND den Partner
/// (`who:"partner"`) schicken.
async fn send_subtitle(rooms: &Rooms, room: &str, id: u64, dir: &str, text: &str) {
    let me = serde_json::json!({
        "type": "text", "who": "me", "dir": dir, "text": text
    })
    .to_string();
    let partner = serde_json::json!({
        "type": "text", "who": "partner", "dir": dir, "text": text
    })
    .to_string();
    rooms.send_to(room, id, Out::Text(me)).await;
    rooms.broadcast(room, id, Out::Text(partner)).await;
}

/// Minimales JSON-String-Escaping fuer Fehlertexte.
fn json_escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn partner_subtitle_umschreiben() {
        let msg = r#"{"type":"text","dir":"out","text":"你好"}"#;
        let got = partner_subtitle(msg).unwrap();
        let v: serde_json::Value = serde_json::from_str(&got).unwrap();
        assert_eq!(v["type"], "text");
        assert_eq!(v["who"], "partner");
        assert_eq!(v["dir"], "out");
        assert_eq!(v["text"], "你好");
    }

    #[test]
    fn partner_subtitle_ignoriert_fremdes() {
        assert!(partner_subtitle(r#"{"type":"hello","key":"x"}"#).is_none());
        assert!(partner_subtitle("kein json {").is_none());
        assert!(partner_subtitle(r#"{"type":"text"}"#).is_none());
    }

    #[test]
    fn partner_json_mit_und_ohne() {
        let mit = partner_json(&Some(PeerInfo {
            lang: "de".to_owned(),
            transport: Transport::Pcm,
        }));
        let v: serde_json::Value = serde_json::from_str(&mit).unwrap();
        assert_eq!(v["type"], "partner");
        assert_eq!(v["partner"]["lang"], "de");
        assert_eq!(v["partner"]["transport"], "pcm");

        let ohne = partner_json(&None);
        let v: serde_json::Value = serde_json::from_str(&ohne).unwrap();
        assert_eq!(v["type"], "partner");
        assert!(v["partner"].is_null());
    }

    #[test]
    fn json_escape_quote_backslash() {
        assert_eq!(json_escape(r#"a"b\c"#), r#"a\"b\\c"#);
    }
}
