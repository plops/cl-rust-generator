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

use crate::config::Transport;
use crate::gemini;
use crate::languages::{self, DEFAULT_LANG};
use crate::state::{Out, PEER_BUFFER, PartnerRx, PeerInfo, Rooms};
use crate::voices::{self, Model};

/// Fortlaufende Peer-Id (nur fuer Logging und Broadcast-Ausschluss).
static NEXT_PEER: AtomicU64 = AtomicU64::new(1);

/// Standard-Raumname, wenn `?room=` fehlt oder leer ist.
const DEFAULT_ROOM: &str = "default-room";

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
/// (Zielsprache folgt automatisch aus dem Partner). `voice` ist die
/// Sprecherstimme (`auto` oder ein Prebuilt-Name); fehlt sie, gilt die
/// Default-Stimme der eigenen Sprache.
#[derive(Debug, serde::Deserialize, Default)]
pub struct WsParams {
    pub room: Option<String>,
    pub transport: Option<String>,
    pub lang: Option<String>,
    pub voice: Option<String>,
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
    // Fehlt/leer die Stimme → Default-Stimme der eigenen Sprache.
    let voice = p
        .voice
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| voices::default_voice(&lang).to_owned());
    let info = PeerInfo {
        lang,
        transport,
        voice,
    };
    ws.on_upgrade(move |sock| peer_loop(sock, state, room, info))
}

/// Hilfsfunktion: Text-Frame bauen.
pub(crate) fn text(msg: impl Into<String>) -> Message {
    Message::Text(Utf8Bytes::from(msg.into()))
}

/// `partner`-Event als JSON fuer EINEN Peer.
///
/// Enthaelt neben der Partner-Info (`lang`, `transport`, `voice`) die
/// Modell-Entscheidung fuer DIESEN Peer: aus der eigenen Sprache+Stimme und
/// der Partner-Sprache folgt (via `gemini::setup_for`) das Modell und — fuer
/// PCM-Peers — das fertige `setup`-Objekt, das der Browser nur noch an Gemini
/// schickt. So gibt es EINE Quelle der Wahrheit (keine Duplikat-Logik im JS).
///
/// `model` ist `"translate"` oder `"live"` (nur zur Anzeige fuer Opus-Peers
/// mitgesendet). Ohne Partner: `partner:null`, kein Setup.
fn partner_json(
    own_lang: &str,
    own_voice: &str,
    own_transport: Transport,
    partner: &Option<PeerInfo>,
) -> String {
    let mut root = serde_json::json!({ "type": "partner", "partner": partner });
    if let Some(p) = partner {
        match gemini::setup_for(own_lang, &p.lang, own_voice) {
            Ok((model, setup)) => {
                root["model"] = serde_json::Value::String(model_name(model).to_owned());
                // Nur PCM-Peers brauchen das fertige Setup (Browser → Gemini).
                if own_transport == Transport::Pcm {
                    // `setup` ist bereits gueltiges JSON → als Wert einbetten.
                    if let Ok(v) = serde_json::from_str::<serde_json::Value>(&setup) {
                        root["setup"] = v["setup"].clone();
                    }
                }
            }
            Err(e) => {
                root["model"] = serde_json::Value::Null;
                root["setup_error"] = serde_json::Value::String(e);
            }
        }
    }
    root.to_string()
}

/// Modell-Name fuers Protokoll.
fn model_name(m: Model) -> &'static str {
    match m {
        Model::Translate => "translate",
        Model::Live => "live",
    }
}

/// Ein Peer von Beitritt bis Austritt.
async fn peer_loop(sock: WebSocket, state: AppState, room: String, info: PeerInfo) {
    let id = NEXT_PEER.fetch_add(1, Ordering::Relaxed);
    let transport = info.transport;
    let own_lang = info.lang.clone();
    let own_voice = info.voice.clone();
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
        Transport::Pcm => {
            run_pcm(
                state.clone(),
                &room,
                id,
                &own_lang,
                &own_voice,
                send,
                recv,
                rx,
                partner_rx,
            )
            .await;
        }
        Transport::Opus => {
            crate::relay_opus::run_opus(
                state.clone(),
                &room,
                id,
                &own_lang,
                &own_voice,
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
    own_lang: &str,
    own_voice: &str,
    send: SplitSink<WebSocket, Message>,
    mut recv: SplitStream<WebSocket>,
    rx: mpsc::Receiver<Out>,
    mut partner_rx: PartnerRx,
) {
    let sender = spawn_pcm_sender(send, rx);

    // Initialen Partner-Zustand sofort melden.
    let start = partner_rx.borrow_and_update().clone();
    send_partner(
        &state.rooms,
        room,
        id,
        own_lang,
        own_voice,
        Transport::Pcm,
        &start,
    )
    .await;

    loop {
        tokio::select! {
            changed = partner_rx.changed() => {
                if changed.is_err() { break; }
                let p = partner_rx.borrow_and_update().clone();
                send_partner(&state.rooms, room, id, own_lang, own_voice, Transport::Pcm, &p).await;
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

/// Persoenliches `partner`-Event an genau diesen Peer senden (inkl. Modell und
/// — bei PCM — fertigem Setup fuer DIESEN Peer).
#[allow(clippy::too_many_arguments)]
pub(crate) async fn send_partner(
    rooms: &Rooms,
    room: &str,
    id: u64,
    own_lang: &str,
    own_voice: &str,
    own_transport: Transport,
    partner: &Option<PeerInfo>,
) {
    rooms
        .send_to(
            room,
            id,
            Out::Text(partner_json(own_lang, own_voice, own_transport, partner)),
        )
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
    fn partner_json_mit_setup_und_model() {
        // PCM-Peer (de, Stimme Charon), Partner spricht zh-Hans:
        // feste Stimme + beide live → Live-Modell mit Setup fuer den Browser.
        let mit = partner_json(
            "de",
            "Charon",
            Transport::Pcm,
            &Some(PeerInfo {
                lang: "zh-Hans".to_owned(),
                transport: Transport::Opus,
                voice: "Kore".to_owned(),
            }),
        );
        let v: serde_json::Value = serde_json::from_str(&mit).unwrap();
        assert_eq!(v["type"], "partner");
        assert_eq!(v["partner"]["lang"], "zh-Hans");
        assert_eq!(v["partner"]["transport"], "opus");
        assert_eq!(v["partner"]["voice"], "Kore");
        assert_eq!(v["model"], "live");
        // PCM-Peer bekommt das fertige Setup (Browser → Gemini).
        assert_eq!(
            v["setup"]["generationConfig"]["speechConfig"]["voiceConfig"]["prebuiltVoiceConfig"]["voiceName"],
            "Charon"
        );

        // Opus-Peer bekommt `model`, aber KEIN Setup (Server haelt die Sitzung).
        let opus = partner_json(
            "de",
            voices::AUTO,
            Transport::Opus,
            &Some(PeerInfo {
                lang: "en".to_owned(),
                transport: Transport::Pcm,
                voice: "auto".to_owned(),
            }),
        );
        let v: serde_json::Value = serde_json::from_str(&opus).unwrap();
        assert_eq!(v["model"], "translate");
        assert!(v["setup"].is_null(), "opus-peer ohne setup");

        // Ohne Partner: partner:null, kein model/setup.
        let ohne = partner_json("de", "auto", Transport::Pcm, &None);
        let v: serde_json::Value = serde_json::from_str(&ohne).unwrap();
        assert_eq!(v["type"], "partner");
        assert!(v["partner"].is_null());
        assert!(v["model"].is_null());
    }
}
