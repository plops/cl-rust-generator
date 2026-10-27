//! 06 — Relay: WebSocket-Endpunkt `/ws` fuer PCM- und Opus-Peers.
//!
//! `GET /ws?room=R&transport=pcm|opus&lang=de&target=zh-Hans`.
//! Defaults: `room=default-room`, `transport=pcm`, `lang`/`target` ueber
//! `languages::canonical` mit `DEFAULT_LANG`/`DEFAULT_TARGET`.
//!
//! Intern transportiert der Raum immer PCM 24 kHz (`Out::Pcm24`) oder
//! Untertitel-Text (`Out::Text`). Jeder Peer entscheidet selbst, wie er
//! ankommendes Audio ausliefert:
//! - PCM-Peer: `Out::Pcm24` roh als Binary; Binaer rein → Broadcast an Partner.
//! - Opus-Peer: `Out::Pcm24` durch eigenen `DownlinkEncoder` (je Paket ein
//!   Binary); Binaer rein = Opus-Paket → `UplinkDecoder` → Gemini-Sitzung,
//!   deren Audio-Antworten als `Out::Pcm24` an den Partner gehen.
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
use crate::languages::{self, DEFAULT_LANG, DEFAULT_TARGET};
use crate::state::{Out, PEER_BUFFER, PeerInfo, Rooms};

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

/// Query-Parameter des `/ws`-Endpunkts.
#[derive(Debug, serde::Deserialize, Default)]
pub struct WsParams {
    pub room: Option<String>,
    pub transport: Option<String>,
    pub lang: Option<String>,
    pub target: Option<String>,
}

/// Upgrade-Handler: Parameter auflsen, dann `peer_loop`.
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
    let target = languages::canonical(p.target.as_deref().unwrap_or(""), DEFAULT_TARGET).to_owned();
    let info = PeerInfo {
        lang,
        target,
        transport,
    };
    ws.on_upgrade(move |sock| peer_loop(sock, state, room, info))
}

/// Hilfsfunktion: Text-Frame bauen.
fn text(msg: impl Into<String>) -> Message {
    Message::Text(Utf8Bytes::from(msg.into()))
}

/// `peers`-Event als JSON-String fuer einen Raum bauen.
async fn peers_json(rooms: &Rooms, room: &str) -> String {
    let infos = rooms.infos(room).await;
    serde_json::json!({ "type": "peers", "peers": infos }).to_string()
}

/// `peers`-Event an alle Peers im Raum senden.
async fn broadcast_peers(rooms: &Rooms, room: &str) {
    let msg = peers_json(rooms, room).await;
    rooms.send_all(room, Out::Text(msg)).await;
}

/// Ein Peer von Beitritt bis Austritt.
async fn peer_loop(sock: WebSocket, state: AppState, room: String, info: PeerInfo) {
    let id = NEXT_PEER.fetch_add(1, Ordering::Relaxed);
    let transport = info.transport;
    let target = info.target.clone();
    let (tx, rx) = mpsc::channel::<Out>(PEER_BUFFER);

    if !state.rooms.join(&room, id, tx, info).await {
        tracing::warn!(room = %room, peer = id, "abgewiesen (raum voll)");
        let mut sock = sock;
        let _ = sock
            .send(text(r#"{"type":"error","msg":"raum voll"}"#))
            .await;
        let _ = sock.close().await;
        return;
    }
    tracing::info!(room = %room, peer = id, ?transport, "beigetreten");
    broadcast_peers(&state.rooms, &room).await;

    let (send, recv) = sock.split();

    match transport {
        Transport::Pcm => run_pcm(state.clone(), &room, id, send, recv, rx).await,
        Transport::Opus => {
            run_opus(state.clone(), &room, id, &target, send, recv, rx).await;
        }
    }

    state.rooms.leave(&room, id).await;
    broadcast_peers(&state.rooms, &room).await;
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

/// PCM-Peer: Binary → Broadcast an Partner; Untertitel-Text weiterreichen.
async fn run_pcm(
    state: AppState,
    room: &str,
    id: u64,
    send: SplitSink<WebSocket, Message>,
    mut recv: SplitStream<WebSocket>,
    rx: mpsc::Receiver<Out>,
) {
    let sender = spawn_pcm_sender(send, rx);

    while let Some(msg) = recv.next().await {
        match msg {
            Ok(Message::Binary(b)) => {
                let n = state
                    .rooms
                    .broadcast(room, id, Out::Pcm24(b.to_vec()))
                    .await;
                tracing::debug!(room = %room, peer = id, bytes = b.len(), beliefert = n, "pcm-frame verteilt");
            }
            Ok(Message::Text(t)) => {
                if let Some(fwd) = partner_subtitle(&t) {
                    state.rooms.broadcast(room, id, Out::Text(fwd)).await;
                }
            }
            Ok(Message::Close(_)) => break,
            Err(e) => {
                tracing::debug!(room = %room, peer = id, fehler = %e, "pcm-lesefehler");
                break;
            }
            _ => {}
        }
    }
    sender.abort();
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

/// Opus-Peer: hello → Key, Gemini-Sitzung, dann Opus rein/PCM raus.
#[allow(clippy::too_many_arguments)]
async fn run_opus(
    state: AppState,
    room: &str,
    id: u64,
    target: &str,
    send: SplitSink<WebSocket, Message>,
    mut recv: SplitStream<WebSocket>,
    rx: mpsc::Receiver<Out>,
) {
    let sender = spawn_opus_sender(send, rx);

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

    // Gemini-Sitzung oeffnen.
    let (ev_tx, mut ev_rx) = mpsc::channel::<Event>(64);
    let session = match gemini::connect(&state.gemini_url, &key, target, ev_tx).await {
        Ok(s) => s,
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

    loop {
        tokio::select! {
            // Events aus der Gemini-Sitzung.
            ev = ev_rx.recv() => {
                match ev {
                    Some(Event::Ready) => {
                        state.rooms.send_to(room, id, Out::Text(r#"{"type":"ready"}"#.to_owned())).await;
                    }
                    Some(Event::Audio(pcm24)) => {
                        // Uebersetzung geht an den Partner, NICHT an sich selbst.
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
                        state.rooms.send_to(room, id, Out::Text(r#"{"type":"error","msg":"gemini verbindung beendet"}"#.to_owned())).await;
                        break;
                    }
                    None => break,
                }
            }
            // Frames vom Opus-Peer.
            msg = recv.next() => {
                match msg {
                    Some(Ok(Message::Binary(b))) => {
                        match dec.decode(&b) {
                            Ok(samples) => {
                                let pcm16 = i16_to_le(&samples);
                                if session.tx.try_send(pcm16).is_err() {
                                    tracing::debug!(room = %room, peer = id, "gemini-eingang voll/zu");
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
    fn json_escape_quote_backslash() {
        assert_eq!(json_escape(r#"a"b\c"#), r#"a\"b\\c"#);
    }
}
