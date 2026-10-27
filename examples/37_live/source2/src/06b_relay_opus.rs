//! 06b — Opus-Pfad des Relays (Sparmodus).
//!
//! Ausgelagert aus `06_relay.rs`, damit beide Dateien unter 600 Zeilen
//! bleiben. Hier lebt alles, was NUR der Opus-Peer braucht: der Sende-Task
//! mit SILK-Encoder, das `hello`-Lesen (Key), die partner-gesteuerte
//! Gemini-Sitzung (`waiting`/`ready`) und die Untertitel-Verteilung.
//!
//! Der Server haelt hier die Gemini-Sitzung selbst (Server-Key), waehrend der
//! PCM-Peer in `06_relay.rs` das fertige Setup aus dem `partner`-Event an
//! seinen eigenen Browser-Gemini-Kanal weiterreicht.
//!
//! Der API-Key wird NIE geloggt und nie in einer Fehlermeldung durchgereicht.

use std::time::Duration;

use axum::extract::ws::{Message, WebSocket};
use futures_util::{
    SinkExt, StreamExt,
    stream::{SplitSink, SplitStream},
};
use tokio::sync::mpsc;

use crate::codec::{DownlinkEncoder, UplinkDecoder, i16_to_le};
use crate::config::Transport;
use crate::gemini::{self, Event};
use crate::relay::{AppState, send_partner, text};
use crate::state::{Out, PartnerRx, PeerInfo, Rooms};

/// Wartezeit auf die erste `hello`-Nachricht eines Opus-Peers.
const HELLO_TIMEOUT: Duration = Duration::from_secs(5);

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
/// - Partner da → Sitzung (Modell/Stimme via `setup_for`) starten; `ready`
///   erst nach `setupComplete`.
/// - Partner wechselt (andere Sprache) → alte Sitzung beenden, neue starten.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn run_opus(
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
    let sender = spawn_opus_sender(send, rx);

    // Partner-Event sofort melden (unabhaengig von hello/Gemini), damit das
    // Frontend „Partner spricht: …" bzw. „Warte auf Partner…" sofort zeigt.
    let start_partner = partner_rx.borrow_and_update().clone();
    send_partner(
        &state.rooms,
        room,
        id,
        own_lang,
        own_voice,
        Transport::Opus,
        &start_partner,
    )
    .await;

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
        own_voice,
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
                apply_partner(&state, room, id, own_lang, own_voice, &key, &ev_tx, &mut active, &p).await;
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
///   - sonst alte Sitzung schliessen, neue (Modell/Stimme via `setup_for`)
///     starten.
#[allow(clippy::too_many_arguments)]
async fn apply_partner(
    state: &AppState,
    room: &str,
    id: u64,
    own_lang: &str,
    own_voice: &str,
    key: &str,
    ev_tx: &mpsc::Sender<Event>,
    active: &mut Option<ActiveSession>,
    partner: &Option<PeerInfo>,
) {
    // Dem Opus-Peer immer sein persoenliches partner-Event schicken, damit das
    // Frontend „Partner spricht: …" bzw. „Warte auf Partner…" anzeigen kann.
    send_partner(
        &state.rooms,
        room,
        id,
        own_lang,
        own_voice,
        Transport::Opus,
        partner,
    )
    .await;
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
            // Modell + Setup aus eigener Sprache+Stimme und Partner-Sprache.
            let setup = match gemini::setup_for(own_lang, &p.lang, own_voice) {
                Ok((model, json)) => {
                    tracing::info!(room = %room, peer = id, ziel = %p.lang, ?model, "modell gewaehlt");
                    json
                }
                Err(e) => {
                    tracing::warn!(room = %room, peer = id, fehler = %e, "kein modell fuer sprachpaar");
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
                    return;
                }
            };
            match gemini::connect(&state.gemini_url, key, &setup, ev_tx.clone()).await {
                Ok(session) => {
                    tracing::info!(room = %room, peer = id, ziel = %p.lang, "gemini-sitzung gestartet");
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
    fn json_escape_quote_backslash() {
        assert_eq!(json_escape(r#"a"b\c"#), r#"a\"b\\c"#);
    }
}
