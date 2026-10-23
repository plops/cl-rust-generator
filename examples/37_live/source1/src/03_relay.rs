//! 03 — Relay: WebSocket-Endpunkt `/ws?room=NAME`.
//!
//! Transport vor Protokoll: Binaer-Frames (PCM16) werden 1:1 an alle
//! Peers desselben Raums weitergereicht, nie zurueck an den Sender.
//! Text wird ignoriert (ausser winzigem Hello), Fehler schließen sauber.

use std::{
    collections::HashMap,
    sync::Arc,
    sync::atomic::{AtomicU64, Ordering},
};

use axum::{
    extract::{
        Query, State,
        ws::{Message, WebSocket, WebSocketUpgrade},
    },
    response::IntoResponse,
};
use futures_util::{SinkExt, StreamExt};

use crate::state::Rooms;

static NEXT_PEER: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, serde::Deserialize, Default)]
pub struct WsParams {
    pub room: Option<String>,
}

/// Upgrade-Handler: Raum aus Query (`?room=`), sonst `default-room`.
pub async fn ws_handler(
    ws: WebSocketUpgrade,
    Query(p): Query<WsParams>,
    State(rooms): State<Arc<Rooms>>,
) -> impl IntoResponse {
    let room = p
        .room
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| "default-room".to_owned());
    ws.on_upgrade(move |sock| peer_loop(sock, rooms, room))
}

async fn peer_loop(sock: WebSocket, rooms: Arc<Rooms>, room: String) {
    let id = NEXT_PEER.fetch_add(1, Ordering::Relaxed);
    let (tx, mut rx) = tokio::sync::mpsc::channel::<Vec<u8>>(crate::state::PEER_BUFFER);
    if !rooms.join(&room, id, tx).await {
        let mut sock = sock;
        let _ = sock
            .send(Message::Text(format!("raum voll: {room}").into()))
            .await;
        let _ = sock.close().await;
        return;
    }
    tracing::info!(room = %room, peer = id, "beigetreten");
    let (mut send, mut recv) = sock.split();

    let fwd = tokio::spawn(async move {
        while let Some(bytes) = rx.recv().await {
            if send.send(Message::Binary(bytes.into())).await.is_err() {
                break;
            }
        }
    });

    while let Some(msg) = recv.next().await {
        match msg {
            Ok(Message::Binary(b)) => {
                rooms.broadcast(&room, id, b.to_vec()).await;
            }
            Ok(Message::Text(_)) => {}
            Ok(Message::Close(_)) | Err(_) => break,
            _ => {}
        }
    }
    fwd.abort();
    rooms.leave(&room, id).await;
    tracing::info!(room = %room, peer = id, "verlassen");
}

/// Leere-Raum-Helfer fuer Tests (bleibt, weil `ws_handler`-Default hier liegt).
#[allow(dead_code)]
pub fn default_room(params: &HashMap<String, String>) -> String {
    params
        .get("room")
        .filter(|s| !s.trim().is_empty())
        .cloned()
        .unwrap_or_else(|| "default-room".to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_raum_und_leere_query() {
        assert_eq!(default_room(&HashMap::new()), "default-room");
        let mut q = HashMap::new();
        q.insert("room".into(), "  ".into());
        assert_eq!(default_room(&q), "default-room");
        q.insert("room".into(), "transl-room-1".into());
        assert_eq!(default_room(&q), "transl-room-1");
    }
}
