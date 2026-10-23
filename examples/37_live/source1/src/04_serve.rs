//! 04 — Verdrahtung zum Netz: Router, statische Dateien, Health, API.
//!
//! Routen: `GET /` (statisch), `GET /ws?room=`, `GET /healthz`,
//! `GET /api/rooms`, `GET /api/modes`. Kein Verhalten ausser Routing.

use std::sync::Arc;

use axum::{Json, Router, extract::State, routing::get};
use serde_json::{Value, json};
use tower_http::services::ServeDir;

use crate::{config::MODES, relay::ws_handler, state::Rooms};

/// Router bauen (wird von `main` serviert und von Tests benutzt).
pub fn router(rooms: Arc<Rooms>, dir: &str) -> Router {
    Router::new()
        .route("/ws", get(ws_handler))
        .route("/healthz", get(|| async { "ok" }))
        .route("/api/rooms", get(api_rooms))
        .route("/api/modes", get(api_modes))
        .fallback_service(ServeDir::new(dir).append_index_html_on_directories(true))
        .with_state(rooms)
}

async fn api_rooms(State(rooms): State<Arc<Rooms>>) -> Json<Value> {
    Json(json!(rooms.snapshot().await))
}

async fn api_modes() -> Json<Value> {
    Json(json!(
        MODES
            .iter()
            .map(|m| json!({"id": m.id, "label_de": m.label_de, "voice": m.voice}))
            .collect::<Vec<_>>()
    ))
}
