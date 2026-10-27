//! 07 — Router: Routen, statische Dateien, Health und JSON-APIs.
//!
//! Routen: `GET /ws` (Relay), `GET /healthz` (`ok`),
//! `GET /api/rooms` (JSON `{raum: [PeerInfo..]}`),
//! `GET /api/languages` (Defaults + Sprachtabelle),
//! `GET /api/config` (`{"server_key": bool}` — ob der Sparmodus ohne
//! eigenen Key moeglich ist), Fallback `ServeDir`.
//! Kein Verhalten ausser Routing; Zustand ist `AppState` aus `06_relay`.

use axum::{Json, Router, extract::State, routing::get};
use serde_json::{Value, json};
use tower_http::services::ServeDir;

use crate::languages::{DEFAULT_LANG, DEFAULT_TARGET, LANGUAGES};
use crate::relay::{AppState, ws_handler};
use crate::voices::{self, VOICES};

/// Router bauen (von `main` serviert und von Tests benutzt).
pub fn router(state: AppState, dir: &str) -> Router {
    Router::new()
        .route("/ws", get(ws_handler))
        .route("/healthz", get(|| async { "ok" }))
        .route("/api/rooms", get(api_rooms))
        .route("/api/languages", get(api_languages))
        .route("/api/config", get(api_config))
        .fallback_service(ServeDir::new(dir).append_index_html_on_directories(true))
        .with_state(state)
}

/// `/api/rooms`: Momentaufnahme Raum → Liste von `PeerInfo` (inkl. Transport).
async fn api_rooms(State(state): State<AppState>) -> Json<Value> {
    Json(json!(state.rooms.snapshot().await))
}

/// `/api/languages`: Defaults, vollstaendige Sprachtabelle (mit Modell-Flags),
/// die 30 Prebuilt-Stimmen und die Default-Stimme je Sprache (als Regel-Abbild:
/// `{code: voiceName|auto}` nur fuer Sprachen mit fester Default-Stimme).
async fn api_languages() -> Json<Value> {
    // Nur Sprachen mit fester Default-Stimme auflisten; alles andere ist
    // `auto` (Browser-Default). Seit „Automatisch fuer alle" leer.
    let mut default_voice = serde_json::Map::new();
    for l in LANGUAGES.iter() {
        let dv = voices::default_voice(l.code);
        if dv != voices::AUTO {
            default_voice.insert(l.code.to_owned(), json!(dv));
        }
    }
    Json(json!({
        "default_lang": DEFAULT_LANG,
        "default_target": DEFAULT_TARGET,
        "languages": LANGUAGES.as_slice(),
        "voices": VOICES.as_slice(),
        "auto": voices::AUTO,
        "default_voice": default_voice,
    }))
}

/// `/api/config`: nur Ja/Nein, ob der Server einen Key hat (nie der Key selbst).
async fn api_config(State(state): State<AppState>) -> Json<Value> {
    Json(json!({ "server_key": state.server_key.is_some() }))
}
