//! live-silk — Server-Einstieg (nur Verdrahtung).
//!
//! `Serve`: Raumbuch + `AppState` bauen, Router servieren.
//! `Check`/`Watch`: Status-TUI aus `08_status`.
//! Logt beim Start, OB ein Server-Key vorhanden ist (ja/nein, nie der Wert).

use std::sync::Arc;

use live_silk::{
    config::{Action, Config, MAX_PEERS},
    relay::AppState,
    serve, state, status,
};

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let (cfg, action) = match Config::from_args(&args) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(2);
        }
    };

    // Eigene Ziele ausfuehrlich (mit --verbose), Fremd-Crates immer ruhig.
    // EnvFilter versteht zusaetzlich RUST_LOG (z. B. RUST_LOG=debug).
    let dirs = if cfg.verbose {
        "warn,live_silk=debug"
    } else {
        "warn,live_silk=info"
    };
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
    let filter = tracing_subscriber::EnvFilter::new(dirs);
    tracing_subscriber::fmt()
        .with_target(false)
        .with_max_level(tracing::Level::TRACE)
        .finish()
        .with(filter)
        .init();

    match action {
        Action::Serve => serve(cfg).await,
        Action::Check { base } => status::check(&base),
        Action::Watch { base } => status::watch(&base),
    }
}

/// Server starten: Raumbuch, `AppState`, `axum::serve`.
async fn serve(cfg: Config) {
    // Key nur als "vorhanden ja/nein" loggen, nie den Wert.
    let server_key = cfg.server_key();
    tracing::info!(
        key_vorhanden = server_key.is_some(),
        "server-key aus --key-file"
    );

    let rooms = Arc::new(state::Rooms::new(MAX_PEERS));
    let app_state = AppState {
        rooms,
        gemini_url: cfg.gemini_url.clone(),
        server_key,
    };
    let app = serve::router(app_state, &cfg.dir);

    let listener = tokio::net::TcpListener::bind(cfg.listen)
        .await
        .expect("bind");
    tracing::info!(listen = %cfg.listen, "live-silk hoert");
    eprintln!("live-silk hoert auf {}", cfg.listen);
    axum::serve(listener, app).await.expect("serve");
}
