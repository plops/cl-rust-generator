//! Anti-Tinder MVP: schlanke, privatsphäre-zentrierte Partnerbörse.
//!
//! Nur Verdrahtung: Module deklarieren (Dateien tragen Nummernpräfixe per
//! `#[path]`, da Rust-Modulnamen nicht mit Ziffern beginnen dürfen),
//! Config laden, Logging starten, DB verbinden, Router bauen, Server starten.

// Modul-Reihenfolge folgt dem logischen Init-/Datenfluss (siehe plan.md).
#[path = "08_ai_check.rs"]
mod ai_check;
#[path = "01_config.rs"]
mod config;
#[path = "02_db.rs"]
mod db;
#[path = "05_handlers_auth.rs"]
mod handlers_auth;
#[path = "06_handlers_profile.rs"]
mod handlers_profile;
#[path = "10_matching.rs"]
mod matching;
#[path = "03_models.rs"]
mod models;
#[path = "09_payments.rs"]
mod payments;
#[path = "12_routes.rs"]
mod routes;
#[path = "04_scoring.rs"]
mod scoring;
#[path = "07_telemetry.rs"]
mod telemetry;
#[path = "11_views.rs"]
mod views;

use std::net::SocketAddr;

use routes::{AppState, build_router};

#[tokio::main]
async fn main() {
    if let Err(e) = run().await {
        eprintln!("fatal: {e}");
        std::process::exit(1);
    }
}

async fn run() -> Result<(), Box<dyn std::error::Error>> {
    dotenvy::dotenv().ok();
    let config = config::Config::from_env().map_err(|e| format!("config: {e}"))?;
    telemetry::init_tracing();

    let pool = db::create_pool(&config.database_url)
        .await
        .map_err(|e| format!("db: {e}"))?;
    db::run_migrations(&pool)
        .await
        .map_err(|e| format!("migrate: {e}"))?;
    tracing::info!("database connected, migrations applied");

    let state = AppState {
        pool: pool.clone(),
        crypto: db::CryptoKey::new(config.data_encryption_key),
        ai: ai_check::AiCheckerKind::from_config(&config),
        config: config.clone(),
    };

    // Täglicher Top-5-Job als Hintergrund-Task (Anti-Swiping).
    tokio::spawn(crate::matching::run_daily_task(
        pool,
        state.crypto.clone(),
        config.match_job_interval_hours,
    ));

    let app = build_router(state);
    let addr = SocketAddr::from(([0, 0, 0, 0], config.port));
    let listener = tokio::net::TcpListener::bind(addr).await?;
    tracing::info!(%addr, "listening");
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .await?;
    Ok(())
}
