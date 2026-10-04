//! 07_telemetry: strukturiertes Logging via `tracing`.
//!
//! MVP-Format: lesbares Pretty-Logging in Dev, JSON via `LOG_FORMAT=json`.

use tracing_subscriber::EnvFilter;

/// Einmalig beim Serverstart aufrufen. Ist idempotent (`try_init`).
pub fn init_tracing() {
    let filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("dating_mvp=debug"));
    let json = std::env::var("LOG_FORMAT").is_ok_and(|v| v.eq_ignore_ascii_case("json"));
    if json {
        let _ = tracing_subscriber::fmt()
            .json()
            .with_env_filter(filter)
            .try_init();
    } else {
        let _ = tracing_subscriber::fmt().with_env_filter(filter).try_init();
    }
}
