//! 12_routes: Router-Aufbau, App-State, Rate-Limit, Static-Files.
//!
//! Rate-Limit via `tower-governor` (Peer-IP; großzügig für MVP/E2E).
//! WICHTIG: `into_make_service_with_connect_info::<SocketAddr>()` in main,
//! sonst fehlt die Client-IP für das Limit.

use axum::Router;
use axum::extract::State;
use axum::http::HeaderMap;
use axum::response::Html;
use axum::routing::{get, post};
use tower_governor::GovernorLayer;
use tower_governor::governor::GovernorConfigBuilder;
use tower_http::services::ServeDir;
use tower_http::trace::TraceLayer;

use crate::ai_check::AiCheckerKind;
use crate::config::Config;
use crate::db::CryptoKey;
use crate::views::{AppError, IndexTemplate};
use askama::Template;

#[derive(Clone)]
pub struct AppState {
    pub pool: sqlx::PgPool,
    pub config: Config,
    pub crypto: CryptoKey,
    pub ai: AiCheckerKind,
}

pub fn build_router(state: AppState) -> Router {
    // Governor-Periode = 1s / rps (Builder-`per_second` wäre 1 Token/N Sekunden!).
    // Defaults: 10 echte req/s, Burst 30 — schützt vor Abuse.
    let period =
        std::time::Duration::from_nanos(1_000_000_000 / state.config.rate_limit_per_second);
    let governor = GovernorConfigBuilder::default()
        .period(period)
        .burst_size(state.config.rate_limit_burst)
        .finish()
        .expect("governor config");

    Router::new()
        .route("/", get(index))
        .route(
            "/register",
            get(crate::handlers_auth::get_register).post(crate::handlers_auth::post_register),
        )
        .route(
            "/login",
            get(crate::handlers_auth::get_login).post(crate::handlers_auth::post_login),
        )
        .route("/logout", post(crate::handlers_auth::post_logout))
        .route("/pay/checkout", get(crate::payments::get_checkout))
        .route("/pay/confirm", post(crate::payments::post_confirm))
        .route("/pay/success", get(crate::payments::get_success))
        .route("/pay/cancel", get(crate::payments::get_cancel))
        .route(
            "/profile/edit",
            get(crate::handlers_profile::get_profile_edit),
        )
        .route("/profile", post(crate::handlers_profile::post_profile))
        .route(
            "/profile/{id}",
            get(crate::handlers_profile::get_profile_view),
        )
        .route("/matches", get(crate::matching::get_matches))
        .route("/matches/list", get(crate::matching::get_matches_list))
        .route("/matches/recompute", post(crate::matching::post_recompute))
        .route("/like/{id}", post(crate::matching::post_like))
        .route("/mutual/{id}", get(crate::matching::get_mutual))
        .route("/health", get(|| async { "ok" }))
        .nest_service("/static", ServeDir::new("static"))
        .layer(GovernorLayer::new(governor))
        .layer(TraceLayer::new_for_http())
        .with_state(state)
}

async fn index(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Html<String>, AppError> {
    // Eingeloggte + Zahlende direkt zu den Matches schicken.
    if let Ok(user) = crate::handlers_auth::require_login(&state, &headers).await
        && user.paid
    {
        return Err(AppError::SeeOther("/matches"));
    }
    Ok(Html(IndexTemplate.render()?))
}
