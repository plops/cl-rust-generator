//! 09_payments: Einmalgebühr (10 €) als Bot-Hürde — Stripe-Mock.
//!
//! MVP-Flow (Testmodus-kompatibel benannt):
//! `GET /pay/checkout` (Mock-Checkoutseite) → `POST /pay/confirm`
//! (Zahlung simulieren) → `paid=true` → Redirect zum Profil.
//! Mit `STRIPE_SECRET_KEY` wäre hier der echte Checkout anzubinden
//! (Stub: `use_real_stripe`, dann weiterhin Mock-Seite + Hinweis).

use axum::extract::{Query, State};
use axum::http::HeaderMap;
use axum::response::{Html, Redirect};
use serde::Deserialize;
use uuid::Uuid;

use crate::handlers_auth::{mark_user_paid, require_login};
use crate::routes::AppState;
use crate::views::{AppError, PayCheckoutTemplate, PayResultTemplate};
use askama::Template;

pub const PRICE_LABEL: &str = "10,00 € einmalig";

#[derive(Debug, Clone)]
pub struct CheckoutSession {
    pub id: String,
}

pub fn create_mock_checkout(user_id: &Uuid) -> CheckoutSession {
    CheckoutSession {
        id: format!("cs_mock_{}", user_id.as_simple()),
    }
}

pub fn is_valid_mock_session(session_id: &str, user_id: &Uuid) -> bool {
    session_id == create_mock_checkout(user_id).id
}

#[derive(Deserialize)]
pub struct CheckoutQuery {
    pub session: Option<String>,
}

fn checkout_html(
    email: &str,
    session_id: &str,
    price_id: &str,
    real_stripe: bool,
) -> Result<Html<String>, AppError> {
    let tpl = PayCheckoutTemplate {
        email: email.to_owned(),
        session_id: session_id.to_owned(),
        price_label: PRICE_LABEL.to_owned(),
        price_id: price_id.to_owned(),
        real_stripe,
    };
    Ok(Html(tpl.render()?))
}

/// GET /pay/checkout — zeigt die Mock-Checkoutseite (oder Success bei `paid`).
pub async fn get_checkout(
    State(state): State<AppState>,
    headers: HeaderMap,
    Query(query): Query<CheckoutQuery>,
) -> Result<Html<String>, AppError> {
    let user = require_login(&state, &headers).await?;
    // Falls bereits bezahlt: direkt weiter.
    if user.paid {
        return Err(AppError::SeeOther("/profile/edit"));
    }
    let session = create_mock_checkout(&user.id);
    if let Some(given) = query.session
        && given != session.id
    {
        return Err(AppError::BadRequest("Ungültige Checkout-Session.".into()));
    }
    checkout_html(
        &user.email,
        &session.id,
        &state.config.stripe_price_id,
        state.config.use_real_stripe(),
    )
}

/// POST /pay/confirm — simuliert die erfolgreiche Zahlung.
pub async fn post_confirm(
    State(state): State<AppState>,
    headers: HeaderMap,
    axum::extract::Form(form): axum::extract::Form<ConfirmForm>,
) -> Result<Redirect, AppError> {
    let user = require_login(&state, &headers).await?;
    if !is_valid_mock_session(&form.session, &user.id) {
        return Err(AppError::BadRequest("Ungültige Checkout-Session.".into()));
    }
    mark_user_paid(&state.pool, &user.id, &form.session).await?;
    tracing::info!(user_id = %user.id, "mock payment confirmed");
    Ok(Redirect::to("/pay/success"))
}

#[derive(Deserialize)]
pub struct ConfirmForm {
    pub session: String,
}

/// GET /pay/success — Bestätigung nach Zahlung.
pub async fn get_success(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Html<String>, AppError> {
    let _ = require_login(&state, &headers).await?;
    let tpl = PayResultTemplate {
        success: true,
        message: "Zahlung erfolgreich! Du kannst jetzt dein Profil anlegen.".to_owned(),
    };
    Ok(Html(tpl.render()?))
}

/// GET /pay/cancel — Abbruchseite.
pub async fn get_cancel() -> Result<Html<String>, AppError> {
    let tpl = PayResultTemplate {
        success: false,
        message: "Zahlung abgebrochen. Ohne die Einmalgebühr ist kein Zugang möglich.".to_owned(),
    };
    Ok(Html(tpl.render()?))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mock_session_roundtrip() {
        let id = Uuid::new_v4();
        let session = create_mock_checkout(&id);
        assert!(session.id.starts_with("cs_mock_"));
        assert!(is_valid_mock_session(&session.id, &id));
        assert!(!is_valid_mock_session(&session.id, &Uuid::new_v4()));
        assert!(!is_valid_mock_session("cs_mock_garbage", &id));
    }
}
