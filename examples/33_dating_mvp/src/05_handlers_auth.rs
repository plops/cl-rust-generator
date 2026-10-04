//! 05_handlers_auth: Registrierung, Login/Logout, Cookie-Session.
//!
//! Session: minimales HMAC-SHA256-signiertes Cookie (`dating_session`),
//! keine Session-Tabelle nötig. Passwörter: Argon2-Hash.
//!
//! Nach Registrierung (`paid=false`) geht es zur Paywall (`/pay/checkout`);
//! erst nach Zahlung sind Profil/Matches zugänglich (`require_paid`).

use argon2::{Argon2, PasswordHash, PasswordHasher, PasswordVerifier};
use axum::extract::{Form, State};
use axum::http::{HeaderMap, HeaderValue, header};
use axum::response::{Html, Redirect};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD as B64U};
use hmac::{Hmac, KeyInit, Mac};
use sha2::Sha256;
use sqlx::PgPool;
use uuid::Uuid;

use crate::models::{LoginForm, MIN_PASSWORD_LEN, RegisterForm, User, is_valid_email};
use crate::routes::AppState;
use crate::views::{AppError, LoginTemplate, RegisterTemplate};
use askama::Template;

pub const SESSION_COOKIE: &str = "dating_session";

type HmacSha256 = Hmac<Sha256>;

/// Kodiert `user_id` als `base64(id).base64(hmac)`-Cookie-Wert.
pub fn encode_session(user_id: &Uuid, secret: &str) -> String {
    let id_part = B64U.encode(user_id.as_bytes());
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).expect("hmac key");
    mac.update(id_part.as_bytes());
    let sig_part = B64U.encode(mac.finalize().into_bytes());
    format!("{id_part}.{sig_part}")
}

/// Verifiziert das Cookie und gibt die User-ID zurück (None = ungültig).
pub fn decode_session(value: &str, secret: &str) -> Option<Uuid> {
    let (id_part, sig_part) = value.split_once('.')?;
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).ok()?;
    mac.update(id_part.as_bytes());
    let expected = B64U.encode(mac.finalize().into_bytes());
    // Konstanter Vergleich wäre ideal; für MVP reicht der HMAC-Schutz.
    if subtle_eq(&expected, sig_part) {
        let bytes = B64U.decode(id_part).ok()?;
        Uuid::from_slice(&bytes).ok()
    } else {
        None
    }
}

fn subtle_eq(a: &str, b: &str) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.bytes()
        .zip(b.bytes())
        .fold(0u8, |acc, (x, y)| acc | (x ^ y))
        == 0
}

fn session_cookie_value(headers: &HeaderMap) -> Option<String> {
    let cookie = headers.get(header::COOKIE)?.to_str().ok()?;
    cookie
        .split(';')
        .filter_map(|part| {
            let (k, v) = part.trim().split_once('=')?;
            (k == SESSION_COOKIE).then(|| v.trim().to_owned())
        })
        .next()
}

fn set_session_cookie(headers: &mut HeaderMap, value: &str) {
    let cookie = format!("{SESSION_COOKIE}={value}; Path=/; HttpOnly; SameSite=Lax");
    headers.insert(
        header::SET_COOKIE,
        HeaderValue::from_str(&cookie).expect("cookie"),
    );
}

fn clear_session_cookie(headers: &mut HeaderMap) {
    let cookie = format!("{SESSION_COOKIE}=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax");
    headers.insert(
        header::SET_COOKIE,
        HeaderValue::from_str(&cookie).expect("cookie"),
    );
}

/// Aktuell eingeloggter User (ohne Paywall-Prüfung).
pub async fn require_login(state: &AppState, headers: &HeaderMap) -> Result<User, AppError> {
    let id = session_cookie_value(headers)
        .and_then(|v| decode_session(&v, &state.config.session_secret))
        .ok_or(AppError::Unauthorized)?;
    find_user_by_id(&state.pool, &id)
        .await?
        .ok_or(AppError::Unauthorized)
}

/// Eingeloggter UND bezahlter User (für Profil/Matches/Likes).
pub async fn require_paid(state: &AppState, headers: &HeaderMap) -> Result<User, AppError> {
    let user = require_login(state, headers).await?;
    if user.paid {
        Ok(user)
    } else {
        Err(AppError::PaymentRequired)
    }
}

pub fn hash_password(password: &str) -> Result<String, AppError> {
    // password-hash 0.6: Salt wird automatisch per getrandom erzeugt.
    Argon2::default()
        .hash_password(password.as_bytes())
        .map(|h| h.to_string())
        .map_err(|e| AppError::Internal(format!("hashing failed: {e}")))
}

pub fn verify_password(hash: &str, password: &str) -> bool {
    let Ok(parsed) = PasswordHash::new(hash) else {
        return false;
    };
    Argon2::default()
        .verify_password(password.as_bytes(), &parsed)
        .is_ok()
}

// --- DB-Helfer (laufzeitgeprüfte Queries: offline baut ohne DB) ---

pub async fn create_user(pool: &PgPool, email: &str, hash: &str) -> Result<User, sqlx::Error> {
    sqlx::query_as::<_, User>(
        "INSERT INTO users (id, email, password_hash) VALUES ($1, $2, $3)
         RETURNING id, email, password_hash, created_at, paid, stripe_session_id",
    )
    .bind(Uuid::new_v4())
    .bind(email.trim().to_lowercase())
    .bind(hash)
    .fetch_one(pool)
    .await
}

pub async fn find_user_by_email(pool: &PgPool, email: &str) -> Result<Option<User>, sqlx::Error> {
    sqlx::query_as::<_, User>(
        "SELECT id, email, password_hash, created_at, paid, stripe_session_id
         FROM users WHERE email = $1",
    )
    .bind(email.trim().to_lowercase())
    .fetch_optional(pool)
    .await
}

pub async fn find_user_by_id(pool: &PgPool, id: &Uuid) -> Result<Option<User>, sqlx::Error> {
    sqlx::query_as::<_, User>(
        "SELECT id, email, password_hash, created_at, paid, stripe_session_id
         FROM users WHERE id = $1",
    )
    .bind(id)
    .fetch_optional(pool)
    .await
}

pub async fn mark_user_paid(pool: &PgPool, id: &Uuid, session_id: &str) -> Result<(), sqlx::Error> {
    sqlx::query("UPDATE users SET paid = TRUE, stripe_session_id = $2 WHERE id = $1")
        .bind(id)
        .bind(session_id)
        .execute(pool)
        .await?;
    Ok(())
}

// --- Handler ---

pub async fn get_register() -> Result<Html<String>, AppError> {
    let tpl = RegisterTemplate { error: None };
    Ok(Html(tpl.render()?))
}

pub async fn post_register(
    State(state): State<AppState>,
    Form(form): Form<RegisterForm>,
) -> Result<(HeaderMap, Redirect), AppError> {
    let email = form.email.trim().to_lowercase();
    if !is_valid_email(&email) {
        return render_register_error("Bitte eine gültige E-Mail-Adresse angeben.");
    }
    if form.password.len() < MIN_PASSWORD_LEN {
        return render_register_error("Das Passwort muss mindestens 8 Zeichen haben.");
    }
    if find_user_by_email(&state.pool, &email).await?.is_some() {
        return render_register_error("Diese E-Mail ist bereits registriert. Bitte einloggen.");
    }
    let hash = hash_password(&form.password)?;
    let user = create_user(&state.pool, &email, &hash).await?;
    tracing::info!(user_id = %user.id, "user registered, redirecting to paywall");
    let mut headers = HeaderMap::new();
    set_session_cookie(
        &mut headers,
        &encode_session(&user.id, &state.config.session_secret),
    );
    Ok((headers, Redirect::to("/pay/checkout")))
}

fn render_register_error(msg: &str) -> Result<(HeaderMap, Redirect), AppError> {
    Err(AppError::BadRequest(format!(
        "Registrierung fehlgeschlagen: {msg} <a href=\"/register\">Zurück</a>"
    )))
}

pub async fn get_login() -> Result<Html<String>, AppError> {
    let tpl = LoginTemplate { error: None };
    Ok(Html(tpl.render()?))
}

pub async fn post_login(
    State(state): State<AppState>,
    Form(form): Form<LoginForm>,
) -> Result<(HeaderMap, Redirect), AppError> {
    let user = find_user_by_email(&state.pool, &form.email)
        .await?
        .filter(|u| verify_password(&u.password_hash, &form.password));
    let Some(user) = user else {
        return Err(AppError::BadRequest(
            "Login fehlgeschlagen: E-Mail oder Passwort falsch. <a href=\"/login\">Zurück</a>"
                .into(),
        ));
    };
    let mut headers = HeaderMap::new();
    set_session_cookie(
        &mut headers,
        &encode_session(&user.id, &state.config.session_secret),
    );
    let target = if user.paid {
        "/matches"
    } else {
        "/pay/checkout"
    };
    Ok((headers, Redirect::to(target)))
}

pub async fn post_logout() -> (HeaderMap, Redirect) {
    let mut headers = HeaderMap::new();
    clear_session_cookie(&mut headers);
    (headers, Redirect::to("/"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_cookie_roundtrip() {
        let id = Uuid::new_v4();
        let secret = "test-secret";
        let encoded = encode_session(&id, secret);
        assert_eq!(decode_session(&encoded, secret), Some(id));
    }

    #[test]
    fn session_tamper_and_wrong_secret_fail() {
        let id = Uuid::new_v4();
        let encoded = encode_session(&id, "secret-a");
        assert_eq!(decode_session(&encoded, "secret-b"), None);
        let tampered = format!("{encoded}x");
        assert_eq!(decode_session(&tampered, "secret-a"), None);
        assert_eq!(decode_session("garbage", "secret-a"), None);
    }

    #[test]
    fn password_hash_verify_roundtrip() {
        let hash = hash_password("sicheres-passwort-123").unwrap();
        assert_ne!(hash, "sicheres-passwort-123");
        assert!(verify_password(&hash, "sicheres-passwort-123"));
        assert!(!verify_password(&hash, "falsches-passwort"));
        assert!(!verify_password("kein-hash", "x"));
    }
}
