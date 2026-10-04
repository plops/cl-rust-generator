//! 11_views: Askama-Template-Structs und der zentrale `AppError`-Typ.
//!
//! Integration nach Askama-0.13+-Empfehlung: manuell `render()` + `Html()`.
//! Jedes Template bekommt genau die Daten, die es anzeigen darf —
//! `ProfileViewTemplate` erhält z. B. nur `PublicProfile` (Tresor-Prinzip).

use askama::Template;
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Redirect, Response};
use thiserror::Error;

use crate::models::PublicProfile;

#[derive(Debug, Error)]
pub enum AppError {
    #[error("database error: {0}")]
    Db(#[from] sqlx::Error),
    #[error("template error: {0}")]
    Render(#[from] askama::Error),
    #[error("login required")]
    Unauthorized,
    #[error("payment required")]
    PaymentRequired,
    #[error("redirect to {0}")]
    SeeOther(&'static str),
    #[error("not found")]
    NotFound,
    #[error("bad request")]
    BadRequest(String),
    #[error("internal error: {0}")]
    Internal(String),
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        match self {
            Self::Unauthorized => Redirect::to("/login").into_response(),
            Self::PaymentRequired => Redirect::to("/pay/checkout").into_response(),
            Self::SeeOther(target) => Redirect::to(target).into_response(),
            Self::NotFound => {
                let tpl = ErrorTemplate {
                    status: 404,
                    message: "Seite nicht gefunden.",
                };
                (
                    StatusCode::NOT_FOUND,
                    Html(tpl.render().unwrap_or_default()),
                )
                    .into_response()
            }
            Self::BadRequest(msg) => {
                let tpl = ErrorTemplate {
                    status: 400,
                    message: &msg,
                };
                (
                    StatusCode::BAD_REQUEST,
                    Html(tpl.render().unwrap_or_default()),
                )
                    .into_response()
            }
            Self::Db(e) => {
                tracing::error!(error = %e, "database error");
                let tpl = ErrorTemplate {
                    status: 500,
                    message: "Datenbankfehler. Bitte später erneut versuchen.",
                };
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Html(tpl.render().unwrap_or_default()),
                )
                    .into_response()
            }
            Self::Render(e) => {
                tracing::error!(error = %e, "template error");
                (StatusCode::INTERNAL_SERVER_ERROR, "Darstellungsfehler.").into_response()
            }
            Self::Internal(msg) => {
                tracing::error!(msg = %msg, "internal error");
                (StatusCode::INTERNAL_SERVER_ERROR, "Interner Fehler.").into_response()
            }
        }
    }
}

#[derive(Template)]
#[template(path = "index.html")]
pub struct IndexTemplate;

#[derive(Template)]
#[template(path = "register.html")]
pub struct RegisterTemplate {
    pub error: Option<String>,
}

#[derive(Template)]
#[template(path = "login.html")]
pub struct LoginTemplate {
    pub error: Option<String>,
}

/// Eine `<select>`-Option. `selected` wird in Rust vorberechnet, damit
/// Templates keine String-Vergleiche brauchen (Askama-`==`-Limitation).
#[derive(Debug, Clone)]
pub struct SelectOption {
    pub value: String,
    pub label: String,
    pub selected: bool,
}

impl SelectOption {
    pub fn list(options: &[(&str, &str)], current: &str) -> Vec<Self> {
        options
            .iter()
            .map(|(value, label)| Self {
                value: (*value).to_owned(),
                label: (*label).to_owned(),
                selected: *value == current,
            })
            .collect()
    }
}

#[derive(Template)]
#[template(path = "profile_form.html")]
pub struct ProfileFormTemplate {
    pub error: Option<String>,
    pub is_new: bool,
    pub first_name: String,
    pub age: String,
    pub genders: Vec<SelectOption>,
    pub looking_fors: Vec<SelectOption>,
    pub mbtis: Vec<SelectOption>,
    pub hobbies: String,
    pub job_title: String,
    pub family_plans: Vec<SelectOption>,
    pub bio: String,
    pub photo_url: String,
    pub income_expectations: Vec<SelectOption>,
}

#[derive(Template)]
#[template(path = "profile_view.html")]
pub struct ProfileViewTemplate {
    pub profile: PublicProfile,
    pub is_own: bool,
    pub is_mutual: bool,
    pub liked_by_viewer: bool,
    /// Nur fürs eigene Profil: "Dabei seit … · Stand …".
    pub own_stats: Option<String>,
}

#[derive(Template)]
#[template(path = "matches.html")]
pub struct MatchesTemplate;

#[derive(Debug, Clone)]
pub struct MatchCard {
    pub user_id: String,
    pub first_name: String,
    pub age: i32,
    pub mbti: String,
    pub job_title: String,
    pub score: i32,
}

#[derive(Template)]
#[template(path = "matches_list.html")]
pub struct MatchesListTemplate {
    pub matches: Vec<MatchCard>,
    pub computed_today: bool,
}

#[derive(Template)]
#[template(path = "mutual.html")]
pub struct MutualTemplate {
    pub other_name: String,
    pub my_signal: String,
    pub their_signal: String,
}

#[derive(Template)]
#[template(path = "pay_checkout.html")]
pub struct PayCheckoutTemplate {
    pub email: String,
    pub session_id: String,
    pub price_label: String,
    pub price_id: String,
    pub real_stripe: bool,
}

#[derive(Template)]
#[template(path = "pay_result.html")]
pub struct PayResultTemplate {
    pub success: bool,
    pub message: String,
}

#[derive(Template)]
#[template(path = "error.html")]
pub struct ErrorTemplate<'a> {
    pub status: u16,
    pub message: &'a str,
}
