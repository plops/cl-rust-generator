//! 06_handlers_profile: Profil anlegen/bearbeiten + öffentliche Ansicht.
//!
//! Ablauf beim Speichern: Formular validieren → AI-Spam-Check für `bio` →
//! Hidden-Felder verschlüsseln → Upsert. Die öffentliche Ansicht rendert
//! ausschließlich `PublicProfile` — Hidden-Klartexte erreichen nie das HTML.

use axum::extract::{Form, Path, State};
use axum::http::HeaderMap;
use axum::response::{Html, Redirect};
use sqlx::PgPool;
use uuid::Uuid;

use crate::db::{CryptoKey, decrypt_field, encrypt_field};
use crate::handlers_auth::require_paid;
use crate::matching::is_mutual;
use crate::models::{
    MBTI_TYPES, Profile, ProfileForm, PublicProfile, ValidatedProfile, validate_profile_form,
};
use crate::routes::AppState;
use crate::views::{AppError, ProfileFormTemplate, ProfileViewTemplate, SelectOption};
use askama::Template;

const GENDER_OPTIONS: [(&str, &str); 3] = [("w", "weiblich"), ("m", "männlich"), ("d", "divers")];
const FAMILY_OPTIONS: [(&str, &str); 3] = [
    ("kinderwunsch", "Kinderwunsch"),
    ("keine_kinder", "Keine Kinder"),
    ("egal", "Egal"),
];
const EXPECTATION_OPTIONS: [(&str, &str); 4] = [
    ("any", "Egal"),
    ("low", "eher niedrig ok"),
    ("medium", "mittel"),
    ("high", "eher hoch"),
];

/// Baut das Formular-Template aus einem `ProfileForm` (Single Source of Truth).
fn to_template(form: &ProfileForm, error: Option<String>, is_new: bool) -> ProfileFormTemplate {
    let mbti_pairs: Vec<(&str, &str)> = MBTI_TYPES.iter().map(|t| (*t, *t)).collect();
    ProfileFormTemplate {
        error,
        is_new,
        first_name: form.first_name.clone(),
        age: form.age.clone(),
        genders: SelectOption::list(&GENDER_OPTIONS, form.gender.trim()),
        looking_fors: SelectOption::list(&GENDER_OPTIONS, form.looking_for.trim()),
        mbtis: SelectOption::list(&mbti_pairs, &form.mbti.trim().to_uppercase()),
        hobbies: form.hobbies.clone(),
        job_title: form.job_title.clone(),
        family_plans: SelectOption::list(&FAMILY_OPTIONS, form.family_plan.trim()),
        bio: form.bio.clone(),
        photo_url: form.photo_url.clone(),
        income_expectations: SelectOption::list(
            &EXPECTATION_OPTIONS,
            form.income_expectation.trim(),
        ),
    }
}

fn profile_to_form(p: &Profile) -> ProfileForm {
    ProfileForm {
        first_name: p.first_name.clone(),
        age: p.age.to_string(),
        gender: p.gender.clone(),
        looking_for: p.looking_for.clone(),
        mbti: p.mbti.clone(),
        hobbies: p.hobbies.join(", "),
        job_title: p.job_title.clone(),
        family_plan: p.family_plan.clone(),
        bio: p.bio.clone(),
        photo_url: p.photo_url.clone(),
        signal_contact: String::new(),
        income: String::new(),
        wealth: String::new(),
        intimate_prefs: String::new(),
        income_expectation: p.income_expectation.clone(),
    }
}

pub async fn find_profile(pool: &PgPool, user_id: &Uuid) -> Result<Option<Profile>, sqlx::Error> {
    sqlx::query_as::<_, Profile>(
        "SELECT user_id, first_name, age, gender, looking_for, mbti, hobbies, job_title,
                family_plan, bio, photo_url, signal_contact_enc, income_enc, wealth_enc,
                intimate_prefs_enc, income_expectation, updated_at
         FROM profiles WHERE user_id = $1",
    )
    .bind(user_id)
    .fetch_optional(pool)
    .await
}

pub async fn upsert_profile(
    pool: &PgPool,
    user_id: &Uuid,
    v: &ValidatedProfile,
    key: &CryptoKey,
) -> Result<(), AppError> {
    let enc = |s: &str| {
        if s.is_empty() {
            Ok(String::new())
        } else {
            encrypt_field(key, s).map_err(|e| AppError::Internal(format!("encrypt: {e}")))
        }
    };
    sqlx::query(
        "INSERT INTO profiles (user_id, first_name, age, gender, looking_for, mbti, hobbies,
            job_title, family_plan, bio, photo_url, signal_contact_enc, income_enc, wealth_enc,
            intimate_prefs_enc, income_expectation, updated_at)
         VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16, now())
         ON CONFLICT (user_id) DO UPDATE SET
            first_name = $2, age = $3, gender = $4, looking_for = $5, mbti = $6, hobbies = $7,
            job_title = $8, family_plan = $9, bio = $10, photo_url = $11, signal_contact_enc = $12,
            income_enc = $13, wealth_enc = $14, intimate_prefs_enc = $15, income_expectation = $16,
            updated_at = now()",
    )
    .bind(user_id)
    .bind(&v.first_name)
    .bind(v.age)
    .bind(&v.gender)
    .bind(&v.looking_for)
    .bind(&v.mbti)
    .bind(&v.hobbies)
    .bind(&v.job_title)
    .bind(&v.family_plan)
    .bind(&v.bio)
    .bind(&v.photo_url)
    .bind(enc(&v.signal_contact)?)
    .bind(enc(&v.income)?)
    .bind(enc(&v.wealth)?)
    .bind(enc(&v.intimate_prefs)?)
    .bind(&v.income_expectation)
    .execute(pool)
    .await?;
    Ok(())
}

/// GET /profile/edit — Editor (vorausgefüllt, falls Profil existiert).
pub async fn get_profile_edit(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Html<String>, AppError> {
    let user = require_paid(&state, &headers).await?;
    let existing = find_profile(&state.pool, &user.id).await?;
    let tpl = match existing {
        Some(p) => {
            let form = profile_to_form(&p);
            to_template(&form, None, false)
        }
        None => {
            let form = ProfileForm {
                gender: "w".into(),
                looking_for: "m".into(),
                mbti: "INFJ".into(),
                family_plan: "egal".into(),
                income_expectation: "any".into(),
                ..ProfileForm::default()
            };
            to_template(&form, None, true)
        }
    };
    Ok(Html(tpl.render()?))
}

/// POST /profile — validieren, AI-Check, verschlüsselt speichern.
pub async fn post_profile(
    State(state): State<AppState>,
    headers: HeaderMap,
    Form(form): Form<ProfileForm>,
) -> Result<axum::response::Response, AppError> {
    use axum::response::IntoResponse;
    let user = require_paid(&state, &headers).await?;
    let validated = match validate_profile_form(&form) {
        Ok(v) => v,
        Err(errors) => return render_form_error(&form, &errors.join(" ")),
    };
    if !state.ai.check(&validated.bio).await {
        tracing::info!(user_id = %user.id, "profile bio blocked by AI check");
        return render_form_error(
            &form,
            "Der Profiltext wurde als Werbung/Spam erkannt. Bitte ohne Links und Kontaktinfos formulieren.",
        );
    }
    upsert_profile(&state.pool, &user.id, &validated, &state.crypto).await?;
    tracing::info!(user_id = %user.id, "profile saved");
    if is_htmx(&headers) {
        let body = format!(
            "<p><strong>Gespeichert!</strong> <a href=\"/profile/{}\">Profil ansehen</a> · <a href=\"/matches\">Zu den Matches</a></p>",
            user.id
        );
        Ok(Html(body).into_response())
    } else {
        Ok(Redirect::to(&format!("/profile/{}", user.id)).into_response())
    }
}

fn render_form_error(form: &ProfileForm, msg: &str) -> Result<axum::response::Response, AppError> {
    use axum::response::IntoResponse;
    let tpl = to_template(form, Some(msg.to_owned()), false);
    Ok((
        axum::http::StatusCode::UNPROCESSABLE_ENTITY,
        Html(tpl.render()?),
    )
        .into_response())
}

fn is_htmx(headers: &HeaderMap) -> bool {
    headers.contains_key("hx-request") || headers.contains_key("HX-Request")
}

/// GET /profile/:id — öffentliche Ansicht. Hidden-Felder erscheinen NUR als 🔒.
pub async fn get_profile_view(
    State(state): State<AppState>,
    headers: HeaderMap,
    Path(id): Path<Uuid>,
) -> Result<Html<String>, AppError> {
    let viewer = require_paid(&state, &headers).await?;
    let profile = find_profile(&state.pool, &id)
        .await?
        .ok_or(AppError::NotFound)?;
    let public = PublicProfile::from_profile(&profile);
    // Sicherstellen, dass kein Ciphertext versehentlich öffentlich wird:
    debug_assert_no_ciphertext_leak(&public, &profile);
    let mutual = is_mutual(&state.pool, &viewer.id, &id).await?;
    let liked = crate::matching::has_liked(&state.pool, &viewer.id, &id).await?;
    let is_own = viewer.id == id;
    let own_stats = if is_own {
        let member = crate::handlers_auth::find_user_by_id(&state.pool, &id)
            .await?
            .map(|u| u.created_at.format("%d.%m.%Y").to_string())
            .unwrap_or_default();
        Some(format!(
            "Dabei seit {} · Profilstand {}",
            member,
            profile.updated_at.format("%d.%m.%Y %H:%M")
        ))
    } else {
        None
    };
    let tpl = ProfileViewTemplate {
        profile: public,
        is_own,
        is_mutual: mutual,
        liked_by_viewer: liked,
        own_stats,
    };
    Ok(Html(tpl.render()?))
}

/// Dev-Guard: Ciphertext darf nie in öffentlichen Structs landen.
fn debug_assert_no_ciphertext_leak(public: &PublicProfile, raw: &Profile) {
    let json = serde_json::to_string(public).unwrap_or_default();
    for enc in [
        &raw.signal_contact_enc,
        &raw.income_enc,
        &raw.wealth_enc,
        &raw.intimate_prefs_enc,
    ] {
        if !enc.is_empty() {
            debug_assert!(!json.contains(enc), "ciphertext leak into public profile");
        }
    }
}

/// Entschlüsselt eine Band-Stufe ("low|medium|high"), Fallback "medium".
pub fn decrypt_band(key: &CryptoKey, enc: &str) -> String {
    if enc.is_empty() {
        return "medium".to_owned();
    }
    match decrypt_field(key, enc) {
        Ok(v) => {
            let v = v.trim().to_lowercase();
            if ["low", "medium", "high"].contains(&v.as_str()) {
                v
            } else {
                "medium".to_owned()
            }
        }
        Err(e) => {
            tracing::warn!(error = %e, "band decrypt failed, using neutral");
            "medium".to_owned()
        }
    }
}
