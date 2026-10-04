//! 03_models: User, Profile, Sichtbarkeits-Flags und Formular-Validierung.
//!
//! Regel: `PublicProfile` enthält NIEMALS verschlüsselte Hidden-Felder —
//! weder als Klartext noch als Ciphertext. Die UI zeigt dafür ein 🔒-Symbol.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

pub const MBTI_TYPES: [&str; 16] = [
    "INTJ", "INTP", "ENTJ", "ENTP", "INFJ", "INFP", "ENFJ", "ENFP", "ISTJ", "ISFJ", "ESTJ", "ESFJ",
    "ISTP", "ISFP", "ESTP", "ESFP",
];

pub const GENDERS: [&str; 3] = ["w", "m", "d"];
pub const FAMILY_PLANS: [&str; 3] = ["kinderwunsch", "keine_kinder", "egal"];
pub const BANDS: [&str; 3] = ["low", "medium", "high"];
pub const EXPECTATIONS: [&str; 4] = ["low", "medium", "high", "any"];

pub const MIN_AGE: i32 = 18;
pub const MAX_AGE: i32 = 120;
pub const MAX_BIO_LEN: usize = 500;
pub const MIN_PASSWORD_LEN: usize = 8;

#[derive(Debug, Clone, sqlx::FromRow)]
pub struct User {
    pub id: Uuid,
    pub email: String,
    pub password_hash: String,
    pub created_at: DateTime<Utc>,
    pub paid: bool,
    /// Stripe-Checkout-Referenz (Audit/Webhooks); derzeit nur geschrieben.
    #[allow(dead_code)]
    pub stripe_session_id: Option<String>,
}

#[derive(Debug, Clone, sqlx::FromRow)]
pub struct Profile {
    pub user_id: Uuid,
    pub first_name: String,
    pub age: i32,
    pub gender: String,
    pub looking_for: String,
    pub mbti: String,
    pub hobbies: Vec<String>,
    pub job_title: String,
    pub family_plan: String,
    pub bio: String,
    pub photo_url: String,
    pub signal_contact_enc: String,
    pub income_enc: String,
    pub wealth_enc: String,
    pub intimate_prefs_enc: String,
    pub income_expectation: String,
    pub updated_at: DateTime<Utc>,
}

/// Nur öffentliche Felder. Konstruierbar ausschließlich via `from_profile`,
/// damit Hidden-Felder nicht versehentlich in Templates landen.
#[derive(Debug, Clone, Serialize)]
pub struct PublicProfile {
    pub user_id: Uuid,
    pub first_name: String,
    pub age: i32,
    pub gender: String,
    pub mbti: String,
    pub hobbies: Vec<String>,
    pub job_title: String,
    pub family_plan: String,
    pub bio: String,
    pub photo_url: String,
    /// true, wenn Hidden-Felder hinterlegt sind (UI zeigt 🔒 Tresor-Symbol).
    pub has_vault: bool,
}

impl PublicProfile {
    pub fn from_profile(p: &Profile) -> Self {
        let has_vault = !p.signal_contact_enc.is_empty()
            || !p.income_enc.is_empty()
            || !p.wealth_enc.is_empty()
            || !p.intimate_prefs_enc.is_empty();
        Self {
            user_id: p.user_id,
            first_name: p.first_name.clone(),
            age: p.age,
            gender: p.gender.clone(),
            mbti: p.mbti.clone(),
            hobbies: p.hobbies.clone(),
            job_title: p.job_title.clone(),
            family_plan: p.family_plan.clone(),
            bio: p.bio.clone(),
            photo_url: p.photo_url.clone(),
            has_vault,
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct RegisterForm {
    pub email: String,
    pub password: String,
}

#[derive(Debug, Deserialize)]
pub struct LoginForm {
    pub email: String,
    pub password: String,
}

/// Alle Felder als String: freundliche Fehlermeldungen statt 422-Abbruch.
#[derive(Debug, Deserialize, Clone, Default)]
pub struct ProfileForm {
    pub first_name: String,
    pub age: String,
    pub gender: String,
    pub looking_for: String,
    pub mbti: String,
    pub hobbies: String,
    pub job_title: String,
    pub family_plan: String,
    pub bio: String,
    pub photo_url: String,
    pub signal_contact: String,
    pub income: String,
    pub wealth: String,
    pub intimate_prefs: String,
    pub income_expectation: String,
}

#[derive(Debug, Clone)]
pub struct ValidatedProfile {
    pub first_name: String,
    pub age: i32,
    pub gender: String,
    pub looking_for: String,
    pub mbti: String,
    pub hobbies: Vec<String>,
    pub job_title: String,
    pub family_plan: String,
    pub bio: String,
    pub photo_url: String,
    pub signal_contact: String,
    pub income: String,
    pub wealth: String,
    pub intimate_prefs: String,
    pub income_expectation: String,
}

pub fn is_valid_email(email: &str) -> bool {
    let email = email.trim();
    match email.split_once('@') {
        Some((local, domain)) => {
            !local.is_empty() && domain.contains('.') && !domain.starts_with('.')
        }
        None => false,
    }
}

pub fn parse_hobbies(raw: &str) -> Vec<String> {
    raw.split(',')
        .map(|h| h.trim().to_lowercase())
        .filter(|h| !h.is_empty())
        .collect()
}

/// Validiert das Profilformular. Gibt bei Fehlern ALLE Probleme zurück.
pub fn validate_profile_form(form: &ProfileForm) -> Result<ValidatedProfile, Vec<String>> {
    let mut errors = Vec::new();

    let first_name = form.first_name.trim().to_owned();
    if first_name.is_empty() {
        errors.push("Bitte einen Vornamen angeben.".to_owned());
    }

    let age: i32 = form.age.trim().parse().unwrap_or(-1);
    if !(MIN_AGE..=MAX_AGE).contains(&age) {
        errors.push(format!(
            "Das Alter muss zwischen {MIN_AGE} und {MAX_AGE} liegen."
        ));
    }

    let gender = form.gender.trim().to_lowercase();
    if !GENDERS.contains(&gender.as_str()) {
        errors.push("Bitte ein Geschlecht wählen (w/m/d).".to_owned());
    }
    let looking_for = form.looking_for.trim().to_lowercase();
    if !GENDERS.contains(&looking_for.as_str()) {
        errors.push("Bitte wählen, wen du suchst (w/m/d).".to_owned());
    }

    let mbti = form.mbti.trim().to_uppercase();
    if !MBTI_TYPES.contains(&mbti.as_str()) {
        errors.push("Bitte einen gültigen MBTI-Typ wählen.".to_owned());
    }

    let family_plan = form.family_plan.trim().to_lowercase();
    if !FAMILY_PLANS.contains(&family_plan.as_str()) {
        errors.push("Bitte eine Familienplanung wählen.".to_owned());
    }

    let bio = form.bio.trim().to_owned();
    if bio.chars().count() > MAX_BIO_LEN {
        errors.push(format!(
            "Der Profiltext darf max. {MAX_BIO_LEN} Zeichen haben."
        ));
    }

    let income = form.income.trim().to_lowercase();
    if !income.is_empty() && !BANDS.contains(&income.as_str()) {
        errors.push("Bitte eine Einkommensstufe wählen.".to_owned());
    }
    let wealth = form.wealth.trim().to_lowercase();
    if !wealth.is_empty() && !BANDS.contains(&wealth.as_str()) {
        errors.push("Bitte eine Vermögensstufe wählen.".to_owned());
    }
    let income_expectation = form.income_expectation.trim().to_lowercase();
    if !EXPECTATIONS.contains(&income_expectation.as_str()) {
        errors.push("Bitte eine Einkommenserwartung wählen.".to_owned());
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(ValidatedProfile {
        first_name,
        age,
        gender,
        looking_for,
        mbti,
        hobbies: parse_hobbies(&form.hobbies),
        job_title: form.job_title.trim().to_owned(),
        family_plan,
        bio,
        photo_url: form.photo_url.trim().to_owned(),
        signal_contact: form.signal_contact.trim().to_owned(),
        income,
        wealth,
        intimate_prefs: form.intimate_prefs.trim().to_owned(),
        income_expectation,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid_form() -> ProfileForm {
        ProfileForm {
            first_name: "Alex".into(),
            age: "30".into(),
            gender: "w".into(),
            looking_for: "m".into(),
            mbti: "INFJ".into(),
            hobbies: "Klettern, Kochen".into(),
            job_title: "Ärztin".into(),
            family_plan: "kinderwunsch".into(),
            bio: "Liebt Berge und Bücher.".into(),
            photo_url: "https://example.com/a.jpg".into(),
            signal_contact: "alex.42".into(),
            income: "medium".into(),
            wealth: "low".into(),
            intimate_prefs: "egal".into(),
            income_expectation: "any".into(),
        }
    }

    #[test]
    fn valid_form_passes() {
        let v = validate_profile_form(&valid_form()).expect("valid form");
        assert_eq!(v.age, 30);
        assert_eq!(v.hobbies, vec!["klettern".to_owned(), "kochen".to_owned()]);
        assert_eq!(v.mbti, "INFJ");
    }

    #[test]
    fn underage_and_bad_mbti_fail_together() {
        let mut form = valid_form();
        form.age = "16".into();
        form.mbti = "XXXX".into();
        form.first_name = "  ".into();
        let errors = validate_profile_form(&form).unwrap_err();
        assert_eq!(errors.len(), 3);
    }

    #[test]
    fn email_check() {
        assert!(is_valid_email("a@b.de"));
        assert!(!is_valid_email("keine-mail"));
        assert!(!is_valid_email("a@b"));
        assert!(!is_valid_email("@b.de"));
    }

    #[test]
    fn public_profile_leaks_no_hidden_fields() {
        let profile = Profile {
            user_id: Uuid::new_v4(),
            first_name: "Sam".into(),
            age: 28,
            gender: "m".into(),
            looking_for: "w".into(),
            mbti: "ENTP".into(),
            hobbies: vec!["rad".into()],
            job_title: "Dev".into(),
            family_plan: "egal".into(),
            bio: "hi".into(),
            photo_url: "".into(),
            signal_contact_enc: "ENC-SIGNAL".into(),
            income_enc: "ENC-INCOME".into(),
            wealth_enc: "ENC-WEALTH".into(),
            intimate_prefs_enc: "ENC-PREFS".into(),
            income_expectation: "any".into(),
            updated_at: Utc::now(),
        };
        let public = PublicProfile::from_profile(&profile);
        let json = serde_json::to_string(&public).unwrap();
        for secret in ["ENC-SIGNAL", "ENC-INCOME", "ENC-WEALTH", "ENC-PREFS"] {
            assert!(!json.contains(secret), "leak of {secret}");
        }
        assert!(public.has_vault);
    }
}
