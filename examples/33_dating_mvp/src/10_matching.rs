//! 10_matching: Top-5-Tagesmatches, Likes und Signal-Austausch.
//!
//! *Anti-Swiping:* Nutzer sehen max. 5 Profile/Tag. Der Job
//! (`run_daily_task`) berechnet sie einmal täglich neu; `GET /matches`
//! rechnet bei Bedarf faul nach (leerer Tag → sofort berechnen).
//! Bei gegenseitigem Like wird beiden der Signal-Kontakt des anderen
//! entschlüsselt angezeigt — es gibt KEINEN In-App-Chat.

use axum::extract::{Path, State};
use axum::http::HeaderMap;
use axum::response::{Html, Redirect};
use chrono::NaiveDate;
use sqlx::PgPool;
use uuid::Uuid;

use crate::db::{CryptoKey, decrypt_field};
use crate::handlers_auth::require_paid;
use crate::handlers_profile::{decrypt_band, find_profile};
use crate::models::{Profile, PublicProfile};
use crate::routes::AppState;
use crate::scoring::{ScoreProfile, compatibility_score};
use crate::views::{AppError, MatchCard, MatchesListTemplate, MatchesTemplate, MutualTemplate};
use askama::Template;

pub const TOP_N: i64 = 5;

#[derive(Debug, Clone)]
pub struct ScoredCandidate {
    pub candidate_id: Uuid,
    pub score: u8,
}

fn to_score_profile(p: &Profile, key: &CryptoKey) -> ScoreProfile {
    ScoreProfile {
        age: p.age,
        gender: p.gender.clone(),
        looking_for: p.looking_for.clone(),
        family_plan: p.family_plan.clone(),
        mbti: p.mbti.clone(),
        hobbies: p.hobbies.clone(),
        income_band: decrypt_band(key, &p.income_enc),
        wealth_band: decrypt_band(key, &p.wealth_enc),
        income_expectation: p.income_expectation.clone(),
    }
}

/// Berechnet die Top-5 für `user_id` und persistiert sie für heute.
/// MVP: Vollscan über bezahlte Profile (für MVP-Größe ok, dokumentiert).
pub async fn compute_top_matches(
    pool: &PgPool,
    key: &CryptoKey,
    user_id: &Uuid,
) -> Result<Vec<ScoredCandidate>, sqlx::Error> {
    let Some(seeker) = find_profile(pool, user_id).await? else {
        return Ok(Vec::new());
    };
    let seeker_score = to_score_profile(&seeker, key);

    let candidates = sqlx::query_as::<_, Profile>(
        "SELECT p.user_id, p.first_name, p.age, p.gender, p.looking_for, p.mbti, p.hobbies,
                p.job_title, p.family_plan, p.bio, p.photo_url, p.signal_contact_enc,
                p.income_enc, p.wealth_enc, p.intimate_prefs_enc, p.income_expectation,
                p.updated_at
         FROM profiles p JOIN users u ON u.id = p.user_id
         WHERE u.paid = TRUE AND p.user_id <> $1",
    )
    .bind(user_id)
    .fetch_all(pool)
    .await?;

    let mut scored: Vec<ScoredCandidate> = candidates
        .iter()
        .map(|c| ScoredCandidate {
            candidate_id: c.user_id,
            score: compatibility_score(&seeker_score, &to_score_profile(c, key)),
        })
        .filter(|s| s.score > 0)
        .collect();
    // Deterministisch: Score absteigend, dann UUID aufsteigend.
    scored.sort_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then(a.candidate_id.cmp(&b.candidate_id))
    });
    scored.truncate(TOP_N as usize);

    let today: NaiveDate = chrono::Utc::now().date_naive();
    sqlx::query("DELETE FROM daily_matches WHERE user_id = $1 AND match_day = $2")
        .bind(user_id)
        .bind(today)
        .execute(pool)
        .await?;
    for (i, s) in scored.iter().enumerate() {
        sqlx::query(
            "INSERT INTO daily_matches (user_id, candidate_id, match_day, score, rank)
             VALUES ($1, $2, $3, $4, $5)",
        )
        .bind(user_id)
        .bind(s.candidate_id)
        .bind(today)
        .bind(i32::from(s.score))
        .bind(i as i32 + 1)
        .execute(pool)
        .await?;
    }
    Ok(scored)
}

/// Hintergrund-Job: alle X Stunden für alle zahlenden Nutzer mit Profil neu rechnen.
pub async fn run_daily_task(pool: PgPool, key: CryptoKey, interval_hours: u64) {
    let period = std::time::Duration::from_secs(interval_hours.max(1) * 3600);
    tracing::info!(hours = interval_hours, "starting daily match task");
    loop {
        tokio::time::sleep(period).await;
        match sqlx::query_scalar::<_, Uuid>(
            "SELECT p.user_id FROM profiles p JOIN users u ON u.id = p.user_id WHERE u.paid = TRUE",
        )
        .fetch_all(&pool)
        .await
        {
            Ok(ids) => {
                for id in ids {
                    if let Err(e) = compute_top_matches(&pool, &key, &id).await {
                        tracing::warn!(user_id = %id, error = %e, "daily match failed");
                    }
                }
                tracing::info!("daily match task completed");
            }
            Err(e) => tracing::warn!(error = %e, "daily match task: user scan failed"),
        }
    }
}

pub async fn add_like(pool: &PgPool, liker: &Uuid, liked: &Uuid) -> Result<(), sqlx::Error> {
    sqlx::query("INSERT INTO likes (liker_id, liked_id) VALUES ($1, $2) ON CONFLICT DO NOTHING")
        .bind(liker)
        .bind(liked)
        .execute(pool)
        .await?;
    Ok(())
}

pub async fn has_liked(pool: &PgPool, liker: &Uuid, liked: &Uuid) -> Result<bool, sqlx::Error> {
    let row: Option<(Uuid,)> =
        sqlx::query_as("SELECT liked_id FROM likes WHERE liker_id = $1 AND liked_id = $2")
            .bind(liker)
            .bind(liked)
            .fetch_optional(pool)
            .await?;
    Ok(row.is_some())
}

pub async fn is_mutual(pool: &PgPool, a: &Uuid, b: &Uuid) -> Result<bool, sqlx::Error> {
    Ok(has_liked(pool, a, b).await? && has_liked(pool, b, a).await?)
}

async fn cards_for_today(
    state: &AppState,
    user_id: &Uuid,
) -> Result<(Vec<MatchCard>, bool), AppError> {
    let today: NaiveDate = chrono::Utc::now().date_naive();
    let mut rows: Vec<(Uuid, i32)> = sqlx::query_as(
        "SELECT candidate_id, score FROM daily_matches
         WHERE user_id = $1 AND match_day = $2 ORDER BY rank ASC",
    )
    .bind(user_id)
    .bind(today)
    .fetch_all(&state.pool)
    .await?;
    let mut computed = !rows.is_empty();
    if rows.is_empty() {
        // Faul nachrechnen, damit neue Profile sofort Matches sehen.
        let fresh = compute_top_matches(&state.pool, &state.crypto, user_id).await?;
        rows = fresh
            .into_iter()
            .map(|s| (s.candidate_id, i32::from(s.score)))
            .collect();
        computed = true;
    }
    let mut cards = Vec::new();
    for (candidate_id, score) in rows {
        if let Some(p) = find_profile(&state.pool, &candidate_id).await? {
            let public = PublicProfile::from_profile(&p);
            cards.push(MatchCard {
                user_id: candidate_id.to_string(),
                first_name: public.first_name,
                age: public.age,
                mbti: public.mbti,
                job_title: public.job_title,
                score,
            });
        }
    }
    Ok((cards, computed))
}

/// GET /matches — Seite mit HTMX-Container.
pub async fn get_matches(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Html<String>, AppError> {
    let _ = require_paid(&state, &headers).await?;
    Ok(Html(MatchesTemplate.render()?))
}

/// GET /matches/list — HTMX-Partial mit den Match-Cards.
pub async fn get_matches_list(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Html<String>, AppError> {
    let user = require_paid(&state, &headers).await?;
    let (matches, computed_today) = cards_for_today(&state, &user.id).await?;
    Ok(Html(
        MatchesListTemplate {
            matches,
            computed_today,
        }
        .render()?,
    ))
}

/// POST /matches/recompute — manuelles Neuberechnen (MVP-Demo + E2E).
pub async fn post_recompute(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Redirect, AppError> {
    let user = require_paid(&state, &headers).await?;
    compute_top_matches(&state.pool, &state.crypto, &user.id).await?;
    Ok(Redirect::to("/matches"))
}

/// POST /like/:id — Like vergeben; bei Match weiter zu /mutual/:id.
pub async fn post_like(
    State(state): State<AppState>,
    headers: HeaderMap,
    Path(id): Path<Uuid>,
) -> Result<Redirect, AppError> {
    let user = require_paid(&state, &headers).await?;
    if user.id == id {
        return Err(AppError::BadRequest(
            "Man kann sich nicht selbst liken.".into(),
        ));
    }
    if find_profile(&state.pool, &id).await?.is_none() {
        return Err(AppError::NotFound);
    }
    add_like(&state.pool, &user.id, &id).await?;
    if is_mutual(&state.pool, &user.id, &id).await? {
        tracing::info!(a = %user.id, b = %id, "mutual match");
        Ok(Redirect::to(&format!("/mutual/{id}")))
    } else {
        Ok(Redirect::to("/matches"))
    }
}

/// GET /mutual/:id — nur bei gegenseitigem Like: Signal-Kontakte beider Seiten.
pub async fn get_mutual(
    State(state): State<AppState>,
    headers: HeaderMap,
    Path(id): Path<Uuid>,
) -> Result<Html<String>, AppError> {
    let user = require_paid(&state, &headers).await?;
    if !is_mutual(&state.pool, &user.id, &id).await? {
        return Err(AppError::NotFound);
    }
    let mine = find_profile(&state.pool, &user.id)
        .await?
        .ok_or(AppError::NotFound)?;
    let theirs = find_profile(&state.pool, &id)
        .await?
        .ok_or(AppError::NotFound)?;
    let my_signal = decrypt_field(&state.crypto, &mine.signal_contact_enc).unwrap_or_default();
    let their_signal = decrypt_field(&state.crypto, &theirs.signal_contact_enc).unwrap_or_default();
    let tpl = MutualTemplate {
        other_name: theirs.first_name,
        my_signal,
        their_signal,
    };
    Ok(Html(tpl.render()?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::Profile as DbProfile;

    fn db_profile(user_id: Uuid, gender: &str, looking_for: &str) -> DbProfile {
        DbProfile {
            user_id,
            first_name: "T".into(),
            age: 30,
            gender: gender.into(),
            looking_for: looking_for.into(),
            mbti: "INFJ".into(),
            hobbies: vec!["x".into()],
            job_title: "".into(),
            family_plan: "egal".into(),
            bio: "".into(),
            photo_url: "".into(),
            signal_contact_enc: String::new(),
            income_enc: String::new(),
            wealth_enc: String::new(),
            intimate_prefs_enc: String::new(),
            income_expectation: "any".into(),
            updated_at: chrono::Utc::now(),
        }
    }

    #[test]
    fn score_profile_conversion_neutral_on_empty_vault() {
        let key = CryptoKey([1u8; 32]);
        let sp = to_score_profile(&db_profile(Uuid::new_v4(), "w", "m"), &key);
        assert_eq!(sp.income_band, "medium");
        assert_eq!(sp.wealth_band, "medium");
    }

    #[test]
    fn top_n_is_five() {
        assert_eq!(TOP_N, 5);
    }
}
