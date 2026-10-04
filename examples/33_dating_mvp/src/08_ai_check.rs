//! 08_ai_check: Spam-/Werbungs-Filter für Profiltexte.
//!
//! MVP: `MockAiChecker` mit Heuristik (Links, OnlyFans-/Messenger-Muster,
//! Geld-Maschen). `HttpAiChecker` ist der Stub für eine echte LLM-API
//! (OpenAI/Anthropic-kompatibel, aktiv via `AI_MODE=http`).

use crate::config::Config;
use serde::{Deserialize, Serialize};

/// Blockierte Substrings (lowercase) → Profiltext ist NICHT sauber.
const BLOCKED_SUBSTRINGS: &[&str] = &[
    "onlyfans",
    "t.me/",
    "telegram.me",
    "http://",
    "https://",
    "www.",
    ".com/",
    "cashapp",
    "cash.app",
    "whatsapp",
    "signal.me",
    "escort",
    "porn",
    "xxx",
    "geld verdienen",
    "crypto",
    "bitcoin",
    "telegram:",
];

const MAX_CHECK_LEN: usize = 2000;

pub trait AiChecker: Send + Sync {
    fn check(&self, text: &str) -> impl std::future::Future<Output = bool> + Send;
}

#[derive(Clone, Default)]
pub struct MockAiChecker;

impl AiChecker for MockAiChecker {
    async fn check(&self, text: &str) -> bool {
        is_clean_heuristic(text)
    }
}

pub fn is_clean_heuristic(text: &str) -> bool {
    if text.chars().count() > MAX_CHECK_LEN {
        return false;
    }
    let lower = text.to_lowercase();
    !BLOCKED_SUBSTRINGS.iter().any(|pat| lower.contains(pat))
}

#[derive(Clone)]
pub struct HttpAiChecker {
    client: reqwest::Client,
    api_url: String,
    api_key: String,
}

#[derive(Serialize)]
struct CheckRequest<'a> {
    text: &'a str,
}

#[derive(Deserialize)]
struct CheckResponse {
    #[serde(default)]
    is_clean: Option<bool>,
    #[serde(default)]
    clean: Option<bool>,
}

impl AiChecker for HttpAiChecker {
    async fn check(&self, text: &str) -> bool {
        // Erst Heuristik (billig), dann LLM (teuer).
        if !is_clean_heuristic(text) {
            return false;
        }
        match self
            .client
            .post(&self.api_url)
            .bearer_auth(&self.api_key)
            .json(&CheckRequest { text })
            .send()
            .await
        {
            Ok(resp) => match resp.json::<CheckResponse>().await {
                Ok(parsed) => parsed.is_clean.or(parsed.clean).unwrap_or(true),
                Err(e) => {
                    tracing::warn!(error = %e, "AI check: bad LLM response, fail-open");
                    true
                }
            },
            Err(e) => {
                // MVP-Entscheidung: fail-open mit Warnung, damit ein LLM-Ausfall
                // keine Registrierungen blockiert. Prod: fail-closed + Retry.
                tracing::warn!(error = %e, "AI check: LLM unreachable, fail-open");
                true
            }
        }
    }
}

/// Enum-Dispatch statt `dyn` (native `async`-Trait-Methoden sind nicht
/// objekt-sicher) — keine `async-trait`-Abhängigkeit nötig.
#[derive(Clone)]
pub enum AiCheckerKind {
    Mock(MockAiChecker),
    Http(HttpAiChecker),
}

impl AiCheckerKind {
    pub fn from_config(config: &Config) -> Self {
        match config.ai_mode {
            crate::config::AiMode::Http => {
                match (config.ai_api_url.clone(), config.ai_api_key.clone()) {
                    (Some(api_url), Some(api_key)) => Self::Http(HttpAiChecker {
                        client: reqwest::Client::new(),
                        api_url,
                        api_key,
                    }),
                    _ => {
                        tracing::warn!("AI_MODE=http without AI_API_URL/KEY, using mock");
                        Self::Mock(MockAiChecker)
                    }
                }
            }
            crate::config::AiMode::Mock => Self::Mock(MockAiChecker),
        }
    }

    pub async fn check(&self, text: &str) -> bool {
        match self {
            Self::Mock(m) => m.check(text).await,
            Self::Http(h) => h.check(text).await,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn clean_texts_pass() {
        let checker = MockAiChecker;
        for text in [
            "",
            "Liebt Berge, Bücher und lange Spaziergänge.",
            "Ärztin aus München, 30. Klettern & Kochen. Kinderwunsch. 🙂",
        ] {
            assert!(checker.check(text).await, "should pass: {text}");
        }
    }

    #[tokio::test]
    async fn spam_texts_fail() {
        let checker = MockAiChecker;
        for text in [
            "Folgt mir auf OnlyFans!",
            "Schreib mir auf Telegram: t.me/scammer",
            "Besuch https://boese-seite.example für mehr",
            "Schick Geld via CashApp an $scam",
            "Geld verdienen mit Crypto und Bitcoin!!",
            "Mein Whatsapp: 0151... ruf an",
        ] {
            assert!(!checker.check(text).await, "should fail: {text}");
        }
    }

    #[tokio::test]
    async fn oversized_text_fails() {
        let checker = MockAiChecker;
        assert!(!checker.check(&"a".repeat(MAX_CHECK_LEN + 1)).await);
        assert!(checker.check(&"a".repeat(MAX_CHECK_LEN)).await);
    }
}
