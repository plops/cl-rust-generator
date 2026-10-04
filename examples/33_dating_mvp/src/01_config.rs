//! 01_config: Umgebungsvariablen, Secrets, AI-/Stripe-Modi.
//!
//! `from_lookup` erlaubt deterministische Tests ohne globale Env-Mutation
//! (`std::env::set_var` ist in der Rust Edition 2024 `unsafe`).

use base64::{Engine as _, engine::general_purpose::STANDARD as B64};
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AiMode {
    Mock,
    Http,
}

#[derive(Clone, Debug)]
pub struct Config {
    pub database_url: String,
    pub data_encryption_key: [u8; 32],
    pub port: u16,
    pub session_secret: String,
    pub stripe_secret_key: Option<String>,
    pub stripe_price_id: String,
    pub ai_mode: AiMode,
    pub ai_api_url: Option<String>,
    pub ai_api_key: Option<String>,
    pub match_job_interval_hours: u64,
    /// Echte Requests/Sekunde pro Peer-IP (Governor-Periode = 1s / Wert).
    pub rate_limit_per_second: u64,
    pub rate_limit_burst: u32,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ConfigError {
    #[error("missing env var: {0}")]
    Missing(&'static str),
    #[error("invalid {0}: {1}")]
    Invalid(&'static str, String),
}

impl Config {
    pub fn from_env() -> Result<Self, ConfigError> {
        Self::from_lookup(|k| std::env::var(k).ok())
    }

    pub fn from_lookup<F>(get: F) -> Result<Self, ConfigError>
    where
        F: Fn(&str) -> Option<String>,
    {
        let req = |key: &'static str| get(key).ok_or(ConfigError::Missing(key));
        let opt = |key: &str| {
            let v = get(key).unwrap_or_default();
            let v = v.trim().to_owned();
            if v.is_empty() { None } else { Some(v) }
        };

        let database_url = req("DATABASE_URL")?;
        let key_b64 = req("DATA_ENCRYPTION_KEY")?;
        let key_bytes = B64
            .decode(key_b64.trim())
            .map_err(|e| ConfigError::Invalid("DATA_ENCRYPTION_KEY", e.to_string()))?;
        if key_bytes.len() != 32 {
            return Err(ConfigError::Invalid(
                "DATA_ENCRYPTION_KEY",
                format!("expected 32 bytes, got {}", key_bytes.len()),
            ));
        }
        let mut data_encryption_key = [0u8; 32];
        data_encryption_key.copy_from_slice(&key_bytes);

        let port: u16 = get("PORT")
            .unwrap_or_else(|| "3000".to_owned())
            .trim()
            .parse()
            .map_err(|e: std::num::ParseIntError| ConfigError::Invalid("PORT", e.to_string()))?;

        let session_secret = get("SESSION_SECRET")
            .filter(|s| !s.trim().is_empty())
            .unwrap_or_else(|| "dev-session-secret-wechsel-mich-bitte-aus".to_owned());

        let ai_mode = match get("AI_MODE")
            .unwrap_or_default()
            .trim()
            .to_lowercase()
            .as_str()
        {
            "http" => AiMode::Http,
            _ => AiMode::Mock,
        };

        let match_job_interval_hours: u64 = get("MATCH_JOB_INTERVAL_HOURS")
            .unwrap_or_else(|| "24".to_owned())
            .trim()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                ConfigError::Invalid("MATCH_JOB_INTERVAL_HOURS", e.to_string())
            })?;

        let rate_limit_per_second: u64 = get("RATE_LIMIT_PER_SECOND")
            .unwrap_or_else(|| "10".to_owned())
            .trim()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                ConfigError::Invalid("RATE_LIMIT_PER_SECOND", e.to_string())
            })?;
        if rate_limit_per_second == 0 {
            return Err(ConfigError::Invalid(
                "RATE_LIMIT_PER_SECOND",
                "must be >= 1".to_owned(),
            ));
        }
        let rate_limit_burst: u32 = get("RATE_LIMIT_BURST")
            .unwrap_or_else(|| "30".to_owned())
            .trim()
            .parse()
            .map_err(|e: std::num::ParseIntError| {
                ConfigError::Invalid("RATE_LIMIT_BURST", e.to_string())
            })?;
        if rate_limit_burst == 0 {
            return Err(ConfigError::Invalid(
                "RATE_LIMIT_BURST",
                "must be >= 1".to_owned(),
            ));
        }

        Ok(Self {
            database_url,
            data_encryption_key,
            port,
            session_secret,
            stripe_secret_key: opt("STRIPE_SECRET_KEY"),
            stripe_price_id: get("STRIPE_PRICE_ID")
                .filter(|s| !s.trim().is_empty())
                .unwrap_or_else(|| "price_test_10eur_einmalig".to_owned()),
            ai_mode,
            ai_api_url: opt("AI_API_URL"),
            ai_api_key: opt("AI_API_KEY"),
            match_job_interval_hours,
            rate_limit_per_second,
            rate_limit_burst,
        })
    }

    /// true, wenn ein echter Stripe-Key konfiguriert ist (sonst Mock-Flow).
    pub fn use_real_stripe(&self) -> bool {
        self.stripe_secret_key.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn base_vars() -> HashMap<String, String> {
        let key = B64.encode([7u8; 32]);
        HashMap::from([
            (
                "DATABASE_URL".to_owned(),
                "postgres://localhost/x".to_owned(),
            ),
            ("DATA_ENCRYPTION_KEY".to_owned(), key),
        ])
    }

    fn lookup(vars: &HashMap<String, String>) -> impl Fn(&str) -> Option<String> + '_ {
        |k| vars.get(k).cloned()
    }

    #[test]
    fn parses_minimal_config_with_defaults() {
        let vars = base_vars();
        let cfg = Config::from_lookup(lookup(&vars)).expect("minimal config parses");
        assert_eq!(cfg.port, 3000);
        assert_eq!(cfg.ai_mode, AiMode::Mock);
        assert_eq!(cfg.match_job_interval_hours, 24);
        assert_eq!(cfg.rate_limit_per_second, 10);
        assert_eq!(cfg.rate_limit_burst, 30);
        assert!(!cfg.use_real_stripe());
        assert_eq!(cfg.data_encryption_key, [7u8; 32]);
    }

    #[test]
    fn missing_database_url_errors() {
        let vars = HashMap::new();
        let err = Config::from_lookup(lookup(&vars)).unwrap_err();
        assert_eq!(err, ConfigError::Missing("DATABASE_URL"));
    }

    #[test]
    fn short_encryption_key_errors() {
        let mut vars = base_vars();
        vars.insert("DATA_ENCRYPTION_KEY".to_owned(), B64.encode([1u8; 16]));
        let err = Config::from_lookup(lookup(&vars)).unwrap_err();
        assert!(matches!(
            err,
            ConfigError::Invalid("DATA_ENCRYPTION_KEY", _)
        ));
    }

    #[test]
    fn invalid_base64_key_errors() {
        let mut vars = base_vars();
        vars.insert(
            "DATA_ENCRYPTION_KEY".to_owned(),
            "!!!kein-base64!!!".to_owned(),
        );
        let err = Config::from_lookup(lookup(&vars)).unwrap_err();
        assert!(matches!(
            err,
            ConfigError::Invalid("DATA_ENCRYPTION_KEY", _)
        ));
    }

    #[test]
    fn invalid_port_errors() {
        let mut vars = base_vars();
        vars.insert("PORT".to_owned(), "kein-port".to_owned());
        let err = Config::from_lookup(lookup(&vars)).unwrap_err();
        assert!(matches!(err, ConfigError::Invalid("PORT", _)));
    }

    #[test]
    fn http_ai_mode_and_stripe_key_parsed() {
        let mut vars = base_vars();
        vars.insert("AI_MODE".to_owned(), "http".to_owned());
        vars.insert("AI_API_URL".to_owned(), "https://llm.example/v1".to_owned());
        vars.insert("STRIPE_SECRET_KEY".to_owned(), "sk_test_123".to_owned());
        vars.insert("MATCH_JOB_INTERVAL_HOURS".to_owned(), "1".to_owned());
        let cfg = Config::from_lookup(lookup(&vars)).expect("full config parses");
        assert_eq!(cfg.ai_mode, AiMode::Http);
        assert!(cfg.use_real_stripe());
        assert_eq!(cfg.match_job_interval_hours, 1);
    }
}
