//! `06_config` — TOML-Konfiguration: Pan + Regeln (S4).
//!
//! `Config::parse` liest `rules.toml`-Text (Vorlage `rules.example.toml`,
//! `schema_version`-Check); ohne Datei gelten eingebaute Defaults
//! (`Config::defaults`, Datei-Einlesen macht `main`). Parser ohne
//! serde-Dep (händisch aus `toml::Value`). Alles ohne X11/Modell testbar.

use std::time::{Duration, Instant};

/// Erwartete Schema-Version der TOML-Datei.
pub const SCHEMA_VERSION: i64 = 1;

/// Pan-Einstellungen aus `[pan]` (Defaults = source5-Verhalten).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PanConfig {
    /// Pan-Schritt = ROI-Größe / `step_divisor`.
    pub step_divisor: u32,
    /// Pan-Schritt mindestens `step_min_px`.
    pub step_min_px: u32,
    /// Zoom-Stufen (ROI-Kanten in px, aufsteigend).
    pub roi_steps: Vec<u32>,
    /// Start-Größe (muss in `roi_steps` liegen).
    pub default_size: u32,
}

impl Default for PanConfig {
    fn default() -> Self {
        Self {
            step_divisor: 16,
            step_min_px: 8,
            roi_steps: vec![320, 480, 640, 960, 1280],
            default_size: 640,
        }
    }
}

/// Eine Automations-Aktion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Linksklick auf die Box-Mitte.
    Click,
    /// Klick, dann Text tippen (+ optional Enter).
    ClickAndType { text: String, press_enter: bool },
}

/// Eine Regel: Pattern (Substring, case-insensitiv) → Aktion + Cooldown.
#[derive(Debug, Clone)]
pub struct Rule {
    /// Anzeigename (fürs Log).
    pub name: String,
    /// Rohes Match-Pattern (Normalisierung macht `07_match` beim Match).
    pub pattern: String,
    /// Auszulösende Aktion.
    pub action: Action,
    /// Mindestabstand zwischen zwei Feuern.
    pub cooldown: Duration,
    pub(crate) last_fired: Option<Instant>,
}

/// Konfiguration: Pan + Regeln.
#[derive(Debug, Clone, Default)]
pub struct Config {
    /// Pan-Einstellungen.
    pub pan: PanConfig,
    /// Automations-Regeln (leere Liste = keine Aktionen).
    pub rules: Vec<Rule>,
}

/// Konfigurations-Fehler (tragbar, ohne IO-Typ im Enum).
#[derive(Debug, PartialEq, Eq)]
pub enum ConfigError {
    /// TOML-Syntaxfehler (mit Position aus dem Parser).
    Parse(String),
    /// `schema_version` fehlt oder falsch.
    Schema(String),
    /// Inhaltlich ungültig (leere Steps, unbekannte Aktion, ...).
    Invalid(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(e) => write!(f, "TOML-Fehler: {e}"),
            Self::Schema(e) => write!(f, "Schema-Fehler: {e}"),
            Self::Invalid(e) => write!(f, "Ungültige Konfiguration: {e}"),
        }
    }
}

impl std::error::Error for ConfigError {}

/// Lädt die TOML-Datei (Pfad aus CLI); fehlt sie, gelten eingebaute
/// Defaults (Hinweis auf stderr). Einlesen liegt hier, damit `main`
/// nur verdrahtet.
pub fn load_from_file(path: &str) -> Result<Config, String> {
    match std::fs::read_to_string(path) {
        Ok(text) => Config::parse(&text).map_err(|e| e.to_string()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            eprintln!("Hinweis: {path} fehlt — eingebaute Defaults aktiv.");
            Ok(Config::defaults())
        }
        Err(e) => Err(format!("kann {path} nicht lesen: {e}")),
    }
}

fn as_u32(map: &toml::map::Map<String, toml::Value>, key: &str) -> Result<u32, ConfigError> {
    map.get(key)
        .and_then(toml::Value::as_integer)
        .and_then(|v| u32::try_from(v).ok())
        .ok_or_else(|| ConfigError::Invalid(format!("`{key}` fehlt oder ist keine Zahl")))
}

impl Config {
    /// Eingebaute Defaults (keine Datei).
    #[must_use]
    pub fn defaults() -> Self {
        Self {
            pan: PanConfig::default(),
            rules: Vec::new(),
        }
    }

    /// Lädt TOML-Text (Datei-Einlesen macht `main`, S6).
    pub fn parse(text: &str) -> Result<Self, ConfigError> {
        // Hinweis: `Table` (Dokument) parsen, nicht `Value` (Einzelwert).
        let root: toml::map::Map<String, toml::Value> = text
            .parse()
            .map_err(|e: toml::de::Error| ConfigError::Parse(e.to_string()))?;

        let schema = root
            .get("schema_version")
            .and_then(toml::Value::as_integer)
            .ok_or_else(|| ConfigError::Schema("`schema_version` fehlt".into()))?;
        if schema != SCHEMA_VERSION {
            return Err(ConfigError::Schema(format!(
                "erwartet {SCHEMA_VERSION}, gefunden {schema}"
            )));
        }

        let mut cfg = Self::defaults();
        if let Some(pan) = root.get("pan").and_then(toml::Value::as_table) {
            cfg.pan = Self::parse_pan(pan)?;
        }
        if let Some(rules) = root.get("rule").and_then(toml::Value::as_array) {
            for (i, r) in rules.iter().enumerate() {
                let t = r.as_table().ok_or_else(|| {
                    ConfigError::Invalid(format!("`rule[{i}]` ist keine Tabelle"))
                })?;
                cfg.rules.push(Self::parse_rule(t, i)?);
            }
        }
        Ok(cfg)
    }

    fn parse_pan(pan: &toml::map::Map<String, toml::Value>) -> Result<PanConfig, ConfigError> {
        let step_divisor = as_u32(pan, "step_divisor")?;
        let step_min_px = as_u32(pan, "step_min_px")?;
        if step_divisor == 0 {
            return Err(ConfigError::Invalid("`step_divisor` muss > 0 sein".into()));
        }
        let steps_val = pan
            .get("roi_steps")
            .and_then(toml::Value::as_array)
            .ok_or_else(|| ConfigError::Invalid("`roi_steps` fehlt oder ist kein Array".into()))?;
        if steps_val.is_empty() {
            return Err(ConfigError::Invalid(
                "`roi_steps` darf nicht leer sein".into(),
            ));
        }
        let mut roi_steps = Vec::with_capacity(steps_val.len());
        for v in steps_val {
            let s = v
                .as_integer()
                .and_then(|n| u32::try_from(n).ok())
                .ok_or_else(|| {
                    ConfigError::Invalid("`roi_steps` braucht positive Zahlen".into())
                })?;
            if s == 0 {
                return Err(ConfigError::Invalid(
                    "`roi_steps` braucht positive Zahlen".into(),
                ));
            }
            roi_steps.push(s);
        }
        let default_size = as_u32(pan, "default_size")?;
        if !roi_steps.contains(&default_size) {
            return Err(ConfigError::Invalid(format!(
                "`default_size` ({default_size}) muss in `roi_steps` liegen"
            )));
        }
        Ok(PanConfig {
            step_divisor,
            step_min_px,
            roi_steps,
            default_size,
        })
    }

    fn parse_rule(t: &toml::map::Map<String, toml::Value>, i: usize) -> Result<Rule, ConfigError> {
        let str_field = |key: &str| {
            t.get(key)
                .and_then(toml::Value::as_str)
                .map(str::to_string)
                .ok_or_else(|| {
                    ConfigError::Invalid(format!("`rule[{i}].{key}` fehlt oder ist kein String"))
                })
        };
        let name = str_field("name")?;
        let pattern = str_field("pattern")?;
        if pattern.is_empty() {
            return Err(ConfigError::Invalid(format!(
                "`rule[{i}].pattern` darf nicht leer sein"
            )));
        }
        let action_name = str_field("action")?;
        let action = match action_name.as_str() {
            "click" => Action::Click,
            "click_and_type" => Action::ClickAndType {
                text: str_field("text")?,
                press_enter: t
                    .get("press_enter")
                    .and_then(toml::Value::as_bool)
                    .unwrap_or(false),
            },
            other => {
                return Err(ConfigError::Invalid(format!(
                    "`rule[{i}].action` unbekannt: {other} \
                     (erlaubt: click, click_and_type)"
                )));
            }
        };
        let cooldown_secs = t
            .get("cooldown_secs")
            .and_then(toml::Value::as_integer)
            .and_then(|n| u64::try_from(n).ok())
            .ok_or_else(|| {
                ConfigError::Invalid(format!("`rule[{i}].cooldown_secs` fehlt oder ist ungültig"))
            })?;
        Ok(Rule {
            name,
            pattern,
            action,
            cooldown: Duration::from_secs(cooldown_secs),
            last_fired: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = include_str!("../rules.example.toml");

    #[test]
    fn example_parses_with_two_rules() {
        let cfg = Config::parse(EXAMPLE).unwrap();
        assert_eq!(cfg.pan.step_divisor, 16);
        assert_eq!(cfg.pan.step_min_px, 8);
        assert_eq!(cfg.pan.default_size, 640);
        assert_eq!(cfg.rules.len(), 2);
        assert!(matches!(cfg.rules[0].action, Action::Click));
        assert!(matches!(cfg.rules[1].action, Action::ClickAndType { .. }));
    }

    #[test]
    fn missing_file_means_defaults() {
        let cfg = Config::defaults();
        assert_eq!(cfg.pan, PanConfig::default());
        assert!(cfg.rules.is_empty());
    }

    #[test]
    fn bad_schema_and_content_fail() {
        assert!(matches!(
            Config::parse("schema_version = 99"),
            Err(ConfigError::Schema(_))
        ));
        assert!(matches!(
            Config::parse("kein = \"toml"),
            Err(ConfigError::Parse(_))
        ));
        assert!(matches!(
            Config::parse(
                "schema_version = 1\n[pan]\nstep_divisor = 0\n\
                 step_min_px = 8\nroi_steps = [640]\ndefault_size = 640"
            ),
            Err(ConfigError::Invalid(_))
        ));
        assert!(matches!(
            Config::parse(
                "schema_version = 1\n[[rule]]\nname = \"x\"\n\
                 pattern = \"y\"\naction = \"zauber\"\ncooldown_secs = 1"
            ),
            Err(ConfigError::Invalid(_))
        ));
    }
}
