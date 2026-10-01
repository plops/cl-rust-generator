//! Konfiguration: MCP-Endpunkt, Rate-Limit-Delay, CLI-Argumente, Frage-Prompt.

use std::path::PathBuf;
use std::time::Duration;

use crate::types::Repo;

/// Streamable-HTTP-Endpunkt des DeepWiki MCP.
pub const DEFAULT_ENDPOINT: &str = "https://mcp.deepwiki.com/mcp";
/// MCP-Protokollversion aus dem Auftrag.
pub const DEFAULT_PROTOCOL_VERSION: &str = "2024-11-05";
/// Delay zwischen zwei DeepWiki-Anfragen (Mitte des geforderten 1,5–2-s-Fensters).
pub const DEFAULT_DELAY_MS: u64 = 1750;
/// Realer Tool-Name auf dem Server (per `tools/list` verifiziert).
pub const PRIMARY_TOOL: &str = "ask_wiki_question";
/// Alternativname aus dem Auftrag; wird als Fallback versucht.
pub const FALLBACK_TOOL: &str = "ask_question";
/// Umgebungsvariable für den Delay (wirkt, wenn `--delay-ms` fehlt).
pub const DELAY_ENV_VAR: &str = "DEEPWIKI_DELAY_MS";
/// Obere Schranke für eine einzelne MCP-Anfrage (KI-Antworten brauchen teils Minuten).
pub const REQUEST_TIMEOUT_SECS: u64 = 300;

/// Laufzeit-Konfiguration des Analysators.
#[derive(Debug, Clone)]
pub struct Config {
    pub endpoint: String,
    pub protocol_version: String,
    pub delay: Duration,
    pub primary_tool: String,
    pub fallback_tool: String,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            endpoint: DEFAULT_ENDPOINT.to_owned(),
            protocol_version: DEFAULT_PROTOCOL_VERSION.to_owned(),
            delay: Duration::from_millis(DEFAULT_DELAY_MS),
            primary_tool: PRIMARY_TOOL.to_owned(),
            fallback_tool: FALLBACK_TOOL.to_owned(),
        }
    }
}

/// Geparste CLI-Argumente: genau ein optionales Positionsargument (Eingabedatei).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CliArgs {
    pub input_file: Option<PathBuf>,
    pub delay: Duration,
}

/// Ergebnis der Argument-Auswertung: laufen oder Hilfe anzeigen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgsOutcome {
    Run(CliArgs),
    Help,
}

/// Parst `cargo run [--delay-ms N] [DATEI]`; ohne Datei wird `stdin` gelesen.
pub fn parse_args<I, S>(raw: I) -> Result<ArgsOutcome, String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let mut input_file: Option<PathBuf> = None;
    let mut delay_ms: Option<u64> = None;
    let mut items = raw.into_iter().peekable();

    while let Some(item) = items.next() {
        let arg = item.as_ref();
        if arg == "-h" || arg == "--help" {
            return Ok(ArgsOutcome::Help);
        }
        if let Some(value) = arg.strip_prefix("--delay-ms=") {
            delay_ms = Some(parse_delay(value)?);
        } else if arg == "--delay-ms" {
            let value = items.next().ok_or_else(|| {
                String::from("Fehlender Wert: --delay-ms <MILLISEKUNDEN> erwartet")
            })?;
            delay_ms = Some(parse_delay(value.as_ref())?);
        } else if arg == "-" {
            // Explizites stdin, äquivalent zu „keine Datei“.
        } else if arg.starts_with('-') {
            return Err(format!("Unbekannte Option '{arg}'.\n{}", usage()));
        } else if input_file.is_none() {
            input_file = Some(PathBuf::from(arg));
        } else {
            return Err(format!(
                "Nur eine Eingabedatei erlaubt, zusätzliches Argument '{arg}'.\n{}",
                usage()
            ));
        }
    }

    let delay = match delay_ms {
        Some(ms) => Duration::from_millis(ms),
        None => env_delay().unwrap_or_else(|| Duration::from_millis(DEFAULT_DELAY_MS)),
    };
    Ok(ArgsOutcome::Run(CliArgs { input_file, delay }))
}

fn parse_delay(value: &str) -> Result<u64, String> {
    value
        .parse::<u64>()
        .map_err(|_| format!("Ungültiger Delay '{value}': Millisekunden als Zahl erwartet"))
}

fn env_delay() -> Option<Duration> {
    std::env::var(DELAY_ENV_VAR)
        .ok()?
        .parse::<u64>()
        .ok()
        .map(Duration::from_millis)
}

pub fn usage() -> String {
    format!(
        "Verwendung: github-trending-algos [--delay-ms MS] [DATEI]\n\
         Liest Trending-Text aus DATEI oder stdin und analysiert jedes Repo via DeepWiki MCP.\n\
         Default-Delay zwischen Anfragen: {DEFAULT_DELAY_MS} ms (Env: {DELAY_ENV_VAR}=MS)."
    )
}

/// Baut den deutschen Analyse-Prompt für ein Repository (Format exakt laut Auftrag).
pub fn build_question(repo: &Repo) -> String {
    let full = repo.full_name();
    format!(
        "Ziel-Repository: {full}\n\n\
         Erstelle eine vollständige, tiefgehende technische Analyse auf Deutsch für dieses Repository im folgenden Format:\n\n\
         # {full}\n\n\
         ## GitHub & DeepWiki\n\
         - GitHub: https://github.com/{full}\n\
         - DeepWiki: https://deepwiki.com/{full}\n\n\
         ## Kurze Einführung\n\
         2-3 prägnante Sätze zu Kernfunktion, Einsatzzweck und Zielgruppe.\n\n\
         ## Die 3 wichtigsten (oder komplexesten) Algorithmen\n\
         Beschreibe die 3 Kernalgorithmen oder zentralen Datenstrukturen:\n\
         1. Name & Verortung im Code (Modulpfade, Structs)\n\
         2. Detaillierte technische Funktionsweise\n\
         3. \"Warum prägend\": Warum macht genau dieser Algorithmus die Software zu dem, was sie ist (z. B. Performance, Skalierbarkeit, O-Komplexität)?\n\n\
         ## Architektur & Zusammenspiel\n\
         Ein valides ```mermaid Diagramm, das visualisiert, wie diese Komponenten/Algorithmen ineinandergreifen.\n\n\
         ## Notes\n\
         Ehrenhafte Erwähnungen weiterer technischer Highlights, Nebenläufigkeitsmodelle oder Caching-Strategien.\n\n\
         Wichtig: Antworte komplett auf Deutsch auf Senior-Entwickler-Niveau."
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_config_matches_spec() {
        let config = Config::default();
        assert_eq!(config.endpoint, DEFAULT_ENDPOINT);
        assert_eq!(config.protocol_version, DEFAULT_PROTOCOL_VERSION);
        assert_eq!(config.delay, Duration::from_millis(1750));
        assert_eq!(config.primary_tool, "ask_wiki_question");
        assert_eq!(config.fallback_tool, "ask_question");
    }

    #[test]
    fn args_default_to_stdin() {
        let outcome = parse_args::<Vec<String>, String>(Vec::new()).expect("ok");
        let ArgsOutcome::Run(args) = outcome else {
            panic!("Hilfe unerwartet");
        };
        assert_eq!(args.input_file, None);
        assert_eq!(args.delay, Duration::from_millis(DEFAULT_DELAY_MS));
    }

    #[test]
    fn args_accept_file_and_delay_forms() {
        let outcome = parse_args(["--delay-ms", "2000", "trend.txt"]).expect("ok");
        let ArgsOutcome::Run(args) = outcome else {
            panic!("Hilfe unerwartet");
        };
        assert_eq!(args.input_file, Some(PathBuf::from("trend.txt")));
        assert_eq!(args.delay, Duration::from_millis(2000));

        let outcome = parse_args(["--delay-ms=500"]).expect("ok");
        let ArgsOutcome::Run(args) = outcome else {
            panic!("Hilfe unerwartet");
        };
        assert_eq!(args.delay, Duration::from_millis(500));
    }

    #[test]
    fn args_reject_unknown_and_extra() {
        assert!(parse_args(["--turbo"]).is_err());
        assert!(parse_args(["a.txt", "b.txt"]).is_err());
        assert!(parse_args(["--delay-ms"]).is_err());
        assert!(parse_args(["--delay-ms=abc"]).is_err());
    }

    #[test]
    fn dash_means_stdin() {
        let outcome = parse_args(["-"]).expect("ok");
        let ArgsOutcome::Run(args) = outcome else {
            panic!("Hilfe unerwartet");
        };
        assert_eq!(args.input_file, None);
    }

    #[test]
    fn args_help_flag() {
        assert_eq!(parse_args(["--help"]).expect("ok"), ArgsOutcome::Help);
        assert_eq!(parse_args(["-h"]).expect("ok"), ArgsOutcome::Help);
    }

    #[test]
    fn question_contains_all_required_blocks() {
        let repo = Repo::new("rustfs", "rustfs").expect("valid");
        let question = build_question(&repo);
        for needle in [
            "Ziel-Repository: rustfs/rustfs",
            "# rustfs/rustfs",
            "## GitHub & DeepWiki",
            "https://github.com/rustfs/rustfs",
            "https://deepwiki.com/rustfs/rustfs",
            "## Kurze Einführung",
            "## Die 3 wichtigsten (oder komplexesten) Algorithmen",
            "Warum prägend",
            "## Architektur & Zusammenspiel",
            "```mermaid",
            "## Notes",
            "komplett auf Deutsch auf Senior-Entwickler-Niveau",
        ] {
            assert!(question.contains(needle), "Prompt fehlt: {needle}");
        }
    }
}
