//! 01 — Konfiguration: CLI und Transportart.
//!
//! Der Server dient (`serve`) oder fragt einen laufenden Server ab
//! (`--check` / `--watch`, TUI-light in `08_status`).

use std::net::SocketAddr;

/// Hoechstens zwei Clients pro Raum (Dolmetsch-Paar).
pub const MAX_PEERS: usize = 2;

/// Offizieller Gemini-Live-Endpunkt (v1beta, ohne `?key=`).
pub const GEMINI_URL: &str = "wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent";

/// Uebersetzungsmodell (per Browser-HIL in source1 belegt).
pub const GEMINI_MODEL: &str = "models/gemini-3.5-live-translate-preview";

/// Wie ein Peer Audio austauscht.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Transport {
    /// Wie source1: Browser spricht selbst mit Gemini, PCM 24 kHz ueber den Relay.
    #[default]
    Pcm,
    /// Sparmodus: Opus-SILK hoch und runter, Server haelt die Gemini-Sitzung.
    Opus,
}

impl Transport {
    /// `"opus"` → `Opus`, alles andere (auch leer) → `Pcm`.
    pub fn parse(s: &str) -> Self {
        if s.eq_ignore_ascii_case("opus") {
            Self::Opus
        } else {
            Self::Pcm
        }
    }
}

/// Servereinstellung aus der CLI.
#[derive(Debug, Clone)]
pub struct Config {
    /// Lauschadresse, z. B. `0.0.0.0:3000`.
    pub listen: SocketAddr,
    /// Verzeichnis mit `index.html` und `app.js`.
    pub dir: String,
    /// Optionaler Server-Key (Opus-Clients brauchen dann keinen eigenen).
    pub key_file: Option<String>,
    /// Gemini-Endpunkt (Tests: Fake-Server `ws://127.0.0.1:…`).
    pub gemini_url: String,
    /// Ausfuehrliche Logs (nie Audio, nie Key).
    pub verbose: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            listen: "0.0.0.0:3000".parse().expect("feste Adresse"),
            dir: "public".to_owned(),
            key_file: None,
            gemini_url: GEMINI_URL.to_owned(),
            verbose: false,
        }
    }
}

/// Betriebsart: dienen oder Terminal-Status.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Serve,
    Check { base: String },
    Watch { base: String },
}

impl Config {
    /// Kleine CLI ohne Zusatz-Crate. `Err` enthaelt den Hilfetext.
    pub fn from_args(args: &[String]) -> Result<(Self, Action), String> {
        let mut cfg = Self::default();
        let mut action = Action::Serve;
        let mut it = args.iter().skip(1);
        while let Some(a) = it.next() {
            let mut val = |name: &str| -> Result<String, String> {
                it.next()
                    .cloned()
                    .ok_or(format!("{name} braucht einen Wert"))
            };
            match a.as_str() {
                "--listen" => {
                    cfg.listen = val("--listen")?
                        .parse()
                        .map_err(|_| "--listen ungueltig".to_owned())?;
                }
                "--dir" => cfg.dir = val("--dir")?,
                "--key-file" => cfg.key_file = Some(val("--key-file")?),
                "--gemini-url" => cfg.gemini_url = val("--gemini-url")?,
                "--check" => {
                    action = Action::Check {
                        base: val("--check")?,
                    }
                }
                "--watch" => {
                    action = Action::Watch {
                        base: val("--watch")?,
                    }
                }
                "--verbose" | "-v" => cfg.verbose = true,
                "-h" | "--help" => return Err(Self::usage()),
                other => return Err(format!("unbekannt: {other}\n{}", Self::usage())),
            }
        }
        Ok((cfg, action))
    }

    /// Server-Key lesen (getrimmt); `None` ohne `--key-file` oder bei leerer Datei.
    pub fn server_key(&self) -> Option<String> {
        let raw = std::fs::read_to_string(self.key_file.as_ref()?).ok()?;
        let k = raw.trim();
        (!k.is_empty()).then(|| k.to_owned())
    }

    fn usage() -> String {
        "live-silk [--listen ADDR] [--dir PFAD] [--key-file DATEI] [--gemini-url URL] [--verbose] [--check URL | --watch URL]".to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(v: &[&str]) -> Vec<String> {
        std::iter::once("live-silk")
            .chain(v.iter().copied())
            .map(str::to_owned)
            .collect()
    }

    #[test]
    fn defaults() {
        let (cfg, action) = Config::from_args(&args(&[])).unwrap();
        assert_eq!(action, Action::Serve);
        assert_eq!(cfg.gemini_url, GEMINI_URL);
        assert!(cfg.key_file.is_none() && !cfg.verbose);
    }

    #[test]
    fn alle_optionen() {
        let (cfg, _) = Config::from_args(&args(&[
            "--listen",
            "127.0.0.1:4000",
            "--dir",
            "x",
            "--key-file",
            "k.txt",
            "--gemini-url",
            "ws://127.0.0.1:9/",
            "-v",
        ]))
        .unwrap();
        assert_eq!(cfg.listen.port(), 4000);
        assert_eq!(cfg.dir, "x");
        assert_eq!(cfg.key_file.as_deref(), Some("k.txt"));
        assert_eq!(cfg.gemini_url, "ws://127.0.0.1:9/");
        assert!(cfg.verbose);
    }

    #[test]
    fn check_und_fehler() {
        let (_, a) = Config::from_args(&args(&["--check", "http://h:1"])).unwrap();
        assert_eq!(
            a,
            Action::Check {
                base: "http://h:1".into()
            }
        );
        assert!(Config::from_args(&args(&["--listen"])).is_err());
        assert!(Config::from_args(&args(&["--quatsch"])).is_err());
    }

    #[test]
    fn transport_parse() {
        assert_eq!(Transport::parse("opus"), Transport::Opus);
        assert_eq!(Transport::parse("OPUS"), Transport::Opus);
        assert_eq!(Transport::parse("pcm"), Transport::Pcm);
        assert_eq!(Transport::parse(""), Transport::Pcm);
    }
}
