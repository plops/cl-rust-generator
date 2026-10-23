//! 01 — Konfiguration: CLI, Modi-Tabelle (4 Sprachpaare).
//!
//! Der Relay uebersetzt nichts selbst; die Tabelle beschreibt, was das
//! Frontend bei Gemini anfordert. Sie wird auch ueber `/api/modes`
//! ausgeliefert, damit Frontend und Doku nie auseinanderlaufen.

use std::net::SocketAddr;

/// Ein Sprachmodus: welche Quellsprache wird in welche Zielsprache
/// gedolmetscht, mit welcher Gemini-Stimme.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Mode {
    /// Stabile Kennung, auch `<option value=...>` im Frontend.
    pub id: &'static str,
    /// Deutsche Anzeige-Bezeichnung.
    pub label_de: &'static str,
    /// Systemanweisung an Gemini (englisch, strikt: nur Uebersetzung).
    pub instruction_en: &'static str,
    /// Stimmenname (`Aoede`, `Puck`, `Charon`, `Fenrir`, `Kore`).
    pub voice: &'static str,
}

/// Alle vier unterstuetzten Sprachpaare in fester Reihenfolge.
pub const MODES: [Mode; 4] = [
    Mode {
        id: "de-to-zh",
        label_de: "Ich spreche Deutsch → Partner hört Chinesisch",
        instruction_en: "You are an ultra-low latency simultaneous speech-to-speech interpreter. Listen to the German audio. Instantly translate and speak it in Chinese (Mandarin). CRITICAL: Produce ONLY the direct translation audio. Do NOT talk back, converse, explain, or add comments.",
        voice: "Aoede",
    },
    Mode {
        id: "zh-to-de",
        label_de: "Ich spreche Chinesisch → Partner hört Deutsch",
        instruction_en: "You are an ultra-low latency simultaneous speech-to-speech interpreter. Listen to the Chinese audio. Instantly translate and speak it in German. CRITICAL: Produce ONLY the direct translation audio. Do NOT talk back, converse, explain, or add comments.",
        voice: "Puck",
    },
    Mode {
        id: "en-to-zh",
        label_de: "Ich spreche Englisch → Partner hört Chinesisch",
        instruction_en: "Simultaneous interpreter English to Mandarin Chinese. Output ONLY translated speech.",
        voice: "Aoede",
    },
    Mode {
        id: "zh-to-en",
        label_de: "Ich spreche Chinesisch → Partner hört Englisch",
        instruction_en: "Simultaneous interpreter Mandarin Chinese to English. Output ONLY translated speech.",
        voice: "Puck",
    },
];

/// Modus per Kennung suchen (ergibt `None` bei unbekannter Kennung).
pub fn find_mode(id: &str) -> Option<Mode> {
    MODES.iter().copied().find(|m| m.id == id)
}

/// Servereinstellung aus CLI/Umgebung.
#[derive(Debug, Clone)]
pub struct Config {
    /// Lauschadresse, z. B. `0.0.0.0:3000`.
    pub listen: SocketAddr,
    /// Verzeichnis mit `index.html`.
    pub dir: String,
    /// Max. Peers pro Raum (Lastgrenze gegen langsame Verbraucher).
    pub max_peers_per_room: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            listen: "0.0.0.0:3000".parse().expect("feste Adresse"),
            dir: "public".to_owned(),
            max_peers_per_room: 8,
        }
    }
}

impl Config {
    /// Sehr kleine CLI: `--listen ADDR`, `--dir PFAD`, `--check URL`,
    /// `--watch URL`. Gibt `Err` mit Hilfetext bei falscher Nutzung aus.
    pub fn from_args(args: &[String]) -> Result<(Self, Action), String> {
        let mut cfg = Self::default();
        let mut action = Action::Serve;
        let mut i = 1;
        while i < args.len() {
            match args[i].as_str() {
                "--listen" => {
                    i += 1;
                    cfg.listen = args
                        .get(i)
                        .ok_or("--listen braucht ADDR")?
                        .parse()
                        .map_err(|_| "--listen ungueltig")?;
                }
                "--dir" => {
                    i += 1;
                    cfg.dir = args.get(i).ok_or("--dir braucht PFAD")?.clone();
                }
                "--check" => {
                    i += 1;
                    let base = args.get(i).ok_or("--check braucht URL")?.clone();
                    action = Action::Check { base };
                }
                "--watch" => {
                    i += 1;
                    let base = args.get(i).ok_or("--watch braucht URL")?.clone();
                    action = Action::Watch { base };
                }
                "-h" | "--help" => return Err(Self::usage()),
                other => return Err(format!("unbekannt: {other}\n{}", Self::usage())),
            }
            i += 1;
        }
        Ok((cfg, action))
    }

    fn usage() -> String {
        "live-relay [--listen ADDR] [--dir PFAD] [--check URL | --watch URL]".to_owned()
    }
}

/// Betriebsart: dienen oder Terminal-Status (TUI-light, nur `std`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Serve,
    Check { base: String },
    Watch { base: String },
}
