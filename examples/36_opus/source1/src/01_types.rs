//! Kerntypen: Fehler, Encoder-Modi, Frame-Metadaten, Summary, CLI-Config.
//!
//! Keine Abhaengigkeiten ausser `std` (+ `opus-pure`-Fehler als Quelle).

use std::fmt;
use std::path::PathBuf;

/// Alle Fehler von `opus-vis`, mit Exit-Code-Mapping fuer `main`.
#[derive(Debug)]
pub enum Error {
    /// IO-Fehler (Datei lesen/schreiben).
    Io(std::io::Error),
    /// Codec-/Container-Fehler aus `opus-pure`.
    Opus(opus_pure::Error),
    /// Leeres Opus-Paket (kein TOC-Byte vorhanden).
    EmptyPacket,
    /// Defektes Paket (z.B. Frame-Zahl 0 bei Packing-Code 3).
    BadPacket(&'static str),
    /// Fenster-Fehler (nur Feature `show`).
    Show(String),
    /// Mehr als 2 Kanaele werden nicht visualisiert.
    UnsupportedChannels(u8),
    /// Falscher Aufruf (fuer `--help`-nahe Meldungen).
    Usage(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => write!(f, "io: {err}"),
            Self::Opus(err) => write!(f, "opus: {err}"),
            Self::EmptyPacket => write!(f, "opus: leeres Paket (kein TOC-Byte)"),
            Self::BadPacket(why) => write!(f, "opus: defektes Paket ({why})"),
            Self::Show(msg) => write!(f, "fenster: {msg}"),
            Self::UnsupportedChannels(ch) => {
                write!(f, "nur 1-2 Kanaele werden unterstuetzt, Datei hat {ch}")
            }
            Self::Usage(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    /// Exit-Code: 1 = Aufruffehler, 2 = Analyse-/IO-Fehler.
    #[must_use]
    pub fn exit_code(&self) -> u8 {
        match self {
            Self::Usage(_) => 1,
            _ => 2,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<opus_pure::Error> for Error {
    fn from(err: opus_pure::Error) -> Self {
        Self::Opus(err)
    }
}

/// Opus-Codiermodus aus dem TOC-Byte: Sprache (SILK), Hybrid, Musik (CELT).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// SILK: sprach-optimiert.
    Silk,
    /// Hybrid: SILK unten, CELT oben.
    Hybrid,
    /// CELT: musik-optimiert.
    Celt,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Silk => "SILK",
            Self::Hybrid => "Hybrid",
            Self::Celt => "CELT",
        };
        write!(f, "{s}")
    }
}

/// Audio-Bandbreite aus dem TOC-Byte (RFC 6716, oberer -3-dB-Punkt grob).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bandwidth {
    /// Schmalband: bis ~4 kHz.
    Narrow,
    /// Mittelband: bis ~6 kHz.
    Medium,
    /// Breitband: bis ~8 kHz.
    Wide,
    /// Superbreitband: bis ~12 kHz.
    SuperWide,
    /// Vollband: bis ~20 kHz.
    Full,
}

impl Bandwidth {
    /// Obere Grenzfrequenz in Hz (fuer Achse und BW-Linie).
    #[must_use]
    pub fn hz(self) -> u32 {
        match self {
            Self::Narrow => 4000,
            Self::Medium => 6000,
            Self::Wide => 8000,
            Self::SuperWide => 12000,
            Self::Full => 20000,
        }
    }

    /// Kurzkuerzel fuer Legende/JSON.
    #[must_use]
    pub fn short(self) -> &'static str {
        match self {
            Self::Narrow => "NB",
            Self::Medium => "MB",
            Self::Wide => "WB",
            Self::SuperWide => "SWB",
            Self::Full => "FB",
        }
    }
}

/// Metadaten eines Pakets (= ein oder mehrere Frames gleicher Config).
#[derive(Debug, Clone, PartialEq)]
pub struct FrameMeta {
    /// Laufende Nummer (0-basiert).
    pub index: usize,
    /// Startzeit in ms (kumulierte Dauern).
    pub t_start_ms: f64,
    /// Paketdauer in ms (Frames × Frame-Dauer).
    pub duration_ms: f64,
    /// Codiermodus.
    pub mode: Mode,
    /// Audio-Bandbreite.
    pub bandwidth: Bandwidth,
    /// Kanaele (1/2).
    pub channels: u8,
    /// Paketgroesse in Bytes.
    pub bytes: usize,
    /// Momentan-Bitrate in kbit/s.
    pub kbps: f64,
}

/// Datei-Summary fuer Kopfzeile und JSON.
#[derive(Debug, Clone, PartialEq)]
pub struct Summary {
    /// Zahl der Audio-Pakete.
    pub frames: usize,
    /// Dauer in ms.
    pub duration_ms: f64,
    /// Summe Paket-Bytes (Audio ohne Container).
    pub audio_bytes: u64,
    /// Datei-Bytes gesamt (Container inkl. Header).
    pub total_bytes: u64,
    /// Mittlere Audio-Bitrate in kbit/s.
    pub avg_kbps: f64,
    /// Roh-Bitrate (s16, 48 kHz) in kbit/s.
    pub raw_kbps: f64,
    /// Kompressionsfaktor (roh / gesamt).
    pub ratio: f64,
    /// Container-Overhead in Prozent der Datei.
    pub overhead_pct: f64,
    /// Modusanteile (Zeit): SILK, Hybrid, CELT.
    pub frac_silk: f64,
    /// Siehe `frac_silk`.
    pub frac_hybrid: f64,
    /// Siehe `frac_silk`.
    pub frac_celt: f64,
    /// Kanaele aus dem Opus-Header.
    pub channels: u8,
}

/// Analyse-Ergebnis: Frames + Summary.
#[derive(Debug, Clone, PartialEq)]
pub struct Analysis {
    /// Alle Pakete in Zeitreihenfolge.
    pub frames: Vec<FrameMeta>,
    /// Aggregierte Kennzahlen.
    pub summary: Summary,
}

/// Eingabe: Datei oder stdin (`-`).
#[derive(Debug, Clone, PartialEq)]
pub enum Input {
    /// Von stdin lesen.
    Stdin,
    /// Aus Datei lesen.
    Path(PathBuf),
}

/// Ausgabe: Datei oder stdout (`-`).
#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    /// Nach stdout schreiben.
    Stdout,
    /// In Datei schreiben.
    Path(PathBuf),
}

/// Geparste CLI-Argumente.
#[derive(Debug, Clone, PartialEq)]
pub struct CliConfig {
    /// Eingabedatei.
    pub input: Input,
    /// PNG-Ausgabe (Default: Eingabe-Stamm + `.png`).
    pub png_out: Output,
    /// JSON-Ausgabe (optional).
    pub json_out: Option<Output>,
    /// Plot-Breite (`None` = automatisch).
    pub width: Option<u32>,
    /// Fenster anzeigen.
    pub show: bool,
    /// Fenster-Frames (Test-Haken, Default `u64::MAX`).
    pub show_frames: u64,
}
