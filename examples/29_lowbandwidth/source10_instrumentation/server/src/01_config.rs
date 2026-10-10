//! `01_config` — Kommandozeile des GPU-Servers (clap-derive).

use clap::Parser;

/// `lbw-server` — Minimal Low-Bandwidth Remote Desktop Server (GPU).
///
/// Ohne Authentifizierung: nur an localhost binden oder per `ssh -L`/`-R`
/// zugreifen! Display-Auswahl per `$DISPLAY`.
#[derive(Clone, Debug, Parser)]
#[command(name = "lbw-server", version)]
pub struct Config {
    /// Adresse (Default nur localhost).
    #[arg(long, default_value = "127.0.0.1:7878")]
    pub listen: String,
    /// Linke obere Ecke des Ausschnitts.
    #[arg(long, default_value_t = 0)]
    pub x: u32,
    /// Linke obere Ecke des Ausschnitts.
    #[arg(long, default_value_t = 0)]
    pub y: u32,
    /// AV1-Quantizer 0..=255 (höher = kleiner/schlechter).
    #[arg(long, default_value_t = 180)]
    pub quantizer: usize,
    /// Modellverzeichnis (PP-OCRv6). Fehlt es, startet der Server nicht.
    #[arg(long, default_value = "models")]
    pub models: String,
    /// Alles auf CPU erzwingen (Default: Hybrid Detektor-CUDA/Erkenner-CPU).
    #[arg(long)]
    pub cpu: bool,
    /// Aufnahme aller Nachrichten + Timings in diese Datei (.lbwlog).
    #[arg(long)]
    pub record: Option<String>,
    /// Pipeline-Log.
    #[arg(short, long)]
    pub verbose: bool,
}

impl Config {
    /// Prüft Wertebereiche (clap parst nur Typen).
    pub fn validate(&self) -> Result<(), String> {
        if self.quantizer > 255 {
            return Err("--quantizer muss zwischen 0 und 255 liegen".into());
        }
        Ok(())
    }

    /// Bindet die Adresse nicht an localhost?
    #[must_use]
    pub fn is_public(&self) -> bool {
        !(self.listen.starts_with("127.")
            || self.listen.starts_with("[::1]")
            || self.listen.starts_with("localhost"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_are_local() {
        let c = Config::try_parse_from(["lbw-server"]).unwrap();
        assert_eq!((c.listen.as_str(), c.quantizer), ("127.0.0.1:7878", 180));
        assert!(!c.is_public() && !c.verbose && !c.cpu);
        assert_eq!(c.record, None);
        c.validate().unwrap();
    }

    #[test]
    fn options_are_parsed() {
        let c = Config::try_parse_from([
            "lbw-server",
            "--listen",
            "0.0.0.0:9",
            "--x",
            "10",
            "--y",
            "20",
            "--quantizer",
            "99",
            "--models",
            "/m",
            "--cpu",
            "--record",
            "/tmp/x.lbwlog",
            "-v",
        ])
        .unwrap();
        assert!(c.is_public());
        assert_eq!((c.x, c.y, c.quantizer), (10, 20, 99));
        assert_eq!(c.models, "/m");
        assert_eq!(c.record.as_deref(), Some("/tmp/x.lbwlog"));
        assert!(c.verbose && c.cpu);
        c.validate().unwrap();
    }

    #[test]
    fn bad_values_are_rejected() {
        // Unbekannte Option scheitert schon beim Parsen.
        assert!(Config::try_parse_from(["lbw-server", "--bogus"]).is_err());
        assert!(Config::try_parse_from(["lbw-server", "--no-ocr"]).is_err());
        assert!(Config::try_parse_from(["lbw-server", "--dump"]).is_err());
        assert!(Config::try_parse_from(["lbw-server", "--no-input"]).is_err());
        assert!(Config::try_parse_from(["lbw-server", "--size"]).is_err());
        assert!(Config::try_parse_from(["lbw-server", "--quantizer", "x"]).is_err());
        // Falsche Wertebereiche scheitern bei validate().
        let c = Config::try_parse_from(["lbw-server", "--quantizer", "256"]).unwrap();
        assert!(c.validate().is_err());
    }
}
