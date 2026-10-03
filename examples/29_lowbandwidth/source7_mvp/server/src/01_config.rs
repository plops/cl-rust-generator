//! `01_config` — Kommandozeile des MVP-Servers (clap-derive).

use clap::Parser;

/// `lbw-server` — Minimal Low-Bandwidth Remote Desktop Server (MVP).
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
    /// Kantenlänge des quadratischen Ausschnitts (Vielfaches von 64).
    #[arg(long, default_value_t = 640)]
    pub size: u32,
    /// AV1-Quantizer 0..=255 (höher = kleiner/schlechter).
    #[arg(long, default_value_t = 180)]
    pub quantizer: usize,
    /// Modellverzeichnis (PP-OCRv6). Fehlt es, läuft der Server ohne Text.
    #[arg(long, default_value = "models")]
    pub models: String,
    /// Kein OCR (nur AV1-Kacheln).
    #[arg(long)]
    pub no_ocr: bool,
    /// Eingaben des Clients ignorieren.
    #[arg(long)]
    pub no_input: bool,
    /// Pipeline-Log.
    #[arg(short, long)]
    pub verbose: bool,
}

impl Config {
    /// Prüft Wertebereiche (clap parst nur Typen).
    pub fn validate(&self) -> Result<(), String> {
        if self.size < 64 || self.size > 2048 || !self.size.is_multiple_of(64) {
            return Err("--size muss ein Vielfaches von 64 zwischen 64 und 2048 sein".into());
        }
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
    fn defaults_are_local_and_640() {
        let c = Config::try_parse_from(["lbw-server"]).unwrap();
        assert_eq!(
            (c.listen.as_str(), c.size, c.quantizer),
            ("127.0.0.1:7878", 640, 180)
        );
        assert!(!c.is_public() && !c.no_ocr && !c.no_input && !c.verbose);
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
            "--size",
            "512",
            "--quantizer",
            "99",
            "--models",
            "/m",
            "--no-ocr",
            "--no-input",
            "-v",
        ])
        .unwrap();
        assert!(c.is_public());
        assert_eq!((c.x, c.y, c.size, c.quantizer), (10, 20, 512, 99));
        assert_eq!(c.models, "/m");
        assert!(c.no_ocr && c.no_input && c.verbose);
        c.validate().unwrap();
    }

    #[test]
    fn bad_values_are_rejected() {
        // Unbekannte Option scheitert schon beim Parsen.
        assert!(Config::try_parse_from(["lbw-server", "--bogus"]).is_err());
        assert!(Config::try_parse_from(["lbw-server", "--size", "x"]).is_err());
        // Falsche Wertebereiche scheitern bei validate().
        for args in [
            vec!["--size", "100"],
            vec!["--size", "0"],
            vec!["--quantizer", "256"],
        ] {
            let mut v = vec!["lbw-server"];
            v.extend(args);
            let c = Config::try_parse_from(v).unwrap();
            assert!(c.validate().is_err());
        }
    }
}
