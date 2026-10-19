//! `01_config` — Kommandozeile des GPU-Servers (clap-derive).

use clap::Parser;

/// `lbw-server` — Minimal Low-Bandwidth Remote Desktop Server (GPU).
///
/// Nur an localhost binden — davor steht immer der SSH-Tunnel (Clients
/// bauen ihn selbst). Zusätzlich beweist jeder Client das LBW-Token
/// (Challenge/Auth). Display-Auswahl per `$DISPLAY`.
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
    /// LBW-Token direkt (Alternative zu --token-file; keine Defaults!).
    #[arg(long)]
    pub token: Option<String>,
    /// Datei mit dem LBW-Token (0600, eine Zeile).
    #[arg(long)]
    pub token_file: Option<String>,
    /// Kanalrate in Byte/s: Kacheln werden auf diesen Rinnsal dosiert
    /// (Texte haben Vorfahrt und gehen sofort raus).
    #[arg(long, default_value_t = 6000)]
    pub budget: u32,
    /// Kachel-Auflösung: 1 = voll, 2 = halb, 4 = viertel (kodierte Pixel;
    /// der Client skaliert auf den vollen Fußabdruck hoch).
    #[arg(long, default_value_t = 1)]
    pub tile_scale: u32,
    /// Quantizer für die eine hochwertige Vollbild-Kachel (F3).
    #[arg(long, default_value_t = 180)]
    pub hq_quantizer: usize,
    /// Pipeline-Log.
    #[arg(short, long)]
    pub verbose: bool,
    /// Ablaufverfolgung in diese Datei (Live-Diagnose, s. `trace`; ohne
    /// Flag vollständig abgeschaltet).
    #[arg(long)]
    pub trace: Option<String>,
}

impl Config {
    /// Prüft Wertebereiche (clap parst nur Typen).
    pub fn validate(&self) -> Result<(), String> {
        if self.quantizer > 255 {
            return Err("--quantizer muss zwischen 0 und 255 liegen".into());
        }
        if self.hq_quantizer > 255 {
            return Err("--hq-quantizer muss zwischen 0 und 255 liegen".into());
        }
        if self.budget == 0 {
            return Err("--budget muss größer als 0 sein".into());
        }
        if !matches!(self.tile_scale, 1 | 2 | 4) {
            return Err("--tile-scale kennt nur 1, 2 oder 4".into());
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

    /// LBW-Token als Bytes (--token schlägt --token-file).
    pub fn token(&self) -> Result<Vec<u8>, String> {
        if let Some(t) = &self.token {
            return Ok(t.trim().as_bytes().to_vec());
        }
        if let Some(p) = &self.token_file {
            let s = std::fs::read_to_string(p).map_err(|e| format!("{p}: {e}"))?;
            return Ok(s.trim().as_bytes().to_vec());
        }
        Err("kein LBW-Token (--token oder --token-file)".into())
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
        assert_eq!(c.token, None);
        assert_eq!(c.token_file, None);
        assert_eq!(c.budget, 6000);
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
            "--token",
            "geheim",
            "--budget",
            "9000",
            "-v",
        ])
        .unwrap();
        assert!(c.is_public());
        assert_eq!((c.x, c.y, c.quantizer), (10, 20, 99));
        assert_eq!(c.budget, 9000);
        assert_eq!(c.models, "/m");
        assert_eq!(c.token.as_deref(), Some("geheim"));
        assert!(c.verbose && c.cpu);
        assert!(c.token().is_ok());
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
        let c = Config::try_parse_from(["lbw-server", "--budget", "0"]).unwrap();
        assert!(c.validate().is_err());
    }
}
