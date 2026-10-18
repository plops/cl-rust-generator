//! `01_config` — Kommandozeile des Clients (clap-derive).

use clap::Parser;

/// `lbw-client` — Minimal Low-Bandwidth Remote Desktop Client.
/// Das Bild ist immer 1280×720 (F1: HUD an/aus). `connect` zeigt auf das
/// Tunnel-Ende (russh-Forward oder `ssh -L`); das LBW-Token beweist die
/// Berechtigung (Challenge/Auth).
#[derive(Clone, Debug, Parser)]
#[command(name = "lbw-client", version)]
pub struct Config {
    /// Tunnel-Ende (typ. lokaler Forward-Port).
    #[arg(long, default_value = "127.0.0.1:7878")]
    pub connect: String,
    /// LBW-Token direkt (Alternative zu --token-file).
    #[arg(long)]
    pub token: Option<String>,
    /// Datei mit dem LBW-Token (0600, eine Zeile).
    #[arg(long)]
    pub token_file: Option<String>,
    /// Debug: lokaler Forward-Port (Default ephemer). Fixiert lässt er
    /// den Klartext-Strom per Wireshark auf `lo` mitschneiden.
    #[arg(long)]
    pub tap: Option<u16>,
    /// Schriftdatei (Default: System-Fonts, s. `06_font`).
    #[arg(long)]
    pub font: Option<String>,
    /// SSH-Ziel (`user@host[:port]`): Tunnel statt Direkt-Connect.
    #[arg(long)]
    pub via: Option<String>,
    /// Identity-Datei fürs Ziel (OpenSSH-Format, inkl. Ed25519).
    #[arg(long)]
    pub via_identity: Option<String>,
    /// Jump-Host (`user@host[:port]`, wiederholbar, verkettbar).
    #[arg(long)]
    pub jump: Vec<String>,
    /// Identity-Datei für alle Jumps.
    #[arg(long)]
    pub jump_identity: Option<String>,
    /// Ziel-sshd aus Sicht des letzten Jumps (`host:port`, z. B.
    /// `127.0.0.1:2332`); ohne Jump ignoriert.
    #[arg(long)]
    pub target_sshd: Option<String>,
    /// LBW-Port am Ziel (Default 7878).
    #[arg(long, default_value_t = 7878)]
    pub lbw_port: u16,
    /// Erwartete Hostkey-Fingerprints (`SHA256:…`, Hop-Reihenfolge).
    #[arg(long)]
    pub pin: Vec<String>,
}

impl Config {
    /// LBW-Token als Bytes (--token schlägt --token-file).
    pub fn token_bytes(&self) -> Result<Vec<u8>, String> {
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
    fn defaults_and_options() {
        let c = Config::try_parse_from(["lbw-client"]).unwrap();
        assert_eq!(c.connect, "127.0.0.1:7878");
        assert_eq!(c.token, None);
        assert_eq!(c.tap, None);
        let c = Config::try_parse_from(["lbw-client", "--connect", "h:1"]).unwrap();
        assert_eq!(c.connect, "h:1");
        let c = Config::try_parse_from(["lbw-client", "--token", "x", "--tap", "9999"]).unwrap();
        assert_eq!(c.token.as_deref(), Some("x"));
        assert_eq!(c.tap, Some(9999));
        assert_eq!(c.token_bytes().unwrap(), b"x");
        assert!(Config::try_parse_from(["lbw-client", "--bogus"]).is_err());
    }
}
