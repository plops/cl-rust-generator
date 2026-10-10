//! `01_config` — Kommandozeile des GPU-Clients (clap-derive).

use clap::Parser;

/// `lbw-client` — Minimal Low-Bandwidth Remote Desktop Client (GPU).
/// Das Bild ist immer 1280×720 (F1: HUD an/aus).
#[derive(Clone, Debug, Parser)]
#[command(name = "lbw-client", version)]
pub struct Config {
    /// Server (typ. Ende von `ssh -L`).
    #[arg(long, default_value = "127.0.0.1:7878")]
    pub connect: String,
    /// Aufnahme aller Nachrichten + Timings in diese Datei (.lbwlog).
    #[arg(long)]
    pub record: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_and_options() {
        let c = Config::try_parse_from(["lbw-client"]).unwrap();
        assert_eq!(c.connect, "127.0.0.1:7878");
        assert_eq!(c.record, None);
        let c = Config::try_parse_from(["lbw-client", "--connect", "h:1"]).unwrap();
        assert_eq!(c.connect, "h:1");
        let c = Config::try_parse_from(["lbw-client", "--record", "/tmp/x.lbwlog"]).unwrap();
        assert_eq!(c.record.as_deref(), Some("/tmp/x.lbwlog"));
        assert!(Config::try_parse_from(["lbw-client", "--bogus"]).is_err());
    }
}
