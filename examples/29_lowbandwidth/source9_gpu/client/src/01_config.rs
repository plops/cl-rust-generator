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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_and_options() {
        let c = Config::try_parse_from(["lbw-client"]).unwrap();
        assert_eq!(c.connect, "127.0.0.1:7878");
        let c = Config::try_parse_from(["lbw-client", "--connect", "h:1"]).unwrap();
        assert_eq!(c.connect, "h:1");
        assert!(Config::try_parse_from(["lbw-client", "--bogus"]).is_err());
    }
}
