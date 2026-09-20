//! 00_preset: Persistente Konfiguration (TOML) mit Format-Versionierung.
//! Wird vor allem anderen geladen (daher Nummer 00). Unbekannte `ver`
//! wird mit klarer Meldung abgelehnt statt abzustuerzen (s. plan.md Kap. 5).

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

/// Preset-Format-Version.
pub const PRESET_VER: u32 = 1;

/// Host-seitig gespeicherte Session-Einstellungen (kein Firmware-Flash).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Preset {
    pub ver: u32,
    pub bpm: f64,
    pub bars: u32,
    pub gain_db: f32,
}

impl Default for Preset {
    fn default() -> Self {
        Self {
            ver: PRESET_VER,
            bpm: 174.0,
            bars: 52,
            gain_db: -6.0,
        }
    }
}

impl Preset {
    /// TOML parsen + Version pruefen.
    pub fn from_toml(text: &str) -> Result<Self> {
        let p: Preset = toml::from_str(text).context("preset parse")?;
        if p.ver != PRESET_VER {
            anyhow::bail!("unsupported preset ver {} (expected {PRESET_VER})", p.ver);
        }
        if !(60.0..=220.0).contains(&p.bpm) {
            anyhow::bail!("bpm out of range: {}", p.bpm);
        }
        Ok(p)
    }

    pub fn load(path: &str) -> Result<Self> {
        let text = std::fs::read_to_string(path).with_context(|| format!("read {path}"))?;
        Self::from_toml(&text)
    }

    pub fn save(&self, path: &str) -> Result<()> {
        let text = toml::to_string_pretty(self).context("preset serialize")?;
        std::fs::write(path, text).with_context(|| format!("write {path}"))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_default() {
        let p = Preset::default();
        let text = toml::to_string_pretty(&p).unwrap();
        assert_eq!(Preset::from_toml(&text).unwrap(), p);
    }

    #[test]
    fn wrong_ver_is_rejected() {
        let text = "ver = 99\nbpm = 174.0\nbars = 52\ngain_db = -6.0\n";
        assert!(Preset::from_toml(text).is_err());
    }

    #[test]
    fn bpm_out_of_range_is_rejected() {
        let text = "ver = 1\nbpm = 400.0\nbars = 52\ngain_db = -6.0\n";
        assert!(Preset::from_toml(text).is_err());
    }
}
