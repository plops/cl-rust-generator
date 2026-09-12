//! Mode command/response types shared by host and firmware (protocol v2).
//!
//! Wire enums live in `super::{HostCmd, DeviceResp}`; this module holds the
//! per-mode parameter structs, validation, and pure conversions.

/// Mode ids on the wire (`DeviceResp::ModeOk::mode`, `ModeStop` target).
pub mod id {
    pub const NONE: u8 = 0;
    pub const A_SCOPE: u8 = 1;
    pub const B_VNA: u8 = 2;
    pub const C_AWG: u8 = 3;
    pub const D_CAP: u8 = 4;
    pub const E_FREQ: u8 = 5;
}

/// Frequency-counter configuration (mode E, spec §3.5).
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct FreqConfig {
    /// Comparator trigger level in mV, 0..=3300 (VDDA-referenced).
    pub level_mv: u16,
    /// Comparator hysteresis step, 0..=3 (None/Low/Medium/High).
    pub hyst: u8,
    /// TIM2 digital input-filter step, 0..=15.
    pub filter: u8,
    /// Gate time in ms, 10..=10000.
    pub gate_ms: u32,
}

impl FreqConfig {
    pub fn validate(&self) -> Result<(), u8> {
        if self.level_mv > 3300 {
            return Err(super::err::BAD_ARG);
        }
        if self.hyst > 3 {
            return Err(super::err::BAD_ARG);
        }
        if self.filter > 15 {
            return Err(super::err::BAD_ARG);
        }
        if !(10..=10000).contains(&self.gate_ms) {
            return Err(super::err::BAD_ARG);
        }
        Ok(())
    }

    /// Counts collected in `gate_ms` → Hz (integer, rounds down).
    pub fn hz_from_counts(counts: u32, gate_ms: u32) -> u32 {
        if gate_ms == 0 {
            return 0;
        }
        ((counts as u64 * 1000) / gate_ms as u64).min(u32::MAX as u64) as u32
    }
}

/// DAC code (12 bit) for a trigger level in mV against 3.3 V VDDA.
pub fn dac12_from_mv(mv: u16) -> u16 {
    ((mv.min(3300) as u32 * 4095) / 3300) as u16
}

/// Oscilloscope acquisition config (mode A, spec §3.1). RTS first, ETS later.
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ScopeConfig {
    /// 0 = single ADC, 1 = 16 MSPS interleaved (ADC1..4, stage 2).
    pub interleaved: u8,
    /// Trigger level in mV (COMP1 + DAC3), 0..=3300.
    pub level_mv: u16,
}

impl ScopeConfig {
    pub fn validate(&self) -> Result<(), u8> {
        if self.interleaved > 1 {
            return Err(super::err::BAD_ARG);
        }
        if self.level_mv > 3300 {
            return Err(super::err::BAD_ARG);
        }
        Ok(())
    }
}

/// AWG output config (mode C, spec §3.3).
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct AwgConfig {
    /// Output frequency in Hz, 1..=1_000_000.
    pub freq_hz: u32,
}

impl AwgConfig {
    pub fn validate(&self) -> Result<(), u8> {
        if (1..=1_000_000).contains(&self.freq_hz) {
            Ok(())
        } else {
            Err(super::err::BAD_ARG)
        }
    }
}

/// Capacitance-meter config (mode D, spec §3.4).
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CapConfig {
    /// TDM channel index, 0..=2.
    pub pin: u8,
}

/// VNA sweep config (mode B, spec §3.2).
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct VnaConfig {
    pub f0_hz: u32,
    pub f1_hz: u32,
    /// Sweep points, 2..=128 (bounds total sweep time).
    pub points: u16,
}

impl VnaConfig {
    /// Frequencies in Hz for all sweep points (linear, inclusive).
    pub fn freq_at(&self, i: usize) -> u32 {
        if self.points <= 1 {
            return self.f0_hz;
        }
        let span = self.f1_hz.saturating_sub(self.f0_hz) as u64;
        self.f0_hz + (span * i as u64 / (self.points as u64 - 1)) as u32
    }

    pub fn validate(&self) -> Result<(), u8> {
        if self.f0_hz == 0 || self.f0_hz > 1_000_000 {
            return Err(super::err::BAD_ARG);
        }
        if self.f1_hz < self.f0_hz || self.f1_hz > 1_000_000 {
            return Err(super::err::BAD_ARG);
        }
        if !(2..=128).contains(&self.points) {
            return Err(super::err::BAD_ARG);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn freq_config_bounds() {
        let ok = FreqConfig {
            level_mv: 1650,
            hyst: 2,
            filter: 4,
            gate_ms: 1000,
        };
        assert!(ok.validate().is_ok());
        assert!(FreqConfig {
            level_mv: 3301,
            ..ok
        }
        .validate()
        .is_err());
        assert!(FreqConfig { hyst: 4, ..ok }.validate().is_err());
        assert!(FreqConfig { filter: 16, ..ok }.validate().is_err());
        assert!(FreqConfig { gate_ms: 9, ..ok }.validate().is_err());
        assert!(FreqConfig {
            gate_ms: 10001,
            ..ok
        }
        .validate()
        .is_err());
    }

    #[test]
    fn hz_math() {
        assert_eq!(FreqConfig::hz_from_counts(1000, 1000), 1000);
        assert_eq!(FreqConfig::hz_from_counts(500, 1000), 500);
        assert_eq!(FreqConfig::hz_from_counts(1, 10), 100);
        assert_eq!(FreqConfig::hz_from_counts(7, 0), 0);
        assert_eq!(FreqConfig::hz_from_counts(u32::MAX, 1), u32::MAX);
    }

    #[test]
    fn scope_config_bounds() {
        assert!(ScopeConfig {
            interleaved: 0,
            level_mv: 3300
        }
        .validate()
        .is_ok());
        assert!(ScopeConfig {
            interleaved: 2,
            level_mv: 0
        }
        .validate()
        .is_err());
        assert!(ScopeConfig {
            interleaved: 0,
            level_mv: 3301
        }
        .validate()
        .is_err());
    }

    #[test]
    fn awg_config_bounds() {
        assert!(AwgConfig { freq_hz: 1 }.validate().is_ok());
        assert!(AwgConfig { freq_hz: 1_000_000 }.validate().is_ok());
        assert!(AwgConfig { freq_hz: 0 }.validate().is_err());
        assert!(AwgConfig { freq_hz: 1_000_001 }.validate().is_err());
    }

    #[test]
    fn vna_config_sweep() {
        let cfg = VnaConfig {
            f0_hz: 1000,
            f1_hz: 3000,
            points: 3,
        };
        assert!(cfg.validate().is_ok());
        assert_eq!(cfg.freq_at(0), 1000);
        assert_eq!(cfg.freq_at(1), 2000);
        assert_eq!(cfg.freq_at(2), 3000);
        assert!(VnaConfig { f0_hz: 0, ..cfg }.validate().is_err());
        assert!(VnaConfig { f1_hz: 999, ..cfg }.validate().is_err());
        assert!(VnaConfig { points: 1, ..cfg }.validate().is_err());
        assert!(VnaConfig { points: 129, ..cfg }.validate().is_err());
    }

    #[test]
    fn dac12_mapping() {
        assert_eq!(dac12_from_mv(0), 0);
        assert_eq!(dac12_from_mv(3300), 4095);
        assert_eq!(dac12_from_mv(1650), 2047);
        assert_eq!(dac12_from_mv(9999), 4095);
    }
}
