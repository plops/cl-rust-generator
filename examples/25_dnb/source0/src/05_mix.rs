//! 05_mix: Bus-Summe, Gain-Clamp (Gehoer-Schutz), Soft-Clip.
//! Segens-Kette Declick/DC-Block/Limiter (fundsp) folgt in Task S4;
//! hier deterministische, testbare Pegel-Bausteine.

/// Maximal zulaessiges Gain in dB (Software-Limit, s. plan.md Kap. 5).
pub const MAX_GAIN_DB: f32 = 6.0;

/// dB in linear umrechnen, auf `MAX_GAIN_DB` geclampt.
pub fn gain_linear(gain_db: f32) -> f32 {
    let clamped = gain_db.min(MAX_GAIN_DB);
    10.0f32.powf(clamped / 20.0)
}

/// Soft-Clip via tanh (Schutz vor digitalem Hard-Clip).
pub fn soft_clip(x: f32) -> f32 {
    x.tanh()
}

/// Mische Bass + Drums mit Sektions-Gain und Master-Gain, dann Soft-Clip.
pub fn mix(bass: f32, drums: f32, section_gain: f32, gain_db: f32) -> f32 {
    soft_clip((bass + drums) * section_gain * gain_linear(gain_db))
}

/// Master-Kette: DC-Block (One-Pole-Hochpass, ~5 Hz) + Peak-Limiter.
/// Deterministisch und zustandsbehaftet — ein `Master` pro Render/Stream.
/// Ziel: Peak <= `ceiling` (default −1 dBFS ≈ 0.891), kein DC-Offset.
pub struct Master {
    dc_in: f32,
    dc_out: f32,
    ceiling: f32,
}

impl Master {
    pub fn new(sample_rate: f32, ceiling_dbfs: f32) -> Self {
        let _ = sample_rate;
        Self {
            dc_in: 0.0,
            dc_out: 0.0,
            ceiling: 10.0f32.powf(ceiling_dbfs / 20.0),
        }
    }

    pub fn process(&mut self, x: f32) -> f32 {
        // DC-Block: y[n] = x[n] - x[n-1] + 0.999 * y[n-1].
        let y = x - self.dc_in + 0.999 * self.dc_out;
        self.dc_in = x;
        self.dc_out = y;
        // Brickwall: Soft-Knee via tanh auf Ceiling normiert.
        (y / self.ceiling).tanh() * self.ceiling
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gain_clamps_at_plus_6db() {
        assert!((gain_linear(99.0) - gain_linear(6.0)).abs() < 1e-6);
    }

    #[test]
    fn minus_6db_halves_roughly() {
        assert!((gain_linear(-6.0) - 0.501).abs() < 0.01);
    }

    #[test]
    fn soft_clip_bounds_output() {
        assert!(soft_clip(10.0).abs() <= 1.0);
        assert!(soft_clip(-10.0).abs() <= 1.0);
    }

    #[test]
    fn mix_is_finite() {
        let m = mix(0.5, 0.3, 0.9, -6.0);
        assert!(m.is_finite() && m.abs() <= 1.0);
    }

    #[test]
    fn master_removes_dc_and_limits_peak() {
        let mut m = Master::new(44100.0, -1.0);
        // DC-Eingang -> Ausgang klingt auf ~0 ab.
        for _ in 0..20000 {
            m.process(0.5);
        }
        assert!(m.process(0.5).abs() < 0.01, "dc must be blocked");
        // Uebersteuerung -> Peak bleibt unter Ceiling, endlich.
        let mut peak = 0.0f32;
        for _ in 0..1000 {
            let s = m.process(5.0);
            assert!(s.is_finite());
            peak = peak.max(s.abs());
        }
        assert!(peak <= 0.892, "peak={peak}");
    }

    #[test]
    fn master_passes_sine_roughly() {
        let mut m = Master::new(44100.0, -1.0);
        for _ in 0..5000 {
            m.process(0.0);
        }
        let mut peak = 0.0f32;
        for i in 0..4410 {
            let s = m.process((i as f32 * 0.01).sin() * 0.4);
            peak = peak.max(s.abs());
        }
        assert!((peak - 0.4).abs() < 0.03, "sine should pass, peak={peak}");
    }
}
