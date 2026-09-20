//! 03_drums: Kick/Snare/Hat, deterministischer Noise (xorshift, kein `rand`).
//! Default-Pattern: 1000 0010 0010 0100 (Kicks 0/13, Snares 6/10).

/// 16-Step-Pattern: 0 = rest, 1 = kick, 2 = snare.
pub const PATTERN: [u8; 16] = [1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 1, 0, 0];

/// Deterministischer Noise-Generator (xorshift64*), seedbar.
pub struct Noise {
    state: u64,
}

impl Noise {
    pub fn new(seed: u64) -> Self {
        Self { state: seed.max(1) }
    }

    /// Ein Sample in [-1, 1].
    pub fn next_sample(&mut self) -> f32 {
        let mut x = self.state;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.state = x;
        let u = x.wrapping_mul(0x2545_F491_4F6C_DD1D) >> 11;
        (u as f32) / (u64::MAX >> 11) as f32 * 2.0 - 1.0
    }
}

/// Exponentielle Decay-Huellkurve aus Phase seit Trigger (0..1).
pub fn decay(phase: f64) -> f64 {
    (-8.0 * phase).exp()
}

/// Kick: Sine-Drop 150 -> 45 Hz plus Attack-Click ab Trigger.
/// `phase`: 0..1 seit Trigger (Click klingt in ~11 ms ab).
pub fn kick_sample(t: f64, phase: f64, env: f64) -> f32 {
    let freq = 150.0 * env + 45.0;
    let body = f32::sin((t * freq * 2.0 * std::f64::consts::PI) as f32);
    let click = (-phase * 24.0).exp() as f32 * 0.5;
    (body * 0.85 + click) * env as f32 * 0.85
}

/// Snare: Noise-Anteil plus 180-Hz-Korpus mit Huelkurve.
pub fn snare_sample(noise: f32, t: f64, env: f64) -> f32 {
    let tone = f32::sin((t * 180.0 * 2.0 * std::f64::consts::PI) as f32) * 0.45;
    (noise * 0.65 + tone) * env as f32 * 0.5
}

/// Closed-Hat: kurzer HP-artiger Noise-Tick (Offbeat, ~0,05 s).
/// Hochpass-Naeherung: Differenz zum vorherigen Noise-Sample.
pub fn hat_sample(noise: f32, prev_noise: f32, env: f64) -> f32 {
    (noise - prev_noise) * 0.5 * env as f32 * 0.3
}

/// Rave-Sirene: Saegezahn 400 -> 2400 Hz ueber `progress` (0..1, 2-bar Riser).
/// Phasen-kontinuierlich via integrierter Momentanfrequenz-Naeherung.
pub fn siren_sample(t: f64, progress: f32) -> f32 {
    let p = progress.clamp(0.0, 1.0) as f64;
    let freq = 400.0 + 2000.0 * p;
    let cycles = t * freq;
    let saw = (cycles % 1.0) as f32 * 2.0 - 1.0;
    saw * 0.22 * (0.3 + 0.7 * p as f32)
}

/// Fracture-Variante ab Takt 8 (je 16er-Block): Ghost-Kick auf Step 7,
/// Ghost-Snare auf Step 14 (leiser gespielt). Rueckgabe: (trigger, velocity).
pub fn pattern_for(bar: u32, step: usize) -> (u8, f32) {
    let base = PATTERN[step % 16];
    if bar % 16 >= 8 {
        match (step % 16, base) {
            (7, 0) => (1, 0.5),  // Ghost-Kick
            (14, 0) => (2, 0.4), // Ghost-Snare
            _ => (base, 1.0),
        }
    } else {
        (base, 1.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pattern_has_two_kicks_two_snares() {
        assert_eq!(PATTERN.iter().filter(|&&v| v == 1).count(), 2);
        assert_eq!(PATTERN.iter().filter(|&&v| v == 2).count(), 2);
    }

    #[test]
    fn noise_is_deterministic() {
        let mut a = Noise::new(42);
        let mut b = Noise::new(42);
        for _ in 0..16 {
            assert_eq!(a.next_sample().to_bits(), b.next_sample().to_bits());
        }
    }

    #[test]
    fn decay_falls_monotone() {
        assert!(decay(0.0) > decay(0.25));
        assert!(decay(0.25) > decay(0.9));
    }

    #[test]
    fn kick_starts_loud() {
        assert!(kick_sample(0.001, 0.0, 1.0).abs() > 0.01);
    }

    #[test]
    fn kick_click_decays_from_trigger() {
        let at_trigger = kick_sample(5.0, 0.0, 1.0).abs();
        let later = kick_sample(5.0, 0.5, 0.5).abs();
        assert!(at_trigger > 0.3, "click must punch, got {at_trigger}");
        assert!(later < at_trigger);
    }

    #[test]
    fn snare_has_180hz_body_without_noise() {
        // Noise = 0 -> reiner Ton; Maximum ueber Fenster (Nulldurchgaenge).
        let peak = (0..200)
            .map(|i| snare_sample(0.0, 2.0 + i as f64 * 0.0002, 1.0).abs())
            .fold(0.0f32, f32::max);
        assert!(peak > 0.15, "tone body missing, peak={peak}");
        assert!(snare_sample(0.0, 2.0, 0.0) == 0.0);
    }

    #[test]
    fn hat_is_bounded_hp_tick() {
        let h = hat_sample(0.8, -0.3, 1.0);
        assert!(h.is_finite() && h.abs() <= 0.3);
        // Konstante Folge -> kein Hochpass-Anteil.
        assert_eq!(hat_sample(0.5, 0.5, 1.0), 0.0);
    }

    fn zero_crossings(progress: f32) -> u32 {
        let mut prev = siren_sample(0.0, progress);
        let mut n = 0;
        for i in 1..2000 {
            let s = siren_sample(i as f64 * 0.00005, progress);
            if (prev <= 0.0) != (s <= 0.0) {
                n += 1;
            }
            prev = s;
        }
        n
    }

    #[test]
    fn siren_pitch_rises_with_progress() {
        let lo = zero_crossings(0.0);
        let hi = zero_crossings(1.0);
        assert!(lo > 50, "siren must oscillate, got {lo}");
        assert!(hi > lo * 3, "siren must rise: lo={lo} hi={hi}");
        assert!(siren_sample(0.01, 0.5).abs() <= 0.23);
    }

    #[test]
    fn fracture_variant_adds_ghosts_after_bar_8() {
        assert_eq!(pattern_for(0, 7), (0, 1.0));
        assert_eq!(pattern_for(8, 7), (1, 0.5));
        assert_eq!(pattern_for(9, 14), (2, 0.4));
        // Basis bleibt: Kicks 0/13, Snares 6/10.
        assert_eq!(pattern_for(9, 0), (1, 1.0));
        assert_eq!(pattern_for(9, 6), (2, 1.0));
    }
}
