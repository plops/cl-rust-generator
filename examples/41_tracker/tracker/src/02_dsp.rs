//! Small, allocation-free DSP building blocks.

use std::f32::consts::{PI, TAU};

/// xorshift32 white noise in -1..1.
#[derive(Clone, Copy, Debug)]
pub struct Rng(u32);

impl Rng {
    pub fn new(seed: u32) -> Rng {
        Rng(seed.max(1))
    }
    pub fn noise(&mut self) -> f32 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        self.0 = x;
        (x as f32 / u32::MAX as f32) * 2.0 - 1.0
    }
}

/// PolyBLEP residual that removes most aliasing at a waveform step.
fn poly_blep(t: f32, dt: f32) -> f32 {
    if t < dt {
        let t = t / dt;
        t + t - t * t - 1.0
    } else if t > 1.0 - dt {
        let t = (t - 1.0) / dt;
        t * t + t + t + 1.0
    } else {
        0.0
    }
}

/// Phase accumulator with band-limited saw/square and a plain sine.
#[derive(Clone, Copy, Debug, Default)]
pub struct Osc {
    pub phase: f32,
}

impl Osc {
    fn advance(&mut self, dt: f32) {
        self.phase += dt;
        if self.phase >= 1.0 {
            self.phase -= self.phase.floor();
        }
    }
    /// `dt` = frequency / sample rate.
    pub fn saw(&mut self, dt: f32) -> f32 {
        let v = 2.0 * self.phase - 1.0 - poly_blep(self.phase, dt);
        self.advance(dt);
        v
    }
    pub fn square(&mut self, dt: f32) -> f32 {
        let p = self.phase;
        let mut v = if p < 0.5 { 1.0 } else { -1.0 };
        v += poly_blep(p, dt);
        v -= poly_blep((p + 0.5) % 1.0, dt);
        self.advance(dt);
        v
    }
    pub fn sine(&mut self, dt: f32) -> f32 {
        let v = (self.phase * TAU).sin();
        self.advance(dt);
        v
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FilterMode {
    LowPass,
    HighPass,
    BandPass,
}

/// RBJ-cookbook biquad, transposed direct form II.
#[derive(Clone, Copy, Debug)]
pub struct Biquad {
    b0: f32,
    b1: f32,
    b2: f32,
    a1: f32,
    a2: f32,
    z1: f32,
    z2: f32,
}

impl Default for Biquad {
    fn default() -> Self {
        Biquad {
            b0: 1.0,
            b1: 0.0,
            b2: 0.0,
            a1: 0.0,
            a2: 0.0,
            z1: 0.0,
            z2: 0.0,
        }
    }
}

impl Biquad {
    pub fn set(&mut self, mode: FilterMode, freq: f32, q: f32, sr: f32) {
        let w = TAU * freq.clamp(10.0, sr * 0.45) / sr;
        let (s, c) = w.sin_cos();
        let alpha = s / (2.0 * q.max(0.1));
        let a0 = 1.0 + alpha;
        let (b0, b1, b2) = match mode {
            FilterMode::LowPass => ((1.0 - c) / 2.0, 1.0 - c, (1.0 - c) / 2.0),
            FilterMode::HighPass => ((1.0 + c) / 2.0, -(1.0 + c), (1.0 + c) / 2.0),
            FilterMode::BandPass => (alpha, 0.0, -alpha),
        };
        self.b0 = b0 / a0;
        self.b1 = b1 / a0;
        self.b2 = b2 / a0;
        self.a1 = -2.0 * c / a0;
        self.a2 = (1.0 - alpha) / a0;
    }
    pub fn reset(&mut self) {
        self.z1 = 0.0;
        self.z2 = 0.0;
    }
    pub fn process(&mut self, x: f32) -> f32 {
        let y = self.b0 * x + self.z1;
        self.z1 = self.b1 * x - self.a1 * y + self.z2;
        self.z2 = self.b2 * x - self.a2 * y;
        y
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Stage {
    Idle,
    Attack,
    Decay,
    Release,
}

/// ADSR envelope: linear attack, exponential decay and release.
#[derive(Clone, Copy, Debug)]
pub struct Env {
    stage: Stage,
    pub level: f32,
    attack_step: f32,
    decay_coef: f32,
    release_coef: f32,
    sustain: f32,
}

impl Default for Env {
    fn default() -> Self {
        Env {
            stage: Stage::Idle,
            level: 0.0,
            attack_step: 1.0,
            decay_coef: 0.0,
            release_coef: 0.0,
            sustain: 0.0,
        }
    }
}

/// Per-sample multiplier that falls to -60 dB in `secs`.
pub fn decay_coef(secs: f32, sr: f32) -> f32 {
    if secs <= 0.0 {
        0.0
    } else {
        (-6.9078 / (secs * sr)).exp()
    }
}

impl Env {
    pub fn trigger(&mut self, a: f32, d: f32, s: f32, r: f32, sr: f32) {
        self.attack_step = 1.0 / (a * sr).max(1.0);
        self.decay_coef = decay_coef(d, sr);
        self.release_coef = decay_coef(r, sr);
        self.sustain = s.clamp(0.0, 1.0);
        self.stage = Stage::Attack;
    }
    pub fn release(&mut self) {
        if self.stage != Stage::Idle {
            self.stage = Stage::Release;
        }
    }
    pub fn is_idle(&self) -> bool {
        self.stage == Stage::Idle
    }
    pub fn step(&mut self) -> f32 {
        match self.stage {
            Stage::Idle => {}
            Stage::Attack => {
                self.level += self.attack_step;
                if self.level >= 1.0 {
                    self.level = 1.0;
                    self.stage = Stage::Decay;
                }
            }
            Stage::Decay => {
                self.level = self.sustain + (self.level - self.sustain) * self.decay_coef;
                if self.sustain == 0.0 && self.level < 1e-4 {
                    self.stage = Stage::Idle;
                    self.level = 0.0;
                }
            }
            Stage::Release => {
                self.level *= self.release_coef;
                if self.level < 1e-4 {
                    self.stage = Stage::Idle;
                    self.level = 0.0;
                }
            }
        }
        self.level
    }
}

/// Saturation: `drive` 0 is (almost) clean, 1 is heavy. Output stays within ±1.
pub fn saturate(x: f32, drive: f32) -> f32 {
    if drive <= 0.0 {
        return x;
    }
    let g = 1.0 + drive * 6.0;
    (x * g).tanh() / g.tanh()
}

/// Constant-power pan law, `pan` in -1..1 → (left, right) gains.
pub fn pan_gains(pan: f32) -> (f32, f32) {
    let a = (pan.clamp(-1.0, 1.0) + 1.0) * PI / 4.0;
    (a.cos(), a.sin())
}

#[cfg(test)]
mod tests {
    use super::*;
    const SR: f32 = 48000.0;

    fn rms_through(f: &mut Biquad, freq: f32) -> f32 {
        let mut osc = Osc::default();
        let n = 9600;
        let mut acc = 0.0;
        for i in 0..n {
            let y = f.process(osc.sine(freq / SR));
            if i > n / 2 {
                acc += y * y;
            }
        }
        (acc / (n / 2) as f32).sqrt()
    }

    #[test]
    fn lowpass_attenuates_highs() {
        let mut f = Biquad::default();
        f.set(FilterMode::LowPass, 500.0, 0.707, SR);
        let pass = rms_through(&mut f, 100.0);
        f.reset();
        let stop = rms_through(&mut f, 8000.0);
        assert!(pass > 0.65, "pass {pass}");
        let db = 20.0 * (stop / pass).log10();
        assert!(db < -30.0, "only {db} dB");
    }

    #[test]
    fn highpass_attenuates_lows() {
        let mut f = Biquad::default();
        f.set(FilterMode::HighPass, 5000.0, 0.707, SR);
        assert!(rms_through(&mut f, 100.0) < 0.01);
    }

    #[test]
    fn oscillators_stay_bounded() {
        let mut o = Osc::default();
        for _ in 0..48000 {
            let s = o.saw(1234.5 / SR);
            assert!(s.abs() <= 1.1, "{s}");
        }
        for _ in 0..48000 {
            assert!(o.square(321.0 / SR).abs() <= 1.1);
        }
    }

    #[test]
    fn envelope_decays_to_idle() {
        let mut e = Env::default();
        e.trigger(0.001, 0.1, 0.0, 0.05, SR);
        let peak = (0..200).map(|_| e.step()).fold(0.0, f32::max);
        assert!((peak - 1.0).abs() < 1e-6);
        for _ in 0..(SR as usize) {
            e.step();
        }
        assert!(e.is_idle());
    }

    #[test]
    fn saturation_bounded_and_odd() {
        for i in -100..=100 {
            let x = i as f32 / 10.0;
            let y = saturate(x, 0.8);
            assert!(y.abs() <= 1.0 + 1e-3);
            assert!((saturate(-x, 0.8) + y).abs() < 1e-6);
        }
    }

    #[test]
    fn noise_is_zero_mean() {
        let mut r = Rng::new(7);
        let m: f32 = (0..100_000).map(|_| r.noise()).sum::<f32>() / 100_000.0;
        assert!(m.abs() < 0.02, "{m}");
    }
}
