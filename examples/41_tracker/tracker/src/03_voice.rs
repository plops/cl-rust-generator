//! One monophonic voice. A tracker channel owns exactly one voice.
//!
//! Signal path per sample:
//! `oscillators → filter → saturate(drive) → tone low-pass (+ clean sub) → envelope`.
//! The tone filter after the saturation removes the fizzy, "8-bit" harmonics
//! that distortion creates; the clean sub path keeps the bass fundamental undistorted.
//!
//! Drum kinds treat `C-4` as their "natural" pitch; other notes transpose
//! relative to it. Melodic kinds play the note frequency directly.

use crate::dsp::{Biquad, Env, FilterMode, Osc, Rng, decay_coef, saturate};
use crate::types::{Kind, Note, Patch, midi_freq};

/// Semitone index of C-4 (see [`midi_freq`]).
const C4: f32 = 48.0;
/// 808 cowbell partials are 540 Hz and 800 Hz.
const BELL_RATIO: f32 = 800.0 / 540.0;
/// 808 hi-hat: six square oscillators at these (inharmonic) frequencies give the metal.
const HAT_FREQS: [f32; 6] = [205.3, 304.4, 369.6, 522.7, 540.0, 800.0];

#[derive(Clone, Debug)]
pub struct Voice {
    instr: usize,
    kind: Kind,
    /// Current pitch in semitones (0 = C-0); `target` is the glide goal.
    semi: f32,
    target: f32,
    glide_coef: f32,
    vel: f32,
    env: Env,
    pitch_env: f32,
    pitch_coef: f32,
    pitch_depth: f32,
    osc: [Osc; 6],
    filt: Biquad,
    tone: Biquad,
    cutoff: f32,
    reso: f32,
    drive: f32,
    volume: f32,
    send: f32,
    detune: f32,
    rng: Rng,
    pos: f64,
}

impl Default for Voice {
    fn default() -> Self {
        Voice {
            instr: 0,
            kind: Kind::Lead,
            semi: C4,
            target: C4,
            glide_coef: 1.0,
            vel: 1.0,
            env: Env::default(),
            pitch_env: 0.0,
            pitch_coef: 0.0,
            pitch_depth: 0.0,
            osc: [Osc::default(); 6],
            filt: Biquad::default(),
            tone: Biquad::default(),
            cutoff: 18000.0,
            reso: 0.707,
            drive: 0.0,
            volume: 0.0,
            send: 0.0,
            detune: 1.0,
            rng: Rng::new(0x1234_5678),
            pos: 0.0,
        }
    }
}

fn filter_mode(kind: Kind) -> FilterMode {
    match kind {
        Kind::Hat | Kind::Snare => FilterMode::HighPass,
        _ => FilterMode::LowPass,
    }
}

/// Low-pass after the saturation stage, per kind (Hz).
fn tone_hz(kind: Kind) -> f32 {
    match kind {
        Kind::Kick => 3500.0,
        Kind::Snare => 9000.0,
        Kind::Hat => 14000.0,
        Kind::Reese => 6000.0,
        Kind::Sub => 2200.0,
        Kind::Bell => 7000.0,
        Kind::Lead => 9000.0,
        Kind::Sample => 20000.0,
    }
}

impl Voice {
    pub fn is_active(&self) -> bool {
        !self.env.is_idle()
    }
    pub fn instr(&self) -> usize {
        self.instr
    }
    /// Reverb send of the current patch.
    pub fn send(&self) -> f32 {
        self.send
    }

    pub fn trigger(&mut self, instr: usize, p: &Patch, note: Note, vel: f32, sr: f32) {
        self.instr = instr;
        self.kind = p.kind;
        self.semi = note.0 as f32 - 1.0;
        self.target = self.semi;
        self.glide_coef = 1.0;
        self.vel = vel.clamp(0.0, 1.0);
        self.env
            .trigger(p.attack, p.decay, p.sustain, p.release, sr);
        self.pitch_env = 1.0;
        self.pitch_coef = decay_coef(p.pitch_decay, sr);
        self.pitch_depth = p.pitch_env;
        self.cutoff = p.cutoff;
        self.reso = p.reso;
        self.drive = p.drive;
        self.volume = p.volume;
        self.send = p.reverb.clamp(0.0, 1.0);
        self.detune = (p.detune / 1200.0).exp2();
        self.pos = 0.0;
        if matches!(self.kind, Kind::Kick | Kind::Sub | Kind::Snare) {
            self.osc[0].phase = 0.0;
        }
        self.filt.reset();
        self.tone.reset();
        self.tone
            .set(FilterMode::LowPass, tone_hz(self.kind), 0.707, sr);
        self.update_filter(sr);
    }

    /// Slide to `note` over roughly `secs` without retriggering the envelope.
    pub fn glide_to(&mut self, note: Note, secs: f32, sr: f32) {
        self.target = note.0 as f32 - 1.0;
        self.glide_coef = 1.0 - decay_coef(secs.max(0.001), sr);
    }

    pub fn release(&mut self) {
        self.env.release();
    }
    /// Hard stop (used by note cut); fast release avoids a click.
    pub fn cut(&mut self, sr: f32) {
        self.env.trigger(0.0, 0.0, 0.0, 0.004, sr);
        self.env.release();
    }

    pub fn set_cutoff(&mut self, hz: f32, sr: f32) {
        self.cutoff = hz;
        self.update_filter(sr);
    }
    pub fn set_drive(&mut self, d: f32) {
        self.drive = d;
    }

    fn update_filter(&mut self, sr: f32) {
        let mut hz = self.cutoff;
        if self.kind == Kind::Bell {
            // Brightness follows the pitch, so high and low bells keep their colour.
            hz *= midi_freq(self.semi) / midi_freq(C4 + 12.0);
        }
        self.filt.set(filter_mode(self.kind), hz, self.reso, sr);
    }

    pub fn next(&mut self, patches: &[Patch], sr: f32) -> f32 {
        if self.env.is_idle() {
            return 0.0;
        }
        if self.semi != self.target {
            self.semi += (self.target - self.semi) * self.glide_coef;
            if (self.target - self.semi).abs() < 1e-3 {
                self.semi = self.target;
            }
        }
        let amp = self.env.step();
        self.pitch_env *= self.pitch_coef;
        let semi = self.semi + self.pitch_depth * self.pitch_env;
        let ratio = midi_freq(semi) / midi_freq(C4);
        // (signal through drive + tone filter, clean signal added afterwards)
        let (raw, clean) = match self.kind {
            Kind::Kick => {
                let body = self.osc[0].sine(55.0 * ratio / sr);
                // Short noise click on the attack makes the kick cut through.
                let click = self.rng.noise() * self.pitch_env.powi(6) * 0.3;
                (body + click, 0.0)
            }
            Kind::Snare => {
                let body = self.osc[0].sine(190.0 * ratio / sr) * amp * 0.7;
                let noise = self.filt.process(self.rng.noise()) * 0.8;
                (body + noise, 0.0)
            }
            Kind::Hat => {
                let metal: f32 = HAT_FREQS
                    .iter()
                    .zip(self.osc.iter_mut())
                    .map(|(f, o)| o.square(f * ratio * 1.7 / sr))
                    .sum();
                let s = metal / 6.0 + self.rng.noise() * 0.35;
                (self.filt.process(s) * 1.6, 0.0)
            }
            Kind::Reese => {
                let dt = midi_freq(semi) / sr;
                let saws = self.osc[0].saw(dt * self.detune) + self.osc[1].saw(dt / self.detune);
                let sub = self.osc[2].sine(dt) * 0.6;
                (self.filt.process(saws * 0.4), sub)
            }
            Kind::Sub => (self.osc[0].sine(midi_freq(semi) / sr), 0.0),
            Kind::Bell => {
                let dt = midi_freq(semi) / sr;
                let s = self.osc[0].square(dt) + self.osc[1].square(dt * BELL_RATIO);
                (self.filt.process(s * 0.6), 0.0)
            }
            Kind::Lead => {
                let dt = midi_freq(semi) / sr;
                let s = self.osc[0].saw(dt) + self.osc[1].saw(dt * self.detune);
                (self.filt.process(s * 0.5), 0.0)
            }
            Kind::Sample => (self.sample(patches, semi, sr), 0.0),
        };
        let shaped = self.tone.process(saturate(raw, self.drive));
        ((shaped + clean) * amp * self.vel * self.volume).clamp(-1.0, 1.0)
    }

    fn sample(&mut self, patches: &[Patch], semi: f32, sr: f32) -> f32 {
        let Some(p) = patches.get(self.instr) else {
            return 0.0;
        };
        let data = &p.sample;
        let i = self.pos as usize;
        if i + 1 >= data.len() {
            self.env.release();
            return 0.0;
        }
        let frac = (self.pos - i as f64) as f32;
        let v = data[i] + (data[i + 1] - data[i]) * frac;
        let rate = midi_freq(semi) / midi_freq(C4) * p.sample_rate / sr;
        self.pos += rate as f64;
        self.filt.process(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::parse_note;
    use std::sync::Arc;
    const SR: f32 = 48000.0;

    fn patch(kind: Kind) -> Patch {
        let mut p = Patch::new("t", kind);
        if kind == Kind::Sample {
            p.sample = Arc::from(
                (0..4800)
                    .map(|i| (i as f32 * 0.05).sin())
                    .collect::<Vec<_>>(),
            );
        }
        p
    }

    fn render(kind: Kind, note: &str, n: usize) -> Vec<f32> {
        let p = patch(kind);
        let mut v = Voice::default();
        v.trigger(0, &p, parse_note(note).unwrap(), 1.0, SR);
        (0..n)
            .map(|_| v.next(std::slice::from_ref(&p), SR))
            .collect()
    }

    #[test]
    fn every_kind_sounds_and_decays() {
        for kind in Kind::ALL {
            let p = patch(kind);
            let patches = vec![p.clone()];
            let mut v = Voice::default();
            v.trigger(0, &p, parse_note("C-4").unwrap(), 1.0, SR);
            let mut energy = 0.0;
            for _ in 0..4800 {
                let s = v.next(&patches, SR);
                assert!(s.is_finite(), "{kind:?}");
                assert!(s.abs() <= 1.0, "{kind:?} {s}");
                energy += s * s;
            }
            assert!(energy > 1.0, "{kind:?} too quiet: {energy}");
            v.release();
            for _ in 0..(SR as usize * 3) {
                v.next(&patches, SR);
            }
            assert!(!v.is_active(), "{kind:?} never ends");
        }
    }

    /// Fraction of energy below `hz`, via a one-pole low-pass split.
    fn low_fraction(x: &[f32], hz: f32) -> f32 {
        let a = (-std::f32::consts::TAU * hz / SR).exp();
        let mut lp = 0.0;
        let (mut lo, mut all) = (0.0, 0.0);
        for &s in x {
            lp = s * (1.0 - a) + lp * a;
            lo += lp * lp;
            all += s * s;
        }
        lo / all
    }

    #[test]
    fn reese_keeps_a_clean_fundamental_sub() {
        // D-2 ≈ 73 Hz: most energy must sit at/below ~150 Hz, not in the fizz.
        let x = render(Kind::Reese, "D-2", 24000);
        assert!(low_fraction(&x, 150.0) > 0.5, "{}", low_fraction(&x, 150.0));
    }

    #[test]
    fn hat_is_metallic_not_pure_hiss() {
        // Tone filter at 14 kHz: little energy should remain above 16 kHz.
        let x = render(Kind::Hat, "C-4", 2400);
        let total: f32 = x.iter().map(|s| s * s).sum();
        let mut hp = Biquad::default();
        hp.set(FilterMode::HighPass, 16000.0, 0.707, SR);
        let air: f32 = x.iter().map(|&s| hp.process(s).powi(2)).sum();
        assert!(air / total < 0.15, "too much air: {}", air / total);
    }

    #[test]
    fn glide_reaches_target() {
        let p = patch(Kind::Reese);
        let mut v = Voice::default();
        v.trigger(0, &p, parse_note("C-2").unwrap(), 1.0, SR);
        v.glide_to(parse_note("C-3").unwrap(), 0.05, SR);
        for _ in 0..4800 {
            v.next(std::slice::from_ref(&p), SR);
        }
        assert_eq!(v.semi, v.target);
    }
}
