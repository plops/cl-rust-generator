//! One monophonic voice. A tracker channel owns exactly one voice.
//!
//! Drum kinds treat `C-4` as their "natural" pitch; other notes transpose
//! relative to it. Melodic kinds play the note frequency directly.

use crate::dsp::{Biquad, Env, FilterMode, Osc, Rng, decay_coef, saturate};
use crate::types::{Kind, Note, Patch, midi_freq};

/// Semitone index of C-4 (see [`midi_freq`]).
const C4: f32 = 48.0;
/// 808 cowbell partials are 540 Hz and 800 Hz.
const BELL_RATIO: f32 = 800.0 / 540.0;

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
    osc: [Osc; 3],
    filt: Biquad,
    cutoff: f32,
    reso: f32,
    drive: f32,
    volume: f32,
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
            osc: [Osc::default(); 3],
            filt: Biquad::default(),
            cutoff: 18000.0,
            reso: 0.707,
            drive: 0.0,
            volume: 0.0,
            detune: 1.0,
            rng: Rng::new(0x1234_5678),
            pos: 0.0,
        }
    }
}

fn filter_mode(kind: Kind) -> FilterMode {
    match kind {
        Kind::Hat => FilterMode::HighPass,
        Kind::Snare | Kind::Bell => FilterMode::BandPass,
        _ => FilterMode::LowPass,
    }
}

impl Voice {
    pub fn is_active(&self) -> bool {
        !self.env.is_idle()
    }
    pub fn instr(&self) -> usize {
        self.instr
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
        self.detune = (p.detune / 1200.0).exp2();
        self.pos = 0.0;
        if matches!(self.kind, Kind::Kick | Kind::Sub | Kind::Snare) {
            self.osc[0].phase = 0.0;
        }
        self.filt.reset();
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
        let raw = match self.kind {
            Kind::Kick => {
                let f = 50.0 * midi_freq(semi) / midi_freq(C4);
                self.osc[0].sine(f / sr)
            }
            Kind::Snare => {
                let f = 185.0 * midi_freq(semi) / midi_freq(C4);
                let body = self.osc[0].sine(f / sr) * amp;
                let noise = self.filt.process(self.rng.noise()) * 1.4;
                body * 0.45 + noise
            }
            Kind::Hat => self.filt.process(self.rng.noise()) * 1.5,
            Kind::Reese => {
                let dt = midi_freq(semi) / sr;
                let s = self.osc[0].saw(dt * self.detune)
                    + self.osc[1].saw(dt / self.detune)
                    + 0.6 * self.osc[2].sine(dt * 0.5);
                self.filt.process(s * 0.45)
            }
            Kind::Sub => self.osc[0].sine(midi_freq(semi) / sr),
            Kind::Bell => {
                let dt = midi_freq(semi) / sr;
                let s = self.osc[0].square(dt) + self.osc[1].square(dt * BELL_RATIO);
                self.filt.process(s) * 1.4
            }
            Kind::Lead => {
                let dt = midi_freq(semi) / sr;
                let s = self.osc[0].saw(dt) + self.osc[1].saw(dt * self.detune);
                self.filt.process(s * 0.5)
            }
            Kind::Sample => self.sample(patches, semi, sr),
        };
        saturate(raw, self.drive) * amp * self.vel * self.volume
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
