//! Master bus: Freeverb-style reverb (send effect) and a peak limiter.
//!
//! Buffers are allocated in the constructors (UI thread); processing never allocates.

/// Freeverb tunings at 44.1 kHz (Jezar at Dreampoint, public domain).
const COMBS: [usize; 8] = [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617];
const ALLPASSES: [usize; 4] = [556, 441, 341, 225];
const STEREO_SPREAD: usize = 23;
const INPUT_GAIN: f32 = 0.015;

struct Comb {
    buf: Vec<f32>,
    idx: usize,
    store: f32,
}

impl Comb {
    fn new(len: usize) -> Comb {
        Comb {
            buf: vec![0.0; len.max(1)],
            idx: 0,
            store: 0.0,
        }
    }
    fn process(&mut self, x: f32, feedback: f32, damp: f32) -> f32 {
        let y = self.buf[self.idx];
        self.store = y * (1.0 - damp) + self.store * damp;
        self.buf[self.idx] = x + self.store * feedback;
        self.idx = (self.idx + 1) % self.buf.len();
        y
    }
}

struct Allpass {
    buf: Vec<f32>,
    idx: usize,
}

impl Allpass {
    fn new(len: usize) -> Allpass {
        Allpass {
            buf: vec![0.0; len.max(1)],
            idx: 0,
        }
    }
    fn process(&mut self, x: f32) -> f32 {
        let b = self.buf[self.idx];
        self.buf[self.idx] = x + b * 0.5;
        self.idx = (self.idx + 1) % self.buf.len();
        b - x
    }
}

/// Stereo reverb fed by a mono send. `room` 0..1 sets the tail length, `damp`
/// 0..1 darkens the tail (high frequencies decay faster, like a real room).
pub struct Reverb {
    left: (Vec<Comb>, Vec<Allpass>),
    right: (Vec<Comb>, Vec<Allpass>),
    pub room: f32,
    pub damp: f32,
}

impl Reverb {
    pub fn new(sample_rate: f32) -> Reverb {
        let scale = |n: usize| (n as f32 * sample_rate / 44100.0) as usize;
        let side = |spread: usize| {
            (
                COMBS
                    .iter()
                    .map(|&n| Comb::new(scale(n + spread)))
                    .collect(),
                ALLPASSES
                    .iter()
                    .map(|&n| Allpass::new(scale(n + spread)))
                    .collect(),
            )
        };
        Reverb {
            left: side(0),
            right: side(STEREO_SPREAD),
            room: 0.78,
            damp: 0.4,
        }
    }

    pub fn process(&mut self, send: f32) -> (f32, f32) {
        let feedback = 0.7 + self.room * 0.28;
        let x = send * INPUT_GAIN;
        let run = |(combs, aps): &mut (Vec<Comb>, Vec<Allpass>)| {
            let mut y: f32 = combs
                .iter_mut()
                .map(|c| c.process(x, feedback, self.damp))
                .sum();
            for ap in aps.iter_mut() {
                y = ap.process(y);
            }
            y
        };
        (run(&mut self.left), run(&mut self.right))
    }
}

/// Peak limiter with instant attack and smooth release: the output never
/// exceeds `threshold`, and unlike `tanh` clipping, quiet parts stay untouched.
pub struct Limiter {
    gain: f32,
    release: f32,
    pub threshold: f32,
}

impl Limiter {
    pub fn new(sample_rate: f32) -> Limiter {
        Limiter {
            gain: 1.0,
            release: crate::dsp::decay_coef(0.25, sample_rate),
            threshold: 0.9,
        }
    }
    pub fn process(&mut self, l: f32, r: f32) -> (f32, f32) {
        let peak = l.abs().max(r.abs());
        let target = if peak > self.threshold {
            self.threshold / peak
        } else {
            1.0
        };
        self.gain = if target < self.gain {
            target
        } else {
            target + (self.gain - target) * self.release
        };
        (l * self.gain, r * self.gain)
    }
    /// Current gain reduction in dB (for the meter).
    pub fn reduction_db(&self) -> f32 {
        -20.0 * self.gain.log10()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const SR: f32 = 48000.0;

    #[test]
    fn reverb_impulse_has_decaying_stereo_tail() {
        let mut r = Reverb::new(SR);
        let mut out = Vec::new();
        out.push(r.process(1.0));
        for _ in 0..(SR as usize * 4) {
            out.push(r.process(0.0));
        }
        let energy = |a: usize, b: usize| out[a..b].iter().map(|(l, r)| l * l + r * r).sum::<f32>();
        let sec = SR as usize;
        let early = energy(0, sec / 2);
        let late = energy(3 * sec, 4 * sec);
        assert!(early > 1e-5, "{early}");
        assert!(late < early * 1e-3, "tail decays: {early} → {late}");
        assert!(out.iter().all(|(l, r)| l.is_finite() && r.is_finite()));
        let diff: f32 = out.iter().map(|(l, r)| (l - r).abs()).sum();
        assert!(diff > 1e-3, "left and right differ");
    }

    #[test]
    fn limiter_caps_peaks_and_passes_quiet_signal() {
        let mut lim = Limiter::new(SR);
        assert_eq!(lim.process(0.5, -0.5), (0.5, -0.5));
        let (l, r) = lim.process(3.0, 1.0);
        assert!((l - 0.9).abs() < 1e-6 && (r - 0.3).abs() < 1e-6);
        assert!(lim.reduction_db() > 10.0);
        for _ in 0..(SR as usize) {
            lim.process(0.1, 0.1);
        }
        assert!(lim.reduction_db() < 0.1, "recovers");
    }
}
