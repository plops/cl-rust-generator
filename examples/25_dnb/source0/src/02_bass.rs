//! 02_bass: Reese-Bass + 808-Sub als fundsp-Graph (f32-Seite).
//! Kanonischer Pfad nach fundsp `examples/beep.rs`:
//! `set_sample_rate` + `allocate`, pro Frame `get_mono`.

use fundsp::audiounit::AudioUnit;
use fundsp::prelude32::*;

/// Detuned Reese: 50.0 Hz + 50.8 Hz Saw, LP 280 Hz, Tanh-Saturation.
pub fn build_reese() -> Box<dyn AudioUnit> {
    let graph = (constant(50.0) >> saw() & constant(50.8) >> saw())
        >> lowpass_hz(280.0, 1.0)
        >> shape(Tanh(2.5));
    Box::new(graph)
}

/// Ein Reese-Mono-Sample ziehen (Graph muss vorher `set_sample_rate` sehen).
pub fn reese_sample(node: &mut Box<dyn AudioUnit>) -> f32 {
    node.get_mono() * 0.4
}

/// 808-Sub: Sine mit Pitch-Sweep 120 -> 45 Hz, folgt Kick-Triggern.
/// `t`: absolute Zeit (s), `since_trigger`: Zeit seit Trigger (s, Tail ~0,4 s).
pub fn sub_808_sample(t: f64, since_trigger: f64) -> f32 {
    if since_trigger < 0.0 {
        return 0.0;
    }
    let env = (-since_trigger / 0.12).exp();
    let freq = 75.0 * env + 45.0;
    let phase = t * freq * 2.0 * std::f64::consts::PI;
    (f32::sin(phase as f32) * env as f32 * 0.8).tanh() * 0.6
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reese_renders_nonzero_bounded() {
        let mut n = build_reese();
        n.set_sample_rate(44100.0);
        n.allocate();
        let mut rms = 0.0f32;
        for _ in 0..4410 {
            let s = reese_sample(&mut n);
            assert!(s.is_finite());
            assert!(s.abs() < 1.0);
            rms += s * s;
        }
        assert!(rms > 1e-6, "reese must be audible");
    }

    #[test]
    fn sub_808_decays_and_stays_bounded() {
        // Max ueber ein Zeitfenster (Einzel-Sample kann im Nulldurchgang liegen).
        let peak_at = |since: f64| {
            (0..50)
                .map(|i| sub_808_sample(1.0 + i as f64 * 0.0002, since).abs())
                .fold(0.0f32, f32::max)
        };
        assert!(peak_at(0.0) > 0.1, "808 must start loud");
        assert!(peak_at(0.2) < peak_at(0.0), "808 tail must decay");
        assert!(sub_808_sample(1.0, -0.1) == 0.0);
        for i in 0..100 {
            let s = sub_808_sample(i as f64 * 0.001, i as f64 * 0.001);
            assert!(s.is_finite() && s.abs() <= 0.6);
        }
    }
}
