use source0::{bar_secs, render_bars};

fn rms(frames: &[(f32, f32)]) -> f32 {
    let sum: f64 = frames.iter().map(|(l, _)| (*l as f64) * (*l as f64)).sum();
    (sum / frames.len().max(1) as f64).sqrt() as f32
}

#[test]
fn full_form_renders_finite_bounded_audio() {
    let frames = render_bars(174.0, 52, 8000.0);
    assert!(!frames.is_empty());
    let mut peak = 0.0f32;
    let mut sum = 0.0f64;
    for (l, r) in &frames {
        assert!(l.is_finite() && r.is_finite());
        peak = peak.max(l.abs());
        sum += *l as f64;
    }
    assert!(peak > 0.01, "mix must be audible, peak={peak}");
    // Master-Limiter: Peak bleibt unter -1 dBFS-Ceiling (0.891).
    assert!(peak <= 0.892, "limiter ceiling violated, peak={peak}");
    let mean = sum / frames.len() as f64;
    assert!(mean.abs() < 0.05, "no DC offset, mean={mean}");
}

#[test]
fn bridge_is_quieter_than_rolling_and_tearout() {
    let frames = render_bars(174.0, 52, 8000.0);
    let bar_frames = (bar_secs(174.0) * 8000.0).round() as usize;
    let section = |from: usize, to: usize| &frames[from * bar_frames..to * bar_frames];
    let rolling = rms(section(4, 20));
    let bridge = rms(section(20, 28));
    let tearout = rms(section(32, 48));
    assert!(bridge < rolling, "bridge={bridge} rolling={rolling}");
    assert!(tearout >= rolling, "tearout={tearout} rolling={rolling}");
}
