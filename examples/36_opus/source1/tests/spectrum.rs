//! Spektrum-Host-Tests: Decode + STFT auf synthetischen Fixtures.

mod common;

use opus_pure::Application;
use opus_vis::decode::decode_mono;
use opus_vis::spectrum::{BINS, stft};

fn peak_bin(col: &[f32; BINS]) -> usize {
    col.iter()
        .enumerate()
        .max_by(|a, b| a.1.total_cmp(b.1))
        .unwrap()
        .0
}

#[test]
fn sweep_peak_rises_monotonically() {
    let total = 50; // 1 s Sweep 200 → 4000 Hz.
    let data = common::encode_mono(Application::Audio, 96_000, None, None, total, |fr, off| {
        common::sweep_block(total, fr, off)
    });
    let pcm = decode_mono(&data).unwrap();
    assert!((46_000..=48_000).contains(&pcm.samples.len()));
    let cols = stft(&pcm.samples);
    let peaks: Vec<usize> = cols.iter().map(peak_bin).collect();
    // Anfangs- vs. Enddrittel: deutlich gestiegen, nirgends stark gefallen.
    let third = peaks.len() / 3;
    let avg = |s: &[usize]| s.iter().sum::<usize>() as f32 / s.len() as f32;
    assert!(
        avg(&peaks[2 * third..]) - avg(&peaks[..third]) > 30.0,
        "peaks: {peaks:?}"
    );
    for w in peaks.windows(2) {
        assert!(w[1] + 2 >= w[0], "ruecksprung: {peaks:?}");
    }
}

#[test]
fn noise_spectrum_is_flat_in_midband() {
    let data = common::encode_mono(
        Application::Audio,
        96_000,
        None,
        None,
        25,
        common::noise_block,
    );
    let pcm = decode_mono(&data).unwrap();
    let cols = stft(&pcm.samples);
    // Zeitmittel ueber alle Spalten: Einzel-Bins streuen (Rayleigh),
    // Band-Mittelwerte von Weissrauschen sind flach.
    let mut mean = [0.0f32; BINS];
    for col in &cols {
        for (m, v) in mean.iter_mut().zip(col) {
            *m += v / cols.len() as f32;
        }
    }
    let band = &mean[100..200];
    let quarter = |q: usize| band[q * 25..(q + 1) * 25].iter().sum::<f32>() / 25.0;
    let qs = [quarter(0), quarter(1), quarter(2), quarter(3)];
    for q in &qs {
        assert!((qs[0] - q).abs() < 4.0, "quartale {qs:?}");
    }
    // Physik: |X| ≈ sqrt(N)*RMS ≈ 5.4, Ref 256 → ≈ -33 dB.
    assert!((-45.0..-25.0).contains(&qs[0]), "pegel {}", qs[0]);
}

#[test]
fn decode_trim_keeps_sine_level() {
    let data = common::encode_mono(Application::Audio, 64_000, None, None, 25, |fr, off| {
        common::sine_block(440.0, fr, off)
    });
    let pcm = decode_mono(&data).unwrap();
    assert_eq!(pcm.channels, 1);
    // Pegel nach Trim: 0.5-Amplitude ≈ 0.35 RMS (Anfang ohne Pre-Skip).
    let tail = &pcm.samples[5000..];
    let rms = (tail.iter().map(|s| s * s).sum::<f32>() / tail.len() as f32).sqrt();
    assert!((0.25..0.45).contains(&rms), "rms {rms}");
}
