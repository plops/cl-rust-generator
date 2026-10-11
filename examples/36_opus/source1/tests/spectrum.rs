//! Spektrum-Host-Tests: Decode + STFT + Bitstrom-Modell auf Fixtures.

mod common;

use opus_pure::Application;
use opus_vis::alloc::EBANDS;
use opus_vis::decode::decode_mono;
use opus_vis::spectrum::{BINS, FMAX, FMIN, stft};

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

/// Sweep-Fixture (40 × 20 ms CELT, mono): Bitstrom + STFT + Pre-Skip.
fn sweep_40() -> (Vec<u8>, Vec<f32>, usize) {
    const FRAMES: usize = 40;
    let data = common::encode_mono(Application::Audio, 96_000, None, None, FRAMES, |fr, off| {
        common::sweep_block(FRAMES, fr, off)
    });
    let pcm = decode_mono(&data).unwrap().samples;
    let pre_skip = opus_pure::OggOpusReader::new(&data[..])
        .unwrap()
        .head()
        .pre_skip as usize;
    (data, pcm, pre_skip)
}

/// Soll-Frequenz des Sweeps bei Stream-Sample `s` (200 → 4000 Hz).
fn sweep_freq(s: usize) -> f32 {
    200.0 * 20.0f32.powf(s as f32 / (40.0 * 960.0))
}

/// CELT-Band (0–20) einer Frequenz in Hz.
fn band_of_freq(f: f32) -> usize {
    let mut b = 0;
    while b + 1 < 21 && (EBANDS[b + 1] * 200) as f32 <= f {
        b += 1;
    }
    b
}

/// Log-Bin-Mitte in Hz.
fn bin_freq(j: usize) -> f32 {
    FMIN * (FMAX / FMIN).powf((j as f32 + 0.5) / BINS as f32)
}

#[test]
fn bitstream_peaks_track_sweep() {
    let (data, _pcm, pre_skip) = sweep_40();
    let bs = opus_vis::bitstream::analyze_bytes(&data).unwrap();
    assert!(bs.available.iter().all(|&a| a), "Luecken im Sweep");
    let mut max_dev = 0usize;
    for (h, col) in bs.spectrum.iter().enumerate() {
        let center = pre_skip + h * 960 + 480;
        let expect = band_of_freq(sweep_freq(center));
        let peak = peak_bin(col);
        let got = band_of_freq(bin_freq(peak));
        max_dev = max_dev.max(got.abs_diff(expect));
    }
    eprintln!("Bitstrom-Peak: max-Abw {max_dev} Baender");
    assert!(max_dev <= 1, "Peak wandert nicht mit: {max_dev}");
}

#[test]
fn density_sums_to_allocated_bits() {
    use opus_vis::celt::{CeltState, endband_of, lm_of_ms, parse_celt_frame};
    use opus_vis::types::Mode;

    let (data, _pcm, _pre) = sweep_40();
    let bs = opus_vis::bitstream::analyze_bytes(&data).unwrap();
    // Unabhaengiger Pfad: Pakete direkt parsen, Bits/Band summieren.
    let mut reader = opus_pure::OggOpusReader::new(&data[..]).unwrap();
    let mut packets = Vec::new();
    while let Some(p) = reader.read_packet().unwrap() {
        packets.push(p.data);
    }
    let mut state = CeltState::new();
    let mut last_mode: Option<Mode> = None;
    let mut totals = Vec::new();
    for p in &packets {
        let toc = opus_vis::toc::parse(p).unwrap();
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode != Mode::Celt {
            totals.push(0.0);
            continue;
        }
        let split = opus_vis::split::split(p).unwrap();
        let (lm, end, ch) = (
            lm_of_ms(toc.frame_ms),
            endband_of(toc.bandwidth),
            toc.channels as usize,
        );
        let mut sum = 0.0f32;
        for f in &split.frames {
            let cf = parse_celt_frame(f, lm, ch, 0, end, &mut state).unwrap();
            for b in 0..end {
                sum += cf.coarse_bits[b] as f32 / 8.0 + ch as f32 * cf.ebits[b] as f32;
            }
        }
        totals.push(sum);
    }
    // Hops ↔ Pakete ueber Hop-Mitte (wie im Modul).
    let pre_skip = opus_pure::OggOpusReader::new(&data[..])
        .unwrap()
        .head()
        .pre_skip as usize;
    let mut bounds = vec![0usize];
    for p in &packets {
        let toc = opus_vis::toc::parse(p).unwrap();
        bounds.push(bounds.last().unwrap() + (toc.duration_ms * 48.0) as usize);
    }
    for (h, den) in bs.density.iter().enumerate() {
        let center = pre_skip + h * 960 + 480;
        let pi = bounds.partition_point(|&s| s <= center) - 1;
        let got: f32 = den.iter().sum();
        let want = totals[pi];
        let rel = ((got - want) / want).abs();
        assert!(rel < 1e-4, "hop {h}: dichte {got} vs bits {want}");
    }
}

#[test]
fn divergence_is_near_zero_on_fixture() {
    let (data, pcm, _pre) = sweep_40();
    let bs = opus_vis::bitstream::analyze_bytes(&data).unwrap();
    let cols = stft(&pcm);
    let mut vals = Vec::new();
    let zipped = || cols.iter().zip(bs.spectrum.iter()).zip(bs.available.iter());
    for ((scol, bcol), avail) in zipped() {
        if !avail {
            continue;
        }
        let d = opus_vis::bitstream::divergence(scol, bcol, true);
        vals.extend(d.iter().filter(|v| v.is_finite()).copied());
    }
    vals.sort_by(|a, b| a.total_cmp(b));
    let q = |p: f32| vals[(p * vals.len() as f32) as usize];
    eprintln!(
        "Divergenz p10/p50/p90: {:.1}/{:.1}/{:.1} dB",
        q(0.1),
        q(0.5),
        q(0.9)
    );
    assert!(q(0.5).abs() < 6.0, "globaler Median {} dB", q(0.5));
    // An den spektralen Spitzen muss die Divergenz verschwinden.
    let mut sig = Vec::new();
    for ((scol, bcol), avail) in zipped() {
        if !avail {
            continue;
        }
        let d = opus_vis::bitstream::divergence(scol, bcol, true);
        sig.push(d[peak_bin(scol)]);
    }
    sig.sort_by(|a, b| a.total_cmp(b));
    let med = sig[sig.len() / 2];
    eprintln!("Divergenz Spitzen-Median: {med:.1} dB ({} Hops)", sig.len());
    // Rest-Offset ist methodisch: Sine-Fenster (CELT) vs. Hann (STFT) = 2,1 dB
    // plus Sweep-Verschmierung/Scalloping ~1 dB. Ohne Kalibrierung waeren es -68.
    assert!(med.abs() < 4.0, "spitzen-median {med} dB");
}

#[test]
fn bitstream_peak_matches_stft_level() {
    // Absolute Skalen-Kalibrierung: Sinus 440 Hz / Amplitude 0.5 → Peak ≈ -6 dB.
    let data = common::encode_mono(Application::Audio, 64_000, None, None, 25, |fr, off| {
        common::sine_block(440.0, fr, off)
    });
    let bs = opus_vis::bitstream::analyze_bytes(&data).unwrap();
    let pcm = decode_mono(&data).unwrap();
    let cols = stft(&pcm.samples);
    let h = 12.min(bs.spectrum.len() - 1).min(cols.len() - 1);
    let bs_peak = bs.spectrum[h]
        .iter()
        .cloned()
        .fold(f32::NEG_INFINITY, f32::max);
    let stft_peak = cols[h].iter().cloned().fold(f32::NEG_INFINITY, f32::max);
    eprintln!("CAL: bitstrom {bs_peak:.1} dB, stft {stft_peak:.1} dB");
    assert!((bs_peak - stft_peak).abs() < 2.0, "Skala verstellt");
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
