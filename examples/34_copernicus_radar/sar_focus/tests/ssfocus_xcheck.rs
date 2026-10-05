//! SSFocus-Filter-Gegenprobe (Pflicht): Die dokumentierten Rezepte
//! (`plan.md` §Filter) als unabhängige Anker durch die öffentliche API.
//!
//! Keine Kreisschlüsse: Alle Orakel sind von Hand abgeleitet (Struktur +
//! Zahlen), nicht aus der Implementation kopiert.

use sar_focus::chirp::{ChirpParams, num_tx_samples, replica};
use sar_focus::meta::{slant_range_vec, suppressed_data_time_s};
use sar_focus::rda::{az_freqs, azimuth_filter, d_factor, range_freqs_unshifted, rcmc_filter};
use sar_focus::types::{SPEED_OF_LIGHT, TX_WAVELENGTH_M};

/// Runde synthetische Chirp-Parameter (ungerade ntx → exakte Mitte).
fn synth_chirp() -> ChirpParams {
    ChirpParams {
        txpsf_hz: 20e6,
        txpl_s: 50.02e-6,
        txprr_hz_s: -0.8e12,
        fs_hz: 50e6,
    }
}

#[test]
fn replika_mitte_und_steigung() {
    // ntx = int(50.02e-6 · 50e6) = 2501 (ungerade), Mitte i = 1250, t = 0 →
    // exp(0) = 1 exakt (SSFocus-Formel exp(2jπ(φ₁t + φ₂t²))).
    let p = synth_chirp();
    let ntx = num_tx_samples(&p);
    assert_eq!(ntx, 2501);
    let r = replica(&p);
    assert!((r[1250].re - 1.0).abs() < 1e-6);
    assert!(r[1250].im.abs() < 1e-6);
    // Handorakel Phasensteigung an der Mitte:
    // φ₁ = 20e6 − 0.8e12·50.02e-6/2 = −8000 Hz,
    // Δφ = 2π(φ₁/fs + φ₂/fs²) = 2π(−1.6e-4 − 1.6e-4) = −0.0020106193.
    let num = r[1251].re * r[1250].re + r[1251].im * r[1250].im;
    let den = r[1251].im * r[1250].re - r[1251].re * r[1250].im;
    let slope = den.atan2(num);
    assert!((slope + 0.002_010_619_3).abs() < 1e-6, "slope = {slope}");
}

#[test]
fn slant_range_nach_rezept() {
    // SSFocus: R[j] = c·(Rang·PRI + SWST + suppressed + j/fs)/2,
    // suppressed = 320/(8·F_REF) mit F_REF = 37.53472224 MHz.
    let supp = suppressed_data_time_s();
    assert!((supp - 320.0 / (8.0 * 37_534_722.24)).abs() < 1e-15);
    let r = slant_range_vec(10, 601.15e-6, 83.337e-6, 46.9184e6, 4);
    let t0 = 10.0 * 601.15e-6 + 83.337e-6 + supp;
    assert!(((r[0] - SPEED_OF_LIGHT * t0 / 2.0) / r[0]).abs() < 1e-12);
    // Abtastschritt exakt c/2fs.
    assert!(((r[1] - r[0] - SPEED_OF_LIGHT / (2.0 * 46.9184e6)) / 3.2).abs() < 1e-9);
}

#[test]
fn filter_anker_d1_und_phase() {
    // D(0) = 1 exakt; RCMC-Zeile bei fa = f_DC ist überall 1;
    // Azimut-Filter dort exp(4jπR/λ) (Handphase, R = 950 km).
    assert_eq!(d_factor(0.0, 7100.0), 1.0);
    let (naz, nr) = (8, 16);
    let fa = az_freqs(naz, 1.0 / 1663.5);
    assert_eq!(fa.len(), naz);
    // f_DC auf eine Rasterlinie legen → jene Zeile exakt D = 1.
    let fdc = vec![fa[5]; nr];
    let fr = range_freqs_unshifted(nr, 46.9184e6);
    let veff = vec![7100.0; nr];
    let rcmc = rcmc_filter(naz, nr, &fa, &fr, 950_000.0, &veff, &fdc);
    for r in 0..nr {
        let c = rcmc[5 * nr + r];
        assert!(c.re > 0.999_999 && c.im.abs() < 1e-6, "c = {c}");
    }
    let slant: Vec<f64> = (0..nr).map(|r| 950_000.0 + r as f64).collect();
    let az = azimuth_filter(naz, nr, &fa, &slant, &veff, &fdc);
    let ph = 4.0 * std::f64::consts::PI * 950_003.0 / TX_WAVELENGTH_M;
    let c = az[5 * nr + 3];
    assert!((c.re as f64 - ph.cos()).abs() < 1e-5);
    assert!((c.im as f64 - ph.sin()).abs() < 1e-5);
    // Außerhalb der Mitte: D < 1, RCMC ≠ 1 (Migration existiert).
    assert!(d_factor(fa[0], 7100.0) < 1.0);
    let c0 = rcmc[1]; // fa[0]-Zeile, fr ≠ 0 → Phase ≈ 0,96 rad
    assert!(c0.im.abs() > 0.5, "c0 = {c0}");
}
