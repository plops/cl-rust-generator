//! Gemeinsame Punktziel-Simulation für RDA- und TDBP-Tests (S6-Skala).
//!
//! Rohdatenmodell: verzögerter Sende-Chirp je Puls plus Trägerphase der
//! hyperbolischen Entfernungsänderung `R(η) = √(R₀² + (vη)²)`.

//! Test-Helfer: nicht jedes Target nutzt alle Items.
#![allow(dead_code)]

use sar_focus::chirp::ChirpParams;
use sar_focus::types::{Complex32, SPEED_OF_LIGHT, TX_FREQ_HZ};

pub const NAZ: usize = 1025; // ungerade: Broadside exakt auf Bin 512
pub const NR: usize = 4097; // S6-Chirp (2397) passt mit Reserve
pub const J0: usize = 2048; // Ziel-Range-Bin
pub const PRI: f64 = 22_564.0 / 37_534_722.24;
pub const FS: f64 = 46_918_402.8;
pub const V: f64 = 7100.0;
pub const R0: f64 = 950_000.0;

pub fn chirp() -> ChirpParams {
    ChirpParams {
        txpsf_hz: 21.09e6,
        txpl_s: 51.099e-6,
        txprr_hz_s: -826.4e9,
        fs_hz: FS,
    }
}

/// Roh-Echo eines Punktziels (σ = 1) bei (512, J0).
/// Gibt `(Daten, Slant-Achse, Zeilenstart-t0)` zurück.
pub fn simulate_raw() -> (Vec<Complex32>, Vec<f64>, f64) {
    let c = chirp();
    let phi1 = c.txpsf_hz + c.txprr_hz_s * c.txpl_s / 2.0;
    let phi2 = c.txprr_hz_s / 2.0;
    // Zeilenstart so, dass die Ziel-Verzögerung exakt auf Sample J0 liegt.
    let t_start = 2.0 * R0 / SPEED_OF_LIGHT - J0 as f64 / FS;
    let slant: Vec<f64> = (0..NR)
        .map(|j| (t_start + j as f64 / FS) * SPEED_OF_LIGHT / 2.0)
        .collect();
    assert!((slant[J0] - R0).abs() < 1e-6);
    let mut data = vec![Complex32::zero(); NAZ * NR];
    for p in 0..NAZ {
        let eta = (p as f64 - 512.0) * PRI;
        let r = (R0 * R0 + (V * eta) * (V * eta)).sqrt();
        let tau = 2.0 * r / SPEED_OF_LIGHT;
        // Trägerphase am Echo (Azimut-Phasenhistorie).
        let pc = -2.0 * std::f64::consts::PI * TX_FREQ_HZ * tau;
        let (cs, sn) = (pc.cos() as f32, pc.sin() as f32);
        for j in 0..NR {
            let u = t_start + j as f64 / FS - tau;
            if u.abs() <= c.txpl_s / 2.0 {
                let ph = 2.0 * std::f64::consts::PI * (phi1 * u + phi2 * u * u);
                let (br, bi) = (ph.cos() as f32, ph.sin() as f32);
                // Basisband-Chirp × Trägerphase.
                data[p * NR + j] = Complex32::new(br * cs - bi * sn, br * sn + bi * cs);
            }
        }
    }
    (data, slant, t_start)
}

/// Stärkstes Pixel als `(az, range, Leistung)`.
#[allow(dead_code)] // nicht jede Testdatei nutzt alle Helfer
pub fn peak(img: &[Complex32], nrange: usize) -> (usize, usize, f32) {
    let (mut bi, mut bv) = (0, 0.0f32);
    for (i, c) in img.iter().enumerate() {
        if c.norm_sqr() > bv {
            bv = c.norm_sqr();
            bi = i;
        }
    }
    (bi / nrange, bi % nrange, bv)
}

/// −3-dB-Breite eines 1D-Leistungsschnitts in Pixeln.
#[allow(dead_code)] // nicht jede Testdatei nutzt alle Helfer
pub fn fwhm(cut: &[f32], at: usize) -> usize {
    let half = cut[at] / 2.0;
    let mut lo = at;
    while lo > 0 && cut[lo - 1] >= half {
        lo -= 1;
    }
    let mut hi = at;
    while hi + 1 < cut.len() && cut[hi + 1] >= half {
        hi += 1;
    }
    hi - lo + 1
}
