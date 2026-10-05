//! TDBP-Physiktest: gleiche Simulation wie RDA, Fokus per Rückprojektion.
//!
//! Die Rohdaten aus `common` werden range-komprimiert + rückversetzt und auf
//! einem 65×65-Fenster um das Ziel fokussiert (129 Pulse Sub-Apertur).
//! Prüft Peak-Lage, PSF-Breiten, Kohärenzgewinn und Parallel-Identität.

mod common;

use common::{FS, J0, NAZ, NR, PRI, R0, V, chirp, fwhm, peak, simulate_raw};
use sar_focus::chirp::num_tx_samples;
use sar_focus::range::RangeCompressor;
use sar_focus::rda::correlation_shift_samples;
use sar_focus::tdbp::{TdbpGrid, TdbpInput, tdbp_cpu, tdbp_cpu_parallel};
use sar_focus::types::{TX_WAVELENGTH_M, Vec3d};

const WIN: usize = 65;
const NP: usize = 129;

/// Range-komprimierte + rückversetzte Daten (erste NP Pulse), Geometrie.
fn setup() -> (Vec<sar_focus::types::Complex32>, TdbpGrid, Vec<Vec3d>, f64) {
    let (mut data, _, t0) = simulate_raw();
    let comp = RangeCompressor::new(&chirp(), NR);
    comp.compress_rows(&mut data[..NP * NR]);
    let shift = correlation_shift_samples(num_tx_samples(&chirp()), NR);
    for p in 0..NP {
        data[p * NR..(p + 1) * NR].rotate_right(shift);
    }
    // Gerade Bahn im lokalen Rahmen (exakt die Sim-Geometrie).
    let dx = V * PRI;
    let h = 693_000.0;
    let yc = (R0 * R0 - h * h).sqrt();
    let dy = (sar_focus::types::SPEED_OF_LIGHT / 2.0 / FS) / (yc / R0);
    let plat: Vec<Vec3d> = (0..NP)
        .map(|p| Vec3d::new((p as f64 - (NAZ - 1) as f64 / 2.0) * dx, 0.0, h))
        .collect();
    // Fenster 65×65 um das Ziel (global (512, J0) → lokal (32, 32)).
    let grid = TdbpGrid {
        naz: WIN,
        nrange: WIN,
        x0: (480.0 - 512.0) * dx - 0.5 * dx,
        dx_az: dx,
        y_near: yc + (2016.0 - J0 as f64) * dy - 0.5 * dy,
        dy_gr: dy,
        re_m: 0.0,
    };
    (data, grid, plat, t0)
}

fn input<'a>(data: &'a [sar_focus::types::Complex32], plat: &'a [Vec3d], t0: f64) -> TdbpInput<'a> {
    TdbpInput {
        data: &data[..NP * NR],
        npulse: NP,
        nsample: NR,
        plat,
        t0_s: t0,
        dt_s: 1.0 / FS,
        lambda: TX_WAVELENGTH_M,
    }
}

#[test]
fn tdbp_fokussiert_exakt() {
    let (data, grid, plat, t0) = setup();
    let inp = input(&data, &plat, t0);
    let img = tdbp_cpu(&grid, &inp, NP);
    let (pa, pr, _) = peak(&img, WIN);
    assert_eq!((pa, pr), (32, 32), "Peak bei ({pa}, {pr})");
    // Azimut-FWHM: Theorie λR/2L mit L = 128·dx ≈ 546 m → 11,3 px.
    let row: Vec<f32> = (0..WIN).map(|a| img[a * WIN + pr].norm_sqr()).collect();
    let azw = fwhm(&row, pa);
    assert!((8..=15).contains(&azw), "Azimut-FWHM = {azw} px");
    // Range-FWHM: Theorie 0,89·fs/B ≈ 1,0 px.
    let col: Vec<f32> = (0..WIN).map(|r| img[pa * WIN + r].norm_sqr()).collect();
    let rw = fwhm(&col, pr);
    assert!((1..=2).contains(&rw), "Range-FWHM = {rw} px");
}

#[test]
fn tdbp_kohaerenz_und_parallel() {
    let (data, grid, plat, t0) = setup();
    let inp = input(&data, &plat, t0);
    // Kohärenz: 128 vs. 32 Pulse → 16× Leistung am Ziel-Pixel.
    // (Argmax-Gleichheit wäre zu streng: Die 32er-Teilapertur schielt stark
    // und kippt das Maximum um 1 Bin neben das Ziel.)
    let full = tdbp_cpu(&grid, &inp, 128);
    let part = tdbp_cpu(&grid, &inp, 32);
    let c = 32 * WIN + 32;
    let ratio = full[c].norm_sqr() / part[c].norm_sqr();
    assert!(
        (12.0..20.0).contains(&ratio),
        "Gewinn {ratio}, erwartet ≈ 16"
    );
    // Parallel bit-identisch (auch krumme Strangzahlen).
    for threads in [1, 3, 64] {
        let par = tdbp_cpu_parallel(&grid, &inp, NP, threads);
        let serial = tdbp_cpu(&grid, &inp, NP);
        assert_eq!(par, serial, "threads={threads}");
    }
}
