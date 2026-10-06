//! Integrationstests: SPH-Kerne gegen analytische Werte + Eigenschaften.

use sph::sph_math::{poly6, poly6_coef, pressure, spiky_grad_factor, visc_coef, visc_laplacian};
use std::f32::consts::PI;

fn approx(a: f32, b: f32) {
    let tol = 1e-5 * b.abs().max(1.0);
    assert!((a - b).abs() < tol, "{a} ≈ {b}");
}

#[test]
fn koeffizienten_und_poly6_werte() {
    approx(poly6_coef(1.0), 4.0 / PI);
    approx(poly6(0.0, 1.0), 4.0 / PI);
    approx(poly6(0.5, 1.0), 1.6875 / PI);
    approx(poly6(0.0, 0.04), 4.0 / (PI * 0.04 * 0.04));
    // Monoton fallend im Support, null außerhalb.
    let h = 0.04;
    let mut prev = poly6(0.0, h);
    let mut r = 0.002;
    while r < h {
        let w = poly6(r, h);
        assert!(w <= prev && w > 0.0, "r={r}: {w} <= {prev}");
        prev = w;
        r += 0.002;
    }
    assert_eq!(poly6(h, h), 0.0);
}

#[test]
fn spiky_ist_negativ_und_klingt_ab() {
    approx(spiky_grad_factor(0.5, 1.0), -15.0 / PI);
    // Betrag fällt mit r (schwächere Abstoßung aus der Ferne).
    let h = 1.0;
    let mut r = 0.05;
    let mut prev = spiky_grad_factor(r, h).abs();
    r += 0.05;
    while r < h {
        let cur = spiky_grad_factor(r, h).abs();
        assert!(cur <= prev, "r={r}: {cur} <= {prev}");
        prev = cur;
        r += 0.05;
    }
}

#[test]
fn viskositaet_ist_linear_fallend() {
    // ∇²W = c(h−r): exakt linear in r.
    let h = 0.04;
    let c = visc_coef(h);
    for r in [0.0, 0.01, 0.02, 0.03, 0.039] {
        approx(visc_laplacian(r, h), c * (h - r));
    }
    assert_eq!(visc_laplacian(h, h), 0.0);
}

#[test]
fn druck_waechst_linear_mit_ueberdichte() {
    approx(pressure(1500.0, 1000.0, 2000.0), 1_000_000.0);
    // Monoton in der Dichte, nie negativ.
    let mut prev = 0.0;
    let mut rho = 0.0;
    while rho <= 3000.0 {
        let p = pressure(rho, 1000.0, 2000.0);
        assert!(p >= prev && p >= 0.0);
        prev = p;
        rho += 100.0;
    }
}
