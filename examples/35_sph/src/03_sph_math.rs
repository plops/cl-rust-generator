//! SPH-Kernelfunktionen als gerätekompatible reine Funktionen.
//!
//! Dieselben Funktionen laufen auf CPU (Backend/Tests) und GPU (aus
//! `#[kernel]` aufgerufen): nur `core`-Arithmetik + `sqrt`, keine
//! Allokation, keine Panikpfade. 2D-Formulierung nach Müller et al.

use core::f32::consts::PI;

/// Normierung des Poly6-Kerns: 4 / (π h⁸). Vor der Schleife berechnen.
pub fn poly6_coef(h: f32) -> f32 {
    4.0 / (PI * h * h * h * h * h * h * h * h)
}

/// Poly6-Glättungskern W(r,h) für die Dichte (skalar, symmetrisch).
///
/// Exakt 0 für r ≥ h oder r < 0 (kein Einfluss außerhalb des Supports).
pub fn poly6(r: f32, h: f32) -> f32 {
    if r < 0.0 || r >= h {
        return 0.0;
    }
    let d = h * h - r * r;
    poly6_coef(h) * d * d * d
}

/// Normierung des Spiky-Gradienten: 30 / (π h⁵). Vor der Schleife berechnen.
pub fn spiky_coef(h: f32) -> f32 {
    30.0 / (PI * h * h * h * h * h)
}

/// Skalarer Faktor s des Spiky-Gradienten mit ∇W = s · q, q = pᵢ − pⱼ.
///
/// Enthält das negative Vorzeichen (Abstoßung aus Hochdruckzonen) und die
/// Division durch r; exakt 0 für r ≤ 0 (keine Selbstkraft-Singularität)
/// oder r ≥ h.
pub fn spiky_grad_factor(r: f32, h: f32) -> f32 {
    if r <= 0.0 || r >= h {
        return 0.0;
    }
    let t = h - r;
    -spiky_coef(h) * t * t / r
}

/// Normierung des Viskositäts-Laplace: 20 / (π h⁵).
pub fn visc_coef(h: f32) -> f32 {
    20.0 / (PI * h * h * h * h * h)
}

/// Viskositäts-Laplacian ∇²W(r,h) (skalar, überall regulär inkl. r = 0).
pub fn visc_laplacian(r: f32, h: f32) -> f32 {
    if r < 0.0 || r >= h {
        return 0.0;
    }
    visc_coef(h) * (h - r)
}

/// Zustandsgleichung P = k(ρ − ρ₀), negativer Druck auf 0 geklemmt.
///
/// Die Klemmung opfert leichte Kohäsion für numerische Stabilität
/// (verhindert Klumpen-Instabilität bei Unterdichte).
pub fn pressure(density: f32, rest_density: f32, stiffness: f32) -> f32 {
    (stiffness * (density - rest_density)).max(0.0)
}

/// Euklidischer Abstand zweier 2D-Punkte (auch für Device-Code).
pub fn dist(a: [f32; 2], b: [f32; 2]) -> f32 {
    let dx = a[0] - b[0];
    let dy = a[1] - b[1];
    (dx * dx + dy * dy).sqrt()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn approx(a: f32, b: f64) {
        let tol = 1e-5 * b.abs().max(1.0);
        assert!((a as f64 - b).abs() < tol, "{a} ≈ {b}");
    }

    #[test]
    fn poly6_trifft_analytische_werte() {
        approx(poly6(0.0, 1.0), 4.0 / PI as f64); // 4/π
        approx(poly6(0.5, 1.0), 1.6875 / PI as f64); // 4/π·0.75³
        assert_eq!(poly6(1.0, 1.0), 0.0);
        assert_eq!(poly6(2.0, 1.0), 0.0);
        assert_eq!(poly6(-0.1, 1.0), 0.0);
        // Skalierung: W(0, h) = 4/(πh²).
        approx(poly6(0.0, 0.04), 4.0 / (PI as f64 * 0.04 * 0.04));
    }

    #[test]
    fn spiky_ist_abstossend_und_regulaer() {
        approx(spiky_grad_factor(0.5, 1.0), -15.0 / PI as f64); // −30/π·0.25/0.5
        assert!(spiky_grad_factor(0.3, 1.0) < 0.0);
        assert_eq!(spiky_grad_factor(0.0, 1.0), 0.0); // keine Singularität
        assert_eq!(spiky_grad_factor(1.0, 1.0), 0.0);
        assert_eq!(spiky_grad_factor(1.5, 1.0), 0.0);
    }

    #[test]
    fn viskositaet_ist_positiv_und_daempfend() {
        approx(visc_laplacian(0.5, 1.0), 10.0 / PI as f64); // 20/π·0.5
        approx(visc_laplacian(0.0, 1.0), 20.0 / PI as f64); // regulär bei r=0
        assert_eq!(visc_laplacian(1.0, 1.0), 0.0);
        assert_eq!(visc_laplacian(2.0, 1.0), 0.0);
    }

    #[test]
    fn druck_klemmt_unterdichte_auf_null() {
        assert_eq!(pressure(1200.0, 1000.0, 2000.0), 400_000.0);
        assert_eq!(pressure(1000.0, 1000.0, 2000.0), 0.0);
        assert_eq!(pressure(500.0, 1000.0, 2000.0), 0.0);
    }
}
