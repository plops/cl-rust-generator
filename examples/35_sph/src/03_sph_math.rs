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

/// Erlaubter Unterdruck als Bruchteil von k·ρ₀ (Kohäsions-Limit).
///
/// 0,1 begrenzt die Zugspannung auf 10 % des Vollvakuum-Drucks: genug, um
/// Rand-Katapult und Zerstäuben zu dämpfen (Anziehung über den symmetrischen
/// Druckterm), zu klein für Klumpen-Instabilität im Volumen.
pub const TENSION_RATIO: f32 = 0.1;

/// Zustandsgleichung P = k(ρ − ρ₀), Unterdruck auf −cap begrenzt.
///
/// Anders als die reine 0-Klemmung erlaubt das schwache Kohäsion: Partikel
/// in Unterdichte (freie Oberfläche, Gischt) ziehen sich sanft an, statt
/// als drucklose Fragmente auseinanderzufliegen. NaN-Dichte gibt 0
/// (keine Phantom-Anziehung); die Funktion bleibt gerätekompatibel.
pub fn pressure(density: f32, rest_density: f32, stiffness: f32) -> f32 {
    if density.is_nan() {
        return 0.0;
    }
    let cap = TENSION_RATIO * stiffness * rest_density;
    (stiffness * (density - rest_density)).max(-cap)
}

/// Partikelabstand aus Masse/Ruhedichte (exakt, skaliert mit N und `--h`).
///
/// Referenzradius der Zug-Rampe: Am Abstand selbst ist die Anziehung 0
/// (ruhendes Volumen, keine negative Steifigkeit), jenseits widersteht sie
/// der Dehnung, innerhalb gilt nur Abstoßung + Viskosität.
pub fn cohesion_core(mass: f32, rest_density: f32) -> f32 {
    (mass / rest_density).sqrt()
}

/// Zug-Rampe: 0 am Partikelabstand → 1 in der Ferne (stabile Kohäsion).
///
/// Skaliert den Unterdruck-Anteil des Druckterms mit `1−(W(r)/W(s))⁴`
/// (quadratische Distanzen, keine Wurzel). Anziehung mit negativer
/// Steifigkeit am Ruhezustand wäre tensile Instabilität (Dauer-Jitter);
/// die Rampe gibt Bindungen positive Steifigkeit: Dehnung zieht zurück,
/// Ruhelage ist kräftefrei, Überlappung stößt ab (positiver Druck).
/// Für h ≤ s (entartet, keine Nachbarn) gracefully 0.
pub fn tension_ramp(r2: f32, h2: f32, s2: f32) -> f32 {
    let q = (h2 - r2) / (h2 - s2).max(1e-12);
    let q3 = q * q * q;
    (1.0 - q3 * q3 * q3 * q3).max(0.0)
}

/// Druckterm mit Zug-Rampe: Abstoßung unverändert, Anziehung skaliert.
pub fn ramped_pterm(pterm: f32, r2: f32, h2: f32, s2: f32) -> f32 {
    if pterm < 0.0 {
        pterm * tension_ramp(r2, h2, s2)
    } else {
        pterm
    }
}

/// XSPH-Glättung (Monaghan): Nachbarschaftsmittel der Geschwindigkeit.
///
/// `corr = Σ (m/ρ̄)(vⱼ−vᵢ)W` wird in `k_integrate` als `v + ε·corr` für die
/// Positions-Integration verwendet. Dämpft Gitter-Jitter (tensile
/// Instabilität), erhält Volumenströmung und Impuls. ε ∈ [0,1).
pub const XSPH_EPS: f32 = 0.5;

/// XSPH-Paarbeitrag eines Nachbarn (symmetrische Dichte, Poly6-Gewicht).
pub fn xsph_pair(mass: f32, rho_avg: f32, vi: [f32; 2], vj: [f32; 2], r: f32, h: f32) -> [f32; 2] {
    let w = mass / rho_avg.max(1e-6) * poly6(r, h);
    [w * (vj[0] - vi[0]), w * (vj[1] - vi[1])]
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
    fn druck_erlaubt_begrenzten_unterdruck() {
        assert_eq!(pressure(1200.0, 1000.0, 2000.0), 400_000.0);
        assert_eq!(pressure(1000.0, 1000.0, 2000.0), 0.0);
        // Halbe Ruhedichte: roh −1 MPa, geklemmt auf −cap = −200 kPa.
        assert_eq!(pressure(500.0, 1000.0, 2000.0), -200_000.0);
        // Tiefes Vakuum sättigt am Cap, nie darunter.
        assert_eq!(pressure(0.0, 1000.0, 2000.0), -200_000.0);
        assert_eq!(pressure(-500.0, 1000.0, 2000.0), -200_000.0);
        // NaN bleibt drucklos (keine Phantom-Kohäsion).
        assert_eq!(pressure(f32::NAN, 1000.0, 2000.0), 0.0);
    }

    #[test]
    fn kohaesions_kern_ist_partikelabstand() {
        // Masse aus ρ₀·s² → Kern exakt s (skaliert mit N und h).
        for (m, rho0, s) in [(0.0374, 1000.0, 0.006114), (0.2986, 1000.0, 0.01728)] {
            let core = cohesion_core(m, rho0);
            assert!((core - s).abs() < 1e-4, "{core} ≈ {s}");
        }
    }

    #[test]
    fn xsph_mittelt_relativgeschwindigkeiten() {
        // Ruhende Nachbarschaft → keine Korrektur.
        assert_eq!(
            xsph_pair(0.0374, 1000.0, [1.0, 2.0], [1.0, 2.0], 0.01, 0.04),
            [0.0, 0.0]
        );
        // Schnellerer Nachbar zieht mit, linear in Δv (exakt, ×2 ist exakt).
        let c = xsph_pair(0.0374, 1000.0, [0.0, 0.0], [2.0, 0.0], 0.01, 0.04);
        assert!(c[0] > 0.0 && c[1] == 0.0);
        let c2 = xsph_pair(0.0374, 1000.0, [0.0, 0.0], [4.0, 0.0], 0.01, 0.04);
        assert_eq!(c2, [2.0 * c[0], 0.0]);
        // Außerhalb des Supports exakt 0.
        assert_eq!(
            xsph_pair(0.0374, 1000.0, [0.0, 0.0], [2.0, 0.0], 0.05, 0.04),
            [0.0, 0.0]
        );
    }

    #[test]
    fn zug_rampe_steigt_von_null_auf_eins() {
        let h2 = 0.04 * 0.04;
        let s2 = 0.006 * 0.006;
        assert_eq!(tension_ramp(s2, h2, s2), 0.0); // Ruhelage kräftefrei
        assert_eq!(tension_ramp(0.0, h2, s2), 0.0); // Kern ohne Singularität
        approx(tension_ramp(0.02 * 0.02, h2, s2), 0.958377); // ~3s fast voll
        assert_eq!(ramped_pterm(2.0, s2, h2, s2), 2.0); // Abstoßung pur
        assert_eq!(ramped_pterm(-2.0, s2, h2, s2), 0.0); // Anziehung am Abstand 0
        // Monoton wachsend zwischen s und h.
        let mut prev = 0.0f32;
        for k in 1..=10 {
            let r = 0.006 + (0.039 - 0.006) * k as f32 / 10.0;
            let f = tension_ramp(r * r, h2, s2);
            assert!(f >= prev, "r={r}: {f} >= {prev}");
            prev = f;
        }
        assert!(prev > 0.99); // Fern fast 1
    }
}
