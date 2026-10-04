//! Vorwärtssimulation: range-komprimierte Chirp-Echos (CPU).
//!
//! Für jeden Puls `p` und jeden Streuer `k` wird die Zweiwege-Laufzeit
//! `τ = 2·R/c` berechnet und das Echo
//! `s(p,t) = Σ σ·sinc(B·(t−τ))·exp(−j·2π·f0·τ)`
//! auf die Fast-Time-Achse aufaddiert (Prompt, Schritt 2).

use crate::phantom::PointTarget;
use crate::types::{Complex32, RadarParams, SPEED_OF_LIGHT, SceneGeometry, Vec3};
use std::f32::consts::PI;

/// Überabtastung der Fast-Time-Achse relativ zur Bandbreite:
/// `dt = 1 / (OVERSAMPLE · B)`. 8× macht die lineare Interpolation im
/// TDBP-Kernel genau genug (< −40 dB Fehler am Sinc-Hauptzipfel).
pub const OVERSAMPLE: f32 = 8.0;

/// Zusatz-Samples links/rechts des geometrischen Fensters, damit auch
/// Sinc-Nebenzipfel von Randstreuern erfasst werden.
pub const MARGIN_SAMPLES: u32 = 32;

/// Rohdatenmatrix: `samples[p · num_samples + s]`, puls-major.
#[derive(Clone, Debug)]
pub struct RawData {
    pub samples: Vec<Complex32>,
    pub num_pulses: u32,
    pub num_samples: u32,
    /// Zeit des ersten Samples in Sekunden.
    pub t0: f32,
    /// Abtastintervall in Sekunden.
    pub dt: f32,
}

impl RawData {
    pub fn sample(&self, pulse: u32, s: u32) -> Complex32 {
        self.samples[(pulse * self.num_samples + s) as usize]
    }
}

/// Normierte Sinc-Funktion `sin(πx)/(πx)`, `sinc(0) = 1`.
pub fn sinc(x: f32) -> f32 {
    if x == 0.0 {
        1.0
    } else {
        let px = PI * x;
        px.sin() / px
    }
}

/// Fast-Time-Achse aus der Szenengeometrie (Ecken × Pulse), unabhängig vom
/// Phantom — leere Phantome liefern daher Nullsignal bei gleichen Dims.
pub fn time_axis(geo: SceneGeometry, radar: RadarParams) -> (f32, f32, u32) {
    let dt = 1.0 / (OVERSAMPLE * radar.bandwidth);
    let w = geo.width as f32 * geo.dx;
    let h = geo.height as f32 * geo.dy;
    let mut rmin = f32::MAX;
    let mut rmax = f32::MIN;
    for &cy in &[geo.y0, geo.y0 + h] {
        for &cx in &[geo.x0, geo.x0 + w] {
            let corner = Vec3::new(cx, cy, 0.0);
            for p in 0..geo.num_pulses {
                let r = geo.pulse_pos(p).dist(corner);
                rmin = rmin.min(r);
                rmax = rmax.max(r);
            }
        }
    }
    let margin = MARGIN_SAMPLES as f32 * dt;
    let t0 = 2.0 * rmin / SPEED_OF_LIGHT - margin;
    let t1 = 2.0 * rmax / SPEED_OF_LIGHT + margin;
    let n = ((t1 - t0) / dt).ceil() as u32;
    (t0, dt, n)
}

/// Simuliert die Echos aller Streuer für alle Pulse.
pub fn simulate(geo: SceneGeometry, radar: RadarParams, targets: &[PointTarget]) -> RawData {
    let (t0, dt, n) = time_axis(geo, radar);
    let mut samples = vec![Complex32::zero(); (geo.num_pulses * n) as usize];
    for p in 0..geo.num_pulses {
        let pp = geo.pulse_pos(p);
        for k in targets {
            let r = pp.dist(Vec3::new(k.x, k.y, k.z));
            let tau = 2.0 * r / SPEED_OF_LIGHT;
            // Trägerphase am Echo-Peak (range-komprimiertes Modell).
            let carrier = Complex32::from_polar(k.sigma, -2.0 * PI * radar.f0 * tau);
            let row = &mut samples[(p * n) as usize..((p + 1) * n) as usize];
            for (s, cell) in row.iter_mut().enumerate() {
                let t = t0 + s as f32 * dt;
                let w = sinc(radar.bandwidth * (t - tau));
                *cell = *cell + Complex32::new(carrier.re * w, carrier.im * w);
            }
        }
    }
    RawData {
        samples,
        num_pulses: geo.num_pulses,
        num_samples: n,
        t0,
        dt,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phantom::single_point;

    fn setup() -> (SceneGeometry, RadarParams) {
        (
            SceneGeometry::default_scene(64, 64, 16),
            RadarParams::x_band(),
        )
    }

    #[test]
    fn dims_und_achse() {
        let (geo, radar) = setup();
        let raw = simulate(geo, radar, &[]);
        assert_eq!(raw.num_pulses, 16);
        assert_eq!(raw.samples.len(), 16 * raw.num_samples as usize);
        assert!((raw.dt - 1.0 / (8.0 * 300.0e6)).abs() < 1e-15);
        // Fenster deckt die geometrische Laufzeitspanne ab.
        let rmin = geo.pulse_pos(8).dist(geo.center());
        let tau = 2.0 * rmin / SPEED_OF_LIGHT;
        assert!(raw.t0 < tau && tau < raw.t0 + raw.num_samples as f32 * raw.dt);
    }

    #[test]
    fn peak_lage_und_phase() {
        let (geo, radar) = setup();
        let targets = single_point(geo);
        let raw = simulate(geo, radar, &targets);
        // Peak von Puls 8 muss bei τ = 2R/c liegen (±1 Sample).
        let p = 8;
        let r = geo
            .pulse_pos(p)
            .dist(Vec3::new(targets[0].x, targets[0].y, 0.0));
        let tau = 2.0 * r / SPEED_OF_LIGHT;
        let expect = (tau - raw.t0) / raw.dt;
        let peak = (0..raw.num_samples)
            .max_by(|&a, &b| {
                raw.sample(p, a)
                    .norm_sqr()
                    .total_cmp(&raw.sample(p, b).norm_sqr())
            })
            .unwrap();
        assert!((peak as f32 - expect).abs() <= 1.0);
        // Trägerphase am Peak: −2πf0τ (mod 2π, ±0,1 rad).
        let got = raw.sample(p, peak);
        let want_phase = -2.0 * PI * radar.f0 * tau;
        let mut d = (got.im.atan2(got.re) - want_phase) % (2.0 * PI);
        if d > PI {
            d -= 2.0 * PI;
        }
        if d < -PI {
            d += 2.0 * PI;
        }
        assert!(d.abs() < 0.1, "Phasenfehler {d}");
        // Peak-Amplitude ≈ σ (Sinc-Maximum).
        assert!((got.norm() - 1.0).abs() < 0.05);
    }

    #[test]
    fn leer_und_linear() {
        let (geo, radar) = setup();
        let empty = simulate(geo, radar, &[]);
        assert!(empty.samples.iter().all(|&c| c == Complex32::zero()));
        // Doppeltes σ → doppelte Amplitude (Linearität).
        let mut t = single_point(geo);
        t[0].sigma = 2.0;
        let double = simulate(geo, radar, &t);
        t[0].sigma = 1.0;
        let single = simulate(geo, radar, &t);
        for (d, s) in double.samples.iter().zip(single.samples.iter()) {
            assert!((d.re - 2.0 * s.re).abs() < 1e-5);
            assert!((d.im - 2.0 * s.im).abs() < 1e-5);
        }
    }
}
