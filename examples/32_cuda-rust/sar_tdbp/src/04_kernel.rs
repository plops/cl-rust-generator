//! TDBP-Rückprojektion: CPU-Referenz (Host) und GPU-Kernel (Device).
//!
//! Beide nutzen dieselbe Mathematik: pro Pixel und Puls Abstand `d`,
//! Range-Interpolation bei `τ = 2d/c` und Matched-Filter `e^{+j·4πd/λ}`.
//! Der Device-Teil (`#[cuda_module]`, Phase 3) steht am Dateiende.

use crate::simulator::RawData;
use crate::types::{Complex32, RadarParams, SPEED_OF_LIGHT, SceneGeometry};
use std::f32::consts::PI;

/// Lineare Interpolation im Range-Signal von Puls `p` an der
/// (gebrochenen) Sample-Position `s`; außerhalb des Fensters → 0.
pub fn interp_linear(raw: &RawData, pulse: u32, s: f32) -> Complex32 {
    let n = raw.num_samples as f32;
    if s.is_nan() || s < 0.0 || s > n - 1.0 {
        return Complex32::zero();
    }
    let s0 = s.floor() as u32;
    let a = raw.sample(pulse, s0);
    if s0 + 1 >= raw.num_samples {
        return a;
    }
    let b = raw.sample(pulse, s0 + 1);
    let f = s - s0 as f32;
    Complex32::new(a.re + (b.re - a.re) * f, a.im + (b.im - a.im) * f)
}

/// Matched Filter `e^{+j·4πd/λ}`: dreht die Trägerphase `−2πf0τ`
/// des Echos zurück, sodass sich alle Pulse am Streuer kohärent addieren.
pub fn matched_phase(dist: f32, lambda: f32) -> Complex32 {
    Complex32::from_polar(1.0, 4.0 * PI * dist / lambda)
}

/// CPU-Referenz der Backprojection über die ersten `pulse_limit` Pulse.
pub fn tdbp_cpu(
    raw: &RawData,
    geo: SceneGeometry,
    radar: RadarParams,
    pulse_limit: u32,
) -> Vec<Complex32> {
    let lambda = radar.lambda();
    let np = pulse_limit.min(raw.num_pulses);
    let mut out = vec![Complex32::zero(); (geo.width * geo.height) as usize];
    for py in 0..geo.height {
        for px in 0..geo.width {
            let pos = geo.pixel_pos(px, py);
            let mut acc = Complex32::zero();
            for p in 0..np {
                let d = geo.pulse_pos(p).dist(pos);
                let tau = 2.0 * d / SPEED_OF_LIGHT;
                let s = (tau - raw.t0) / raw.dt;
                acc = acc + interp_linear(raw, p, s) * matched_phase(d, lambda);
            }
            out[(py * geo.width + px) as usize] = acc;
        }
    }
    out
}

/// Stärkstes Pixel: `(linearer Index, Leistung |I|²)`.
pub fn peak_power(img: &[Complex32]) -> (usize, f32) {
    let mut best = 0;
    let mut best_v = 0.0f32;
    for (i, c) in img.iter().enumerate() {
        if c.norm_sqr() > best_v {
            best_v = c.norm_sqr();
            best = i;
        }
    }
    (best, best_v)
}

// ── Device-Teil: TDBP-Kernel (ein Thread pro Pixel, 1D-Grid). ──

use cuda_device::{DisjointSlice, kernel, launch_bounds, launch_contract, thread};
use cuda_host::cuda_module;

#[cuda_module]
pub mod kernels {
    use super::*;
    use crate::types::{Complex32, Vec3};
    use core::f32::consts::PI;

    /// Time-Domain Backprojection: `raw` ist puls-major
    /// `[num_pulses × num_samples]`, `plat` die Antennenpositionen.
    /// Nur die ersten `pulse_limit` Pulse werden akkumuliert (inkrementelle
    /// Apertur für die GUI-Animation, ohne Re-Upload).
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    // Flache Skalar-ABI ist Absicht: Der Kernel liest Geometrie direkt aus
    // dem PTX-Parameterraum, ohne struct-Deserialisierung auf dem Device.
    #[allow(clippy::too_many_arguments)]
    pub fn tdbp(
        raw: &[Complex32],
        plat: &[Vec3],
        mut out: DisjointSlice<Complex32>,
        width: u32,
        height: u32,
        num_pulses: u32,
        num_samples: u32,
        pulse_limit: u32,
        lambda: f32,
        c: f32,
        dt: f32,
        t0: f32,
        x0: f32,
        y0: f32,
        dx: f32,
        dy: f32,
    ) {
        if width == 0 || height == 0 {
            return;
        }
        let idx = thread::index_1d();
        let lin = idx.get() as u32;
        let px = lin % width;
        let py = lin / width;
        if px >= width || py >= height {
            return;
        }
        let pos = Vec3::new(
            x0 + (px as f32 + 0.5) * dx,
            y0 + (py as f32 + 0.5) * dy,
            0.0,
        );
        let mut acc_re = 0.0f32;
        let mut acc_im = 0.0f32;
        let np = pulse_limit.min(num_pulses);
        let mut p = 0u32;
        while p < np {
            let ap = plat[p as usize];
            let ddx = ap.x - pos.x;
            let ddy = ap.y - pos.y;
            let ddz = ap.z - pos.z;
            let d = (ddx * ddx + ddy * ddy + ddz * ddz).sqrt();
            // Range-Interpolation (gleiche Formel wie `interp_linear`).
            let s = (2.0 * d / c - t0) / dt;
            let n = num_samples as f32;
            let (s_re, s_im) = if s >= 0.0 && s <= n - 1.0 {
                let s0 = s as u32;
                let a = raw[(p * num_samples + s0) as usize];
                if s0 + 1 >= num_samples {
                    (a.re, a.im)
                } else {
                    let b = raw[(p * num_samples + s0 + 1) as usize];
                    let f = s - s0 as f32;
                    (a.re + (b.re - a.re) * f, a.im + (b.im - a.im) * f)
                }
            } else {
                (0.0, 0.0)
            };
            // Matched Filter e^{+j·4πd/λ} (gleiche Formel wie Host).
            let ph = 4.0 * PI * d / lambda;
            let m_re = ph.cos();
            let m_im = ph.sin();
            acc_re += s_re * m_re - s_im * m_im;
            acc_im += s_re * m_im + s_im * m_re;
            p += 1;
        }
        if let Some(o) = out.get_mut(idx) {
            *o = Complex32::new(acc_re, acc_im);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phantom::PointTarget;
    use crate::simulator::simulate;

    /// PSF-Messgeometrie: 8×8 m auf 65×65 px (Target exakt auf Pixelmitte),
    /// Seitenblick (Mitte 60 m neben der Bahn), 4-m-Apertur → beide
    /// Auflösungen sind einige Pixel breit.
    fn psf_setup() -> (SceneGeometry, RadarParams, Vec<PointTarget>) {
        let geo = SceneGeometry {
            width: 65,
            height: 65,
            x0: -4.0,
            y0: 56.0,
            dx: 8.0 / 65.0,
            dy: 8.0 / 65.0,
            platform_height: 100.0,
            aperture_len: 4.0,
            num_pulses: 64,
        };
        let radar = RadarParams::x_band();
        let c = geo.pixel_pos(32, 32);
        let targets = vec![PointTarget {
            x: c.x,
            y: c.y,
            z: 0.0,
            sigma: 1.0,
        }];
        (geo, radar, targets)
    }

    /// −3-dB-Breite eines 1D-Schnitts (zusammenhängende Pixel ≥ max/2).
    fn fwhm(cut: &[f32], peak: usize, d: f32) -> f32 {
        let half = cut[peak] / 2.0;
        let mut lo = peak;
        while lo > 0 && cut[lo - 1] >= half {
            lo -= 1;
        }
        let mut hi = peak;
        while hi + 1 < cut.len() && cut[hi + 1] >= half {
            hi += 1;
        }
        (hi - lo + 1) as f32 * d
    }

    #[test]
    fn interpolation() {
        let (geo, radar, _) = psf_setup();
        let raw = simulate(geo, radar, &[]);
        assert_eq!(interp_linear(&raw, 0, -1.0), Complex32::zero());
        assert_eq!(
            interp_linear(&raw, 0, raw.num_samples as f32),
            Complex32::zero()
        );
        // Rampensignal: exakte Werte an ganzen, Mittel am halben Sample.
        let n = raw.num_samples;
        let mut ramp = raw.clone();
        for s in 0..n {
            ramp.samples[s as usize] = Complex32::new(s as f32, 0.0);
        }
        assert_eq!(interp_linear(&ramp, 0, 3.0).re, 3.0);
        assert_eq!(interp_linear(&ramp, 0, 3.5).re, 3.5);
    }

    #[test]
    fn fokus_und_psf() {
        let (geo, radar, targets) = psf_setup();
        let raw = simulate(geo, radar, &targets);
        let img = tdbp_cpu(&raw, geo, radar, u32::MAX);
        let w = geo.width as usize;
        let (peak, peak_v) = peak_power(&img);
        let (qx, qy) = (peak % w, peak / w);
        // Peak exakt am Streuer-Pixel (32, 32).
        assert_eq!((qx, qy), (32, 32));
        // Boden-Range-Schnitt (y): FWHM ≈ ΔR·R/y_c ≈ 0,97 m.
        let col: Vec<f32> = (0..geo.height as usize)
            .map(|y| img[y * w + qx].norm_sqr())
            .collect();
        let range_fwhm = fwhm(&col, qy, geo.dy);
        assert!(
            (range_fwhm - 0.97).abs() < 0.4,
            "Range-FWHM {range_fwhm} m, erwartet ≈ 0,97 m"
        );
        // Azimuth-Schnitt (x): FWHM ≈ λR/2L ≈ 0,44 m.
        let row: Vec<f32> = (0..w).map(|x| img[qy * w + x].norm_sqr()).collect();
        let az_fwhm = fwhm(&row, qx, geo.dx);
        assert!(
            (az_fwhm - 0.44).abs() < 0.18,
            "Azimuth-FWHM {az_fwhm} m, erwartet ≈ 0,44 m"
        );
        // Nebenzipfel außerhalb 10-px-Radius < −8 dB.
        let mut side = 0.0f32;
        for y in 0..geo.height as usize {
            for x in 0..w {
                let r2 = (x as i32 - 32).pow(2) + (y as i32 - 32).pow(2);
                if r2 > 100 {
                    side = side.max(img[y * w + x].norm_sqr());
                }
            }
        }
        let sidelobe_db = 10.0 * (side / peak_v).log10();
        assert!(sidelobe_db < -8.0, "Nebenzipfel {sidelobe_db} dB");
    }

    #[test]
    fn puls_limit_kohaerenz() {
        let (geo, radar, targets) = psf_setup();
        let raw = simulate(geo, radar, &targets);
        let full = tdbp_cpu(&raw, geo, radar, 64);
        let part = tdbp_cpu(&raw, geo, radar, 16);
        let _w = geo.width as usize;
        // Gleiche Peak-Lage …
        assert_eq!(peak_power(&full).0, peak_power(&part).0);
        // … aber kohärenter Gewinn ≈ (64/16)² = 16 in Leistung.
        let ratio = peak_power(&full).1 / peak_power(&part).1;
        assert!(
            (12.0..20.0).contains(&ratio),
            "Gewinn {ratio}, erwartet ≈ 16"
        );
    }
}
