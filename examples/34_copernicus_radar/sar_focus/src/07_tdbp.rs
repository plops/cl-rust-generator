//! Time-Domain-Backprojection auf der CPU (Referenz, `f64`-Geometrie).
//!
//! Pro Pixel und Puls: Abstand `d`, Range-Interpolation bei `τ = 2d/c`,
//! Matched-Filter `e^{+j·4πd/λ}`, kohärent aufsummieren (Muster aus
//! `sar_tdbp`, dort `04_kernel.rs`). Eingang: range-komprimierte,
//! rückversetzte Zeilen; Geometrie in `f64` (bei ~900 km versagt `f32`).

use crate::types::{Complex32, SPEED_OF_LIGHT, Vec3d, WGS84_A_M, WGS84_B_M};

/// TDBP-Pixelraster im lokalen Rahmen (x = Azimut, y = Ground-Range).
#[derive(Clone, Copy, Debug)]
pub struct TdbpGrid {
    pub naz: usize,
    pub nrange: usize,
    /// Azimut der Bildecke (m), Pixelmitten bei `x0 + (i+0,5)·dx`.
    pub x0: f64,
    pub dx_az: f64,
    /// Ground-Range der Bildecke (m).
    pub y_near: f64,
    pub dy_gr: f64,
}

impl TdbpGrid {
    /// Bodenposition der Pixelmitte (z = 0).
    pub fn pixel_pos(self, i: usize, j: usize) -> Vec3d {
        Vec3d::new(
            self.x0 + (i as f64 + 0.5) * self.dx_az,
            self.y_near + (j as f64 + 0.5) * self.dy_gr,
            0.0,
        )
    }
}

/// TDBP-Eingang: range-komprimierte Daten + Pulsgeometrie.
pub struct TdbpInput<'a> {
    /// `(npulse, nsample)`, zeilenmajor, rückversetzt (Sample `s` ↔ `t0+s·dt`).
    pub data: &'a [Complex32],
    pub npulse: usize,
    pub nsample: usize,
    /// Plattformposition je Puls im lokalen Rahmen.
    pub plat: &'a [Vec3d],
    /// Zweiwege-Fast-Time von Sample 0 in s.
    pub t0_s: f64,
    /// Fast-Time-Abtastung in s.
    pub dt_s: f64,
    pub lambda: f64,
}

/// Lineare Interpolation im Range-Signal von Puls `p` (f64-Genauigkeit).
fn interp_linear(data: &[Complex32], nsample: usize, pulse: usize, s: f64) -> (f64, f64) {
    if s.is_nan() || s < 0.0 || s > nsample as f64 - 1.0 {
        return (0.0, 0.0);
    }
    let s0 = s.floor() as usize;
    let a = data[pulse * nsample + s0];
    if s0 + 1 >= nsample {
        return (f64::from(a.re), f64::from(a.im));
    }
    let b = data[pulse * nsample + s0 + 1];
    let f = s - s0 as f64;
    (
        f64::from(a.re) + (f64::from(b.re) - f64::from(a.re)) * f,
        f64::from(a.im) + (f64::from(b.im) - f64::from(a.im)) * f,
    )
}

/// Backprojektion über die ersten `pulse_limit` Pulse (serielle Referenz).
pub fn tdbp_cpu(grid: &TdbpGrid, input: &TdbpInput, pulse_limit: usize) -> Vec<Complex32> {
    let np = pulse_limit.min(input.npulse);
    let mut out = vec![Complex32::zero(); grid.naz * grid.nrange];
    for i in 0..grid.naz {
        for j in 0..grid.nrange {
            out[i * grid.nrange + j] = backproject_pixel(grid, input, np, i, j);
        }
    }
    out
}

/// Mehrsträngige CPU-Referenz (bit-identisch, `std::thread::scope`).
pub fn tdbp_cpu_parallel(
    grid: &TdbpGrid,
    input: &TdbpInput,
    pulse_limit: usize,
    threads: usize,
) -> Vec<Complex32> {
    if grid.naz == 0 || grid.nrange == 0 {
        return Vec::new();
    }
    let np = pulse_limit.min(input.npulse);
    let mut out = vec![Complex32::zero(); grid.naz * grid.nrange];
    let threads = threads.max(1).min(grid.naz);
    let rows_per = grid.naz.div_ceil(threads);
    std::thread::scope(|s| {
        for (chunk, slice) in out.chunks_mut(rows_per * grid.nrange).enumerate() {
            let start = chunk * rows_per;
            s.spawn(move || {
                for (r, row) in slice.chunks_mut(grid.nrange).enumerate() {
                    for (j, cell) in row.iter_mut().enumerate() {
                        *cell = backproject_pixel(grid, input, np, start + r, j);
                    }
                }
            });
        }
    });
    out
}

fn backproject_pixel(
    grid: &TdbpGrid,
    input: &TdbpInput,
    np: usize,
    i: usize,
    j: usize,
) -> Complex32 {
    let pos = grid.pixel_pos(i, j);
    let mut acc_re = 0.0f64;
    let mut acc_im = 0.0f64;
    for p in 0..np {
        let d = input.plat[p].dist(pos);
        let s = (2.0 * d / SPEED_OF_LIGHT - input.t0_s) / input.dt_s;
        let (s_re, s_im) = interp_linear(input.data, input.nsample, p, s);
        // Matched-Filter e^{+j·4πd/λ} (f64-Phase, f64-trig).
        let ph = 4.0 * std::f64::consts::PI * d / input.lambda;
        let (m_re, m_im) = (ph.cos(), ph.sin());
        acc_re += s_re * m_re - s_im * m_im;
        acc_im += s_re * m_im + s_im * m_re;
    }
    Complex32::new(acc_re as f32, acc_im as f32)
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

/// Lokaler Rahmen: Ursprung = Sub-Satelliten-Bodenpunkt, x = along-track,
/// y = rechts (S1 schaut rechts), z = oben. (Linkshändig — für TDBP, das nur
/// Abstände nutzt, irrelevant.)
#[derive(Clone, Copy, Debug)]
pub struct LocalFrame {
    pub origin: Vec3d,
    pub x: Vec3d,
    pub y: Vec3d,
    pub z: Vec3d,
}

impl LocalFrame {
    /// Rahmen aus Orbitposition/-geschwindigkeit (ECEF) der Aperturmitte.
    pub fn from_orbit(pos: Vec3d, vel: Vec3d) -> Self {
        let re = (WGS84_A_M + WGS84_B_M) / 2.0;
        let origin = pos.scale(re / pos.norm());
        let z = origin.scale(1.0 / re);
        let x = vel.sub(z.scale(vel.dot(z)));
        let x = x.scale(1.0 / x.norm());
        // y = x × z zeigt rechts der Flugrichtung (S1-Standard).
        let y = Vec3d::new(
            x.y * z.z - x.z * z.y,
            x.z * z.x - x.x * z.z,
            x.x * z.y - x.y * z.x,
        );
        Self { origin, x, y, z }
    }

    /// ECEF → lokal.
    pub fn to_local(self, p: Vec3d) -> Vec3d {
        let d = p.sub(self.origin);
        Vec3d::new(d.dot(self.x), d.dot(self.y), d.dot(self.z))
    }
}

/// Gerade Bahn: `n` Positionen im Abstand `dx` auf Höhe `h`, bei `y_off`.
pub fn straight_track(n: usize, dx: f64, h: f64, y_off: f64) -> Vec<Vec3d> {
    (0..n)
        .map(|p| Vec3d::new((p as f64 - (n - 1) as f64 / 2.0) * dx, y_off, h))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lokaler_rahmen_aequator() {
        // Äquator, prograd: Ursprung (Re,0,0), x = Ost, y = Süd (rechts), z = oben.
        let re = (WGS84_A_M + WGS84_B_M) / 2.0;
        let f = LocalFrame::from_orbit(Vec3d::new(re + 693_000.0, 0.0, 0.0), Vec3d::new(0.0, 7590.0, 0.0));
        assert!((f.origin.x - re).abs() < 1e-6 && f.origin.y == 0.0);
        assert!((f.x.y - 1.0).abs() < 1e-12);
        assert!((f.y.z + 1.0).abs() < 1e-12); // Süd = rechts bei Ost-Flug
        assert!((f.z.x - 1.0).abs() < 1e-12);
        // Orthonormal.
        assert!(f.x.dot(f.y).abs() < 1e-12);
        assert!(f.x.dot(f.z).abs() < 1e-12);
        assert!(f.y.dot(f.z).abs() < 1e-12);
        // Lokale Plattform: über dem Ursprung in Höhe h.
        let pl = f.to_local(Vec3d::new(re + 693_000.0, 0.0, 0.0));
        assert!((pl.x - 0.0).abs() < 1e-6);
        assert!((pl.z - 693_000.0).abs() < 1e-3);
    }

    #[test]
    fn gerade_bahn_symmetrisch() {
        let t = straight_track(5, 2.0, 100.0, 7.0);
        assert_eq!(t.len(), 5);
        assert_eq!(t[2], Vec3d::new(0.0, 7.0, 100.0));
        assert_eq!(t[0].x, -4.0);
        assert_eq!(t[4].x, 4.0);
    }
}
