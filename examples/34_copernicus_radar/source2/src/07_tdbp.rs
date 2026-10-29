//! Time-Domain-Backprojection (CPU-Referenz in `f64`) für synthetische und reale Punktzieltests.

use crate::types::{C_LIGHT, Complex32, Vec3d};
use std::f64::consts::PI;

/// 2D-Zielgitter im lokalen Koordinatensystem (x = Azimut [m], y = Ground/Slant-Range [m]).
#[derive(Clone, Copy, Debug)]
pub struct TdbpGrid {
    pub n_az: usize,
    pub n_rg: usize,
    pub x0: f64,
    pub dx_az: f64,
    pub y0: f64,
    pub dy_rg: f64,
    pub re_m: f64,
}

impl TdbpGrid {
    #[inline]
    pub fn pixel_pos(&self, i: usize, j: usize) -> Vec3d {
        let x = self.x0 + (i as f64 + 0.5) * self.dx_az;
        let s = self.y0 + (j as f64 + 0.5) * self.dy_rg;
        if self.re_m > 0.0 {
            let th = s / self.re_m;
            Vec3d::new(x, self.re_m * th.sin(), self.re_m * th.cos() - self.re_m)
        } else {
            Vec3d::new(x, s, 0.0)
        }
    }
}

/// Eingabedaten für TDBP (bereits 1D-range-komprimiert und shift-ausgerichtet).
pub struct TdbpInput<'a> {
    pub rc_data: &'a [Complex32],
    pub n_pulses: usize,
    pub n_samples: usize,
    pub plat_pos: &'a [Vec3d],
    pub t0_s: f64,
    pub dt_s: f64,
    pub lambda_m: f64,
}

#[inline]
fn interp_linear(data: &[Complex32], n_samples: usize, pulse: usize, s: f64) -> (f64, f64) {
    if s.is_nan() || s < 0.0 || s > (n_samples.saturating_sub(1)) as f64 {
        return (0.0, 0.0);
    }
    let s0 = s.floor() as usize;
    let a = data[pulse * n_samples + s0];
    if s0 + 1 >= n_samples {
        return (f64::from(a.re), f64::from(a.im));
    }
    let b = data[pulse * n_samples + s0 + 1];
    let f = s - s0 as f64;
    (
        f64::from(a.re) + (f64::from(b.re) - f64::from(a.re)) * f,
        f64::from(a.im) + (f64::from(b.im) - f64::from(a.im)) * f,
    )
}

/// Mehrsträngige CPU-Backprojection über `pulse_limit` Pulse (`std::thread::scope`).
pub fn tdbp_cpu_parallel(grid: &TdbpGrid, input: &TdbpInput, pulse_limit: usize) -> Vec<Complex32> {
    if grid.n_az == 0 || grid.n_rg == 0 {
        return Vec::new();
    }
    let np = pulse_limit.min(input.n_pulses);
    let mut out = vec![Complex32::ZERO; grid.n_az * grid.n_rg];
    let threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(grid.n_az);
    let rows_per = grid.n_az.div_ceil(threads);

    std::thread::scope(|s| {
        for (chunk_idx, slice) in out.chunks_mut(rows_per * grid.n_rg).enumerate() {
            let i0 = chunk_idx * rows_per;
            s.spawn(move || {
                for (di, row) in slice.chunks_mut(grid.n_rg).enumerate() {
                    let i = i0 + di;
                    for (j, cell) in row.iter_mut().enumerate() {
                        let pos = grid.pixel_pos(i, j);
                        let mut acc_re = 0.0f64;
                        let mut acc_im = 0.0f64;
                        for p in 0..np {
                            let d = (input.plat_pos[p] - pos).norm();
                            let s_idx = (2.0 * d / C_LIGHT - input.t0_s) / input.dt_s;
                            let (sr, si) = interp_linear(input.rc_data, input.n_samples, p, s_idx);
                            let ph = 4.0 * PI * d / input.lambda_m;
                            let (ms, mc) = ph.sin_cos();
                            acc_re += sr * mc - si * ms;
                            acc_im += sr * ms + si * mc;
                        }
                        *cell = Complex32::new(acc_re as f32, acc_im as f32);
                    }
                }
            });
        }
    });
    out
}

/// Pearson-Korrelation zweier Log-Power-Felder (`10 * log10(|z|^2 + eps)`).
pub fn correlate_log_power(a: &[Complex32], b: &[Complex32]) -> f64 {
    let n = a.len().min(b.len());
    if n < 2 {
        return 0.0;
    }
    let mut xa = Vec::with_capacity(n);
    let mut xb = Vec::with_capacity(n);
    for i in 0..n {
        xa.push(10.0 * (f64::from(a[i].norm_sqr()) + 1e-12).log10());
        xb.push(10.0 * (f64::from(b[i].norm_sqr()) + 1e-12).log10());
    }
    let ma = xa.iter().sum::<f64>() / (n as f64);
    let mb = xb.iter().sum::<f64>() / (n as f64);
    let mut num = 0.0f64;
    let mut da = 0.0f64;
    let mut db = 0.0f64;
    for i in 0..n {
        let ua = xa[i] - ma;
        let ub = xb[i] - mb;
        num += ua * ub;
        da += ua * ua;
        db += ub * ub;
    }
    let den = (da * db).sqrt();
    if den > 1e-20 { num / den } else { 0.0 }
}
