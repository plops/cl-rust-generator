//! Range-Kompression auf der CPU: `FFT(Zeile)·conj(FFT(Chirp)) → iFFT`.
//!
//! Pro Echo eine Zeilen-FFT (rustfft), multipliziert mit dem
//! Range-Matched-Filter aus `04_chirp`, zurücktransformiert. Zusätzlich
//! FFT-Helfer (Shifts, DFT-Frequenzraster) für den RDA-Arm.

use crate::chirp::{ChirpParams, embed_start, replica};
use crate::types::Complex32;
use num_complex::Complex32 as Nc32;
use rustfft::{Fft, FftPlanner};
use std::sync::Arc;

/// Zeilen-Kompressor: Filter + geplante FFTs werden wiederverwendet.
pub struct RangeCompressor {
    fwd: Arc<dyn Fft<f32>>,
    inv: Arc<dyn Fft<f32>>,
    filter: Vec<Nc32>,
    n: usize,
}

impl RangeCompressor {
    /// Baut den Matched-Filter: Replika einbetten, FFT, konjugieren.
    pub fn new(p: &ChirpParams, n: usize) -> Self {
        let mut planner = FftPlanner::<f32>::new();
        let fwd = planner.plan_fft_forward(n);
        let inv = planner.plan_fft_inverse(n);
        let r = replica(p);
        let ntx = r.len().min(n);
        let start = embed_start(n, ntx).min(n.saturating_sub(ntx));
        let mut h = vec![Nc32::new(0.0, 0.0); n];
        for (i, c) in r[..ntx].iter().enumerate() {
            h[start + i] = Nc32::new(c.re, c.im);
        }
        fwd.process(&mut h);
        for c in h.iter_mut() {
            *c = c.conj();
        }
        Self {
            fwd,
            inv,
            filter: h,
            n,
        }
    }

    pub fn filter(&self) -> &[Nc32] {
        &self.filter
    }

    pub fn len_range(&self) -> usize {
        self.n
    }

    /// Komprimiert eine Zeile in-place (zirkuläre Korrelation).
    pub fn compress_row(&self, row: &mut [Complex32]) {
        debug_assert_eq!(row.len(), self.n);
        let mut buf: Vec<Nc32> = row.iter().map(|c| Nc32::new(c.re, c.im)).collect();
        self.fwd.process(&mut buf);
        for (b, f) in buf.iter_mut().zip(self.filter.iter()) {
            *b *= *f;
        }
        self.inv.process(&mut buf);
        let s = 1.0 / self.n as f32;
        for (r, b) in row.iter_mut().zip(buf.iter()) {
            *r = Complex32::new(b.re * s, b.im * s);
        }
    }

    /// Komprimiert alle Zeilen eines Blocks (`rows × n`, zeilenmajor).
    pub fn compress_rows(&self, data: &mut [Complex32]) {
        debug_assert_eq!(data.len() % self.n, 0);
        for row in data.chunks_mut(self.n) {
            self.compress_row(row);
        }
    }
}

/// `fftshift` für 1D-Puffer (Nullfrequenz in die Mitte).
pub fn fftshift<T: Clone>(buf: &mut [T]) {
    let n = buf.len();
    let half = n.div_ceil(2);
    buf.rotate_right(n - half);
}

/// Inverser Shift (Mitte → Anfang). Für gerade N identisch zu `fftshift`.
pub fn ifftshift<T: Clone>(buf: &mut [T]) {
    let n = buf.len();
    buf.rotate_left(n / 2);
}

/// Exaktes DFT-Frequenzraster (Shift-Konvention): `f[i] = (i−N/2)·fs/N`.
pub fn dft_freqs(n: usize, fs_hz: f64) -> Vec<f64> {
    (0..n)
        .map(|i| (i as f64 - n as f64 / 2.0) * fs_hz / n as f64)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_chirp() -> ChirpParams {
        ChirpParams {
            txpsf_hz: 1.0e6,
            txpl_s: 5.0e-6,
            txprr_hz_s: -4.0e11,
            fs_hz: 10.0e6,
        }
    }

    /// Naive DFT als unabhängiges Orakel (kleine N).
    fn naive_dft(x: &[Nc32]) -> Vec<Nc32> {
        let n = x.len();
        (0..n)
            .map(|k| {
                let mut acc = Nc32::new(0.0, 0.0);
                for (j, &v) in x.iter().enumerate() {
                    let ph = -2.0 * std::f32::consts::PI * k as f32 * j as f32 / n as f32;
                    acc += v * Nc32::new(ph.cos(), ph.sin());
                }
                acc
            })
            .collect()
    }

    #[test]
    fn filter_gegen_ssfocus_rechenweg() {
        // SSFocus-Rechenweg nachgebaut: einbetten → FFT → conj.
        // Orakel ist die naive DFT (unabhängig von rustfft).
        let p = test_chirp();
        let n = 64;
        let comp = RangeCompressor::new(&p, n);
        let r = replica(&p);
        let ntx = r.len();
        let start = embed_start(n, ntx);
        let mut h = vec![Nc32::new(0.0, 0.0); n];
        for (i, c) in r.iter().enumerate() {
            h[start + i] = Nc32::new(c.re, c.im);
        }
        let expect: Vec<Nc32> = naive_dft(&h).iter().map(|c| c.conj()).collect();
        let mut max = 0.0f32;
        for (g, e) in comp.filter().iter().zip(expect.iter()) {
            max = max.max((g - e).norm());
        }
        let peak = expect.iter().map(|c| c.norm()).fold(0.0f32, f32::max);
        assert!(max / peak < 1e-4, "max rel. Abw. {}", max / peak);
    }

    #[test]
    fn chirp_echo_wird_zum_peak() {
        // Echolage: Replika um D Samples verzögert → Peak bei (D − o) mod N.
        let p = test_chirp();
        let n = 256;
        let comp = RangeCompressor::new(&p, n);
        let r = replica(&p);
        let (ntx, o) = (r.len(), embed_start(n, r.len()));
        let delay = 100;
        let mut row = vec![Complex32::zero(); n];
        for (i, c) in r.iter().enumerate() {
            row[delay + i] = *c;
        }
        comp.compress_row(&mut row);
        let peak = row
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.norm_sqr().total_cmp(&b.norm_sqr()))
            .unwrap()
            .0;
        assert_eq!(peak, (delay + n - o) % n, "ntx={ntx} o={o}");
        // Kohärenter Gewinn ≈ ntx (Spitze vs. Eingangsamplitude 1).
        let gain = row[peak].norm();
        assert!(
            (gain - ntx as f32).abs() / (ntx as f32) < 0.05,
            "gain = {gain}, ntx = {ntx}"
        );
        // Hauptzipfel: Theorie FWHM ≈ 0,89·fs/B = 4,45 Samples (Sinc).
        let half = row[peak].norm_sqr() / 2.0;
        let mut w = 0;
        for c in &row {
            if c.norm_sqr() >= half {
                w += 1;
            }
        }
        assert!((4..=6).contains(&w), "FWHM-Breite = {w}");
    }

    #[test]
    fn shifts() {
        let mut v = vec![0, 1, 2, 3, 4, 5];
        fftshift(&mut v);
        assert_eq!(v, vec![3, 4, 5, 0, 1, 2]);
        ifftshift(&mut v);
        assert_eq!(v, vec![0, 1, 2, 3, 4, 5]);
        assert_eq!(dft_freqs(4, 8.0), vec![-4.0, -2.0, 0.0, 2.0]);
    }
}
