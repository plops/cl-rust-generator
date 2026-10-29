//! LFM-Sende-Chirp-Replika, Range-Matched-Filter (mit eingefaltetem Range-Shift)
//! und 1D-Range-Kompression auf der CPU.
//!
//! Wichtig: `TXPRR` und `TXPSF` im Sentinel-1-Header sind 16-Bit **Sign-Magnitude**
//! (`Bit 15 = 0` → positiv, `Bit 15 = 1` → negativ; oder in Sentinel-1-Konvention:
//! `polarity = (code >> 15) & 1`, wobei `polarity == 0` negativ und `polarity == 1` positiv ist!).
//! Moment: Prüfen wir die Vorzeichenkonvention aus `sar_focus/src/02_meta.rs` und `decoder/src/header.rs`!

use crate::types::{Complex32, F_REF};
use num_complex::Complex32 as Nc32;
use rustfft::{Fft, FftPlanner};
use std::sync::Arc;

/// Konvertiert den rohen 16-Bit `txprr_code` in die Chirp-Rampenrate $k_r$ [Hz/s].
///
/// Gemäß ESA S1-IF-ASD-PL-0007 und `sentinel1decoder`:
/// Bit 15 ist das Vorzeichenbit (`0` = positiv, `1` = negativ im Standard-SM, ABER
/// bei S6 (`0x84CE`) und IW1 (`0x8647`) ist Bit 15 = 1 und der reale Chirp ist ein Up-Chirp
/// mit positiver Steigung, wenn man die Replika bei $t \in [-T_p/2, +T_p/2]$ definiert!).
/// In `sar_focus/src/02_meta.rs`: `polarity = (code >> 15) & 1; sign = if polarity == 0 { -1.0 } else { 1.0 }`.
#[inline]
pub fn decode_txprr_hz_s(txprr_code: u16) -> f64 {
    let pol = (txprr_code >> 15) & 1;
    let mag = f64::from(txprr_code & 0x7FFF);
    let sign = if pol == 0 { -1.0 } else { 1.0 };
    sign * mag * F_REF * F_REF / 2_097_152.0
}

/// Konvertiert den rohen 16-Bit `txpsf_code` und `k_r` in die Chirp-Startfrequenz $f_{\text{start}}$ [Hz].
#[inline]
pub fn decode_txpsf_hz(txpsf_code: u16, txprr_hz_s: f64) -> f64 {
    let pol = (txpsf_code >> 15) & 1;
    let mag = f64::from(txpsf_code & 0x7FFF);
    let sign = if pol == 0 { -1.0 } else { 1.0 };
    txprr_hz_s / (4.0 * F_REF) + sign * mag * F_REF / 16_384.0
}

/// Range-Abtastrate $f_s$ [Hz] als rationale Funktion von `rgdec` und $f_{\text{ref}}$.
pub fn decode_rgdec_fs_hz(rgdec: u8) -> Result<f64, crate::types::Error> {
    let (l, m) = match rgdec {
        0 => (3u32, 4u32),
        1 => (2, 3),
        3 => (5, 9),
        4 => (4, 9),
        5 => (3, 8),
        6 => (1, 3),
        7 => (1, 6),
        8 => (3, 7),
        9 => (5, 16),
        10 => (3, 26),
        11 => (4, 11),
        other => {
            return Err(crate::types::Error(format!(
                "Unbekannter RGDEC-Code {other}"
            )));
        }
    };
    Ok(f64::from(l) / f64::from(m) * 4.0 * F_REF)
}

/// Unterdrückte Vorlaufzeit in s: `320 / (8 * F_REF)`.
#[inline]
pub fn suppressed_data_time_s() -> f64 {
    320.0 / (8.0 * F_REF)
}

/// Chirp-Parameter in SI-Einheiten.
#[derive(Clone, Copy, Debug)]
pub struct ChirpParams {
    pub txpsf_hz: f64,
    pub txpl_s: f64,
    pub txprr_hz_s: f64,
    pub fs_hz: f64,
}

impl ChirpParams {
    #[inline]
    pub fn num_samples(&self) -> usize {
        (self.txpl_s * self.fs_hz) as usize
    }

    #[inline]
    pub fn bandwidth_hz(&self) -> f64 {
        self.txprr_hz_s.abs() * self.txpl_s
    }
}

/// Einbett-Startindex der Replika der Länge `ntx` in eine Zeile der Länge `n_rg`.
#[inline]
pub fn embed_start(n_rg: usize, ntx: usize) -> usize {
    ((n_rg.saturating_sub(ntx) as f64 / 2.0).ceil() as usize).saturating_sub(1)
}

/// Ganzzahliger Range-Rückversatz nach der Korrelation (Samples).
#[inline]
pub fn correlation_shift_samples(ntx: usize, n_rg: usize) -> usize {
    (embed_start(n_rg, ntx) + (ntx.saturating_sub(1)) / 2) % n_rg
}

/// Erzeugt die ideale Sende-Chirp-Replika auf dem ADC-Zeitraster.
pub fn build_chirp_replica(p: &ChirpParams) -> Vec<Complex32> {
    let n = p.num_samples();
    let phi1 = p.txpsf_hz + p.txprr_hz_s * p.txpl_s / 2.0;
    let phi2 = p.txprr_hz_s / 2.0;
    (0..n)
        .map(|i| {
            let t = (i as f64 - (n.saturating_sub(1)) as f64 / 2.0) / p.fs_hz;
            let phase = 2.0 * std::f64::consts::PI * (phi1 * t + phi2 * t * t);
            Complex32::from_polar_unit(phase)
        })
        .collect()
}

/// Erzeugt den 1D-Range-Matched-Filter der Länge `n_rg` im unshifteten FFT-Frequenzbereich.
///
/// Wenn `fold_shift == true`, wird die Kreisverschiebung um `correlation_shift_samples(ntx, n_rg)`
/// direkt als linearer Phasenterm $\exp(-j 2\pi k \cdot \text{shift} / N_{\text{rg}})$ in den
/// Filter eingefaltet, sodass nach der Range-iFFT kein separater `rotate_rows`-Pass nötig ist!
pub fn build_range_matched_filter(
    p: &ChirpParams,
    n_rg: usize,
    fold_shift: bool,
) -> Vec<Complex32> {
    let rep = build_chirp_replica(p);
    let ntx = rep.len().min(n_rg);
    let start = embed_start(n_rg, ntx).min(n_rg - ntx);
    let mut h = vec![Nc32::new(0.0, 0.0); n_rg];
    for (i, c) in rep[..ntx].iter().enumerate() {
        h[start + i] = Nc32::new(c.re, c.im);
    }
    let mut planner = FftPlanner::<f32>::new();
    let fwd = planner.plan_fft_forward(n_rg);
    fwd.process(&mut h);

    let shift = if fold_shift {
        correlation_shift_samples(ntx, n_rg)
    } else {
        0
    };
    h.iter()
        .enumerate()
        .map(|(k, c)| {
            let mut val = Complex32::new(c.re, -c.im); // conj(FFT(replica))
            if shift != 0 {
                // Rechtsrotation im Zeitraum um `shift` Samples entspricht im Frequenzraum
                // der Multiplikation mit exp(-j * 2π * k * shift / n_rg).
                let ph = -2.0 * std::f64::consts::PI * (k as f64) * (shift as f64) / (n_rg as f64);
                val = val * Complex32::from_polar_unit(ph);
            }
            val
        })
        .collect()
}

/// CPU-1D-Range-Kompressor (für Clutterlock-Vorstufe und Tests).
pub struct RangeCompressor {
    fwd: Arc<dyn Fft<f32>>,
    inv: Arc<dyn Fft<f32>>,
    filter: Vec<Nc32>,
    n_rg: usize,
}

impl RangeCompressor {
    /// Baut einen Range-Kompressor der Länge `n_rg` mit bereits eingefaltetem Range-Shift.
    pub fn new(p: &ChirpParams, n_rg: usize) -> Self {
        let mut planner = FftPlanner::<f32>::new();
        let fwd = planner.plan_fft_forward(n_rg);
        let inv = planner.plan_fft_inverse(n_rg);
        let filter: Vec<Nc32> = build_range_matched_filter(p, n_rg, true)
            .into_iter()
            .map(|c| Nc32::new(c.re, c.im))
            .collect();
        Self {
            fwd,
            inv,
            filter,
            n_rg,
        }
    }

    pub fn n_rg(&self) -> usize {
        self.n_rg
    }

    pub fn filter(&self) -> &[Nc32] {
        &self.filter
    }

    pub fn row_fft_forward(&self) -> Arc<dyn Fft<f32>> {
        self.fwd.clone()
    }

    pub fn row_fft_inverse(&self) -> Arc<dyn Fft<f32>> {
        self.inv.clone()
    }

    /// Komprimiert eine einzelne Range-Zeile in-place (inkl. Shift-Ausrichtung).
    pub fn compress_row(&self, row: &mut [Complex32]) {
        debug_assert_eq!(row.len(), self.n_rg);
        let mut buf: Vec<Nc32> = row.iter().map(|c| Nc32::new(c.re, c.im)).collect();
        self.fwd.process(&mut buf);
        for (b, &f) in buf.iter_mut().zip(self.filter.iter()) {
            *b *= f;
        }
        self.inv.process(&mut buf);
        let scale = 1.0 / (self.n_rg as f32);
        for (dst, src) in row.iter_mut().zip(buf.iter()) {
            *dst = Complex32::new(src.re * scale, src.im * scale);
        }
    }

    /// Komprimiert alle Zeilen eines 2D-Puffers (`[n_az × n_rg]`) parallel mit `std::thread::scope`.
    pub fn compress_matrix_parallel(&self, data: &mut [Complex32]) {
        let n_rg = self.n_rg;
        if data.is_empty() || n_rg == 0 {
            return;
        }
        let n_rows = data.len() / n_rg;
        let n_threads = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .min(n_rows.max(1));
        let rows_per_thread = n_rows.div_ceil(n_threads);
        std::thread::scope(|s| {
            for chunk in data.chunks_mut(rows_per_thread * n_rg) {
                s.spawn(move || {
                    let mut buf = vec![Nc32::new(0.0, 0.0); n_rg];
                    let scale = 1.0 / (n_rg as f32);
                    for row in chunk.chunks_mut(n_rg) {
                        for (b, c) in buf.iter_mut().zip(row.iter()) {
                            *b = Nc32::new(c.re, c.im);
                        }
                        self.fwd.process(&mut buf);
                        for (b, &f) in buf.iter_mut().zip(self.filter.iter()) {
                            *b *= f;
                        }
                        self.inv.process(&mut buf);
                        for (c, b) in row.iter_mut().zip(buf.iter()) {
                            *c = Complex32::new(b.re * scale, b.im * scale);
                        }
                    }
                });
            }
        });
    }
}
