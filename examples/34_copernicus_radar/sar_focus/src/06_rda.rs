//! Range-Doppler-Azmut (CPU-Referenz, gestuft nach SSFocus `focus_old.py`).
//!
//! Fluss: Range-FFT → Azimut-FFT → ×Range-Filter → ×RCMC-Phasenfilter →
//! Range-iFFT (Range-Doppler) → ×Azimut-Filter je Range-Linie → Azimut-iFFT.
//! Alle Filter sind elementweise Multiplikationen (keine Interpolation).
//!
//! Lehre aus dem Punktzieltest: Der Azimut-Filter gehört in den
//! Range-Doppler-Bereich (je Range-**Linie** mit deren Entfernung) —
//! `focus.py` multipliziert ihn fälschlich im 2D-Frequenzbereich, wo die
//! Range-Achse Frequenz (nicht Entfernung) ist. Dort variiert `R` über die
//! Bins und würfelt die Phase (Testpeak landete auf (512, 1229) statt
//! (512, 2048)); `focus_old.py` macht es gestuft richtig.
//!
//! Bewusste Abweichungen von SSFocus: exaktes DFT-Raster statt `linspace`,
//! exakter Range-Rückversatz statt grobem `ifftshift` (dort 1 Sample daneben),
//! Bulk-`v_eff` der Chunk-Mitte statt voller Matrix.

use crate::chirp::ChirpParams;
use crate::ephem::effective_velocity;
use crate::range::{RangeCompressor, dft_freqs, fftshift, ifftshift};
use crate::types::{Complex32, SPEED_OF_LIGHT, TX_WAVELENGTH_M, Vec3d};
use num_complex::Complex32 as Nc32;
use rustfft::{Fft, FftPlanner};
use std::sync::Arc;

/// RDA-Parameter für einen Azimut-Chunk.
pub struct RdaParams<'a> {
    pub chirp: ChirpParams,
    pub naz: usize,
    pub nrange: usize,
    pub pri_s: f64,
    /// Schrägentfernung je Range-Sample in m (`R[j]`).
    pub slant_m: &'a [f64],
    /// Effektive Geschwindigkeit je Range-Bin in m/s (Chunk-Mitte).
    pub veff_range: &'a [f64],
    /// Doppler-Centroid in Hz (Filter wird um ihn zentriert).
    pub f_dc_hz: f64,
    /// RCMC-Phasenfilter anwenden (false nur für Mit/Ohne-Vergleiche).
    pub apply_rcmc: bool,
}

/// Bulk-`v_eff` je Range-Bin aus dem Orbit der Chunk-Mittellinie.
pub fn veff_mid_range(line_pos: &[Vec3d], line_vel: &[Vec3d], slant_m: &[f64]) -> Vec<f64> {
    let mid = line_pos.len() / 2;
    slant_m
        .iter()
        .map(|&r| effective_velocity(line_vel[mid].norm(), line_pos[mid], r))
        .collect()
}

/// D-Faktor (Migrationsfaktor): `√(1 − λ²f_a²/4v²)`.
pub fn d_factor(fa_hz: f64, veff: f64) -> f64 {
    let x = TX_WAVELENGTH_M * TX_WAVELENGTH_M * fa_hz * fa_hz / (4.0 * veff * veff);
    (1.0 - x.min(1.0)).sqrt()
}

/// Azimut-Frequenzraster in Hz (Shift-Konvention, um `f_dc` zentriert).
pub fn az_freqs(naz: usize, pri_s: f64, f_dc_hz: f64) -> Vec<f64> {
    dft_freqs(naz, 1.0 / pri_s)
        .iter()
        .map(|f| f - f_dc_hz)
        .collect()
}

/// Range-Frequenzraster in Hz (**un**shiftet, FFT-Ausgabeordnung).
pub fn range_freqs_unshifted(nrange: usize, fs_hz: f64) -> Vec<f64> {
    (0..nrange)
        .map(|k| {
            let kk = if k < nrange.div_ceil(2) {
                k as f64
            } else {
                k as f64 - nrange as f64
            };
            kk * fs_hz / nrange as f64
        })
        .collect()
}

/// RCMC-Phasenfilter (bulk, `R₀` = Schwadmitte):
/// `exp(4jπ·f_r·R₀(1/D−1)/c)`, zeilenmajor (Azimut, Range).
pub fn rcmc_filter(
    naz: usize,
    nrange: usize,
    fa: &[f64],
    fr: &[f64],
    r0: f64,
    veff_range: &[f64],
) -> Vec<Nc32> {
    let mut h = Vec::with_capacity(naz * nrange);
    for &f in fa {
        for r in 0..nrange {
            let d = d_factor(f, veff_range[r]);
            let shift = r0 * (1.0 / d - 1.0);
            let ph = 4.0 * std::f64::consts::PI * fr[r] * shift / SPEED_OF_LIGHT;
            h.push(Nc32::new(ph.cos() as f32, ph.sin() as f32));
        }
    }
    h
}

/// Azimut-Matched-Filter je Range-Linie: `exp(4jπ·R[r]·D/λ)`,
/// zeilenmajor (Azimut, Range), gilt im Range-Doppler-Bereich.
pub fn azimuth_filter(
    naz: usize,
    nrange: usize,
    fa: &[f64],
    slant_m: &[f64],
    veff_range: &[f64],
) -> Vec<Nc32> {
    let mut h = Vec::with_capacity(naz * nrange);
    for &f in fa {
        for r in 0..nrange {
            let d = d_factor(f, veff_range[r]);
            let ph = 4.0 * std::f64::consts::PI * slant_m[r] * d / TX_WAVELENGTH_M;
            h.push(Nc32::new(ph.cos() as f32, ph.sin() as f32));
        }
    }
    h
}

/// Ganzzahliger Range-Rückversatz nach der iFFT (Samples).
///
/// Die Korrelation legt den Peak eines Ziels mit Verzögerung `D` auf
/// `(D − o)`, `o` = Einbett-Start; die Chirp-Mitte liegt weitere `(ntx−1)/2`
/// dahinter. Der Versatz stellt Pixel `j` ↔ `R[j]` her.
pub fn correlation_shift_samples(ntx: usize, nrange: usize) -> usize {
    (crate::chirp::embed_start(nrange, ntx) + (ntx - 1) / 2) % nrange
}

/// RDA-Prozessor: Filter + geplante FFTs werden wiederverwendet.
pub struct RdaProcessor {
    naz: usize,
    nrange: usize,
    range_comp: RangeCompressor,
    col_fwd: Arc<dyn Fft<f32>>,
    col_inv: Arc<dyn Fft<f32>>,
    rcmc: Vec<Nc32>,
    az: Vec<Nc32>,
    shift: usize,
    apply_rcmc: bool,
}

impl RdaProcessor {
    pub fn new(p: &RdaParams) -> Self {
        let mut planner = FftPlanner::<f32>::new();
        let col_fwd = planner.plan_fft_forward(p.naz);
        let col_inv = planner.plan_fft_inverse(p.naz);
        let fa = az_freqs(p.naz, p.pri_s, p.f_dc_hz);
        let fr = range_freqs_unshifted(p.nrange, p.chirp.fs_hz);
        let rcmc = rcmc_filter(
            p.naz,
            p.nrange,
            &fa,
            &fr,
            p.slant_m[p.nrange / 2],
            p.veff_range,
        );
        let az = azimuth_filter(p.naz, p.nrange, &fa, p.slant_m, p.veff_range);
        let ntx = crate::chirp::num_tx_samples(&p.chirp);
        Self {
            naz: p.naz,
            nrange: p.nrange,
            range_comp: RangeCompressor::new(&p.chirp, p.nrange),
            col_fwd,
            col_inv,
            rcmc,
            az,
            shift: correlation_shift_samples(ntx, p.nrange),
            apply_rcmc: p.apply_rcmc,
        }
    }

    /// Fokussiert einen Chunk in-place (`naz × nrange`, zeilenmajor).
    pub fn focus(&self, data: &mut [Complex32]) {
        debug_assert_eq!(data.len(), self.naz * self.nrange);
        let (naz, nr) = (self.naz, self.nrange);
        let mut buf: Vec<Nc32> = data.iter().map(|c| Nc32::new(c.re, c.im)).collect();
        // 1. Range-FFTs (zeilenweise, unshiftet).
        self.process_rows(&mut buf, true);
        // 2. Azimut-FFTs (spaltenweise) + Shift → 2D-Frequenzbereich.
        self.process_columns(&mut buf, &self.col_fwd, true);
        // 3. Range- und RCMC-Filter multiplizieren.
        let rf = self.range_comp.filter();
        let apply_rcmc = self.apply_rcmc;
        for a in 0..naz {
            let base = a * nr;
            let brow = &mut buf[base..base + nr];
            let crow = &self.rcmc[base..base + nr];
            for ((v, &f), &c) in brow.iter_mut().zip(rf.iter()).zip(crow.iter()) {
                *v *= f;
                if apply_rcmc {
                    *v *= c;
                }
            }
        }
        // 4. Range-iFFTs → Range-Doppler-Bereich, danach sofort der
        // Range-Rückversatz: Der Azimut-Filter braucht die Energie auf der
        // WAHREN Range-Linie (sonst 4πΔR·D/λ-Phasenfehler mit fa-abhängigem
        // D → Defokus; SSFocus-`focus_old.py` macht hier `ifftshift`).
        self.process_rows(&mut buf, false);
        for a in 0..naz {
            buf[a * nr..(a + 1) * nr].rotate_right(self.shift);
        }
        // 5. Azimut-Filter je Range-Linie, dann Azimut-iFFTs.
        for a in 0..naz {
            for r in 0..nr {
                buf[a * nr + r] *= self.az[a * nr + r];
            }
        }
        self.process_columns(&mut buf, &self.col_inv, false);
        // 6. Normierung (Bild liegt bereits auf wahren Linien).
        let s = 1.0 / (naz * nr) as f32;
        for (r, b) in data.iter_mut().zip(buf.iter()) {
            *r = Complex32::new(b.re * s, b.im * s);
        }
    }

    /// Zeilen-FFTs (`forward` = FFT, sonst iFFT), mehrsträngig.
    fn process_rows(&self, buf: &mut [Nc32], forward: bool) {
        let fft = if forward {
            self.range_comp.row_fft_forward()
        } else {
            self.range_comp.row_fft_inverse()
        };
        std::thread::scope(|s| {
            for row in buf.chunks_mut(self.nrange) {
                let fft = fft.clone();
                s.spawn(move || fft.process(row));
            }
        });
    }

    /// Spalten-FFT: `forward` = FFT+Shift, sonst Unshift+iFFT.
    fn process_columns(&self, buf: &mut [Nc32], fft: &Arc<dyn Fft<f32>>, forward: bool) {
        let (naz, nr) = (self.naz, self.nrange);
        std::thread::scope(|s| {
            let nthreads = std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4)
                .min(nr);
            let cols_per = nr.div_ceil(nthreads);
            // Rohen Zeiger teilen: Spalten sind disjunkt (jede Spalte genau
            // ein Strang) — SAFETY: keine zwei Stränge berühren je dieselbe.
            let ptr = SendPtr(buf.as_mut_ptr());
            for t in 0..nthreads {
                let fft = fft.clone();
                s.spawn(move || {
                    let mut col = vec![Nc32::new(0.0, 0.0); naz];
                    for c in (t * cols_per..((t + 1) * cols_per).min(nr)).rev() {
                        for (a, cell) in col.iter_mut().enumerate() {
                            unsafe { *cell = *ptr.at(a * nr + c) };
                        }
                        if forward {
                            fft.process(&mut col);
                            fftshift(&mut col);
                        } else {
                            ifftshift(&mut col);
                            fft.process(&mut col);
                        }
                        for (a, cell) in col.iter().enumerate() {
                            unsafe { *ptr.at(a * nr + c) = *cell };
                        }
                    }
                });
            }
        });
    }
}

/// Send-fähiger Rohzeiger für disjunkte Spaltenarbeit (SAFETY: der Aufrufer
/// garantiert, dass keine zwei Stränge je dieselbe Spalte berühren).
#[derive(Clone, Copy)]
struct SendPtr(*mut Nc32);
// SAFETY: nur innerhalb `process_columns` mit disjunkten Spalten benutzt.
unsafe impl Send for SendPtr {}

impl SendPtr {
    /// Elementzeiger (Methodenaufruf erfasst das Struct als Ganzes —
    /// Feldzugriff würde präzise das `!Send`-Feld erfassen).
    fn at(self, i: usize) -> *mut Nc32 {
        unsafe { self.0.add(i) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn d_faktor_werte() {
        // Handorakel: λ = 0,0555 m, fa = 800 Hz, v = 7100 m/s →
        // λ²fa²/4v² = 9,776·10⁻⁶ → D = 0,99999511.
        let d = d_factor(800.0, 7100.0);
        assert!((d - 0.999_995_11).abs() < 1e-8, "D = {d}");
        assert_eq!(d_factor(0.0, 7100.0), 1.0);
        assert_eq!(d_factor(-800.0, 7100.0), d); // gerade Funktion
        assert!(d_factor(831.0, 7100.0) < d); // fällt mit |fa|
    }

    #[test]
    fn rcmc_und_azimut_spotwerte() {
        // fa = 0 → D = 1 → RCMC-Shift 0 → Filter 1; Azimut = exp(4jπR/λ).
        let naz = 4;
        let nr = 8;
        let fa = vec![-300.0, -100.0, 0.0, 100.0];
        let fr = range_freqs_unshifted(nr, 8.0);
        assert_eq!(fr[0], 0.0);
        assert_eq!(fr[nr / 2], -4.0);
        let slant: Vec<f64> = (0..nr).map(|r| 900_000.0 + r as f64 * 3.0).collect();
        let veff = vec![7100.0; nr];
        let rcmc = rcmc_filter(naz, nr, &fa, &fr, slant[nr / 2], &veff);
        for r in 0..nr {
            let c = rcmc[2 * nr + r]; // fa = 0-Zeile
            assert!((c.norm() - 1.0).abs() < 1e-6);
            assert!(c.re > 0.999999 && c.im.abs() < 1e-6, "c = {c}");
        }
        let az = azimuth_filter(naz, nr, &fa, &slant, &veff);
        // fa = 0, R = 900006 m: Phase 4πR/λ mod 2π.
        let ph = 4.0 * std::f64::consts::PI * slant[2] / TX_WAVELENGTH_M;
        let c = az[2 * nr + 2];
        assert!((c.re as f64 - ph.cos()).abs() < 1e-5);
        assert!((c.im as f64 - ph.sin()).abs() < 1e-5);
    }
}
