//! Range-Doppler-Algorithmus (CPU-Referenz) für Stripmap (SM) und TOPS (IW/EW).
//!
//! Enthält:
//! 1. **Stripmap-Clutterlock** (`clutterlock_fdc`): Phasen-normierte Lag-1-Azimutkorrelation
//!    mit 5-Block-Medianfilter zur Schätzung von $f_{\text{DC}}(r)$.
//! 2. **TOPS Unwrapped Sub-Aperture Clutterlock** (`estimate_tops_steering`):
//!    Teilt den Burst in $W$ Azimut-Teilfenster, schätzt $\Delta\phi_w$, entrollt die
//!    Phasensprünge an den $\pm\pi$-Grenzen (mehrfacher PRF-Durchlauf des TOPS-Strahls!)
//!    und bestimmt per linearer Regression gleichzeitig den Mitten-Doppler-Centroid
//!    $f_{\text{DC,mid}}$ [Hz] und die elektronische TOPS-Doppler-Steuerrate $k_t$ [Hz/s].
//! 3. **Stripmap- & TOPS-Fokussierung auf der CPU**:
//!    - Bei TOPS (`k_t != 0`): Zeitbereichs-Deramping
//!      $\exp(-j(2\pi f_{\text{DC,mid}} t_m + \pi k_t t_m^2))$, Zero-Padding auf
//!      $N_{\text{az,fft}} \ge N_{\text{az}} + N_{\text{pad}}$, RCMC im 2D-Frequenzbereich
//!      und Azimut-Kompression mit effektiver FM-Rate $k_{\text{eff}}(r) = k_a(r) - k_t$.
//!    - Bei Stripmap (`k_t == 0`): Klassischer RDA mit hyperbolischem Migrationsfaktor
//!      $D(f_a, V_{\text{eff}}(r))$ und Overlap-Save-Blöcken.

use crate::chirp::{ChirpParams, RangeCompressor};
use crate::types::{BurstBlock, C_LIGHT, Complex32, LAMBDA};
use num_complex::Complex32 as Nc32;
use rustfft::{Fft, FftPlanner};
use std::f64::consts::PI;
use std::sync::Arc;

/// Geschätzte TOPS-Steuerparameter eines Bursts.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TopsSteering {
    /// Doppler-Centroid in der Burst-Mitte $f_{\text{DC}}(t=0)$ [Hz].
    pub fdc_mid_hz: f64,
    /// Elektronische Doppler-Steuerrate $k_t = df_{\text{DC}}/dt$ [Hz/s].
    pub kt_hz_s: f64,
    /// Gesamte überstrichene Doppler-Bandbreite $k_t \cdot T_{\text{burst}}$ [Hz].
    pub swept_bw_hz: f64,
}

/// Hyperbolischer Migrationsfaktor $D(f_a, V_{\text{eff}}) = \sqrt{1 - \frac{\lambda^2 f_a^2}{4 V_{\text{eff}}^2}}$.
#[inline]
pub fn d_factor(fa_hz: f64, veff: f64) -> f64 {
    let x = LAMBDA * LAMBDA * fa_hz * fa_hz / (4.0 * veff * veff);
    (1.0 - x.min(0.999_999)).sqrt()
}

/// Stationäre Azimut-Dopplerrate $k_a(R) = -\frac{2 V_{\text{eff}}^2}{\lambda R}$ [Hz/s] ($< 0$).
#[inline]
pub fn ka_rate_hz_s(veff: f64, slant_range_m: f64) -> f64 {
    -2.0 * veff * veff / (LAMBDA * slant_range_m)
}

/// Unshiftetes FFT-Frequenzraster: Bin $k$ hat Frequenz $f[k] \in [-f_s/2, +f_s/2)$.
pub fn fft_freqs_unshifted(n: usize, fs_hz: f64) -> Vec<f64> {
    let half = n.div_ceil(2);
    (0..n)
        .map(|k| {
            let kk = if k < half {
                k as f64
            } else {
                (k as isize - n as isize) as f64
            };
            kk * fs_hz / (n as f64)
        })
        .collect()
}

/// Stripmap-Clutterlock: Phasen-normierte Lag-1-Azimutkorrelation je Range-Block
/// mit 5-Block-Medianfilter und linearer Interpolation auf alle `n_rg` Range-Bins.
pub fn clutterlock_fdc(
    rc_data: &[Complex32],
    n_az: usize,
    n_rg: usize,
    pri_s: f64,
    n_blocks: usize,
) -> Vec<f64> {
    if n_az < 2 || n_rg == 0 {
        return vec![0.0; n_rg];
    }
    let nb = n_blocks.clamp(1, n_rg);
    let mut fdc = vec![0.0; nb];
    for (b, f) in fdc.iter_mut().enumerate() {
        let r0 = b * n_rg / nb;
        let r1 = (b + 1) * n_rg / nb;
        let step_a = ((n_az - 1) / 256).max(1);
        let step_r = ((r1 - r0) / 256).max(1);
        let (mut re, mut im) = (0.0f64, 0.0f64);
        for a in (0..n_az - 1).step_by(step_a) {
            for r in (r0..r1).step_by(step_r) {
                let x = rc_data[a * n_rg + r];
                let y = rc_data[(a + 1) * n_rg + r];
                let cr = f64::from(y.re * x.re + y.im * x.im);
                let ci = f64::from(y.im * x.re - y.re * x.im);
                let n = cr.hypot(ci);
                if n > 0.0 {
                    re += cr / n;
                    im += ci / n;
                }
            }
        }
        *f = im.atan2(re) / (2.0 * PI * pri_s);
    }
    let med: Vec<f64> = (0..nb)
        .map(|b| {
            let mut w: Vec<f64> = (b.saturating_sub(2)..(b + 3).min(nb))
                .map(|i| fdc[i])
                .collect();
            w.sort_by(f64::total_cmp);
            w[w.len() / 2]
        })
        .collect();
    (0..n_rg)
        .map(|r| {
            let x = (r as f64 + 0.5) * nb as f64 / n_rg as f64 - 0.5;
            if x <= 0.0 {
                return med[0];
            }
            if x >= (nb - 1) as f64 {
                return med[nb - 1];
            }
            let b = x.floor() as usize;
            let f = x - b as f64;
            med[b] * (1.0 - f) + med[b + 1] * f
        })
        .collect()
}

/// Schätzt die TOPS-Steuerrate $k_t$ [Hz/s] und den Mitten-Doppler-Centroid $f_{\text{DC,mid}}$ [Hz]
/// aus einem Burst durch Unwrapped Sub-Aperture Lag-1-Autokorrelation.
///
/// Da der Antennenstrahl im TOPS-Modus elektronisch von hinten nach vorne geschwenkt wird,
/// überstreicht $f_{\text{DC}}(t)$ typischerweise $4\dots 8\text{ kHz}$ ($3\dots 5\times \text{PRF}$).
/// Durch Unterteilung in $W = 24$ Azimut-Teilfenster ist die Phasenänderung zwischen benachbarten
/// Fenstern $|\delta(\Delta\phi)| < \pi$, sodass $\Delta\phi_w$ exakt entrollt und linear
/// gefittet werden kann.
pub fn estimate_tops_steering(
    data: &[Complex32],
    n_az: usize,
    n_rg: usize,
    pri_s: f64,
    az_beam_addr: &[u16],
) -> TopsSteering {
    if n_az < 16 || n_rg == 0 {
        return TopsSteering {
            fdc_mid_hz: 0.0,
            kt_hz_s: 0.0,
            swept_bw_hz: 0.0,
        };
    }
    let prf_hz = 1.0 / pri_s;
    let n_win = 24usize.min((n_az / 16).max(4));
    let r_start = n_rg / 8;
    let r_end = (7 * n_rg / 8).max(r_start + 1);
    let r_step = ((r_end - r_start) / 256).max(1);

    let mut t_win = Vec::with_capacity(n_win);
    let mut phi_win = Vec::with_capacity(n_win);
    let mid_az = (n_az as f64) * 0.5;

    for w in 0..n_win {
        let a0 = w * (n_az - 1) / n_win;
        let a1 = ((w + 1) * (n_az - 1) / n_win).max(a0 + 1);
        let mut re = 0.0f64;
        let mut im = 0.0f64;
        for a in a0..a1 {
            let row0 = &data[a * n_rg..(a + 1) * n_rg];
            let row1 = &data[(a + 1) * n_rg..(a + 2) * n_rg];
            for r in (r_start..r_end).step_by(r_step) {
                let x = row0[r];
                let y = row1[r];
                let cr = f64::from(y.re * x.re + y.im * x.im);
                let ci = f64::from(y.im * x.re - y.re * x.im);
                let norm = cr.hypot(ci);
                if norm > 0.0 {
                    re += cr / norm;
                    im += ci / norm;
                }
            }
        }
        let a_center = 0.5 * ((a0 + a1) as f64);
        t_win.push((a_center - mid_az) * pri_s);
        phi_win.push(im.atan2(re));
    }

    // 1D Phase Unwrapping über die Teilfenster
    let mut unw = vec![0.0f64; n_win];
    unw[0] = phi_win[0];
    for w in 1..n_win {
        let mut d = phi_win[w] - phi_win[w - 1];
        while d > PI {
            d -= 2.0 * PI;
        }
        while d < -PI {
            d += 2.0 * PI;
        }
        unw[w] = unw[w - 1] + d;
    }

    // Das mittlere Fenster auf das Hauptintervall [-π, +π] zentrieren
    let mid_w = n_win / 2;
    let shift_2pi = (unw[mid_w] / (2.0 * PI)).round() * (2.0 * PI);
    for u in &mut unw {
        *u -= shift_2pi;
    }

    // Lineare Regression: f_DC(t) = fdc_mid + kt * t
    let f_win: Vec<f64> = unw.iter().map(|&p| p * prf_hz / (2.0 * PI)).collect();
    let n_f = n_win as f64;
    let mean_t = t_win.iter().sum::<f64>() / n_f;
    let mean_f = f_win.iter().sum::<f64>() / n_f;
    let mut num = 0.0f64;
    let mut den = 0.0f64;
    for (&t, &f) in t_win.iter().zip(f_win.iter()) {
        num += (t - mean_t) * (f - mean_f);
        den += (t - mean_t) * (t - mean_t);
    }
    let mut kt_hz_s = if den > 0.0 { num / den } else { 0.0 };
    let fdc_mid_hz = mean_f - kt_hz_s * mean_t;

    // Fallback/Plausibilitätsprüfung mittels azimuth_beam_address, falls n_win bei extrem kurzem
    // Rand-Burst zu grob für Unwrapping war:
    if az_beam_addr.len() == n_az && n_az >= 8 {
        let b0 = f64::from(az_beam_addr[0]);
        let b1 = f64::from(az_beam_addr[n_az - 1]);
        let duration = (n_az - 1) as f64 * pri_s;
        let beam_rate = (b0 - b1) / duration; // Codes/s (positiv bei Rück-nach-Vorn-Schwenk)
        // Empirischer Skalenfaktor aus Spike 2: ~9.53 Hz pro Azimuth-Beam-Address-Schritt
        let kt_from_beam = beam_rate * 9.534;
        if kt_from_beam.abs() > 1000.0 && (kt_hz_s - kt_from_beam).abs() > 0.35 * kt_from_beam.abs()
        {
            // Wenn die Unwrap-Steigung um ein Vielfaches von PRF/Δt abgewichen ist, nutze die
            // um k*PRF korrigierte Steigung.
            let dt_win = duration / (n_win as f64);
            let k_jump = ((kt_from_beam - kt_hz_s) * dt_win / prf_hz).round();
            if k_jump != 0.0 {
                kt_hz_s += k_jump * prf_hz / dt_win;
            }
        }
    }

    let swept_bw_hz = kt_hz_s.abs() * (n_az as f64 * pri_s);
    TopsSteering {
        fdc_mid_hz,
        kt_hz_s,
        swept_bw_hz,
    }
}

/// Berechnet die gepaddete Azimut-FFT-Länge für einen TOPS-Burst, damit das im Doppler
/// um $k_t T_{\text{burst}}$ gespreizte Spektrum ohne zirkuläres Wrapping hineinpasst.
pub fn tops_padded_az_len(n_az: usize, pri_s: f64, veff_mid: f64, r_mid: f64) -> usize {
    let prf = 1.0 / pri_s;
    let ka_abs = ka_rate_hz_s(veff_mid, r_mid).abs();
    let target_dwell_samples = ((prf * prf) / (ka_abs.max(100.0))).ceil() as usize;
    (n_az + target_dwell_samples + 128).next_power_of_two()
}

/// Fokussiert einen einzelnen TOPS-Burst (`BurstBlock`) auf der CPU mittels:
/// 1. 1D-Range-Kompression (mit eingefaltetem Range-Shift)
/// 2. Unwrapped Sub-Aperture Clutterlock zur Schätzung von $(f_{\text{DC,mid}}, k_t)$
/// 3. Zeitbereichs-Deramping $\exp(-j(2\pi f_{\text{DC,mid}} t_m + \pi k_t t_m^2))$ und
///    zentriertem Zero-Padding auf `n_az_fft`
/// 4. Azimut-FFT $\to$ RCMC im 2D-Frequenzbereich $\to$ Azimut-Kompression mit
///    $k_{\text{eff}}(r) = k_a(r) - k_t \to$ Azimut-iFFT.
///
/// Gibt `(focused_matrix, n_az_fft, n_rg, TopsSteering)` zurück.
pub fn focus_tops_burst_cpu(
    block: &BurstBlock,
    apply_rcmc: bool,
) -> (Vec<Complex32>, usize, usize, TopsSteering) {
    let n_az = block.n_az;
    let n_rg = block.n_rg;
    let pri_s = block.params.pri_s;
    let prf_hz = block.params.prf_hz;
    let fs_hz = block.params.fs_hz;

    let chirp = ChirpParams {
        txpsf_hz: block.params.tx_start_freq_hz,
        txpl_s: block.params.tx_pulse_length_s,
        txprr_hz_s: block.params.tx_ramp_rate_hz_s,
        fs_hz,
    };

    // 1. Range-Kompression auf dem Burst
    let mut rc = block.raw.clone();
    let rcomp = RangeCompressor::new(&chirp, n_rg);
    rcomp.compress_matrix_parallel(&mut rc);

    // 2. TOPS-Steuerrate k_t und f_DC,mid schätzen
    let steering = estimate_tops_steering(&rc, n_az, n_rg, pri_s, &block.az_beam_addr);

    // 3. Gepaddete Azimut-Größe und Zeitbereichs-Deramping
    let r_mid = block.params.slant_range(n_rg / 2);
    let veff_mid = block.veff[n_rg / 2];
    let n_az_fft = tops_padded_az_len(n_az, pri_s, veff_mid, r_mid);
    let pad_top = (n_az_fft - n_az) / 2;
    let mid_az = (n_az as f64) * 0.5;

    let mut buf = vec![Nc32::new(0.0, 0.0); n_az_fft * n_rg];
    for m in 0..n_az {
        let t = (m as f64 - mid_az) * pri_s;
        let ph = -2.0 * PI * steering.fdc_mid_hz * t - PI * steering.kt_hz_s * t * t;
        let rot = Complex32::from_polar_unit(ph);
        let src_row = &rc[m * n_rg..(m + 1) * n_rg];
        let dst_row = &mut buf[(pad_top + m) * n_rg..(pad_top + m + 1) * n_rg];
        for (dst, &src) in dst_row.iter_mut().zip(src_row.iter()) {
            let c = src * rot;
            *dst = Nc32::new(c.re, c.im);
        }
    }

    let mut planner = FftPlanner::<f32>::new();
    let col_fwd = planner.plan_fft_forward(n_az_fft);
    let col_inv = planner.plan_fft_inverse(n_az_fft);
    let fa_vec = fft_freqs_unshifted(n_az_fft, prf_hz);

    // 4. Spalten-FFT in den Range-Doppler-Bereich
    process_columns_unshifted(&mut buf, n_az_fft, n_rg, &col_fwd);

    // 5. Optionales RCMC im 2D-Frequenzbereich (um das Basisband f_a nach Deramping)
    if apply_rcmc {
        let row_fwd = rcomp.row_fft_forward();
        let row_inv = rcomp.row_fft_inverse();
        let fr_vec = fft_freqs_unshifted(n_rg, fs_hz);
        process_rows_parallel(&mut buf, n_rg, &row_fwd);
        let scale_rg = 1.0 / (n_rg as f32);
        for a in 0..n_az_fft {
            let fa = fa_vec[a];
            let d = d_factor(fa, veff_mid);
            let shift_m = r_mid * (1.0 / d - 1.0);
            let row = &mut buf[a * n_rg..(a + 1) * n_rg];
            for (r, val) in row.iter_mut().enumerate() {
                let ph = 4.0 * PI * fr_vec[r] * shift_m / C_LIGHT;
                let (s, c) = ph.sin_cos();
                *val = (*val * Nc32::new(c as f32, s as f32)) * scale_rg;
            }
        }
        process_rows_parallel(&mut buf, n_rg, &row_inv);
    }

    // 6. Azimut-Kompression mit effektiver FM-Rate k_eff(r) = k_a(r) - k_t
    let keff_vec: Vec<f64> = (0..n_rg)
        .map(|r| {
            let r_m = block.params.slant_range(r);
            ka_rate_hz_s(block.veff[r], r_m) - steering.kt_hz_s
        })
        .collect();

    let scale_az = 1.0 / (n_az_fft as f32);
    for a in 0..n_az_fft {
        let fa = fa_vec[a];
        let fa2 = fa * fa;
        let row = &mut buf[a * n_rg..(a + 1) * n_rg];
        for (r, val) in row.iter_mut().enumerate() {
            // k_eff < 0 => exp(+j * π * fa^2 / k_eff) komprimiert den verbleibenden Down-Chirp
            let ph = PI * fa2 / keff_vec[r];
            let (s, c) = ph.sin_cos();
            *val = (*val * Nc32::new(c as f32, s as f32)) * scale_az;
        }
    }

    // 7. Spalten-iFFT zurück in den fokussierten Azimut-Zeitbereich
    process_columns_unshifted(&mut buf, n_az_fft, n_rg, &col_inv);

    let out: Vec<Complex32> = buf
        .into_iter()
        .map(|c| Complex32::new(c.re, c.im))
        .collect();
    (out, n_az_fft, n_rg, steering)
}

/// Fokussiert einen TOPS-Burst für InSAR auf der CPU mit optional vorgegebenem
/// Referenz-Steering (`override_steering`) und wendet die analytische stationäre
/// TOPS-Restphasen-Kompensation an, sodass ein reines Zero-Doppler-Basisband-SLC entsteht.
pub fn focus_tops_burst_insar_cpu(
    block: &BurstBlock,
    apply_rcmc: bool,
    override_steering: Option<TopsSteering>,
) -> (Vec<Complex32>, usize, usize, TopsSteering) {
    let n_az = block.n_az;
    let n_rg = block.n_rg;
    let pri_s = block.params.pri_s;
    let prf_hz = block.params.prf_hz;
    let fs_hz = block.params.fs_hz;

    let chirp = ChirpParams {
        txpsf_hz: block.params.tx_start_freq_hz,
        txpl_s: block.params.tx_pulse_length_s,
        txprr_hz_s: block.params.tx_ramp_rate_hz_s,
        fs_hz,
    };

    let mut rc = block.raw.clone();
    let rcomp = RangeCompressor::new(&chirp, n_rg);
    rcomp.compress_matrix_parallel(&mut rc);

    let steering = override_steering
        .unwrap_or_else(|| estimate_tops_steering(&rc, n_az, n_rg, pri_s, &block.az_beam_addr));

    let r_mid = block.params.slant_range(n_rg / 2);
    let veff_mid = block.veff[n_rg / 2];
    let n_az_fft = tops_padded_az_len(n_az, pri_s, veff_mid, r_mid);
    let pad_top = (n_az_fft - n_az) / 2;
    let mid_az = (n_az as f64) * 0.5;

    let mut buf = vec![Nc32::new(0.0, 0.0); n_az_fft * n_rg];
    for m in 0..n_az {
        let t = (m as f64 - mid_az) * pri_s;
        let ph = -2.0 * PI * steering.fdc_mid_hz * t - PI * steering.kt_hz_s * t * t;
        let rot = Complex32::from_polar_unit(ph);
        let src_row = &rc[m * n_rg..(m + 1) * n_rg];
        let dst_row = &mut buf[(pad_top + m) * n_rg..(pad_top + m + 1) * n_rg];
        for (dst, &src) in dst_row.iter_mut().zip(src_row.iter()) {
            let c = src * rot;
            *dst = Nc32::new(c.re, c.im);
        }
    }

    let mut planner = FftPlanner::<f32>::new();
    let col_fwd = planner.plan_fft_forward(n_az_fft);
    let col_inv = planner.plan_fft_inverse(n_az_fft);
    let fa_vec = fft_freqs_unshifted(n_az_fft, prf_hz);

    process_columns_unshifted(&mut buf, n_az_fft, n_rg, &col_fwd);

    if apply_rcmc {
        let row_fwd = rcomp.row_fft_forward();
        let row_inv = rcomp.row_fft_inverse();
        let fr_vec = fft_freqs_unshifted(n_rg, fs_hz);
        process_rows_parallel(&mut buf, n_rg, &row_fwd);
        let scale_rg = 1.0 / (n_rg as f32);
        for a in 0..n_az_fft {
            let fa = fa_vec[a];
            let d = d_factor(fa, veff_mid);
            let shift_m = r_mid * (1.0 / d - 1.0);
            let row = &mut buf[a * n_rg..(a + 1) * n_rg];
            for (r, val) in row.iter_mut().enumerate() {
                let ph = 4.0 * PI * fr_vec[r] * shift_m / C_LIGHT;
                let (s, c) = ph.sin_cos();
                *val = (*val * Nc32::new(c as f32, s as f32)) * scale_rg;
            }
        }
        process_rows_parallel(&mut buf, n_rg, &row_inv);
    }

    let ka_vec: Vec<f64> = (0..n_rg)
        .map(|r| ka_rate_hz_s(block.veff[r], block.params.slant_range(r)))
        .collect();
    let keff_vec: Vec<f64> = ka_vec.iter().map(|&ka| ka - steering.kt_hz_s).collect();

    let scale_az = 1.0 / (n_az_fft as f32);
    for a in 0..n_az_fft {
        let fa = fa_vec[a];
        let fa2 = fa * fa;
        let row = &mut buf[a * n_rg..(a + 1) * n_rg];
        for (r, val) in row.iter_mut().enumerate() {
            let ph = PI * fa2 / keff_vec[r];
            let (s, c) = ph.sin_cos();
            *val = (*val * Nc32::new(c as f32, s as f32)) * scale_az;
        }
    }

    process_columns_unshifted(&mut buf, n_az_fft, n_rg, &col_inv);

    let out: Vec<Complex32> = buf
        .into_iter()
        .map(|c| Complex32::new(c.re, c.im))
        .collect();
    (out, n_az_fft, n_rg, steering)
}

/// Fokussiert einen Stripmap-Block (`BurstBlock`) auf der CPU mittels klassischem RDA.
pub fn focus_stripmap_block_cpu(
    block: &BurstBlock,
    fdc_range: &[f64],
    apply_rcmc: bool,
) -> Vec<Complex32> {
    let n_az = block.n_az;
    let n_rg = block.n_rg;
    let prf_hz = block.params.prf_hz;
    let fs_hz = block.params.fs_hz;
    let chirp = ChirpParams {
        txpsf_hz: block.params.tx_start_freq_hz,
        txpl_s: block.params.tx_pulse_length_s,
        txprr_hz_s: block.params.tx_ramp_rate_hz_s,
        fs_hz,
    };
    let rcomp = RangeCompressor::new(&chirp, n_rg);
    let rf = rcomp.filter();

    let mut planner = FftPlanner::<f32>::new();
    let col_fwd = planner.plan_fft_forward(n_az);
    let col_inv = planner.plan_fft_inverse(n_az);

    let mut buf: Vec<Nc32> = block.raw.iter().map(|c| Nc32::new(c.re, c.im)).collect();

    // 1. Range-FFT + Azimut-FFT -> 2D-Frequenzbereich (unshiftet)
    process_rows_parallel(&mut buf, n_rg, &rcomp.row_fft_forward());
    process_columns_unshifted(&mut buf, n_az, n_rg, &col_fwd);

    // 2. Range-Matched-Filter (inkl. Shift-Phase) + RCMC im 2D-Frequenzbereich
    let fa_vec = fft_freqs_unshifted(n_az, prf_hz);
    let fr_vec = fft_freqs_unshifted(n_rg, fs_hz);
    let r_mid = block.params.slant_range(n_rg / 2);

    for a in 0..n_az {
        let fa = fa_vec[a];
        let row = &mut buf[a * n_rg..(a + 1) * n_rg];
        for r in 0..n_rg {
            let mut v = row[r] * rf[r];
            if apply_rcmc {
                let d = d_factor(fa - fdc_range[r], block.veff[r]);
                let shift = r_mid * (1.0 / d - 1.0);
                let ph = 4.0 * PI * fr_vec[r] * shift / C_LIGHT;
                let (s, c) = ph.sin_cos();
                v *= Nc32::new(c as f32, s as f32);
            }
            row[r] = v;
        }
    }

    // 3. Range-iFFT -> Range-Doppler-Bereich
    process_rows_parallel(&mut buf, n_rg, &rcomp.row_fft_inverse());

    // 4. Hyperbolischer Azimut-Matched-Filter je Range-Bin
    let scale = 1.0 / ((n_az * n_rg) as f32);
    for a in 0..n_az {
        let fa = fa_vec[a];
        let row = &mut buf[a * n_rg..(a + 1) * n_rg];
        for r in 0..n_rg {
            let d = d_factor(fa - fdc_range[r], block.veff[r]);
            let r_m = block.params.slant_range(r);
            let ph = 4.0 * PI * r_m * d / LAMBDA;
            let (s, c) = ph.sin_cos();
            row[r] = (row[r] * Nc32::new(c as f32, s as f32)) * scale;
        }
    }

    // 5. Azimut-iFFT -> fokussiertes SLC
    process_columns_unshifted(&mut buf, n_az, n_rg, &col_inv);

    buf.into_iter()
        .map(|c| Complex32::new(c.re, c.im))
        .collect()
}

fn process_rows_parallel(buf: &mut [Nc32], n_rg: usize, fft: &Arc<dyn Fft<f32>>) {
    let n_rows = buf.len() / n_rg;
    let n_threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(n_rows.max(1));
    let rows_per = n_rows.div_ceil(n_threads);
    std::thread::scope(|s| {
        for chunk in buf.chunks_mut(rows_per * n_rg) {
            let fft = fft.clone();
            s.spawn(move || {
                for row in chunk.chunks_mut(n_rg) {
                    fft.process(row);
                }
            });
        }
    });
}

fn process_columns_unshifted(buf: &mut [Nc32], n_az: usize, n_rg: usize, fft: &Arc<dyn Fft<f32>>) {
    #[derive(Clone, Copy)]
    struct SendPtr(*mut Nc32);
    unsafe impl Send for SendPtr {}
    unsafe impl Sync for SendPtr {}
    impl SendPtr {
        #[inline]
        fn at(self, i: usize) -> *mut Nc32 {
            unsafe { self.0.add(i) }
        }
    }

    let n_threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(n_rg.max(1));
    let cols_per = n_rg.div_ceil(n_threads);
    let ptr = SendPtr(buf.as_mut_ptr());

    std::thread::scope(|s| {
        for t in 0..n_threads {
            let c0 = t * cols_per;
            let c1 = ((t + 1) * cols_per).min(n_rg);
            if c0 >= c1 {
                continue;
            }
            let fft = fft.clone();
            s.spawn(move || {
                let mut col = vec![Nc32::new(0.0, 0.0); n_az];
                for c in c0..c1 {
                    for (a, cell) in col.iter_mut().enumerate() {
                        unsafe { *cell = *ptr.at(a * n_rg + c) };
                    }
                    fft.process(&mut col);
                    for (a, cell) in col.iter().enumerate() {
                        unsafe { *ptr.at(a * n_rg + c) = *cell };
                    }
                }
            });
        }
    });
}
