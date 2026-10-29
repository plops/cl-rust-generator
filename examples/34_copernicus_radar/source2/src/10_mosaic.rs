//! Debursting (Burst-Stitching mit linearer Überblendung), Multi-Swath-Mosaikierung
//! (IW1–IW3, EW1–EW5), Multilooking sowie Export als `.cf`, `.json` und `.png`.

use crate::rda::ka_rate_hz_s;
use crate::types::{BurstBlock, Complex32, Error};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

/// Fokussierter TOPS-Burst im gepaddeten Azimut-Zeitgitter (`n_az_fft × n_rg`).
#[derive(Clone, Debug)]
pub struct FocusedBurst {
    pub burst_index: usize,
    pub n_az_raw: usize,
    pub n_az_fft: usize,
    pub n_rg: usize,
    /// Anzahl der gültigen Range-Samples ohne den Far-Range-Chirp-Roll-Off (`n_rg - n_chirp`).
    pub n_valid_rg: usize,
    pub mid_time_s: f64,
    pub pri_s: f64,
    pub r0_m: f64,
    pub dr_m: f64,
    pub veff_mid: f64,
    pub kt_hz_s: f64,
    pub fdc_mid_hz: f64,
    pub data: Vec<Complex32>,
}

/// Ergebnis eines zusammengefügten Sub-Swaths (oder Stripmap-Bildes).
pub struct DeburstedSwath {
    pub swath_number: u8,
    pub swath_name: String,
    pub n_az: usize,
    pub n_rg: usize,
    pub t_start_s: f64,
    pub dt_az_s: f64,
    pub r0_m: f64,
    pub dr_m: f64,
    /// Intensitätsbild ($|I|^2$) auf dem einheitlichen Sub-Swath-Gitter (`n_az × n_rg`).
    pub power: Vec<f32>,
}

/// Fügt die fokussierten Bursts eines Sub-Swaths (oder eines einzelnen Stripmap-Blocks)
/// zeit- und maßstabsgetreu entlang der Azimut-Achse zusammen (Debursting mit Feathering).
///
/// Mathematischer Hintergrund für TOPS (`kt_hz_s != 0`):
/// Durch das Zeitbereichs-Deramping mit $k_t$ und die anschließende Azimut-Kompression mit
/// $k_{\text{eff}} = k_a - k_t$ wird ein Ziel bei wahrer Null-Doppler-Zeit $t_0$ relativ zur
/// Burst-Mitte $t_{\text{mid},b}$ auf die fokussierte Zeile
/// $$m_{\text{foc}}(t_0) = \frac{N_{\text{az,fft}}}{2} + \frac{\alpha}{\text{PRI}} (t_0 - t_{\text{mid},b})$$
/// abgebildet, wobei $\alpha = \frac{|k_a|}{|k_a - k_t|} \in (0, 1)$ der TOPS-Stauchungsfaktor ist!
/// Ein Ziel im beleuchteten Strahlbereich $t_{\text{beam}} \in [-T_{\text{burst}}/2, +T_{\text{burst}}/2]$
/// hat $t_0 - t_{\text{mid},b} = t_{\text{beam}} / \alpha$, liegt also im fokussierten Bild exakt bei
/// den Zeilen $\frac{N_{\text{az,fft}}}{2} \pm \frac{N_{\text{az,raw}}}{2}$!
/// Zwei aufeinanderfolgende Bursts mit Zeitabstand $\Delta t_{\text{cycle}} = t_{\text{mid},b+1} - t_{\text{mid},b}$
/// sind im fokussierten Zeilenraster um
/// $$\Delta m_{\text{stride}} = \frac{\Delta t_{\text{cycle}}}{\text{PRI}} \cdot \alpha$$
/// Zeilen gegeneinander verschoben.
pub fn deburst_swath(
    swath_number: u8,
    swath_name: &str,
    focused_bursts: &[FocusedBurst],
) -> Result<DeburstedSwath, Error> {
    if focused_bursts.is_empty() {
        return Err(Error("Keine fokussierten Bursts zum Debursting".into()));
    }
    let b0 = &focused_bursts[0];
    let n_rg_full = b0.n_rg;
    let n_rg = b0.n_valid_rg.clamp(1, n_rg_full);
    let r0_m = b0.r0_m;
    let dr_m = b0.dr_m;

    // Stripmap-Sonderfall: 1 Block ohne TOPS-Deramping
    if focused_bursts.len() == 1 && b0.kt_hz_s == 0.0 {
        let n_az = b0.n_az_fft;
        let mut power = Vec::with_capacity(n_az * n_rg);
        for a in 0..n_az {
            let row = &b0.data[a * n_rg_full..a * n_rg_full + n_rg];
            for c in row {
                power.push(c.norm_sqr());
            }
        }
        let t_start_s = b0.mid_time_s - 0.5 * (n_az as f64) * b0.pri_s;
        return Ok(DeburstedSwath {
            swath_number,
            swath_name: swath_name.to_string(),
            n_az,
            n_rg,
            t_start_s,
            dt_az_s: b0.pri_s,
            r0_m,
            dr_m,
            power,
        });
    }

    // Mittlerer Stauchungsfaktor α = |k_a| / |k_a - k_t| in der Schwadmitte
    let r_mid = r0_m + 0.5 * (n_rg as f64) * dr_m;
    let ka_mid = ka_rate_hz_s(b0.veff_mid, r_mid);
    let kt_mean =
        focused_bursts.iter().map(|b| b.kt_hz_s).sum::<f64>() / (focused_bursts.len() as f64);
    let alpha = (ka_mid / (ka_mid - kt_mean)).abs().clamp(0.05, 1.0);
    // Effektives Azimut-Zeitraster einer fokussierten Zeile in wahrer Null-Doppler-Zeit:
    // dt_az_s = PRI / alpha
    let dt_az_s = b0.pri_s / alpha;

    // Exakter Zeilenabstand zwischen aufeinanderfolgenden Bursts:
    // Zwischen Burst b und Burst b+1 liegt die Nahtstelle exakt bei ±0.5 * stride_rows.
    // Wir wählen ein kleines Überblendungsfenster (±overlap_half Zeilen um die Nahtstelle),
    // sodass der lineare Feathering-Übergang von 1 -> 0 für Burst b und 0 -> 1 für Burst b+1
    // sich in jeder Überlappungszeile zu 1.0 addiert und niemals die äußeren Antennen-Nebenkeulen
    // in die Nachbar-Hauptkeule einmischt!
    let nominal_stride = if focused_bursts.len() >= 2 {
        let mut diffs: Vec<f64> = focused_bursts
            .windows(2)
            .map(|w| ((w[1].mid_time_s - w[0].mid_time_s) / dt_az_s).abs())
            .filter(|&d| d > 10.0)
            .collect();
        if diffs.is_empty() {
            (b0.n_az_raw as f64) * 0.90
        } else {
            diffs.sort_by(f64::total_cmp);
            diffs[diffs.len() / 2]
        }
    } else {
        (b0.n_az_raw as f64) * 0.90
    };
    let overlap_half = (nominal_stride * 0.035).round().clamp(8.0, 36.0) as isize;
    let default_half = ((0.5 * nominal_stride).round() as isize + overlap_half)
        .clamp(16, (b0.n_az_fft / 2) as isize);

    let first_half = default_half as usize;
    let t_start_s = b0.mid_time_s - (first_half as f64) * dt_az_s;
    let b_last = focused_bursts.last().unwrap();
    let last_half = default_half as usize;
    let t_end_s = b_last.mid_time_s + (last_half as f64) * dt_az_s;
    let n_az_out = (((t_end_s - t_start_s) / dt_az_s).ceil() as usize).max(1);

    let mut acc_power = vec![0.0f32; n_az_out * n_rg];
    let mut acc_weight = vec![0.0f32; n_az_out];

    for (bi, b) in focused_bursts.iter().enumerate() {
        let half_prev = if bi > 0 {
            let s_prev = ((b.mid_time_s - focused_bursts[bi - 1].mid_time_s) / dt_az_s).abs();
            if s_prev > 10.0 && s_prev < 2.0 * nominal_stride {
                (0.5 * s_prev).round() as isize + overlap_half
            } else {
                default_half
            }
        } else {
            default_half
        };
        let half_next = if bi + 1 < focused_bursts.len() {
            let s_next = ((focused_bursts[bi + 1].mid_time_s - b.mid_time_s) / dt_az_s).abs();
            if s_next > 10.0 && s_next < 2.0 * nominal_stride {
                (0.5 * s_next).round() as isize + overlap_half
            } else {
                default_half
            }
        } else {
            default_half
        };

        let center_row = (b.n_az_fft / 2) as isize;
        let row_start = (center_row - half_prev).max(0) as usize;
        let row_end = ((center_row + half_next) as usize).min(b.n_az_fft);
        let taper_len = (2 * overlap_half as usize).max(4);

        // Sanfter Azimut-Scalloping-Ausgleich innerhalb des aktiven Burst-Fensters:
        // Wir messen das mittlere Azimut-Leistungsprofil (in robuster Mittelung über die
        // mittleren 70% der Range-Spalten) und glätten die TOPS-Antennenkeulen-Abnahme
        // zu den Burst-Rändern hin moderat aus.
        let r_prof0 = n_rg / 6;
        let r_prof1 = (n_rg * 5 / 6).max(r_prof0 + 1);
        let active_len = row_end.saturating_sub(row_start).max(1);
        let mut row_energy = vec![0.0f64; active_len];
        for (idx, local_m) in (row_start..row_end).enumerate() {
            let slice = &b.data[local_m * n_rg_full + r_prof0..local_m * n_rg_full + r_prof1];
            let mut s = 0.0f64;
            for c in slice.iter().step_by(4) {
                s += f64::from(c.norm_sqr());
            }
            row_energy[idx] = s;
        }
        // Stark geglättetes Profil (Boxcar der Breite active_len / 5) relativ zur Burst-Mitte
        let smooth_half = (active_len / 10).max(8);
        let mut smooth_prof = vec![1.0f32; active_len];
        let mid_idx = active_len / 2;
        let mut center_sum = 0.0f64;
        let mut center_cnt = 0usize;
        for &e in &row_energy
            [mid_idx.saturating_sub(smooth_half)..(mid_idx + smooth_half + 1).min(active_len)]
        {
            if e > 0.0 {
                center_sum += e;
                center_cnt += 1;
            }
        }
        let center_ref = if center_cnt > 0 {
            center_sum / (center_cnt as f64)
        } else {
            0.0
        };
        if center_ref > 1e-20 {
            for (idx, g) in smooth_prof.iter_mut().enumerate().take(active_len) {
                let i0 = idx.saturating_sub(smooth_half);
                let i1 = (idx + smooth_half + 1).min(active_len);
                let mean_e = row_energy[i0..i1].iter().sum::<f64>() / ((i1 - i0) as f64);
                if mean_e > center_ref * 0.15 {
                    // Begrenzte Descalloping-Korrektur (max. ±2.2 dB), damit Küstenlinien
                    // keine künstlichen Streifen erzeugen, aber der Antennenabfall kompensiert wird.
                    *g = ((center_ref / mean_e).clamp(0.60, 1.65)) as f32;
                }
            }
        }

        for (idx, local_m) in (row_start..row_end).enumerate() {
            let dm = (local_m as isize) - center_row;
            let t_zero_doppler = b.mid_time_s + (dm as f64) * dt_az_s;
            let out_row_isize = ((t_zero_doppler - t_start_s) / dt_az_s).round() as isize;
            if out_row_isize < 0 || (out_row_isize as usize) >= n_az_out {
                continue;
            }
            let out_row = out_row_isize as usize;

            // Symmetrische Kosinus-/Trapez-Überblendung exakt in der 2*overlap_half-Zone
            let dist_top = local_m - row_start;
            let dist_bot = (row_end - 1) - local_m;
            let dist_edge = dist_top.min(dist_bot);
            let w = if dist_edge < taper_len {
                let u = ((dist_edge + 1) as f32) / ((taper_len + 1) as f32);
                0.5 * (1.0 - (std::f32::consts::PI * u).cos())
            } else {
                1.0f32
            };

            let gain = w * smooth_prof[idx];
            acc_weight[out_row] += w;
            let src_slice = &b.data[local_m * n_rg_full..local_m * n_rg_full + n_rg];
            let dst_slice = &mut acc_power[out_row * n_rg..(out_row + 1) * n_rg];
            for (dst, src) in dst_slice.iter_mut().zip(src_slice.iter()) {
                *dst += gain * src.norm_sqr();
            }
        }
    }

    for a in 0..n_az_out {
        let w = acc_weight[a];
        if w > 0.0 {
            let inv_w = 1.0 / w;
            for v in &mut acc_power[a * n_rg..(a + 1) * n_rg] {
                *v *= inv_w;
            }
        }
    }

    Ok(DeburstedSwath {
        swath_number,
        swath_name: swath_name.to_string(),
        n_az: n_az_out,
        n_rg,
        t_start_s,
        dt_az_s,
        r0_m,
        dr_m,
        power: acc_power,
    })
}

/// Fügt mehrere entburstete Sub-Swaths (z.B. IW1–IW3 oder EW1–EW5) in einem gemeinsamen
/// `(Azimut-Zeit × Slant-Range)`-Koordinatensystem mit radiometrischem Gain-Ausgleich
/// und Kanten-Feathering zu einem einzigen Wide-Swath-Mosaik zusammen.
pub fn mosaic_swaths(
    swaths: &[DeburstedSwath],
    laz: usize,
    lrg: usize,
) -> Result<(Vec<f32>, usize, usize, f64, f64), Error> {
    if swaths.is_empty() {
        return Err(Error("Keine Sub-Swaths zur Mosaikierung".into()));
    }
    if swaths.len() == 1 {
        let s = &swaths[0];
        let (ml, oaz, org) = multilook_power(&s.power, s.n_az, s.n_rg, laz, lrg);
        return Ok((ml, oaz, org, s.r0_m, s.r0_m + (s.n_rg as f64) * s.dr_m));
    }

    // 1. Gemeinsames Zeit- und Slant-Range-Gitter auf Multilook-Auflösung bestimmen
    let t_min = swaths
        .iter()
        .map(|s| s.t_start_s)
        .fold(f64::INFINITY, f64::min);
    let t_max = swaths
        .iter()
        .map(|s| s.t_start_s + (s.n_az as f64) * s.dt_az_s)
        .fold(f64::NEG_INFINITY, f64::max);
    let r_min = swaths.iter().map(|s| s.r0_m).fold(f64::INFINITY, f64::min);
    let r_max = swaths
        .iter()
        .map(|s| s.r0_m + (s.n_rg as f64) * s.dr_m)
        .fold(f64::NEG_INFINITY, f64::max);

    let dt_base = swaths[0].dt_az_s * (laz.max(1) as f64);
    let dr_base = swaths[0].dr_m * (lrg.max(1) as f64);

    let out_az = (((t_max - t_min) / dt_base).ceil() as usize).max(1);
    let out_rg = (((r_max - r_min) / dr_base).ceil() as usize).max(1);

    // 2. Zuerst jeden Sub-Swath individuell multilooken, etwaige sehr schwache Range-Randspalten
    //    (Antennen-Roll-Off an den äußersten Sub-Swath-Kanten) trimmen und die Nahtstellen-Intensität
    //    zwischen benachbarten Elevation-Beams radiometrisch abgleichen.
    struct MlSwath<'a> {
        meta: &'a DeburstedSwath,
        ml: Vec<f32>,
        oaz: usize,
        org: usize,
        c_start: usize,
        c_end: usize,
        dt_ml: f64,
        dr_ml: f64,
        scale: f32,
        col_gain: Vec<f32>,
    }

    let mut ml_swaths = Vec::with_capacity(swaths.len());
    for s in swaths {
        let (ml, oaz, org) = multilook_power(&s.power, s.n_az, s.n_rg, laz, lrg);
        // Zeilenweise normiertes Range-Profil (damit helle Inseln/Küstenzonen in einzelnen
        // Azimut-Abschnitten das geschätzte Elevation-Antennenprofil nicht verzerren)
        let mut row_med = vec![1.0f32; oaz];
        let mut row_tmp = Vec::with_capacity(org);
        for a in 0..oaz {
            row_tmp.clear();
            for r in (org / 8)..(org * 7 / 8).max(org / 8 + 1) {
                let v = ml[a * org + r];
                if v > 0.0 && v.is_finite() {
                    row_tmp.push(v);
                }
            }
            if !row_tmp.is_empty() {
                row_tmp.sort_by(f32::total_cmp);
                row_med[a] = row_tmp[row_tmp.len() / 2].max(1e-20);
            }
        }

        let mut col_prof = vec![0.0f32; org];
        let mut col_tmp = Vec::with_capacity(oaz);
        for r in 0..org {
            col_tmp.clear();
            for a in (0..oaz).step_by(2) {
                let v = ml[a * org + r];
                if v > 0.0 && v.is_finite() {
                    col_tmp.push(v / row_med[a]);
                }
            }
            if !col_tmp.is_empty() {
                col_tmp.sort_by(f32::total_cmp);
                col_prof[r] = col_tmp[col_tmp.len() / 2];
            }
        }
        let mid_r0 = org / 4;
        let mid_r1 = (org * 3 / 4).max(mid_r0 + 1);
        let mut mid_vals: Vec<f32> = col_prof[mid_r0..mid_r1]
            .iter()
            .copied()
            .filter(|&x| x > 0.0)
            .collect();
        mid_vals.sort_by(f32::total_cmp);
        let mid_med = if mid_vals.is_empty() {
            1.0
        } else {
            mid_vals[mid_vals.len() / 2].max(1e-20)
        };

        // Trimme nur echte Null-/Totspalten am äußersten Rand (max. 16 Spalten),
        // damit die physikalische Slant-Range-Überlappung (4–9 km) erhalten bleibt.
        let thresh = mid_med * 0.03;
        let mut c_start = 0usize;
        while c_start < 16.min(org / 8) && col_prof[c_start] < thresh {
            c_start += 1;
        }
        let mut c_end = org;
        while org.saturating_sub(c_end) < 16.min(org / 8)
            && c_end > c_start + 16
            && col_prof[c_end - 1] < thresh
        {
            c_end -= 1;
        }

        // 1) Quadratischer Log-Power-Trend (Elevation Antenna Pattern + R^3 / Inzidenzwinkel-Abfall)
        //    über den inneren Bereich [c_start + edge_w .. c_end - edge_w] mittels Kleinste-Quadrate
        //    bestimmen: ln(col_prof(u)) ≈ c0 + c1*u + c2*u^2 mit u in [-1, 1].
        let valid_w = c_end.saturating_sub(c_start);
        let edge_w = (valid_w / 6).max(8);
        let smooth_w = (valid_w / 40).max(4);
        let mut col_gain = vec![1.0f32; org];

        if valid_w > 2 * edge_w + 8 {
            let fit_r0 = c_start + edge_w / 2;
            let fit_r1 = c_end - edge_w / 2;
            let center = 0.5 * ((c_start + c_end) as f64);
            let half_span = (0.5 * (valid_w as f64)).max(1.0);

            // Orthogonale Legendre-Projektion auf {1, u, 0.5*(3u^2 - 1)} über [fit_r0..fit_r1]
            let mut s1 = 0.0f64;
            let mut su = 0.0f64;
            let mut su2 = 0.0f64;
            let mut sy = 0.0f64;
            let mut suy = 0.0f64;
            let mut su2y = 0.0f64;
            for (r, &cp) in col_prof.iter().enumerate().take(fit_r1).skip(fit_r0) {
                let p = cp / mid_med;
                if p > 0.1 {
                    let u = ((r as f64) - center) / half_span;
                    let p2 = 0.5 * (3.0 * u * u - 1.0);
                    let y = (p as f64).ln();
                    s1 += 1.0;
                    su += u * u;
                    su2 += p2 * p2;
                    sy += y;
                    suy += u * y;
                    su2y += p2 * y;
                }
            }
            let c0 = if s1 > 0.0 { sy / s1 } else { 0.0 };
            let c1 = if su > 1e-6 {
                (suy / su).clamp(-1.20, 1.20)
            } else {
                0.0
            };
            let c2 = if su2 > 1e-6 {
                (su2y / su2).clamp(-0.15, 0.15)
            } else {
                0.0
            };

            for (r, cg) in col_gain.iter_mut().enumerate().take(c_end).skip(c_start) {
                let u = ((r as f64) - center) / half_span;
                let p2 = 0.5 * (3.0 * u * u - 1.0);
                // Kompensiere linearen und quadratischen EAP-Trend relativ zur Swath-Mitte
                let trend_ln = (c0 + c1 * u + c2 * p2).clamp(-1.4, 1.4);
                *cg = ((-trend_ln).exp() as f32).clamp(0.25, 4.0);
            }

            // 2) Zusätzlicher glatter Roll-Off-Ausgleich in den äußeren 16%-Randzonen
            //    (partieller Chirp-Empfang am SWL-Ende und steiler Antennenflanken-Abfall)
            let mut corr_prof = vec![0.0f32; org];
            for r in c_start..c_end {
                corr_prof[r] = col_prof[r] * col_gain[r];
            }
            let left_ref_r0 = c_start + edge_w;
            let left_ref_r1 = (left_ref_r0 + smooth_w).min(c_end);
            let left_ref = corr_prof[left_ref_r0..left_ref_r1].iter().sum::<f32>()
                / ((left_ref_r1 - left_ref_r0).max(1) as f32);

            let right_ref_r1 = c_end - edge_w;
            let right_ref_r0 = right_ref_r1.saturating_sub(smooth_w).max(c_start);
            let right_ref = corr_prof[right_ref_r0..right_ref_r1].iter().sum::<f32>()
                / ((right_ref_r1 - right_ref_r0).max(1) as f32);

            for (r, cg) in col_gain.iter_mut().enumerate().skip(c_start).take(edge_w) {
                let r0 = r.saturating_sub(smooth_w).max(c_start);
                let r1 = (r + smooth_w + 1).min(c_end);
                let local_mean = corr_prof[r0..r1].iter().sum::<f32>() / ((r1 - r0).max(1) as f32);
                if local_mean > left_ref * 0.08 && left_ref > 0.0 {
                    let u = ((c_start + edge_w - r) as f32) / (edge_w as f32);
                    let w_blend = 0.5 * (1.0 - (std::f32::consts::PI * u).cos());
                    let raw_g = (left_ref / local_mean).clamp(0.4, 3.5);
                    *cg *= 1.0 + w_blend * (raw_g - 1.0);
                }
            }
            for (r, cg) in col_gain
                .iter_mut()
                .enumerate()
                .take(c_end)
                .skip(c_end - edge_w)
            {
                let r0 = r.saturating_sub(smooth_w).max(c_start);
                let r1 = (r + smooth_w + 1).min(c_end);
                let local_mean = corr_prof[r0..r1].iter().sum::<f32>() / ((r1 - r0).max(1) as f32);
                if local_mean > right_ref * 0.08 && right_ref > 0.0 {
                    let u = ((r + 1 - (c_end - edge_w)) as f32) / (edge_w as f32);
                    let w_blend = 0.5 * (1.0 - (std::f32::consts::PI * u).cos());
                    let raw_g = (right_ref / local_mean).clamp(0.4, 3.5);
                    *cg *= 1.0 + w_blend * (raw_g - 1.0);
                }
            }
        }

        ml_swaths.push(MlSwath {
            meta: s,
            ml,
            oaz,
            org,
            c_start,
            c_end,
            dt_ml: s.dt_az_s * (laz.max(1) as f64),
            dr_ml: s.dr_m * (lrg.max(1) as f64),
            scale: 1.0,
            col_gain,
        });
    }

    // Radiometrische Kopplung benachbarter Sub-Swaths exakt innerhalb ihrer gemeinsamen
    // physikalischen Slant-Range-Überlappungszone [r_ov_min, r_ov_max]
    for i in 1..ml_swaths.len() {
        let prev_scale = ml_swaths[i - 1].scale;
        let prev_r_start =
            ml_swaths[i - 1].meta.r0_m + (ml_swaths[i - 1].c_start as f64) * ml_swaths[i - 1].dr_ml;
        let prev_r_end =
            ml_swaths[i - 1].meta.r0_m + (ml_swaths[i - 1].c_end as f64) * ml_swaths[i - 1].dr_ml;
        let cur_r_start =
            ml_swaths[i].meta.r0_m + (ml_swaths[i].c_start as f64) * ml_swaths[i].dr_ml;
        let cur_r_end = ml_swaths[i].meta.r0_m + (ml_swaths[i].c_end as f64) * ml_swaths[i].dr_ml;

        let r_ov_min = prev_r_start.max(cur_r_start);
        let r_ov_max = prev_r_end.min(cur_r_end);

        let (prev_start, prev_end, cur_start, cur_end) = if r_ov_max > r_ov_min {
            let ps = (((r_ov_min - ml_swaths[i - 1].meta.r0_m) / ml_swaths[i - 1].dr_ml).floor()
                as usize)
                .clamp(ml_swaths[i - 1].c_start, ml_swaths[i - 1].c_end);
            let pe = (((r_ov_max - ml_swaths[i - 1].meta.r0_m) / ml_swaths[i - 1].dr_ml).ceil()
                as usize)
                .clamp(ps, ml_swaths[i - 1].c_end);
            let cs = (((r_ov_min - ml_swaths[i].meta.r0_m) / ml_swaths[i].dr_ml).floor() as usize)
                .clamp(ml_swaths[i].c_start, ml_swaths[i].c_end);
            let ce = (((r_ov_max - ml_swaths[i].meta.r0_m) / ml_swaths[i].dr_ml).ceil() as usize)
                .clamp(cs, ml_swaths[i].c_end);
            (ps, pe, cs, ce)
        } else {
            let pe = ml_swaths[i - 1].c_end;
            let ps = pe.saturating_sub(64).max(ml_swaths[i - 1].c_start);
            let cs = ml_swaths[i].c_start;
            let ce = (cs + 64).min(ml_swaths[i].c_end);
            (ps, pe, cs, ce)
        };

        let mut prev_samples = Vec::new();
        for a in (0..ml_swaths[i - 1].oaz).step_by(2) {
            for r in prev_start..prev_end {
                let v = ml_swaths[i - 1].ml[a * ml_swaths[i - 1].org + r]
                    * ml_swaths[i - 1].col_gain[r]
                    * prev_scale;
                if v > 0.0 && v.is_finite() {
                    prev_samples.push(v);
                }
            }
        }
        let mut cur_samples = Vec::new();
        for a in (0..ml_swaths[i].oaz).step_by(2) {
            for r in cur_start..cur_end {
                let v = ml_swaths[i].ml[a * ml_swaths[i].org + r] * ml_swaths[i].col_gain[r];
                if v > 0.0 && v.is_finite() {
                    cur_samples.push(v);
                }
            }
        }
        if !prev_samples.is_empty() && !cur_samples.is_empty() {
            prev_samples.sort_by(f32::total_cmp);
            cur_samples.sort_by(f32::total_cmp);
            let p_med = prev_samples[prev_samples.len() / 2];
            let c_med = cur_samples[cur_samples.len() / 2];
            if c_med > 1e-20 {
                ml_swaths[i].scale = (p_med / c_med).clamp(0.2, 5.0);
            }
        }
    }

    // 3. In das globale (out_az × out_rg)-Gitter mit komplementärem Raised-Cosine-Feathering
    //    über die exakte physikalische Slant-Range-Überlappung eintragen
    let mut mosaic_val = vec![0.0f32; out_az * out_rg];
    let mut mosaic_w = vec![0.0f32; out_az * out_rg];

    let swath_r_bounds: Vec<(f64, f64)> = ml_swaths
        .iter()
        .map(|ms| {
            (
                ms.meta.r0_m + (ms.c_start as f64) * ms.dr_ml,
                ms.meta.r0_m + (ms.c_end as f64) * ms.dr_ml,
            )
        })
        .collect();

    for (si, ms) in ml_swaths.iter().enumerate() {
        let valid_cols = ms.c_end.saturating_sub(ms.c_start);
        if valid_cols < 2 {
            continue;
        }
        let (r_sw_start, r_sw_end) = swath_r_bounds[si];
        let r_left_ov_end = if si > 0 {
            swath_r_bounds[si - 1]
                .1
                .max(r_sw_start + 8.0 * ms.dr_ml)
                .min(r_sw_end)
        } else {
            r_sw_start
        };
        let r_right_ov_start = if si + 1 < ml_swaths.len() {
            swath_r_bounds[si + 1]
                .0
                .min(r_sw_end - 8.0 * ms.dr_ml)
                .max(r_sw_start)
        } else {
            r_sw_end
        };

        let t_sw_end = ms.meta.t_start_s + (ms.oaz as f64) * ms.dt_ml;

        let ga0 = (((ms.meta.t_start_s - t_min) / dt_base).floor() as isize).max(0) as usize;
        let ga1 =
            (((t_sw_end - t_min) / dt_base).ceil() as isize).clamp(0, out_az as isize) as usize;
        let gr0 = (((r_sw_start - r_min) / dr_base).floor() as isize).max(0) as usize;
        let gr1 =
            (((r_sw_end - r_min) / dr_base).ceil() as isize).clamp(0, out_rg as isize) as usize;

        for ga in ga0..ga1 {
            let t = t_min + (ga as f64 + 0.5) * dt_base;
            let fa = (t - ms.meta.t_start_s) / ms.dt_ml - 0.5;
            if fa < 0.0 || fa >= (ms.oaz.saturating_sub(1)) as f64 {
                continue;
            }
            let ia0 = fa.floor() as usize;
            let wa = (fa - ia0 as f64) as f32;
            let row0 = &ms.ml[ia0 * ms.org..(ia0 + 1) * ms.org];
            let row1 = &ms.ml[(ia0 + 1) * ms.org..(ia0 + 2) * ms.org];

            for gr in gr0..gr1 {
                let r = r_min + (gr as f64 + 0.5) * dr_base;
                let fr = (r - ms.meta.r0_m) / ms.dr_ml - 0.5;
                if fr < (ms.c_start as f64) || fr >= (ms.c_end.saturating_sub(1)) as f64 {
                    continue;
                }
                let ir0 = fr.floor() as usize;
                let wr_interp = (fr - ir0 as f64) as f32;
                let v0 = row0[ir0] * (1.0 - wr_interp) + row0[ir0 + 1] * wr_interp;
                let v1 = row1[ir0] * (1.0 - wr_interp) + row1[ir0 + 1] * wr_interp;
                let g_col = ms.col_gain[ir0] * (1.0 - wr_interp) + ms.col_gain[ir0 + 1] * wr_interp;
                let p_raw = (v0 * (1.0 - wa) + v1 * wa) * g_col;
                if p_raw <= 0.0 {
                    continue;
                }
                let w_left = if si > 0 && r < r_left_ov_end {
                    let span = (r_left_ov_end - r_sw_start).max(ms.dr_ml);
                    let u = ((r - r_sw_start) / span).clamp(0.0, 1.0) as f32;
                    0.5 * (1.0 - (std::f32::consts::PI * u).cos())
                } else {
                    1.0f32
                };
                let w_right = if si + 1 < ml_swaths.len() && r > r_right_ov_start {
                    let span = (r_sw_end - r_right_ov_start).max(ms.dr_ml);
                    let u = ((r_sw_end - r) / span).clamp(0.0, 1.0) as f32;
                    0.5 * (1.0 - (std::f32::consts::PI * u).cos())
                } else {
                    1.0f32
                };
                let wr = w_left.min(w_right).max(1e-4);
                let idx = ga * out_rg + gr;
                mosaic_val[idx] += wr * p_raw * ms.scale;
                mosaic_w[idx] += wr;
            }
        }
    }

    for (v, &w) in mosaic_val.iter_mut().zip(mosaic_w.iter()) {
        if w > 0.0 {
            *v /= w;
        }
    }

    Ok((mosaic_val, out_az, out_rg, r_min, r_max))
}

fn positive_median(v: &[f32]) -> f32 {
    let step = (v.len() / 32_768).max(1);
    let mut samples: Vec<f32> = v
        .iter()
        .step_by(step)
        .copied()
        .filter(|&x| x > 0.0 && x.is_finite())
        .collect();
    if samples.is_empty() {
        return 1.0;
    }
    samples.sort_by(f32::total_cmp);
    samples[samples.len() / 2]
}

/// Multilooking eines Leistungsbildes über `laz × lrg`-Blöcke.
pub fn multilook_power(
    power: &[f32],
    n_az: usize,
    n_rg: usize,
    laz: usize,
    lrg: usize,
) -> (Vec<f32>, usize, usize) {
    let laz = laz.max(1);
    let lrg = lrg.max(1);
    let oaz = n_az / laz;
    let org = n_rg / lrg;
    let mut out = vec![0.0f32; oaz * org];
    let norm = 1.0 / ((laz * lrg) as f64);
    for a in 0..oaz {
        for r in 0..org {
            let mut sum = 0.0f64;
            for da in 0..laz {
                let row_off = (a * laz + da) * n_rg + r * lrg;
                for dr in 0..lrg {
                    sum += f64::from(power[row_off + dr]);
                }
            }
            out[a * org + r] = (sum * norm) as f32;
        }
    }
    (out, oaz, org)
}

/// Konvertiert ein Leistungsbild in logarithmische dB-Werte ($10 \log_{10}(P)$).
/// Leere Hintergrundpixel ($P \le 0$) werden auf ein niedriges Floor-Niveau gesetzt, das
/// von `db_percentiles` ignoriert wird.
pub fn power_to_db(power: &[f32]) -> Vec<f32> {
    let med = positive_median(power).max(1e-30);
    let floor = med * 1e-6;
    power
        .iter()
        .map(|&p| {
            if p > floor {
                10.0 * p.log10()
            } else {
                10.0 * floor.log10()
            }
        })
        .collect()
}

/// Berechnet Mittelwert und Standardabweichung (in dB) über alle beleuchteten Szene-Pixel
/// (ignoriert unbeleuchtete 0-Ränder und extreme Rausch-Ausreißer unterhalb von `median * 1e-4`).
pub fn db_mean_std(power: &[f32]) -> (f64, f64) {
    let med = positive_median(power).max(1e-30);
    let floor = med * 1e-4;
    let valid: Vec<f64> = power
        .iter()
        .copied()
        .filter(|&p| p > floor && p.is_finite())
        .map(|p| 10.0 * f64::from(p).log10())
        .collect();
    if valid.is_empty() {
        return (0.0, 0.0);
    }
    let n = valid.len() as f64;
    let mean = valid.iter().sum::<f64>() / n;
    let var = valid.iter().map(|&x| (x - mean) * (x - mean)).sum::<f64>() / n;
    (mean, var.sqrt())
}

/// Perzentilberechnung für kontrastoptimierte PNG-Darstellung (ignoriert leere Randpixel).
pub fn db_percentiles(db: &[f32], q_lo: f32, q_hi: f32) -> (f32, f32) {
    let lo_min = db.iter().copied().fold(f32::INFINITY, f32::min);
    let floor = lo_min + 0.5;
    let step = (db.len() / 65_536).max(1);
    let mut vals: Vec<f32> = db
        .iter()
        .step_by(step)
        .copied()
        .filter(|&x| x > floor && x.is_finite())
        .collect();
    if vals.is_empty() {
        return (-60.0, 0.0);
    }
    vals.sort_by(f32::total_cmp);
    let ilo = ((q_lo.clamp(0.0, 1.0) * (vals.len() - 1) as f32) as usize).min(vals.len() - 1);
    let ihi = ((q_hi.clamp(0.0, 1.0) * (vals.len() - 1) as f32) as usize).min(vals.len() - 1);
    (vals[ilo], vals[ihi].max(vals[ilo] + 1.0))
}

/// Speichert ein dB-Bild als Graustufen-PNG mit Perzentil-Spreizung `(2%, 99.5%)`.
pub fn save_db_png(db: &[f32], width: usize, height: usize, path: &Path) -> Result<(), Error> {
    if db.len() != width * height {
        return Err(Error(format!(
            "PNG-Puffergröße {} passt nicht zu {width}×{height}",
            db.len()
        )));
    }
    let (lo, hi) = db_percentiles(db, 0.02, 0.995);
    let span = (hi - lo).max(1e-6);
    let px: Vec<u8> = db
        .iter()
        .map(|&v| (((v - lo) / span).clamp(0.0, 1.0) * 255.0) as u8)
        .collect();
    let img = image::GrayImage::from_raw(width as u32, height as u32, px)
        .ok_or_else(|| Error("Ungültiger Graustufen-Puffer".into()))?;
    img.save(path).map_err(|e| Error(e.to_string()))?;
    Ok(())
}

/// Exportiert ein komplexes oder reelles Bild als `.cf` (verschachtelte `f32`-Paare) plus `.json`-Sidecar.
#[allow(clippy::too_many_arguments)]
pub fn save_cf_and_json(
    path_stem: &Path,
    power_or_amp: &[f32],
    n_az: usize,
    n_rg: usize,
    mode_str: &str,
    r_min_m: f64,
    r_max_m: f64,
    db_std: f64,
) -> Result<(), Error> {
    let cf_path = path_stem.with_extension("cf");
    let json_path = path_stem.with_extension("json");
    {
        let f = File::create(&cf_path)?;
        let mut w = BufWriter::new(f);
        for &p in power_or_amp {
            let amp = p.max(0.0).sqrt();
            w.write_all(&amp.to_le_bytes())?;
            w.write_all(&0.0f32.to_le_bytes())?;
        }
        w.flush()?;
    }
    let json = format!(
        "{{\n  \"mode\": \"{mode_str}\",\n  \"n_az\": {n_az},\n  \"n_rg\": {n_rg},\n  \"slant_range_min_m\": {r_min_m:.2},\n  \"slant_range_max_m\": {r_max_m:.2},\n  \"contrast_std_db\": {db_std:.3}\n}}\n"
    );
    std::fs::write(&json_path, json)?;
    let _ = std::hint::black_box(0usize);
    Ok(())
}

/// Hilfsfunktion für Overlap-Save-Stripmap-Fokussierung auf großen Azimut-Streifen.
pub fn focus_stripmap_overlap_save<F>(
    full_block: &BurstBlock,
    chunk_az: usize,
    overlap_az: usize,
    mut focus_chunk_fn: F,
) -> Result<Vec<f32>, Error>
where
    F: FnMut(&BurstBlock) -> Result<Vec<Complex32>, Error>,
{
    let total_az = full_block.n_az;
    let n_rg = full_block.n_rg;
    if total_az <= chunk_az {
        let slc = focus_chunk_fn(full_block)?;
        return Ok(slc.into_iter().map(|c| c.norm_sqr()).collect());
    }

    let step = chunk_az.saturating_sub(2 * overlap_az).max(1);
    let mut out_power = vec![0.0f32; total_az * n_rg];
    let mut a0 = 0usize;

    while a0 < total_az {
        let a1 = (a0 + chunk_az).min(total_az);
        let cur_az = a1 - a0;
        let sub_block = BurstBlock {
            burst_index: full_block.burst_index,
            swath_number: full_block.swath_number,
            params: full_block.params,
            n_az: cur_az,
            n_rg,
            raw: full_block.raw[a0 * n_rg..a1 * n_rg].to_vec(),
            pulse_times: full_block.pulse_times[a0..a1].to_vec(),
            az_beam_addr: full_block.az_beam_addr[a0..a1].to_vec(),
            veff: full_block.veff.clone(),
            pos_ecef: if full_block.pos_ecef.len() >= a1 {
                full_block.pos_ecef[a0..a1].to_vec()
            } else {
                Vec::new()
            },
            vel_ecef: if full_block.vel_ecef.len() >= a1 {
                full_block.vel_ecef[a0..a1].to_vec()
            } else {
                Vec::new()
            },
        };
        let slc = focus_chunk_fn(&sub_block)?;
        let keep_start = if a0 == 0 { 0 } else { overlap_az.min(cur_az) };
        let keep_end = if a1 == total_az {
            cur_az
        } else {
            cur_az.saturating_sub(overlap_az).max(keep_start)
        };
        for local_a in keep_start..keep_end {
            let global_a = a0 + local_a;
            let src = &slc[local_a * n_rg..(local_a + 1) * n_rg];
            let dst = &mut out_power[global_a * n_rg..(global_a + 1) * n_rg];
            for (d, s) in dst.iter_mut().zip(src.iter()) {
                *d = s.norm_sqr();
            }
        }
        if a1 == total_az {
            break;
        }
        a0 += step;
    }

    Ok(out_power)
}
