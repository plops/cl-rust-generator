//! Quicklook: dB-Bild, Multilook, PNG, ASCII, Kennzahlen.
//!
//! Aus dem fokussierten Komplexbild wird Leistungs-dB, per Multilook auf
//! Quicklook-Größe gemittelt und als PNG (Perzentil-Spreizung) plus ASCII
//! fürs Terminal gerendert. Kennzahlen: Mittel/Std (Kontrast), Top-N-Peaks
//! (Schiffe) mit FWHM-Schnitten gegen die Theorie.

use crate::types::Complex32;

/// Leistung in dB: `10·log10(|c|²)`, Boden bei Peak −120 dB.
pub fn power_db(img: &[Complex32]) -> Vec<f32> {
    let peak = img
        .iter()
        .map(|c| c.norm_sqr())
        .fold(0.0f32, f32::max)
        .max(1e-30);
    let floor = peak * 1e-12;
    img.iter()
        .map(|c| 10.0 * c.norm_sqr().max(floor).log10())
        .collect()
}

/// Multilook: Leistungsmittel über `laz × lrg`-Blöcke (Rest verworfen).
pub fn multilook(
    power: &[f32],
    naz: usize,
    nr: usize,
    laz: usize,
    lrg: usize,
) -> (Vec<f32>, usize, usize) {
    let (oaz, org) = (naz / laz.max(1), nr / lrg.max(1));
    let mut out = vec![0.0f32; oaz * org];
    for a in 0..oaz {
        for r in 0..org {
            let mut s = 0.0f64;
            for da in 0..laz {
                for dr in 0..lrg {
                    s += f64::from(power[(a * laz + da) * nr + r * lrg + dr]);
                }
            }
            out[a * org + r] = (s / (laz * lrg) as f64) as f32;
        }
    }
    (out, oaz, org)
}

/// Perzentil via Histogramm (256 Bins zwischen Min/Max).
fn percentile(v: &[f32], q: f32) -> f32 {
    let (mut lo, mut hi) = (f32::INFINITY, f32::NEG_INFINITY);
    for &x in v {
        lo = lo.min(x);
        hi = hi.max(x);
    }
    if hi <= lo {
        return lo;
    }
    let mut hist = [0u64; 256];
    for &x in v {
        let b = (((x - lo) / (hi - lo) * 255.0) as usize).min(255);
        hist[b] += 1;
    }
    let want = (q.clamp(0.0, 1.0) * v.len() as f32) as u64;
    let mut acc = 0u64;
    for (b, &h) in hist.iter().enumerate() {
        acc += h;
        if acc >= want {
            return lo + (hi - lo) * b as f32 / 255.0;
        }
    }
    hi
}

/// Spreizgrenzen aus Perzentilen (`qlo..qhi`).
pub fn stretch_lo_hi(db: &[f32], qlo: f32, qhi: f32) -> (f32, f32) {
    (percentile(db, qlo), percentile(db, qhi))
}

/// PNG-Quicklook (Graustufen, Perzentil-Spreizung `lo..hi`).
pub fn render_png_gray(
    db: &[f32],
    w: usize,
    h: usize,
    path: &std::path::Path,
    lo: f32,
    hi: f32,
) -> Result<(), String> {
    if db.len() != w * h {
        return Err(format!("Größe {} passt nicht zu {w}×{h}", db.len()));
    }
    let span = (hi - lo).max(1e-6);
    let px: Vec<u8> = db
        .iter()
        .map(|&x| (((x - lo) / span).clamp(0.0, 1.0) * 255.0) as u8)
        .collect();
    let img = image::GrayImage::from_raw(w as u32, h as u32, px)
        .ok_or_else(|| "PNG-Puffer ungültig".to_string())?;
    img.save(path).map_err(|e| e.to_string())?;
    Ok(())
}

/// ASCII-Vorschau fürs Terminal (`cols` Spalten breit).
pub fn render_ascii(db: &[f32], w: usize, h: usize, cols: usize) -> String {
    const RAMP: &[u8] = b" .:-=+*#%@";
    let lo = percentile(db, 0.02);
    let hi = percentile(db, 0.995);
    let span = (hi - lo).max(1e-6);
    let (cw, ch) = (w.div_ceil(cols).max(1), w.div_ceil(cols).max(1) * 2);
    let mut s = String::new();
    for a0 in (0..h).step_by(ch) {
        for r0 in (0..w).step_by(cw) {
            let mut sum = 0.0f64;
            let mut n = 0u64;
            for a in a0..(a0 + ch).min(h) {
                for r in r0..(r0 + cw).min(w) {
                    sum += f64::from(db[a * w + r]);
                    n += 1;
                }
            }
            let v = ((sum / n as f64) as f32 - lo) / span;
            let b = (v.clamp(0.0, 1.0) * (RAMP.len() - 1) as f32) as usize;
            s.push(RAMP[b] as char);
        }
        s.push('\n');
    }
    s
}

/// Mittelwert und Standardabweichung (Kontrast-Kennzahl).
pub fn mean_std(v: &[f32]) -> (f64, f64) {
    let n = v.len().max(1) as f64;
    let m = v.iter().map(|&x| f64::from(x)).sum::<f64>() / n;
    let var = v.iter().map(|&x| (f64::from(x) - m).powi(2)).sum::<f64>() / n;
    (m, var.sqrt())
}

/// Top-`n` lokale Maxima mit Mindestabstand (Schiffskandidaten).
///
/// Rückgabe `(az, range, Leistung)`, absteigend sortiert.
pub fn find_peaks(
    power: &[f32],
    naz: usize,
    nr: usize,
    n: usize,
    min_dist: usize,
) -> Vec<(usize, usize, f32)> {
    let mut cand: Vec<(usize, usize, f32)> = Vec::new();
    for a in 1..naz.saturating_sub(1) {
        for r in 1..nr.saturating_sub(1) {
            let v = power[a * nr + r];
            let mut is_max = true;
            for da in -1i32..=1 {
                for dr in -1i32..=1 {
                    if da == 0 && dr == 0 {
                        continue;
                    }
                    if power[((a as i32 + da) as usize) * nr + ((r as i32 + dr) as usize)] >= v {
                        is_max = false;
                    }
                }
            }
            if is_max {
                cand.push((a, r, v));
            }
        }
    }
    cand.sort_by(|x, y| y.2.total_cmp(&x.2));
    let mut kept: Vec<(usize, usize, f32)> = Vec::new();
    for c in cand {
        if kept.len() >= n {
            break;
        }
        let md = min_dist as i32;
        if kept.iter().all(|&(a, r, _)| {
            (a as i32 - c.0 as i32).abs() > md || (r as i32 - c.1 as i32).abs() > md
        }) {
            kept.push(c);
        }
    }
    kept
}

/// FWHM eines 1D-Schnitts in Pixeln (linear interpolierte Halbwertslage).
pub fn cut_fwhm(cut: &[f32], at: usize) -> f64 {
    let half = cut[at] / 2.0;
    let left = (0..at)
        .rev()
        .find(|&i| cut[i] < half)
        .map(|i| {
            let d = cut[i + 1] - cut[i];
            if d > 0.0 {
                i as f64 + f64::from(half - cut[i]) / f64::from(d)
            } else {
                i as f64 + 1.0
            }
        })
        .unwrap_or(0.0);
    let right = (at + 1..cut.len())
        .find(|&i| cut[i] < half)
        .map(|i| {
            let d = cut[i - 1] - cut[i];
            if d > 0.0 {
                i as f64 - f64::from(half - cut[i]) / f64::from(d)
            } else {
                i as f64 - 1.0
            }
        })
        .unwrap_or(cut.len() as f64 - 1.0);
    (right - left).max(0.0)
}

/// Theoretische Auflösungen in m (Range: `0,886·c/2B`; Azimut: `L/2`).
pub fn theoretical_resolution_m(bandwidth_hz: f64, antenna_len_m: f64) -> (f64, f64) {
    (
        0.886 * crate::types::SPEED_OF_LIGHT / (2.0 * bandwidth_hz),
        antenna_len_m / 2.0,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn db_boden_und_spitze() {
        let img = vec![Complex32::new(1.0, 0.0), Complex32::zero()];
        let db = power_db(&img);
        assert!((db[0] - 0.0).abs() < 1e-6);
        assert!((db[1] + 120.0).abs() < 1e-3);
    }

    #[test]
    fn multilook_mittelt() {
        let p: Vec<f32> = (0..16).map(|x| x as f32).collect();
        let (o, oaz, org) = multilook(&p, 4, 4, 2, 2);
        assert_eq!((oaz, org), (2, 2));
        assert!((o[0] - 2.5).abs() < 1e-6);
        assert!((o[3] - 12.5).abs() < 1e-6);
    }

    #[test]
    fn peaks_und_fwhm() {
        // Dreieck-Peak, FWHM = 2.
        let cut = vec![0.0, 0.5, 1.0, 0.5, 0.0];
        assert!((cut_fwhm(&cut, 2) - 2.0).abs() < 1e-6);
        let mut p = vec![1.0f32; 10 * 10];
        p[5 * 10 + 5] = 100.0;
        p[11] = 50.0;
        let peaks = find_peaks(&p, 10, 10, 2, 2);
        assert_eq!(peaks.len(), 2);
        assert_eq!((peaks[0].0, peaks[0].1), (5, 5));
        // 0,886·299792458/(2·42,2e6) = 3,14711; Antenne 12,3 m → 6,15 m.
        let (rr, ra) = theoretical_resolution_m(42.2e6, 12.3);
        assert!((rr - 3.147_110_400_331_753_6).abs() < 1e-12);
        assert_eq!(ra, 6.15);
    }
}
