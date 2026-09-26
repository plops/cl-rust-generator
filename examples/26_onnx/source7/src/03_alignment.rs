//! `03_alignment` — 5-Punkt-Ähnlichkeitstransformation + Bilinear-Warp.
//!
//! Least-Squares-Fit `x'=a·x−b·y+tx`, `y'=b·x+a·y+ty` (Skale + Rotation +
//! Translation, kein Shear) auf das ArcFace-112-Template, dann Warp des
//! RGB-Frames auf einen 112×112-Crop. Rein, ohne X11/Modell testbar.

use crate::types::{CROP_SIZE, Landmarks5};

/// Offizielle ArcFace-Referenzpunkte für 112×112 (Augen, Nase, Mundwinkel).
pub const ARCFACE_DST: [[f32; 2]; 5] = [
    [38.2946, 51.6963],
    [73.5318, 51.5014],
    [56.0252, 71.7366],
    [41.5493, 92.3655],
    [70.7299, 92.2041],
];

/// Schätzt die 2×3-Vorwärtsmatrix (src→Template) als `[a,−b,tx,b,a,ty]`.
/// `None` bei degenerierten Landmarks (alle Punkte identisch).
pub fn estimate_similarity(src: &[[f32; 2]; 5]) -> Option<[f32; 6]> {
    // Normalgleichung (AᵀA)·p = Aᵀb für p=[a,b,tx,ty], 10 Zeilen.
    let mut ata = [[0.0f64; 4]; 4];
    let mut atb = [0.0f64; 4];
    for (s, d) in src.iter().zip(ARCFACE_DST.iter()) {
        let (x, y) = (f64::from(s[0]), f64::from(s[1]));
        let rows = [
            ([x, -y, 1.0, 0.0], f64::from(d[0])),
            ([y, x, 0.0, 1.0], f64::from(d[1])),
        ];
        for (r, b) in rows {
            for i in 0..4 {
                atb[i] += r[i] * b;
                for j in 0..4 {
                    ata[i][j] += r[i] * r[j];
                }
            }
        }
    }
    let p = solve_4x4(ata, atb)?;
    #[allow(clippy::cast_possible_truncation)]
    let (a, b, tx, ty) = (p[0] as f32, p[1] as f32, p[2] as f32, p[3] as f32);
    Some([a, -b, tx, b, a, ty])
}

/// Invertiert eine 2×3-Ähnlichkeitsmatrix (dst→src für den Warp).
pub fn invert_similarity(m: &[f32; 6]) -> Option<[f32; 6]> {
    let (a, b) = (m[0], m[3]);
    let det = a * a + b * b;
    if det < 1e-12 {
        return None;
    }
    let (tx, ty) = (m[2], m[5]);
    Some([
        a / det,
        b / det,
        -(a * tx + b * ty) / det,
        -b / det,
        a / det,
        (b * tx - a * ty) / det,
    ])
}

/// Wendet eine 2×3-Inversmatrix per bilinearer Interpolation an (Border 0).
pub fn warp_affine_bilinear(
    src_rgb: &[u8],
    src_w: usize,
    src_h: usize,
    minv: &[f32; 6],
) -> Vec<u8> {
    let mut dst = vec![0u8; CROP_SIZE * CROP_SIZE * 3];
    for v in 0..CROP_SIZE {
        for u in 0..CROP_SIZE {
            let x = minv[0] * u as f32 + minv[1] * v as f32 + minv[2];
            let y = minv[3] * u as f32 + minv[4] * v as f32 + minv[5];
            let di = (v * CROP_SIZE + u) * 3;
            if x < 0.0 || y < 0.0 || x > src_w as f32 - 1.0 || y > src_h as f32 - 1.0 {
                continue;
            }
            let (x0, y0) = (x.floor() as usize, y.floor() as usize);
            let (fx, fy) = (x - x0 as f32, y - y0 as f32);
            let x1 = (x0 + 1).min(src_w - 1);
            let y1 = (y0 + 1).min(src_h - 1);
            for c in 0..3 {
                let p00 = f32::from(src_rgb[(y0 * src_w + x0) * 3 + c]);
                let p10 = f32::from(src_rgb[(y0 * src_w + x1) * 3 + c]);
                let p01 = f32::from(src_rgb[(y1 * src_w + x0) * 3 + c]);
                let p11 = f32::from(src_rgb[(y1 * src_w + x1) * 3 + c]);
                let top = p00 + (p10 - p00) * fx;
                let bot = p01 + (p11 - p01) * fx;
                dst[di + c] = (top + (bot - top) * fy).round().clamp(0.0, 255.0) as u8;
            }
        }
    }
    dst
}

/// Alignt das Gesicht auf 112×112; schwarzer Crop bei degenerierten Punkten.
pub fn align_face(src_rgb: &[u8], src_w: usize, src_h: usize, lm: &Landmarks5) -> Vec<u8> {
    let zero = vec![0u8; CROP_SIZE * CROP_SIZE * 3];
    estimate_similarity(&lm.points)
        .and_then(|m| invert_similarity(&m))
        .map_or(zero, |minv| {
            warp_affine_bilinear(src_rgb, src_w, src_h, &minv)
        })
}

/// Löst 4×4-System per Gauß mit Pivot; `None` bei Singularität.
fn solve_4x4(mut a: [[f64; 4]; 4], mut b: [f64; 4]) -> Option<[f64; 4]> {
    for col in 0..4 {
        let mut piv = col;
        for row in col + 1..4 {
            if a[row][col].abs() > a[piv][col].abs() {
                piv = row;
            }
        }
        if a[piv][col].abs() < 1e-12 {
            return None;
        }
        a.swap(col, piv);
        b.swap(col, piv);
        for row in col + 1..4 {
            let f = a[row][col] / a[col][col];
            let prow = a[col];
            for (d, p) in a[row].iter_mut().skip(col).zip(prow.iter().skip(col)) {
                *d -= f * p;
            }
            b[row] -= f * b[col];
        }
    }
    let mut x = [0.0; 4];
    for i in (0..4).rev() {
        let mut s = b[i];
        for k in i + 1..4 {
            s -= a[i][k] * x[k];
        }
        x[i] = s / a[i][i];
    }
    Some(x)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Wendet Vorwärtsmatrix auf Punkt an.
    fn apply(m: &[f32; 6], x: f32, y: f32) -> [f32; 2] {
        [m[0] * x + m[1] * y + m[2], m[3] * x + m[4] * y + m[5]]
    }

    #[test]
    fn fit_recovers_known_transform() {
        // Bekannte Trafo: Skale 2, 30°, Translation (7, -3).
        let (t, tx, ty) = (30.0f32.to_radians(), 7.0, -3.0);
        let (a, b) = (2.0 * t.cos(), 2.0 * t.sin());
        let fwd = [a, -b, tx, b, a, ty];
        // Finde Src-Punkte, die exakt aufs Template abbilden (via Invers).
        let inv = invert_similarity(&fwd).unwrap();
        let src: [[f32; 2]; 5] = ARCFACE_DST.map(|d| apply(&inv, d[0], d[1]));
        let est = estimate_similarity(&src).unwrap();
        for d in ARCFACE_DST {
            let got = apply(&est, apply(&inv, d[0], d[1])[0], apply(&inv, d[0], d[1])[1]);
            assert!((got[0] - d[0]).abs() < 1e-2 && (got[1] - d[1]).abs() < 1e-2);
        }
    }

    #[test]
    fn degenerate_landmarks_yield_none_and_black_crop() {
        let lm = Landmarks5 {
            points: [[5.0, 5.0]; 5],
        };
        assert!(estimate_similarity(&lm.points).is_none());
        let src = vec![255u8; 640 * 640 * 3];
        assert_eq!(align_face(&src, 640, 640, &lm), vec![0u8; 112 * 112 * 3]);
    }

    #[test]
    fn warp_identity_reproduces_top_left() {
        let mut src = vec![0u8; 640 * 640 * 3];
        src[0..3].copy_from_slice(&[9, 8, 7]);
        let id = [1.0, 0.0, 0.0, 0.0, 1.0, 0.0];
        let dst = warp_affine_bilinear(&src, 640, 640, &id);
        assert_eq!(&dst[0..3], &[9, 8, 7]);
        assert_eq!(dst.len(), 112 * 112 * 3);
    }
}
