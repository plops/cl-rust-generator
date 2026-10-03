//! `03_yuv` — Farbraum-Umrechnung RGB ↔ YUV 4:2:0 (BT.601, Full Range).
//!
//! Aus `source6/common/05_yuv.rs` übernommen: Server (Encoder) und Client
//! (Decoder) nutzen exakt dieselben Formeln, damit keine Farbverschiebung
//! entsteht. Integer-Arithmetik (Q8), keine Abhängigkeiten.

/// Drei YUV-Ebenen; `u`/`v` haben halbe Auflösung (aufgerundet).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Yuv420 {
    pub w: usize,
    pub h: usize,
    pub y: Vec<u8>,
    pub u: Vec<u8>,
    pub v: Vec<u8>,
}

impl Yuv420 {
    /// Breite der Chroma-Ebenen.
    #[must_use]
    pub fn cw(&self) -> usize {
        self.w.div_ceil(2)
    }
}

fn clamp8(v: i32) -> u8 {
    v.clamp(0, 255) as u8
}

/// Ein RGB-Pixel → (Y, U, V), BT.601 Full Range.
#[must_use]
pub fn rgb_to_yuv(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
    let (r, g, b) = (i32::from(r), i32::from(g), i32::from(b));
    {
        let y = (77 * r + 150 * g + 29 * b + 128) >> 8;
        let u = (((-43) * r + (-85) * g + 128 * b + 128) >> 8) + 128;
        let v = ((128 * r + (-107) * g + (-21) * b + 128) >> 8) + 128;
        (clamp8(y), clamp8(u), clamp8(v))
    }
}

/// (Y, U, V) → RGB, Umkehrung von [`rgb_to_yuv`].
#[must_use]
pub fn yuv_to_rgb(y: u8, u: u8, v: u8) -> [u8; 3] {
    let (y, u, v) = (i32::from(y), i32::from(u) - 128, i32::from(v) - 128);
    {
        let r = y + ((359 * v + 128) >> 8);
        let g = y - ((88 * u + 183 * v + 128) >> 8);
        let b = y + ((454 * u + 128) >> 8);
        [clamp8(r), clamp8(g), clamp8(b)]
    }
}

/// Interleaved RGB8 (`w*h*3`) → YUV 4:2:0; Chroma = Mittel über 2×2.
#[must_use]
pub fn rgb_to_yuv420(rgb: &[u8], w: usize, h: usize) -> Yuv420 {
    assert!(rgb.len() >= w * h * 3);
    {
        let (cw, ch) = (w.div_ceil(2), h.div_ceil(2));
        {
            let mut out = Yuv420 {
                w,
                h,
                y: vec![0; w * h],
                u: vec![0; cw * ch],
                v: vec![0; cw * ch],
            };
            let mut usum = vec![0u32; cw * ch];
            let mut vsum = vec![0u32; cw * ch];
            let mut cnt = vec![0u32; cw * ch];
            for yy in 0..h {
                for xx in 0..w {
                    let i = (yy * w + xx) * 3;
                    {
                        let (y, u, v) = rgb_to_yuv(rgb[i], rgb[i + 1], rgb[i + 2]);
                        out.y[yy * w + xx] = y;
                        {
                            let c = (yy / 2) * cw + xx / 2;
                            usum[c] += u32::from(u);
                            vsum[c] += u32::from(v);
                            cnt[c] += 1
                        }
                    }
                }
            }
            for c in 0..cw * ch {
                out.u[c] = ((usum[c] + cnt[c] / 2) / cnt[c]) as u8;
                out.v[c] = ((vsum[c] + cnt[c] / 2) / cnt[c]) as u8
            }
            out
        }
    }
}

/// YUV-Ebenen mit beliebigen Strides → RGBA8 (`w*h*4`, Alpha 255).
#[allow(clippy::too_many_arguments)]
pub fn yuv420_to_rgba(
    y: &[u8],
    ys: usize,
    u: &[u8],
    v: &[u8],
    cs: usize,
    w: usize,
    h: usize,
    rgba: &mut [u8],
) {
    for yy in 0..h {
        for xx in 0..w {
            let c = (yy / 2) * cs + xx / 2;
            {
                let [r, g, b] = yuv_to_rgb(y[yy * ys + xx], u[c], v[c]);
                {
                    let o = (yy * w + xx) * 4;
                    rgba[o..o + 4].copy_from_slice(&[r, g, b, 255])
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_error_is_small() {
        // Grobes Raster über den RGB-Würfel: Hin-/Rückweg max. ±3.
        for r in (0..=255).step_by(15) {
            for g in (0..=255).step_by(15) {
                for b in (0..=255).step_by(15) {
                    let (y, u, v) = rgb_to_yuv(r as u8, g as u8, b as u8);
                    {
                        let back = yuv_to_rgb(y, u, v);
                        for (a, o) in [r, g, b].iter().zip(back) {
                            assert!((a - i32::from(o)).abs() <= 3, "{r},{g},{b} -> {back:?}")
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn grey_has_neutral_chroma() {
        assert_eq!(rgb_to_yuv(0, 0, 0), (0, 128, 128));
        assert_eq!(rgb_to_yuv(255, 255, 255), (255, 128, 128))
    }

    #[test]
    fn odd_size_planes() {
        let rgb = vec![200u8; 3 * 3 * 3];
        let yuv = rgb_to_yuv420(&rgb, 3, 3);
        assert_eq!((yuv.y.len(), yuv.u.len(), yuv.cw()), (9, 4, 2));
        {
            let mut rgba = vec![0u8; 3 * 3 * 4];
            yuv420_to_rgba(&yuv.y, 3, &yuv.u, &yuv.v, 2, 3, 3, &mut rgba);
            assert!(
                rgba.chunks(4)
                    .all(|p| { p[0].abs_diff(200) <= 2 && p[3] == 255 })
            )
        }
    }
}
