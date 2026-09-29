//! `03_letterbox` — Ultralytics-kompatibles LetterBox-Preprocessing:
//! Skala `r = min(H/h, W/w)`, Größe `round(w·r)` (Banker's Rounding wie
//! Python), bilinear mit halbpixel-zentrierter Abtastung (wie
//! `cv2.INTER_LINEAR`), zentriert, Pad 114, RGB/255, planar CHW.

use crate::image::Rgb;

/// Graufüllung der Ränder (Ultralytics-Default).
const PAD: f32 = 114.0 / 255.0;

/// Geometrie einer Abbildung Bild → Modell-Input.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Letterbox {
    /// Modell-Input-Breite/-Höhe.
    pub dst_w: usize,
    pub dst_h: usize,
    /// Quellbildgröße.
    pub src_w: usize,
    pub src_h: usize,
    /// Skalierung Quelle → Input.
    pub r: f32,
    /// Skalierte Inhaltsgröße.
    pub nw: usize,
    pub nh: usize,
    /// Linker/oberer Rand in Input-Pixeln.
    pub left: usize,
    pub top: usize,
}

impl Letterbox {
    #[must_use]
    pub fn new(src_w: usize, src_h: usize, dst_w: usize, dst_h: usize) -> Self {
        let r = (dst_h as f64 / src_h as f64).min(dst_w as f64 / src_w as f64);
        let nw = (src_w as f64 * r).round_ties_even() as usize;
        let nh = (src_h as f64 * r).round_ties_even() as usize;
        let dw = (dst_w - nw) as f64 / 2.0;
        let dh = (dst_h - nh) as f64 / 2.0;
        Self {
            dst_w,
            dst_h,
            src_w,
            src_h,
            r: r as f32,
            nw,
            nh,
            left: (dw - 0.1).round_ties_even() as usize,
            top: (dh - 0.1).round_ties_even() as usize,
        }
    }

    /// Schreibt das Bild als planaren f32-Tensor `[3, dst_h, dst_w]`.
    pub fn fill(&self, img: &Rgb, out: &mut [f32]) {
        assert_eq!((img.w, img.h), (self.src_w, self.src_h));
        let plane = self.dst_w * self.dst_h;
        assert!(out.len() >= 3 * plane);
        out[..3 * plane].fill(PAD);
        let xs = taps(self.src_w, self.nw);
        let ys = taps(self.src_h, self.nh);
        let stride = img.w * 3;
        for (dy, &(y0, y1, wy)) in ys.iter().enumerate() {
            let row0 = &img.data[y0 * stride..(y0 + 1) * stride];
            let row1 = &img.data[y1 * stride..(y1 + 1) * stride];
            let base = (self.top + dy) * self.dst_w + self.left;
            for (dx, &(x0, x1, wx)) in xs.iter().enumerate() {
                for c in 0..3 {
                    let a = f32::from(row0[x0 * 3 + c]);
                    let b = f32::from(row0[x1 * 3 + c]);
                    let p = f32::from(row1[x0 * 3 + c]);
                    let q = f32::from(row1[x1 * 3 + c]);
                    let top = a + (b - a) * wx;
                    let bot = p + (q - p) * wx;
                    out[c * plane + base + dx] = (top + (bot - top) * wy) / 255.0;
                }
            }
        }
    }

    /// Box aus Input-Koordinaten (`x1,y1,x2,y2`) zurück ins Quellbild, geclippt.
    #[must_use]
    pub fn to_source(&self, b: [f32; 4]) -> [f32; 4] {
        let (w, h) = (self.src_w as f32, self.src_h as f32);
        let fx = |v: f32| ((v - self.left as f32) / self.r).clamp(0.0, w);
        let fy = |v: f32| ((v - self.top as f32) / self.r).clamp(0.0, h);
        [fx(b[0]), fy(b[1]), fx(b[2]), fy(b[3])]
    }
}

/// Bilinear-Stützstellen pro Zielindex: `(i0, i1, gewicht_i1)`.
fn taps(src: usize, dst: usize) -> Vec<(usize, usize, f32)> {
    let scale = src as f32 / dst as f32;
    (0..dst)
        .map(|d| {
            let f = ((d as f32 + 0.5) * scale - 0.5).max(0.0);
            let i0 = (f as usize).min(src - 1);
            let i1 = (i0 + 1).min(src - 1);
            (i0, i1, f - i0 as f32)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_hd_into_square() {
        let lb = Letterbox::new(1920, 1080, 640, 640);
        assert!((lb.r - 1.0 / 3.0).abs() < 1e-6);
        assert_eq!((lb.nw, lb.nh, lb.left, lb.top), (640, 360, 0, 140));
    }

    #[test]
    fn full_hd_into_rect_has_small_pad() {
        let lb = Letterbox::new(1920, 1080, 640, 384);
        assert_eq!((lb.nw, lb.nh, lb.left, lb.top), (640, 360, 0, 12));
    }

    #[test]
    fn odd_padding_rounds_like_ultralytics() {
        // dh = 0.5 → top = round(0.4) = 0 (unten bekommt den Rest).
        let lb = Letterbox::new(100, 99, 100, 100);
        assert_eq!((lb.nh, lb.top), (99, 0));
    }

    #[test]
    fn box_roundtrip() {
        let lb = Letterbox::new(1920, 1080, 640, 640);
        let src = lb.to_source([10.0, 150.0, 20.0, 160.0]);
        assert_eq!(src, [30.0, 30.0, 60.0, 60.0]);
        // Außerhalb des Inhalts wird geclippt.
        assert_eq!(lb.to_source([0.0, 0.0, 640.0, 640.0])[3], 1080.0);
    }

    #[test]
    fn constant_image_stays_constant_and_pads_gray() {
        let img = Rgb::filled(300, 150, [51, 102, 255]);
        let lb = Letterbox::new(300, 150, 64, 64);
        let mut out = vec![0.0; 3 * 64 * 64];
        lb.fill(&img, &mut out);
        let plane = 64 * 64;
        let inside = (lb.top + 1) * 64 + 5;
        assert!((out[inside] - 0.2).abs() < 1e-6);
        assert!((out[plane + inside] - 0.4).abs() < 1e-6);
        assert!((out[2 * plane + inside] - 1.0).abs() < 1e-6);
        assert!((out[0] - PAD).abs() < 1e-6); // Rand oben
    }

    #[test]
    fn identity_size_copies_pixels() {
        let mut img = Rgb::filled(4, 4, [0; 3]);
        img.put(2, 1, [255, 0, 0]);
        let lb = Letterbox::new(4, 4, 4, 4);
        let mut out = vec![0.0; 3 * 16];
        lb.fill(&img, &mut out);
        assert_eq!(out[4 + 2], 1.0);
        assert_eq!(out[4 + 1], 0.0);
    }
}
