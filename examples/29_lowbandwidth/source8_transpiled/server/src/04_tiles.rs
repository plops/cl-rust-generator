//! `04_tiles` — Änderungserkennung als einzelne Bounding Box plus Text-Maskierung.
//!
//! Statt eines Kachelrasters: Die kleinste Box über allen geänderten Pixeln
//! geht als genau ein AV1-Still-Picture raus — ein Header-Overhead pro Frame
//! (~50 B) statt N×, den unveränderten Rest in der Box komprimiert AV1 als
//! Skip-Blöcke. Der Vergleich läuft zeilenweise über ganze Slices (vom
//! Compiler vektorisiert); nur geänderte Zeilen werden pixelweise vermessen.

use image::RgbImage;

use lbw_common::Rect;

use crate::av1::MIN_TILE;

/// Kleinste Box über allen Änderungen; `prev = None` liefert Vollbild.
/// Kanten werden gerade und mindestens [`MIN_TILE`] (rav1e-Bedingung);
/// `None` heißt Standbild (Funkstille). Beide Bilder müssen gleich groß sein
/// und gerade Kanten ≥ [`MIN_TILE`] haben (640×640 tut das).
#[must_use]
pub fn dirty_bbox(prev: Option<&RgbImage>, cur: &RgbImage) -> Option<Rect> {
    let (w, h) = cur.dimensions();
    {
        let prev = match prev {
            Some(p) => p,
            None => return Some(Rect::new(0, 0, w as u16, h as u16)),
        };
        debug_assert_eq!((prev.width(), prev.height()), (w, h));
        debug_assert!(w >= MIN_TILE as u32 && h >= MIN_TILE as u32 && w % 2 == 0 && h % 2 == 0);
        {
            let stride = w as usize * 3;
            {
                let (a, b) = (prev.as_raw(), cur.as_raw());
                let (mut x0, mut x1, mut y0, mut y1) = (w, 0, h, 0);
                for y in 0..h {
                    let s = y as usize * stride;
                    {
                        let (ra, rb) = (&a[s..s + stride], &b[s..s + stride]);
                        if ra == rb {
                            continue;
                        }
                        y0 = y0.min(y);
                        y1 = y1.max(y);
                        for x in 0..w {
                            let o = x as usize * 3;
                            if ra[o..o + 3] != rb[o..o + 3] {
                                x0 = x0.min(x);
                                break;
                            }
                        }
                        for x in (0..w).rev() {
                            let o = x as usize * 3;
                            if ra[o..o + 3] != rb[o..o + 3] {
                                x1 = x1.max(x);
                                break;
                            }
                        }
                    }
                }
                if x0 > x1 {
                    return None;
                }
                // Auf Mindestgröße und gerade Kanten erweitern, im Bild halten.
                {
                    let t = MIN_TILE as u32;
                    {
                        let mut bw = ((x1 - x0) + 1).max(t).min(w);
                        let mut bh = ((y1 - y0) + 1).max(t).min(h);
                        bw += bw & 1;
                        bh += bh & 1;
                        {
                            let (bw, bh) = (bw.min(w), bh.min(h));
                            {
                                let bx = x0.min(w - bw);
                                let by = y0.min(h - bh);
                                Some(Rect::new(bx as u16, by as u16, bw as u16, bh as u16))
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Maskierungs-Zuschlag je Seite (zusätzlich zum Erkennungs-Padding im
/// `TextItem`-Rechteck): löscht Glyphen-Fransen, die sonst als AV1-Reste
/// Bandbreite kosten. Per Xvfb/xterm-Sweep bestimmt (vgl. `tests/padding.rs`):
/// 6 entfernt die Fransensäume beider Testfonts; größere Werte sparen nur noch
/// dadurch, dass sie den benachbarten Textcursor verschlucken — das bleibt
/// sichtbar, darum ist hier Schluss.
pub const MASK_PAD: u16 = 6;

/// Weitet `r` um `pad` Pixel je Seite auf (im `w`×`h`-Bild gehalten).
/// Detektions-Boxen schneiden Glyphen haarscharf ab — ohne Rand leidet die
/// Erkennung und Fransensäume bleiben als AV1-Reste stehen.
#[must_use]
pub fn pad_rect(r: Rect, pad: u16, w: u32, h: u32) -> Rect {
    let x0 = u32::from(r.x).saturating_sub(u32::from(pad));
    {
        let y0 = u32::from(r.y).saturating_sub(u32::from(pad));
        {
            let x1 = (u32::from(r.x) + u32::from(r.w) + u32::from(pad)).min(w);
            {
                let y1 = (u32::from(r.y) + u32::from(r.h) + u32::from(pad)).min(h);
                Rect::new(x0 as u16, y0 as u16, (x1 - x0) as u16, (y1 - y0) as u16)
            }
        }
    }
}

/// Füllt `r` (aufs Bild begrenzt) mit `c` — für die Text-Maskierung.
pub fn fill_rect(img: &mut RgbImage, r: Rect, c: [u8; 3]) {
    let (w, h) = img.dimensions();
    {
        let x0 = u32::from(r.x).min(w);
        {
            let y0 = u32::from(r.y).min(h);
            {
                let x1 = (x0 + u32::from(r.w)).min(w);
                {
                    let y1 = (y0 + u32::from(r.h)).min(h);
                    {
                        let px = image::Rgb(c);
                        for y in y0..y1 {
                            for x in x0..x1 {
                                img.put_pixel(x, y, px)
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Rechteck als RGB-Bytes (Zeile für Zeile, ohne Padding).
#[must_use]
pub fn crop_rgb(img: &RgbImage, r: Rect) -> Vec<u8> {
    let stride = img.width() * 3;
    {
        let raw = img.as_raw();
        {
            let mut out = Vec::with_capacity(r.area() as usize * 3);
            for row in 0..u32::from(r.h) {
                let s = ((u32::from(r.y) + row) * stride + u32::from(r.x) * 3) as usize;
                out.extend_from_slice(&raw[s..s + (u32::from(r.w) * 3) as usize])
            }
            out
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capture::solid;

    #[test]
    fn first_frame_is_full_screen() {
        let cur = solid(128, 128, [1; 3]);
        assert_eq!(dirty_bbox(None, &cur), Some(Rect::new(0, 0, 128, 128)))
    }

    #[test]
    fn identical_frames_are_silent() {
        let a = solid(128, 128, [7; 3]);
        {
            let b = solid(128, 128, [7; 3]);
            assert_eq!(dirty_bbox(Some(&a), &b), None)
        }
    }

    #[test]
    fn scattered_pixels_yield_single_box() {
        let a = solid(128, 128, [0; 3]);
        {
            let mut b = a.clone();
            b.put_pixel(10, 10, image::Rgb([9; 3]));
            b.put_pixel(100, 100, image::Rgb([9; 3]));
            {
                // 10..=100 → 91 px, auf gerade Kanten erweitert.
                assert_eq!(dirty_bbox(Some(&a), &b), Some(Rect::new(10, 10, 92, 92)))
            }
        }
    }

    #[test]
    fn single_pixel_is_padded_to_minimum() {
        let a = solid(128, 128, [0; 3]);
        {
            let mut b = a.clone();
            b.put_pixel(5, 5, image::Rgb([9; 3]));
            assert_eq!(dirty_bbox(Some(&a), &b), Some(Rect::new(5, 5, 16, 16)))
        }
    }

    #[test]
    fn box_clamps_at_image_edge() {
        let a = solid(128, 128, [0; 3]);
        {
            let mut b = a.clone();
            b.put_pixel(127, 127, image::Rgb([9; 3]));
            assert_eq!(dirty_bbox(Some(&a), &b), Some(Rect::new(112, 112, 16, 16)))
        }
    }

    #[test]
    fn pad_expands_and_clamps() {
        assert_eq!(
            pad_rect(Rect::new(10, 10, 20, 8), 4, 128, 128),
            Rect::new(6, 6, 28, 16)
        );
        assert_eq!(
            pad_rect(Rect::new(10, 10, 20, 8), 0, 128, 128),
            Rect::new(10, 10, 20, 8)
        );
        {
            // Am Rand klemmen statt überlaufen.
            assert_eq!(
                pad_rect(Rect::new(0, 0, 10, 10), 4, 128, 128),
                Rect::new(0, 0, 14, 14)
            )
        }
        assert_eq!(
            pad_rect(Rect::new(120, 120, 8, 8), 4, 128, 128),
            Rect::new(116, 116, 12, 12)
        )
    }

    #[test]
    fn fill_and_crop_roundtrip() {
        let mut img = solid(8, 8, [0; 3]);
        fill_rect(&mut img, Rect::new(2, 1, 3, 2), [9, 8, 7]);
        assert_eq!(img.get_pixel(2, 1).0, [9, 8, 7]);
        assert_eq!(img.get_pixel(5, 1).0, [0; 3]);
        assert_eq!(crop_rgb(&img, Rect::new(2, 1, 3, 2)), [9, 8, 7].repeat(6));
        {
            // Außerhalb clippen statt panicken.
            fill_rect(&mut img, Rect::new(100, 100, 5, 5), [1; 3])
        }
        assert_eq!(img.get_pixel(7, 7).0, [0; 3])
    }
}
