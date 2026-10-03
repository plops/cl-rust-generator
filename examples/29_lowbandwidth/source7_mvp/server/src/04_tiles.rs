//! `04_tiles` — Fest-Raster-Dirty plus Text-Maskierung.
//!
//! Statt Connected Components: Der Bildschirm ist in 64px-Kacheln gerastert;
//! jede Kachel, deren Bytes sich gegenüber dem letzten Frame unterscheiden,
//! wird neu kodiert. Der Vergleich läuft zeilenweise über ganze Slices
//! (vom Compiler vektorisiert).

use image::RgbImage;

use lbw_common::{Rect, TILE};

/// Alle Raster-Kacheln, deren Bytes sich gegenüber `prev` unterscheiden.
/// `prev = None` liefert alle Kacheln (Vollbild nach Connect).
#[must_use]
pub fn dirty_tiles(prev: Option<&RgbImage>, cur: &RgbImage) -> Vec<Rect> {
    let (w, h) = cur.dimensions();
    let t = u32::from(TILE);
    let mut out = Vec::new();
    for y in (0..h).step_by(t as usize) {
        for x in (0..w).step_by(t as usize) {
            let (tw, th) = (t.min(w - x), t.min(h - y));
            let dirty = match prev {
                None => true,
                Some(p) => tile_changed(p, cur, x, y, tw, th),
            };
            if dirty {
                out.push(Rect::new(x as u16, y as u16, tw as u16, th as u16));
            }
        }
    }
    out
}

fn tile_changed(a: &RgbImage, b: &RgbImage, x: u32, y: u32, w: u32, h: u32) -> bool {
    let (stride, raw_a, raw_b) = (a.width() * 3, a.as_raw(), b.as_raw());
    for row in 0..h {
        let s = ((y + row) * stride + x * 3) as usize;
        let e = s + (w * 3) as usize;
        if raw_a[s..e] != raw_b[s..e] {
            return true;
        }
    }
    false
}

/// Füllt `r` (aufs Bild begrenzt) mit `c` — für die Text-Maskierung.
pub fn fill_rect(img: &mut RgbImage, r: Rect, c: [u8; 3]) {
    let (w, h) = img.dimensions();
    let x0 = u32::from(r.x).min(w);
    let y0 = u32::from(r.y).min(h);
    let x1 = (x0 + u32::from(r.w)).min(w);
    let y1 = (y0 + u32::from(r.h)).min(h);
    let px = image::Rgb(c);
    for y in y0..y1 {
        for x in x0..x1 {
            img.put_pixel(x, y, px);
        }
    }
}

/// Rechteck als RGB-Bytes (Zeile für Zeile, ohne Padding).
#[must_use]
pub fn crop_rgb(img: &RgbImage, r: Rect) -> Vec<u8> {
    let stride = img.width() * 3;
    let raw = img.as_raw();
    let mut out = Vec::with_capacity(r.area() as usize * 3);
    for row in 0..u32::from(r.h) {
        let s = ((u32::from(r.y) + row) * stride + u32::from(r.x) * 3) as usize;
        out.extend_from_slice(&raw[s..s + (u32::from(r.w) * 3) as usize]);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capture::solid;

    #[test]
    fn first_frame_is_full_screen() {
        let cur = solid(128, 128, [1; 3]);
        let d = dirty_tiles(None, &cur);
        assert_eq!(d.len(), 4);
        assert_eq!(d[0], Rect::new(0, 0, 64, 64));
    }

    #[test]
    fn identical_frames_have_no_dirty_tiles() {
        let a = solid(128, 128, [7; 3]);
        let b = solid(128, 128, [7; 3]);
        assert!(dirty_tiles(Some(&a), &b).is_empty());
    }

    #[test]
    fn single_pixel_dirties_exactly_one_tile() {
        let a = solid(128, 128, [0; 3]);
        let mut b = a.clone();
        b.put_pixel(100, 10, image::Rgb([9; 3]));
        assert_eq!(dirty_tiles(Some(&a), &b), vec![Rect::new(64, 0, 64, 64)]);
    }

    #[test]
    fn fill_and_crop_roundtrip() {
        let mut img = solid(8, 8, [0; 3]);
        fill_rect(&mut img, Rect::new(2, 1, 3, 2), [9, 8, 7]);
        assert_eq!(img.get_pixel(2, 1).0, [9, 8, 7]);
        assert_eq!(img.get_pixel(5, 1).0, [0; 3]);
        assert_eq!(crop_rgb(&img, Rect::new(2, 1, 3, 2)), [9, 8, 7].repeat(6));
        // Außerhalb clippen statt panicken.
        fill_rect(&mut img, Rect::new(100, 100, 5, 5), [1; 3]);
        assert_eq!(img.get_pixel(7, 7).0, [0; 3]);
    }
}
