//! `02_capture` — X11-Regions-Capture plus BGRA-Konvertierung (S2).
//!
//! 1:1-Fast-Path: Bei `size == MODEL_SIZE` läuft die fusionierte
//! Single-Pass-Schleife (`prepare_native`, aus `main.rs` übernommen,
//! kein Resize-Code). Nur bei anderen ROI-Größen läuft `resize_nearest`
//! (BGRA beliebiger Größe → 640×640-planar). Die Konvertierung ist rein
//! und ohne X11 testbar; nur `screen_size`/`capture_roi` brauchen ein Display.

use crate::view::{MODEL_SIZE, Screen, View};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

/// Bildschirmgröße aus dem X11-Setup (für ROI-Clamp).
pub fn screen_size(conn: &impl Connection, screen_idx: usize) -> Screen {
    let setup = conn.setup();
    let s = &setup.roots[screen_idx];
    Screen {
        w: s.width_in_pixels as i32,
        h: s.height_in_pixels as i32,
    }
}

/// Captured die ROI als BGRA-Bytes (`size²×4`).
pub fn capture_roi(conn: &impl Connection, root: u32, view: &View) -> Vec<u8> {
    xproto::get_image(
        conn,
        ImageFormat::Z_PIXMAP,
        root,
        view.x as i16,
        view.y as i16,
        view.size as u16,
        view.size as u16,
        u32::MAX,
    )
    .unwrap()
    .reply()
    .unwrap()
    .data
}

/// Anzahl Pixel des nativen Modell-Inputs (640²).
pub const NATIVE_PLANE: usize = MODEL_SIZE as usize * MODEL_SIZE as usize;

/// Welcher Konvertierungspfad für diese ROI gilt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConvertPath {
    /// ROI = Modell-Input: direkter Copy+Normalisierungs-Loop.
    Native,
    /// ROI ≠ Modell-Input: Nearest-Resize auf 640×640.
    Scaled,
}

/// Wählt den Pfad (reine Funktion → 1:1-Gate ist testbar).
#[must_use]
pub fn convert_path(view: &View) -> ConvertPath {
    if view.is_native() {
        ConvertPath::Native
    } else {
        ConvertPath::Scaled
    }
}

/// Wahr, wenn sich die ROI geändert hat → Caches (Change-Detector,
/// Boxen, Druck-Dedup) müssen zurückgesetzt werden.
#[must_use]
pub fn view_changed(old: &View, new: &View) -> bool {
    old != new
}

/// BGRA → RGBA-Texturbytes (Swizzle, beliebige Pixelzahl).
pub fn bgra_to_rgba(bgra: &[u8], rgba: &mut [u8]) {
    let n = (bgra.len() / 4).min(rgba.len() / 4);
    let (src, _) = bgra.as_chunks::<4>();
    let (dst, _) = rgba.as_chunks_mut::<4>();
    for (s, d) in src.iter().take(n).zip(dst.iter_mut().take(n)) {
        d.copy_from_slice(&[s[2], s[1], s[0], 255]);
    }
}

/// Fusionierter 1:1-Pfad: BGRA (640²) → planar-normalisiert + RGBA-Textur
/// in einem Durchgang (kein Resize, keine Interpolation).
pub fn prepare_native(bgra: &[u8], planes: &mut [f32], rgba: &mut [u8]) {
    const R_SCALE: f32 = 1.0 / (255.0 * 0.229);
    const R_OFF: f32 = 0.485 / 0.229;
    const G_SCALE: f32 = 1.0 / (255.0 * 0.224);
    const G_OFF: f32 = 0.456 / 0.224;
    const B_SCALE: f32 = 1.0 / (255.0 * 0.225);
    const B_OFF: f32 = 0.406 / 0.225;

    let (r_plane, rest) = planes.split_at_mut(NATIVE_PLANE);
    let (g_plane, b_plane) = rest.split_at_mut(NATIVE_PLANE);
    let (src, _) = bgra.as_chunks::<4>();
    let (dst, _) = rgba.as_chunks_mut::<4>();

    for (i, (s, d)) in src
        .iter()
        .zip(dst.iter_mut())
        .take(NATIVE_PLANE)
        .enumerate()
    {
        let (b, g, r) = (s[0], s[1], s[2]);
        d.copy_from_slice(&[r, g, b, 255]);
        r_plane[i] = r as f32 * R_SCALE - R_OFF;
        g_plane[i] = g as f32 * G_SCALE - G_OFF;
        b_plane[i] = b as f32 * B_SCALE - B_OFF;
    }
}

/// Nearest-Resize: BGRA der Kantenlänge `src_size` → planar-normalisiert
/// der Kantenlänge `dst_size` (für den echten Pfad: beliebig → 640).
/// 1:1 (`src_size == dst_size`) ist exakt identisch zu `prepare_native`.
pub fn resize_nearest_planar(bgra: &[u8], src_size: usize, dst: &mut [f32], dst_size: usize) {
    const R_SCALE: f32 = 1.0 / (255.0 * 0.229);
    const R_OFF: f32 = 0.485 / 0.229;
    const G_SCALE: f32 = 1.0 / (255.0 * 0.224);
    const G_OFF: f32 = 0.456 / 0.224;
    const B_SCALE: f32 = 1.0 / (255.0 * 0.225);
    const B_OFF: f32 = 0.406 / 0.225;

    let plane = dst_size * dst_size;
    let (r_plane, rest) = dst.split_at_mut(plane);
    let (g_plane, b_plane) = rest.split_at_mut(plane);

    for dy in 0..dst_size {
        let sy = (dy * src_size / dst_size).min(src_size - 1);
        for dx in 0..dst_size {
            let sx = (dx * src_size / dst_size).min(src_size - 1);
            let s = (sy * src_size + sx) * 4;
            let d = dy * dst_size + dx;
            r_plane[d] = bgra[s + 2] as f32 * R_SCALE - R_OFF;
            g_plane[d] = bgra[s + 1] as f32 * G_SCALE - G_OFF;
            b_plane[d] = bgra[s] as f32 * B_SCALE - B_OFF;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::View;

    fn pattern_bgra(pixels: usize) -> Vec<u8> {
        (0..pixels * 4)
            .map(|i| (i.wrapping_mul(37) % 251) as u8)
            .collect()
    }

    #[test]
    fn path_is_native_at_640() {
        assert_eq!(convert_path(&View::default()), ConvertPath::Native);
        let mut v = View::default();
        v.size = 320;
        assert_eq!(convert_path(&v), ConvertPath::Scaled);
    }

    #[test]
    fn view_change_detection() {
        let a = View::default();
        assert!(!view_changed(&a, &a));
        let mut b = a;
        b.x += 1;
        assert!(view_changed(&a, &b));
        let mut c = a;
        c.size = 320;
        assert!(view_changed(&a, &c));
    }

    #[test]
    fn bgra_to_rgba_swizzles_exact() {
        let bgra = vec![10, 20, 30, 99, 40, 50, 60, 77];
        let mut rgba = vec![0u8; 8];
        bgra_to_rgba(&bgra, &mut rgba);
        assert_eq!(rgba, vec![30, 20, 10, 255, 60, 50, 40, 255]);
    }

    #[test]
    fn nearest_2x_upscale_is_exact() {
        // 2x2 BGRA -> 4x4 planar: jedes Quellpixel wird ein 2x2-Block.
        let bgra: Vec<u8> = vec![
            0, 0, 255, 0, 0, 255, 0, 0, //
            255, 0, 0, 0, 255, 255, 255, 0,
        ];
        let mut dst = vec![0.0f32; 3 * 16];
        resize_nearest_planar(&bgra, 2, &mut dst, 4);
        // Rot (B=0,G=0,R=255) oben links als 2x2-Block in der R-Plane.
        let r = (255.0f32 / 255.0 - 0.485) / 0.229;
        assert!((dst[0] - r).abs() < 1e-5);
        assert!((dst[1] - r).abs() < 1e-5);
        assert!((dst[4] - r).abs() < 1e-5);
        assert!((dst[5] - r).abs() < 1e-5);
        // Grün oben rechts.
        let g = (255.0f32 / 255.0 - 0.456) / 0.224;
        assert!((dst[2] - r).abs() > 1.0); // dort liegt kein Rot
        assert!((dst[16 + 2] - g).abs() < 1e-5);
    }

    #[test]
    fn native_path_matches_nearest_at_1x1() {
        // 1:1-Gate: Fast-Path und generischer Pfad liefern bit-identische Planes.
        let bgra = pattern_bgra(NATIVE_PLANE);
        let mut planes_a = vec![0.0f32; 3 * NATIVE_PLANE];
        let mut planes_b = vec![0.0f32; 3 * NATIVE_PLANE];
        let mut rgba_a = vec![0u8; 4 * NATIVE_PLANE];
        prepare_native(&bgra, &mut planes_a, &mut rgba_a);
        resize_nearest_planar(
            &bgra,
            MODEL_SIZE as usize,
            &mut planes_b,
            MODEL_SIZE as usize,
        );
        assert_eq!(planes_a, planes_b);
    }
}
