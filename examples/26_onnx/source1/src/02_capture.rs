//! `02_capture` — X11-Regions-Screenshot via x11rb (source1).
//!
//! Reine X11-Verbindung ohne xcap: `GetImage` (ZPixmap) aufs Root-Window
//! des gewaehlten Screens, BGRX→RGB-Konvertierung. `--monitor` waehlt
//! den X-Screen-Index. Geometrie-/Farb-Helfer sind rein und ohne
//! X-Server testbar.

use anyhow::{Context, Result, bail};
use image::{RgbImage, RgbaImage};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::ImageFormat;

/// Screen-Geometrie in Pixeln.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScreenGeom {
    /// Breite in px.
    pub w: u32,
    /// Hoehe in px.
    pub h: u32,
}

/// Screen-relative, geclampte Capture-Region (immer nicht-leer).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClampedRegion {
    /// Linke Kante, screen-relativ.
    pub x: u32,
    /// Obere Kante, screen-relativ.
    pub y: u32,
    /// Breite (> 0).
    pub w: u32,
    /// Hoehe (> 0).
    pub h: u32,
}

/// Schiebt/schneidet die Wunsch-Region auf die Screen-Geometrie
/// (gleiche Semantik wie source0: negativ → 0 + kuerzen, Ueberlauf
/// abschneiden, leere Schnittmenge → Fehler).
pub fn clamp_region(x: i32, y: i32, w: u32, h: u32, mon: ScreenGeom) -> Result<ClampedRegion> {
    if w == 0 || h == 0 {
        bail!("capture region must be non-empty (got w={w} h={h})");
    }
    let mon_w = mon.w as i64;
    let mon_h = mon.h as i64;
    let x0 = (x as i64).clamp(0, mon_w);
    let y0 = (y as i64).clamp(0, mon_h);
    let x1 = ((x as i64) + (w as i64)).clamp(0, mon_w);
    let y1 = ((y as i64) + (h as i64)).clamp(0, mon_h);
    let cw = (x1 - x0).max(0) as u32;
    let ch = (y1 - y0).max(0) as u32;
    if cw == 0 || ch == 0 {
        bail!(
            "capture region ({x},{y} {w}x{h}) is outside screen (0,0 {}x{})",
            mon.w,
            mon.h
        );
    }
    Ok(ClampedRegion {
        x: x0 as u32,
        y: y0 as u32,
        w: cw,
        h: ch,
    })
}

/// Wandelt BGRX-Bytes (Depth-24-ZPixmap, little-endian: B,G,R,pad)
/// in RGB um. `src.len()` muss `w*h*4` sein.
pub fn bgrx_to_rgb(src: &[u8], w: u32, h: u32) -> Result<Vec<u8>> {
    let expect = (w as usize) * (h as usize) * 4;
    if src.len() != expect {
        bail!(
            "bgrx buffer size mismatch: got {} bytes, need {expect}",
            src.len()
        );
    }
    let mut out = Vec::with_capacity((w as usize) * (h as usize) * 3);
    let (chunks, _) = src.as_chunks::<4>();
    for px in chunks {
        out.extend_from_slice(&[px[2], px[1], px[0]]);
    }
    Ok(out)
}

/// X11-Verbindung plus gewaehlter Screen.
pub struct Capturer {
    conn: x11rb::rust_connection::RustConnection,
    root: u32,
    geom: ScreenGeom,
}

impl Capturer {
    /// Verbindet zum Display (`None` = `$DISPLAY`) und waehlt den
    /// `--monitor`-ten X-Screen.
    pub fn connect(screen: usize) -> Result<Self> {
        let (conn, _) =
            x11rb::connect(None).context("connecting to X11 (is a display available?)")?;
        // Werte kopieren, bevor `conn` bewegt wird (`setup()` leiht).
        let (root, geom) = {
            let info = conn.setup();
            let n = info.roots.len();
            let s = info.roots.get(screen).with_context(|| {
                format!("--monitor {screen} out of range ({n} X screens; is X11 running?)")
            })?;
            (
                s.root,
                ScreenGeom {
                    w: u32::from(s.width_in_pixels),
                    h: u32::from(s.height_in_pixels),
                },
            )
        };
        Ok(Self { conn, root, geom })
    }

    /// Screen-Geometrie (fuer Clamp + Meldungen).
    #[must_use]
    pub fn geom(&self) -> ScreenGeom {
        self.geom
    }

    /// Capturet die (bereits geclampte) Region als `RgbaImage` (Alpha 255).
    pub fn capture(&self, region: ClampedRegion) -> Result<RgbaImage> {
        let cookie = x11rb::protocol::xproto::get_image(
            &self.conn,
            ImageFormat::Z_PIXMAP,
            self.root,
            region.x as i16,
            region.y as i16,
            region.w as u16,
            region.h as u16,
            u32::MAX,
        );
        let reply = cookie
            .context("GetImage request")?
            .reply()
            .context("GetImage reply")?;
        if reply.depth != 24 {
            bail!("unsupported root depth {} (need 24)", reply.depth);
        }
        let rgb = bgrx_to_rgb(&reply.data, region.w, region.h)?;
        let mut rgba = Vec::with_capacity((region.w as usize) * (region.h as usize) * 4);
        let (chunks, _) = rgb.as_chunks::<3>();
        for px in chunks {
            rgba.extend_from_slice(&[px[0], px[1], px[2], 255]);
        }
        RgbaImage::from_raw(region.w, region.h, rgba).context("building RgbaImage")
    }

    /// Capturet die Wunsch-Region (clampen + capturen in einem Schritt).
    pub fn capture_region_clamped(&self, x: i32, y: i32, w: u32, h: u32) -> Result<RgbaImage> {
        let region = clamp_region(x, y, w, h, self.geom)?;
        self.capture(region)
    }
}

/// Bequemlichkeit: `RgbaImage` → `RgbImage` (Alpha wegwerfen).
pub fn rgba_image_to_rgb(img: &RgbaImage) -> RgbImage {
    let (w, h) = img.dimensions();
    let raw: Vec<u8> = img.pixels().flat_map(|p| [p[0], p[1], p[2]]).collect();
    RgbImage::from_raw(w, h, raw).expect("length matches w*h*3 by construction")
}

#[cfg(test)]
mod tests {
    use super::*;

    const MON: ScreenGeom = ScreenGeom { w: 1920, h: 1080 };

    #[test]
    fn inside_region_unchanged() {
        let r = clamp_region(10, 20, 800, 600, MON).unwrap();
        assert_eq!(
            r,
            ClampedRegion {
                x: 10,
                y: 20,
                w: 800,
                h: 600
            }
        );
    }

    #[test]
    fn negative_origin_shifts_and_shrinks() {
        let r = clamp_region(-50, -30, 400, 300, MON).unwrap();
        assert_eq!(
            r,
            ClampedRegion {
                x: 0,
                y: 0,
                w: 350,
                h: 270
            }
        );
    }

    #[test]
    fn overflow_is_cut() {
        let r = clamp_region(1800, 1000, 400, 300, MON).unwrap();
        assert_eq!(
            r,
            ClampedRegion {
                x: 1800,
                y: 1000,
                w: 120,
                h: 80
            }
        );
    }

    #[test]
    fn fully_outside_or_empty_errors() {
        assert!(clamp_region(2000, 0, 100, 100, MON).is_err());
        assert!(clamp_region(0, 0, 0, 100, MON).is_err());
    }

    #[test]
    fn bgrx_to_rgb_swaps_byte_exact() {
        // 2x1: [B,G,R,X]-Pixel rot und weiss.
        let src: Vec<u8> = vec![0, 0, 255, 0, 255, 255, 255, 0];
        let out = bgrx_to_rgb(&src, 2, 1).unwrap();
        assert_eq!(out, vec![255, 0, 0, 255, 255, 255]);
    }

    #[test]
    fn bgrx_to_rgb_rejects_bad_length() {
        assert!(bgrx_to_rgb(&[0u8; 7], 2, 1).is_err());
    }

    #[test]
    fn rgba_image_to_rgb_roundtrip() {
        let mut img = RgbaImage::new(2, 1);
        img.put_pixel(0, 0, image::Rgba([10, 20, 30, 255]));
        img.put_pixel(1, 0, image::Rgba([40, 50, 60, 0]));
        let rgb = rgba_image_to_rgb(&img);
        assert_eq!(rgb.as_raw(), &[10, 20, 30, 40, 50, 60]);
    }
}
