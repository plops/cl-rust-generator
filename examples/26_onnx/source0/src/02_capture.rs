//! `02_capture` — X11-Regions-Screenshot via xcap (S1).
//!
//! Monitor-Auswahl, Region-Clamp auf die Monitor-Geometrie und
//! RGBA→RGB-Konvertierung. Geometrie-/Farb-Helfer sind rein und ohne
//! X-Server testbar; nur `select_monitor`/`capture_clamped` brauchen
//! ein Display. xcap-`capture_region` nimmt monitor-relative
//! Koordinaten und addiert den Monitor-Offset intern (verifiziert an
//! xcap 0.9.8 `src/linux/capture.rs`); deshalb clampen wir ebenfalls
//! monitor-relativ.

use anyhow::{bail, Context, Result};
use image::{RgbImage, RgbaImage};

/// Monitor-Geometrie in Screen-Pixeln (aus `Monitor::x/y/width/height`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MonitorGeom {
    /// Breite in px.
    pub w: u32,
    /// Hoehe in px.
    pub h: u32,
}

/// Monitor-relative, geclampte Capture-Region (immer nicht-leer).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClampedRegion {
    /// Linke Kante, monitor-relativ.
    pub x: u32,
    /// Obere Kante, monitor-relativ.
    pub y: u32,
    /// Breite (> 0).
    pub w: u32,
    /// Hoehe (> 0).
    pub h: u32,
}

/// Schiebt/schneidet die Wunsch-Region auf die Monitor-Geometrie.
///
/// Negative `x`/`y` verschieben die Kante auf 0 und kuerzen die
/// Ausdehnung; Ueberlauf am rechten/unteren Rand wird abgeschnitten.
/// Eine leere Schnittmenge (oder `w`/`h` = 0) ist ein Fehler.
pub fn clamp_region(x: i32, y: i32, w: u32, h: u32, mon: MonitorGeom) -> Result<ClampedRegion> {
    if w == 0 || h == 0 {
        bail!("capture region must be non-empty (got w={w} h={h})");
    }
    let mon_w = mon.w as i64;
    let mon_h = mon.h as i64;
    // Als [start, end) in i64, damit negative Kanten nicht unterlaufen.
    let x0 = (x as i64).clamp(0, mon_w);
    let y0 = (y as i64).clamp(0, mon_h);
    let x1 = ((x as i64) + (w as i64)).clamp(0, mon_w);
    let y1 = ((y as i64) + (h as i64)).clamp(0, mon_h);
    let cw = (x1 - x0).max(0) as u32;
    let ch = (y1 - y0).max(0) as u32;
    if cw == 0 || ch == 0 {
        bail!(
            "capture region ({x},{y} {w}x{h}) is outside monitor (0,0 {}x{})",
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

/// Wandelt RGBA-Bytes (xcap-`RgbaImage`-Layout) in RGB um (Alpha weg).
///
/// `src.len()` muss `w*h*4` sein.
pub fn rgba_to_rgb(src: &[u8], w: u32, h: u32) -> Result<Vec<u8>> {
    let expect = (w as usize) * (h as usize) * 4;
    if src.len() != expect {
        bail!(
            "rgba buffer size mismatch: got {} bytes, need {expect}",
            src.len()
        );
    }
    let mut out = Vec::with_capacity((w as usize) * (h as usize) * 3);
    let (chunks, _) = src.as_chunks::<4>();
    for px in chunks {
        out.extend_from_slice(&px[..3]);
    }
    Ok(out)
}

/// Waehlt den `--monitor`-ten Monitor aus `Monitor::all()`.
pub fn select_monitor(index: usize) -> Result<xcap::Monitor> {
    let monitors = xcap::Monitor::all().context("listing X11 monitors")?;
    monitors.into_iter().nth(index).with_context(|| {
        format!("--monitor {index} out of range (no such monitor; is X11 running?)")
    })
}

/// Fragt die Monitor-Geometrie ab (fuer Clamp + Fehlermeldungen).
pub fn monitor_geom(mon: &xcap::Monitor) -> Result<MonitorGeom> {
    Ok(MonitorGeom {
        w: mon.width().context("monitor width")?,
        h: mon.height().context("monitor height")?,
    })
}

/// Capturet die (bereits geclampte) Region als `RgbaImage`.
pub fn capture_clamped(mon: &xcap::Monitor, region: ClampedRegion) -> Result<RgbaImage> {
    mon.capture_region(region.x, region.y, region.w, region.h)
        .with_context(|| {
            format!(
                "capturing region {:?}",
                (region.x, region.y, region.w, region.h)
            )
        })
}

/// Capturet die Wunsch-Region (clampen + capturen in einem Schritt).
pub fn capture_region_clamped(
    mon: &xcap::Monitor,
    x: i32,
    y: i32,
    w: u32,
    h: u32,
) -> Result<RgbaImage> {
    let geom = monitor_geom(mon)?;
    let region = clamp_region(x, y, w, h, geom)?;
    capture_clamped(mon, region)
}

/// Bequemlichkeit: `RgbaImage` → `RgbImage` (Alpha wegwerfen).
pub fn rgba_image_to_rgb(img: &RgbaImage) -> RgbImage {
    let (w, h) = img.dimensions();
    let raw = rgba_to_rgb(img.as_raw(), w, h).expect("RgbaImage buffer is always w*h*4");
    RgbImage::from_raw(w, h, raw).expect("length was just validated")
}

#[cfg(test)]
mod tests {
    use super::*;

    const MON: MonitorGeom = MonitorGeom { w: 1920, h: 1080 };

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
    fn fully_outside_errors() {
        assert!(clamp_region(2000, 0, 100, 100, MON).is_err());
        assert!(clamp_region(0, 1200, 100, 100, MON).is_err());
        assert!(clamp_region(-500, 0, 100, 100, MON).is_err());
    }

    #[test]
    fn zero_size_errors() {
        assert!(clamp_region(0, 0, 0, 100, MON).is_err());
        assert!(clamp_region(0, 0, 100, 0, MON).is_err());
    }

    #[test]
    fn rgba_to_rgb_drops_alpha_byte_exact() {
        // 2x2: rot, gruen, blau, weiss mit verschiedenen Alphas.
        let src: Vec<u8> = vec![
            255, 0, 0, 17, 0, 255, 0, 34, 0, 0, 255, 51, 255, 255, 255, 68,
        ];
        let out = rgba_to_rgb(&src, 2, 2).unwrap();
        assert_eq!(out, vec![255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255]);
    }

    #[test]
    fn rgba_to_rgb_rejects_bad_length() {
        assert!(rgba_to_rgb(&[0u8; 15], 2, 2).is_err());
        assert!(rgba_to_rgb(&[0u8; 17], 2, 2).is_err());
    }

    #[test]
    fn rgba_image_to_rgb_roundtrip() {
        let mut img = RgbaImage::new(3, 1);
        img.put_pixel(0, 0, image::Rgba([10, 20, 30, 255]));
        img.put_pixel(1, 0, image::Rgba([40, 50, 60, 0]));
        img.put_pixel(2, 0, image::Rgba([70, 80, 90, 128]));
        let rgb = rgba_image_to_rgb(&img);
        assert_eq!(rgb.dimensions(), (3, 1));
        assert_eq!(rgb.as_raw(), &[10, 20, 30, 40, 50, 60, 70, 80, 90]);
    }
}
