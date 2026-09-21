//! `05_view` — Anzeige via macroquad (source1).
//!
//! Display-freie Helfer (Zielgroesse, Nearest-Blit, PNG-Speichern) sind
//! rein und ohne GPU testbar (identisch zu source0). `run_window_mq`
//! oeffnet das macroquad-Fenster (braucht Display + GL via miniquad);
//! `--headless`/`--save-frame` umgehen es.

use anyhow::{Context, Result, bail};
use std::path::Path;
use std::time::Duration;

/// Berechnet die Fenster-/Frame-Groesse: explizite `--win-w/--win-h`
/// gewinnen, sonst Capture-Groesse × `--zoom`.
pub fn zoomed_size(
    src_w: u32,
    src_h: u32,
    zoom: u32,
    win_w: Option<u32>,
    win_h: Option<u32>,
) -> Result<(u32, u32)> {
    if let (Some(w), Some(h)) = (win_w, win_h) {
        if w == 0 || h == 0 {
            bail!("window size must be non-empty");
        }
        return Ok((w, h));
    }
    if zoom == 0 {
        bail!("--zoom must be >= 1");
    }
    Ok((src_w * zoom, src_h * zoom))
}

/// Nearest-Neighbor-Blit RGBA → RGBA (Hochskalierung, z. B. Zoom).
pub fn blit_nearest(src: &[u8], sw: u32, sh: u32, dst: &mut [u8], dw: u32, dh: u32) -> Result<()> {
    if src.len() != (sw as usize) * (sh as usize) * 4 {
        bail!("source buffer size mismatch");
    }
    if dst.len() != (dw as usize) * (dh as usize) * 4 {
        bail!("destination buffer size mismatch");
    }
    if sw == 0 || sh == 0 || dw == 0 || dh == 0 {
        bail!("blit dimensions must be non-empty");
    }
    for y in 0..dh {
        let sy = (y as u64 * sh as u64 / dh as u64) as usize;
        for x in 0..dw {
            let sx = (x as u64 * sw as u64 / dw as u64) as usize;
            let s = (sy * sw as usize + sx) * 4;
            let d = ((y * dw + x) as usize) * 4;
            dst[d..d + 4].copy_from_slice(&src[s..s + 4]);
        }
    }
    Ok(())
}

/// Schreibt ein RGBA-Frame als PNG (fuer `--save-frame`, ohne Display).
pub fn save_frame_png(path: &Path, rgba: &[u8], w: u32, h: u32) -> Result<()> {
    if rgba.len() != (w as usize) * (h as usize) * 4 {
        bail!("frame buffer size mismatch");
    }
    image::save_buffer(path, rgba, w, h, image::ColorType::Rgba8)
        .with_context(|| format!("writing {}", path.display()))?;
    Ok(())
}

/// Frame-Periode aus der Framerate (intern auf 1..=30 geclampt).
#[must_use]
pub fn frame_period(fps: u32) -> Duration {
    Duration::from_secs_f64(1.0 / f64::from(fps.clamp(1, 30)))
}

/// Ein annotierter Frame im macroquad-Fenster: `pump` fuellt pro Tick
/// den RGBA-Puffer der Capture-Groesse, Anzeige auf `dst_w` x `dst_h`.
/// Esc oder Schliessen beendet. Muss innerhalb
/// `macroquad::Window::from_config` laufen.
pub async fn run_window_mq(
    src_w: u32,
    src_h: u32,
    dst_w: u32,
    dst_h: u32,
    fps: u32,
    mut pump: impl FnMut(&mut Vec<u8>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    use macroquad::prelude::*;
    if src_w == 0 || src_h == 0 || dst_w == 0 || dst_h == 0 {
        anyhow::bail!("window and capture sizes must be non-empty");
    }
    let mut frame = vec![0u8; (src_w as usize) * (src_h as usize) * 4];
    pump(&mut frame)?;
    let mut img = Image {
        bytes: frame.clone(),
        width: src_w as u16,
        height: src_h as u16,
    };
    let tex = Texture2D::from_rgba8(src_w as u16, src_h as u16, &img.bytes);
    tex.set_filter(FilterMode::Nearest);
    let period = super::frame_period(fps);
    let params = DrawTextureParams {
        dest_size: Some(vec2(dst_w as f32, dst_h as f32)),
        ..Default::default()
    };
    loop {
        let t0 = std::time::Instant::now();
        pump(&mut img.bytes)?;
        tex.update(&img);
        clear_background(BLACK);
        draw_texture_ex(&tex, 0.0, 0.0, WHITE, params.clone());
        if is_key_down(KeyCode::Escape) || is_quit_requested() {
            break;
        }
        next_frame().await;
        let wait = period.saturating_sub(t0.elapsed());
        if !wait.is_zero() {
            std::thread::sleep(wait);
        }
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zoom_scales_capture() {
        assert_eq!(zoomed_size(320, 240, 2, None, None).unwrap(), (640, 480));
        assert_eq!(zoomed_size(320, 240, 1, None, None).unwrap(), (320, 240));
    }

    #[test]
    fn explicit_window_wins_over_zoom() {
        assert_eq!(
            zoomed_size(320, 240, 3, Some(100), Some(90)).unwrap(),
            (100, 90)
        );
    }

    #[test]
    fn bad_sizes_rejected() {
        assert!(zoomed_size(320, 240, 0, None, None).is_err());
        assert!(zoomed_size(320, 240, 1, Some(0), Some(90)).is_err());
    }

    #[test]
    fn blit_2x_is_exact() {
        // 2x2 -> 4x4: jedes Quellpixel wird ein 2x2-Block.
        let src: Vec<u8> = vec![
            255, 0, 0, 255, 0, 255, 0, 255, //
            0, 0, 255, 255, 255, 255, 255, 255,
        ];
        let mut dst = vec![0u8; 4 * 4 * 4];
        blit_nearest(&src, 2, 2, &mut dst, 4, 4).unwrap();
        let at = |x: u32, y: u32| -> [u8; 4] {
            let i = ((y * 4 + x) * 4) as usize;
            [dst[i], dst[i + 1], dst[i + 2], dst[i + 3]]
        };
        assert_eq!(at(0, 0), [255, 0, 0, 255]);
        assert_eq!(at(1, 1), [255, 0, 0, 255]);
        assert_eq!(at(2, 0), [0, 255, 0, 255]);
        assert_eq!(at(0, 2), [0, 0, 255, 255]);
        assert_eq!(at(3, 3), [255, 255, 255, 255]);
    }

    #[test]
    fn blit_rejects_mismatched_buffers() {
        let mut dst = vec![0u8; 10];
        assert!(blit_nearest(&[0u8; 16], 2, 2, &mut dst, 4, 4).is_err());
        assert!(blit_nearest(&[0u8; 4], 0, 2, &mut dst, 1, 1).is_err());
    }

    #[test]
    fn save_frame_roundtrip() {
        let dir = std::env::temp_dir().join(format!("x11_yolo_test_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("frame.png");
        let rgba = vec![9u8; 8 * 6 * 4];
        save_frame_png(&path, &rgba, 8, 6).unwrap();
        let back = image::open(&path).unwrap().to_rgba8();
        assert_eq!(back.dimensions(), (8, 6));
        assert!(back.as_raw().iter().all(|v| *v == 9));
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn period_matches_fps_and_clamps() {
        assert_eq!(frame_period(5), Duration::from_millis(200));
        assert_eq!(frame_period(1), Duration::from_secs(1));
        assert_eq!(frame_period(0), Duration::from_secs(1));
        assert!(frame_period(999) <= Duration::from_millis(34));
    }

    #[test]
    fn save_frame_rejects_bad_length() {
        let dir = std::env::temp_dir();
        assert!(save_frame_png(&dir.join("nope.png"), &[0u8; 5], 8, 6).is_err());
    }
}
