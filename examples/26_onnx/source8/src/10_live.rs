//! `10_live` — Live-Modus: fester Bildschirmausschnitt in exakt der
//! Modell-Eingabegröße (640×640 bei `gpa_640_int8.onnx`) wird fortlaufend
//! gegrabbt, detektiert und mit Boxen im eigenen Fenster gezeigt.
//!
//! Keine Bildskalierung: Ausschnitt = Modell-Input, das Letterbox ist damit
//! die Identität (r = 1, kein Rand) und Boxen gelten direkt in Fensterpixeln.

use crate::capture::Screen;
use crate::detector::{Detector, SHOW};
use crate::window::Window;
use std::time::Instant;

/// Ausschnitt-Position und Laufdauer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LiveCfg {
    pub x: usize,
    pub y: usize,
    /// 0 = bis Escape/q/Fenster-Schließen, sonst nach N Frames enden.
    pub frames: usize,
}

/// Zusammenfassung eines Laufs.
#[derive(Debug)]
pub struct LiveStats {
    pub frames: usize,
    pub mean_ms: f64,
    pub last_boxes: usize,
}

/// Fensterposition neben dem Ausschnitt, damit das Fenster sich nicht
/// selbst abfilmt (Rückkopplung); passt nichts daneben → (0, 0).
#[must_use]
pub fn window_pos(cfg: &LiveCfg, w: usize, screen_w: usize) -> (i16, i16) {
    let x = if cfg.x + 2 * w <= screen_w {
        cfg.x + w
    } else {
        cfg.x.saturating_sub(w) // links daneben, sonst 0
    };
    (x as i16, cfg.y as i16)
}

/// Hauptschleife.
pub fn run(det: &mut Detector, screen: &Screen, cfg: LiveCfg) -> Result<LiveStats, String> {
    let (w, h) = (det.model.in_w, det.model.in_h);
    let (wx, wy) = window_pos(&cfg, w, screen.w);
    let mut win = Window::open("GPA-GUI-Detector live", wx, wy, w as u16, h as u16)?;
    let (mut n, mut sum_ms, mut fps, mut last_boxes) = (0, 0.0, 0.0, 0);
    let mut prev = Instant::now();
    while !win.quit_requested()? {
        let t0 = Instant::now();
        let mut frame = screen.grab_region(cfg.x, cfg.y, w, h)?;
        let grab_ms = t0.elapsed().as_secs_f64() * 1e3;
        let (dets, t) = det.detect(&frame)?;
        last_boxes = 0;
        for d in dets.iter().filter(|d| d.score >= SHOW) {
            frame.draw_rect(d.b[0], d.b[1], d.b[2], d.b[3], 2, [255, 0, 255]);
            last_boxes += 1;
        }

        // Gleitende FPS über die volle Frame-Zeit (inkl. Anzeige).
        let dt = prev.elapsed().as_secs_f64();
        prev = Instant::now();
        fps = if n == 0 {
            1.0 / dt
        } else {
            0.9 * fps + 0.1 / dt
        };
        let hud = format!(
            "{fps:5.1} fps | grab {grab_ms:.1} infer {:.1} ms | {last_boxes} boxen | {} | {w}x{h}@{},{} | q/Esc = Ende",
            t.infer, det.model.provider, cfg.x, cfg.y
        );
        win.show(&frame, &hud)?;
        sum_ms += t0.elapsed().as_secs_f64() * 1e3;
        n += 1;
        if cfg.frames > 0 && n >= cfg.frames {
            break;
        }
    }
    Ok(LiveStats {
        frames: n,
        mean_ms: if n > 0 { sum_ms / n as f64 } else { 0.0 },
        last_boxes,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cfg(x: usize) -> LiveCfg {
        LiveCfg { x, y: 7, frames: 0 }
    }

    #[test]
    fn window_goes_right_left_or_origin() {
        assert_eq!(window_pos(&cfg(0), 640, 1920), (640, 7)); // rechts daneben
        assert_eq!(window_pos(&cfg(1280), 640, 1920), (640, 7)); // links daneben
        assert_eq!(window_pos(&cfg(100), 640, 1000), (0, 7)); // passt nicht
    }
}
