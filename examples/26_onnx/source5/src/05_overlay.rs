//! `05_overlay` — Box-/Label-Render, HUD und Font-Suche (S4).
//!
//! Anzeige-Koordinaten = Detektions-Koordinaten × `scale`
//! (`scale = ausgabe / roi`; bei 1:1 Identität, kein Resampling).
//! Reine Geometrie (`scaled_rect`) ist ohne Display testbar;
//! die `draw_*`-Funktionen brauchen den macroquad-Kontext.

use macroquad::prelude::*;

use crate::detect::TextBox;

const LABEL_FONT_SIZE: u16 = 16;

/// Sucht die Unifont-Datei an den bekannten System-Pfaden (APT-Paket
/// `fonts-unifont` installiert unter `opentype/`, nicht `unifont/`).
pub fn load_font_bytes() -> Vec<u8> {
    const CANDIDATES: &[&str] = &[
        "/usr/share/fonts/opentype/unifont/unifont.otf",
        "/usr/share/fonts/unifont/unifont.otf",
        "/usr/share/fonts/truetype/unifont/unifont.ttf",
    ];
    for path in CANDIDATES {
        if let Ok(bytes) = std::fs::read(path) {
            return bytes;
        }
    }
    panic!(
        "GNU Unifont not found (tried: {}); install it via `apt-get install fonts-unifont`",
        CANDIDATES.join(", ")
    );
}

/// Skaliert eine Box in Anzeige-Koordinaten (rein, testbar).
#[must_use]
pub fn scaled_rect(b: &TextBox, scale: f32) -> (f32, f32, f32, f32) {
    (b.x * scale, b.y * scale, b.w * scale, b.h * scale)
}

/// Zeichnet Boxen + Labels (Koordinaten bereits im 640-Raum der Detektion).
pub fn draw_boxes(boxes: &[TextBox], font: &Font, scale: f32, out: f32) {
    for b in boxes {
        let (x, y, w, h) = scaled_rect(b, scale);
        draw_rectangle_lines(x, y, w, h, 2.0, GREEN);

        if !b.text.is_empty() {
            let dims = measure_text(&b.text, Some(font), LABEL_FONT_SIZE, 1.0);
            let (pad, bw, bh) = (3.0, dims.width + 6.0, dims.height + 6.0);
            let bx = x.clamp(0.0, (out - bw).max(0.0));
            let by = if y >= bh + 2.0 {
                y - bh - 2.0
            } else {
                y + h + 2.0
            };

            draw_rectangle(bx, by, bw, bh, Color::new(0.0, 0.0, 0.0, 0.85));
            draw_rectangle_lines(bx, by, bw, bh, 1.0, YELLOW);

            draw_text_ex(
                &b.text,
                bx + pad,
                by + bh - pad - 2.0,
                TextParams {
                    font: Some(font),
                    font_size: LABEL_FONT_SIZE,
                    color: WHITE,
                    ..Default::default()
                },
            );
        }
    }
}

/// Zeichnet die HUD-Zeilen: Status + ROI/Zoom sowie Tasten-Hilfe.
#[allow(clippy::too_many_arguments)]
pub fn draw_hud(
    font: &Font,
    out: f32,
    idle: bool,
    view_x: i32,
    view_y: i32,
    view_size: u32,
    lines: usize,
    det_ms: f64,
    rec_ms: f64,
) {
    let status_color = if idle {
        Color::new(0.4, 0.8, 1.0, 1.0)
    } else {
        GREEN
    };
    let status_text = if idle { "PAUSED (STATIC)" } else { "ACTIVE" };

    draw_rectangle(0.0, 0.0, out, 24.0, Color::new(0.0, 0.0, 0.0, 0.75));
    draw_text_ex(
        format!(
            "[{status_text}] ROI {view_size}x{view_size}@{view_x},{view_y} \
             | Lines: {lines} | Det: {det_ms:.1}ms | Rec: {rec_ms:.1}ms | FPS: {}",
            get_fps()
        ),
        10.0,
        17.0,
        TextParams {
            font: Some(font),
            font_size: LABEL_FONT_SIZE,
            color: status_color,
            ..Default::default()
        },
    );

    draw_rectangle(0.0, out - 24.0, out, 24.0, Color::new(0.0, 0.0, 0.0, 0.75));
    draw_text_ex(
        "Arrows: move | 1: zoom in | 2: zoom out | Esc: quit",
        10.0,
        out - 7.0,
        TextParams {
            font: Some(font),
            font_size: LABEL_FONT_SIZE,
            color: WHITE,
            ..Default::default()
        },
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    fn box_at(x: f32, y: f32, w: f32, h: f32) -> TextBox {
        TextBox {
            x,
            y,
            w,
            h,
            text: String::new(),
        }
    }

    #[test]
    fn scale_is_identity_at_1x1() {
        let (x, y, w, h) = scaled_rect(&box_at(10.0, 20.0, 100.0, 30.0), 1.0);
        assert_eq!((x, y, w, h), (10.0, 20.0, 100.0, 30.0));
    }

    #[test]
    fn scale_doubles_at_320_roi() {
        // ROI 320 auf 640 Anzeige: Faktor 2.
        let (x, y, w, h) = scaled_rect(&box_at(10.0, 20.0, 100.0, 30.0), 2.0);
        assert_eq!((x, y, w, h), (20.0, 40.0, 200.0, 60.0));
    }

    #[test]
    fn scale_halves_at_1280_roi() {
        let (x, y, w, h) = scaled_rect(&box_at(100.0, 100.0, 200.0, 60.0), 0.5);
        assert_eq!((x, y, w, h), (50.0, 50.0, 100.0, 30.0));
    }
}
