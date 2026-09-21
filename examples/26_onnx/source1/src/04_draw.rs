//! `04_draw` — Overlay-Rasterizer fuer Boxen (S3).
//!
//! Reine CPU-Funktionen auf RGBA-Frames: Rahmen in Klassenfarbe plus
//! Label-Balken (ohne Glyphen — kein Font-Dep). Alles geclippt, ohne
//! GPU testbar.

use crate::infer::{BoundingBox, Detection};
use font8x8::UnicodeFonts;

/// Rahmendicke in px.
pub const THICKNESS: u32 = 2;
/// Hoehe des Label-Balkens ueber der Box in px.
pub const LABEL_BAR: u32 = 12;

/// Kleine Palette; Farbe wird deterministisch aus dem Label gewaehlt.
const PALETTE: [[u8; 4]; 6] = [
    [230, 57, 70, 255],  // rot
    [46, 204, 113, 255], // gruen
    [52, 152, 219, 255], // blau
    [241, 196, 15, 255], // gelb
    [155, 89, 182, 255], // violett
    [230, 126, 34, 255], // orange
];

/// Deterministische Klassenfarbe aus dem Label.
#[must_use]
pub fn color_for_label(label: &str) -> [u8; 4] {
    let mut h: usize = 0;
    for b in label.bytes() {
        h = h.wrapping_mul(31).wrapping_add(b as usize);
    }
    PALETTE[h % PALETTE.len()]
}

/// Setzt ein Pixel, wenn es im Frame liegt (sonst No-Op).
fn put(frame: &mut [u8], fw: u32, fh: u32, x: i64, y: i64, color: [u8; 4]) {
    if x < 0 || y < 0 || x >= fw as i64 || y >= fh as i64 {
        return;
    }
    let i = ((y as u32 * fw + x as u32) * 4) as usize;
    if i + 4 <= frame.len() {
        frame[i..i + 4].copy_from_slice(&color);
    }
}

fn draw_char(frame: &mut [u8], fw: u32, fh: u32, x0: i64, y0: i64, c: char, text_color: [u8; 4]) {
    if let Some(glyph) = font8x8::BASIC_FONTS.get(c) {
        for (gy, row) in glyph.iter().enumerate() {
            for gx in 0..8 {
                if (row & (1 << gx)) != 0 {
                    put(frame, fw, fh, x0 + gx, y0 + gy as i64, text_color);
                }
            }
        }
    }
}

/// Draws a text string starting at (x, y).
pub fn draw_text(
    frame: &mut [u8],
    fw: u32,
    fh: u32,
    mut x: i64,
    y: i64,
    text: &str,
    color: [u8; 4],
) {
    for c in text.chars() {
        draw_char(frame, fw, fh, x, y, c, color);
        x += 8; // character advance
    }
}

/// Zeichnet einen Rahmen (Dicke `THICKNESS`) um die Box (Pixel, float).
pub fn draw_box(frame: &mut [u8], fw: u32, fh: u32, b: &BoundingBox, color: [u8; 4]) {
    let (xa, xb) = (b.x1.min(b.x2).round() as i64, b.x1.max(b.x2).round() as i64);
    let (ya, yb) = (b.y1.min(b.y2).round() as i64, b.y1.max(b.y2).round() as i64);
    for t in 0..THICKNESS as i64 {
        for x in xa..=xb {
            put(frame, fw, fh, x, ya + t, color);
            put(frame, fw, fh, x, yb - t, color);
        }
        for y in ya..=yb {
            put(frame, fw, fh, xa + t, y, color);
            put(frame, fw, fh, xb - t, y, color);
        }
    }
}

/// Zeichnet alle Detektionen: Label-Balken ueber der Box plus Rahmen.
pub fn draw_detections(frame: &mut [u8], fw: u32, fh: u32, dets: &[Detection]) {
    for d in dets {
        let color = color_for_label(d.label);
        let b = &d.bbox;
        let text = format!("{} {:.0}%", d.label, d.conf * 100.0);
        let text_width = (text.len() * 8) as i64;

        // Draw label background bar
        let bar_y1 = (b.y1.round() as i64 - LABEL_BAR as i64).max(0);
        let bar_y2 = b.y1.round() as i64;
        let bar_x1 = b.x1.round() as i64;
        let bar_x2 = (bar_x1 + text_width + 4).min(fw as i64);

        for y in bar_y1..bar_y2 {
            for x in bar_x1..bar_x2 {
                put(frame, fw, fh, x, y, color);
            }
        }

        // Draw label text (white with subtle shadow)
        draw_text(frame, fw, fh, bar_x1 + 3, bar_y1 + 3, &text, [0, 0, 0, 255]);
        draw_text(
            frame,
            fw,
            fh,
            bar_x1 + 2,
            bar_y1 + 2,
            &text,
            [255, 255, 255, 255],
        );

        draw_box(frame, fw, fh, b, color);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::infer::BoundingBox;

    fn det(label: &'static str, x1: f32, y1: f32, x2: f32, y2: f32) -> Detection {
        Detection {
            bbox: BoundingBox { x1, y1, x2, y2 },
            label,
            conf: 0.9,
        }
    }

    fn px(frame: &[u8], fw: u32, x: u32, y: u32) -> [u8; 4] {
        let i = ((y * fw + x) * 4) as usize;
        [frame[i], frame[i + 1], frame[i + 2], frame[i + 3]]
    }

    #[test]
    fn box_corners_set_inside_untouched() {
        let (fw, fh) = (20, 20);
        let mut frame = vec![0u8; (fw * fh * 4) as usize];
        draw_detections(&mut frame, fw, fh, &[det("person", 2.0, 8.0, 10.0, 16.0)]);
        let c = color_for_label("person");
        assert_eq!(px(&frame, fw, 2, 8), c);
        assert_eq!(px(&frame, fw, 10, 16), c);
        // Innenraum bleibt leer.
        assert_eq!(px(&frame, fw, 6, 12), [0, 0, 0, 0]);
        // Weit ausserhalb bleibt leer.
        assert_eq!(px(&frame, fw, 19, 19), [0, 0, 0, 0]);
    }

    #[test]
    fn label_bar_drawn_above_box() {
        let (fw, fh) = (20, 20);
        let mut frame = vec![0u8; (fw * fh * 4) as usize];
        draw_detections(&mut frame, fw, fh, &[det("car", 2.0, 10.0, 8.0, 16.0)]);
        let c = color_for_label("car");
        // Balken: y 4..10 ueber der Box.
        assert_eq!(px(&frame, fw, 5, 5), c);
    }

    #[test]
    fn offscreen_box_does_not_panic() {
        let (fw, fh) = (10, 10);
        let mut frame = vec![0u8; (fw * fh * 4) as usize];
        draw_detections(
            &mut frame,
            fw,
            fh,
            &[
                det("dog", -50.0, -50.0, 500.0, 500.0),
                det("cat", 3.0, 3.0, 5.0, 5.0),
            ],
        );
        // Sichtbare Box ist trotzdem da.
        assert_eq!(px(&frame, fw, 3, 3), color_for_label("cat"));
    }

    #[test]
    fn colors_are_deterministic_and_varied() {
        assert_eq!(color_for_label("person"), color_for_label("person"));
        let set: std::collections::HashSet<[u8; 4]> =
            ["person", "car", "dog", "chair", "tv", "book", "clock"]
                .iter()
                .map(|l| color_for_label(l))
                .collect();
        assert!(set.len() > 1, "palette should vary across labels");
    }
}
