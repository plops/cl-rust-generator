//! `07_render` — Zeichnen: Canvas-Textur, Text in GNU Unifont exakt in die
//! Server-Boxen eingepasst, Auswahlrahmen und HUD.

use macroquad::prelude::*;
use std::time::Instant;

use lbw_common::{Rect, TextItem};

use crate::scene::{Link, Scene};

/// Bekannte Pfade von GNU Unifont (Debian/Ubuntu `fonts-unifont`).
pub const UNIFONT: &[&str] = &[
    "/usr/share/fonts/opentype/unifont/unifont.otf",
    "/usr/share/fonts/unifont/unifont.otf",
    "/usr/share/fonts/truetype/unifont/unifont.ttf",
];

/// Lädt Unifont (oder `path`); `None` → macroquad-Standardschrift.
#[must_use]
pub fn load_font(path: Option<&str>) -> Option<Font> {
    let cands: Vec<&str> = path.map_or_else(|| UNIFONT.to_vec(), |p| vec![p]);
    for p in cands {
        if let Ok(b) = std::fs::read(p)
            && let Ok(f) = load_ttf_font_from_bytes(&b)
        {
            return Some(f);
        }
    }
    eprintln!(
        "[client] Unifont nicht gefunden (apt-get install fonts-unifont), nutze Standardschrift"
    );
    None
}

fn rgb(c: [u8; 3]) -> Color {
    Color::from_rgba(c[0], c[1], c[2], 255)
}

/// Schriftgröße aus der Boxhöhe: DBNet-Boxen sind ~1,3× so hoch wie die Glyphen.
#[must_use]
pub fn font_size_for(r: &Rect) -> u16 {
    ((f32::from(r.h) * 0.8).round() as u16).clamp(8, 96)
}

/// Canvas + Texte zeichnen.
pub struct Renderer {
    img: Image,
    tex: Texture2D,
    font: Option<Font>,
}

impl Renderer {
    #[must_use]
    pub fn new(w: usize, h: usize, font: Option<Font>) -> Self {
        let img = Image::gen_image_color(w as u16, h as u16, BLACK);
        let tex = Texture2D::from_image(&img);
        tex.set_filter(FilterMode::Nearest);
        Self { img, tex, font }
    }

    fn text(&self, t: &TextItem) {
        let r = &t.rect;
        draw_rectangle(r.x.into(), r.y.into(), r.w.into(), r.h.into(), rgb(t.bg));
        let size = font_size_for(r);
        let font = self.font.as_ref();
        let d = measure_text(&t.text, font, size, 1.0);
        if d.width <= 0.0 {
            return;
        }
        // Breite exakt auf die Box (abzüglich Rand) strecken/stauchen.
        let pad = (f32::from(r.h) * 0.1).max(1.0);
        let aspect = ((f32::from(r.w) - 2.0 * pad) / d.width).clamp(0.4, 2.5);
        let y = f32::from(r.y) + (f32::from(r.h) - d.height) / 2.0 + d.offset_y;
        draw_text_ex(
            &t.text,
            f32::from(r.x) + pad,
            y,
            TextParams {
                font,
                font_size: size,
                font_scale: 1.0,
                font_scale_aspect: aspect,
                color: rgb(t.fg),
                ..Default::default()
            },
        );
    }

    /// Kompletter Frame; `sel` = aktives Auswahlrechteck.
    pub fn draw(&mut self, s: &mut Scene, sel: Option<Rect>, hud: bool) {
        if s.dirty {
            self.img.bytes.copy_from_slice(&s.canvas);
            self.tex.update(&self.img);
            s.dirty = false;
        }
        clear_background(BLACK);
        draw_texture(&self.tex, 0.0, 0.0, WHITE);
        for t in s.texts.values() {
            self.text(t);
        }
        if let Some(r) = sel {
            draw_rectangle(
                r.x.into(),
                r.y.into(),
                r.w.into(),
                r.h.into(),
                Color::new(0.2, 0.5, 1.0, 0.25),
            );
            draw_rectangle_lines(r.x.into(), r.y.into(), r.w.into(), r.h.into(), 2.0, BLUE);
        }
        let stale = s.last_rx.elapsed().as_secs();
        let down = !matches!(s.link, Link::Up);
        if hud || down || stale >= 5 {
            self.hud(s, stale);
        }
    }

    fn hud(&self, s: &Scene, stale: u64) {
        let (rate, backlog, _) = s.stats;
        let link = match &s.link {
            Link::Connecting => "verbinde …".to_owned(),
            Link::Up if stale >= 5 => format!("verbunden, seit {stale} s still"),
            Link::Up => "verbunden".to_owned(),
            Link::Down(t, why) => format!(
                "getrennt seit {} s ({why}), verbinde neu …",
                Instant::now().duration_since(*t).as_secs()
            ),
        };
        let txt = format!(
            "{link} | {:.1} kB/s | Backlog {backlog} B | {} Kacheln {} kB | {} Texte | F1 HUD F2 Auswahl F3 Einfügen",
            f64::from(rate) / 1000.0,
            s.tiles,
            s.tile_bytes / 1000,
            s.texts.len()
        );
        let (w, h) = (screen_width(), screen_height());
        draw_rectangle(0.0, h - 20.0, w, 20.0, Color::new(0.0, 0.0, 0.0, 0.75));
        draw_text_ex(
            &txt,
            4.0,
            h - 5.0,
            TextParams {
                font: self.font.as_ref(),
                font_size: 16,
                color: YELLOW,
                ..Default::default()
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn font_size_follows_box_height() {
        assert_eq!(font_size_for(&Rect::new(0, 0, 100, 20)), 16);
        assert_eq!(font_size_for(&Rect::new(0, 0, 100, 2)), 8);
        assert_eq!(font_size_for(&Rect::new(0, 0, 100, 500)), 96);
    }
}
