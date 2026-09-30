//! `06_render` — GNU Unifont per fontdue auf der CPU in einen 640×640-Canvas.
//!
//! Ersetzt das Screen-Capture aus source5: das Bild entsteht deterministisch
//! im Speicher, pro Zeile wird die Tinten-Bounding-Box als Ground Truth
//! gemerkt. Kombinationszeichen (Thai, Devanagari, …) haben in Unifont
//! `advance_width == 0` und negatives `xmin` → einfaches Überlagern genügt.
//! RTL-Zeilen werden gespiegelt (visuelle Reihenfolge, kein Shaping).

use std::collections::HashMap;
use std::path::Path;

/// Kantenlänge des Canvas (= Detektions-Input, kein Resize nötig).
pub const CANVAS: usize = 640;
/// Rand links/rechts/oben in Pixeln.
pub const MARGIN: i32 = 16;

/// Suchliste für GNU Unifont (APT `fonts-unifont`).
pub const FONT_CANDIDATES: &[&str] = &[
    "/usr/share/fonts/opentype/unifont/unifont.otf",
    "/usr/share/fonts/unifont/unifont.otf",
    "/usr/share/fonts/truetype/unifont/unifont.ttf",
];

/// Achsenparalleles Rechteck (Pixel, Canvas-Koordinaten).
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Rect {
    /// Linke Kante.
    pub x: f32,
    /// Obere Kante.
    pub y: f32,
    /// Breite.
    pub w: f32,
    /// Höhe.
    pub h: f32,
}

impl Rect {
    /// Fläche.
    #[must_use]
    pub fn area(&self) -> f32 {
        self.w.max(0.0) * self.h.max(0.0)
    }

    /// Mittelpunkt.
    #[must_use]
    pub fn center(&self) -> (f32, f32) {
        (self.x + self.w / 2.0, self.y + self.h / 2.0)
    }

    /// Kleinstes Rechteck, das beide enthält.
    #[must_use]
    pub fn union(&self, o: &Rect) -> Rect {
        let (x0, y0) = (self.x.min(o.x), self.y.min(o.y));
        let x1 = (self.x + self.w).max(o.x + o.w);
        let y1 = (self.y + self.h).max(o.y + o.h);
        Rect {
            x: x0,
            y: y0,
            w: x1 - x0,
            h: y1 - y0,
        }
    }

    /// Intersection over Union (0..=1).
    #[must_use]
    pub fn iou(&self, o: &Rect) -> f32 {
        let iw = ((self.x + self.w).min(o.x + o.w) - self.x.max(o.x)).max(0.0);
        let ih = ((self.y + self.h).min(o.y + o.h) - self.y.max(o.y)).max(0.0);
        let inter = iw * ih;
        let uni = self.area() + o.area() - inter;
        if uni > 0.0 { inter / uni } else { 0.0 }
    }
}

/// Ground Truth einer gerenderten Zeile.
#[derive(Clone, Debug)]
pub struct GtLine {
    /// Text in visueller Reihenfolge (so, wie er im Bild steht).
    pub text: String,
    /// Tinten-Bounding-Box.
    pub rect: Rect,
}

/// Gerendertes Bild + Ground Truth.
#[derive(Clone, Debug)]
pub struct Canvas {
    /// RGBA, `CANVAS²×4`, schwarz auf weiß.
    pub rgba: Vec<u8>,
    /// Zeilen (nur solche mit Tinte).
    pub lines: Vec<GtLine>,
}

struct Glyph {
    m: fontdue::Metrics,
    bmp: Vec<u8>,
}

/// Unifont-Rasterer mit Glyph-Cache pro (Zeichen, Pixelgröße).
pub struct Raster {
    font: fontdue::Font,
    cache: HashMap<(char, u32), Glyph>,
}

impl Raster {
    /// Lädt die Schrift von `path` oder aus der Suchliste.
    pub fn load(path: Option<&Path>) -> Result<Self, String> {
        Self::from_bytes(font_bytes(path)?)
    }

    /// Aus Font-Bytes (OTF/TTF).
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, String> {
        let font = fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default())?;
        Ok(Self {
            font,
            cache: HashMap::new(),
        })
    }
}

/// Liest Unifont-Bytes von `path` oder aus der Suchliste (auch für HUD-Font).
pub fn font_bytes(path: Option<&Path>) -> Result<Vec<u8>, String> {
    match path {
        Some(p) => std::fs::read(p).map_err(|e| format!("{}: {e}", p.display())),
        None => FONT_CANDIDATES
            .iter()
            .find_map(|p| std::fs::read(p).ok())
            .ok_or_else(|| {
                format!(
                    "GNU Unifont not found (tried: {}); apt-get install fonts-unifont",
                    FONT_CANDIDATES.join(", ")
                )
            }),
    }
}

impl Raster {
    /// Kennt die Schrift das Zeichen?
    #[must_use]
    pub fn has(&self, c: char) -> bool {
        self.font.lookup_glyph_index(c) != 0
    }

    fn glyph(&mut self, c: char, px: u32) -> &Glyph {
        let font = &self.font;
        self.cache.entry((c, px)).or_insert_with(|| {
            let (m, bmp) = font.rasterize(c, px as f32);
            Glyph { m, bmp }
        })
    }

    /// Breite einer Zeile in Pixeln (Summe der Vorschübe).
    pub fn width(&mut self, s: &str, px: u32) -> i32 {
        s.chars()
            .map(|c| self.glyph(c, px).m.advance_width.round() as i32)
            .sum()
    }

    /// Nutzbare Zeilenbreite.
    #[must_use]
    pub fn max_width() -> i32 {
        CANVAS as i32 - 2 * MARGIN
    }

    /// Wie viele Zeilen passen bei `px` untereinander?
    #[must_use]
    pub fn max_lines(px: u32) -> usize {
        (CANVAS - 2 * MARGIN as usize) / line_height(px) as usize
    }

    /// Rendert Zeilen (logische Reihenfolge) schwarz auf weiß.
    pub fn render(&mut self, lines: &[String], px: u32, rtl: bool) -> Canvas {
        let mut rgba = vec![255u8; CANVAS * CANVAS * 4];
        let mut out = Vec::new();
        let ascent = self
            .font
            .horizontal_line_metrics(px as f32)
            .map_or(px as f32 * 0.875, |l| l.ascent)
            .round() as i32;
        for (i, line) in lines.iter().take(Self::max_lines(px)).enumerate() {
            let text: String = if rtl {
                line.chars().rev().collect()
            } else {
                line.clone()
            };
            let base = MARGIN + ascent + i as i32 * line_height(px);
            let mut pen = MARGIN;
            let mut ink: Option<Rect> = None;
            for c in text.chars() {
                let g = self.glyph(c, px);
                let (w, h) = (g.m.width as i32, g.m.height as i32);
                let left = pen + g.m.xmin;
                let top = base - (g.m.ymin + h);
                if w > 0 && h > 0 {
                    blit(&mut rgba, &g.bmp, left, top, w, h);
                    let r = Rect {
                        x: left as f32,
                        y: top as f32,
                        w: w as f32,
                        h: h as f32,
                    };
                    ink = Some(ink.map_or(r, |a| a.union(&r)));
                }
                pen += g.m.advance_width.round() as i32;
            }
            if let Some(rect) = ink {
                out.push(GtLine {
                    text,
                    rect: clip(rect),
                });
            }
        }
        Canvas { rgba, lines: out }
    }
}

/// Zeilenabstand: doppelte Schriftgröße (DBNet trennt Zeilen sauber).
#[must_use]
pub fn line_height(px: u32) -> i32 {
    2 * px as i32
}

fn clip(r: Rect) -> Rect {
    let max = CANVAS as f32;
    let (x0, y0) = (r.x.clamp(0.0, max), r.y.clamp(0.0, max));
    let (x1, y1) = ((r.x + r.w).clamp(0.0, max), (r.y + r.h).clamp(0.0, max));
    Rect {
        x: x0,
        y: y0,
        w: x1 - x0,
        h: y1 - y0,
    }
}

/// Deckungsgrad-Bitmap dunkel auf den Canvas (Maximum bei Überlappung).
fn blit(rgba: &mut [u8], bmp: &[u8], left: i32, top: i32, w: i32, h: i32) {
    for gy in 0..h {
        let y = top + gy;
        if !(0..CANVAS as i32).contains(&y) {
            continue;
        }
        for gx in 0..w {
            let x = left + gx;
            if !(0..CANVAS as i32).contains(&x) {
                continue;
            }
            let v = 255 - bmp[(gy * w + gx) as usize];
            let p = (y as usize * CANVAS + x as usize) * 4;
            let cur = rgba[p];
            let d = cur.min(v);
            rgba[p..p + 3].fill(d);
        }
    }
}

/// Schreibt RGBA als binäres PPM (P6) — für Nachweise ohne Bild-Crate.
pub fn write_ppm(path: &Path, rgba: &[u8], w: usize, h: usize) -> std::io::Result<()> {
    let mut data = format!("P6\n{w} {h}\n255\n").into_bytes();
    data.reserve(w * h * 3);
    let (px, _) = rgba.as_chunks::<4>();
    for p in px.iter().take(w * h) {
        data.extend_from_slice(&p[..3]);
    }
    std::fs::write(path, data)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn raster() -> Raster {
        Raster::load(None).expect("GNU Unifont required: apt-get install fonts-unifont")
    }

    fn dark(c: &Canvas, x: usize, y: usize) -> bool {
        c.rgba[(y * CANVAS + x) * 4] < 128
    }

    #[test]
    fn rect_iou_and_union() {
        let a = Rect {
            x: 0.0,
            y: 0.0,
            w: 10.0,
            h: 10.0,
        };
        let b = Rect {
            x: 5.0,
            y: 0.0,
            w: 10.0,
            h: 10.0,
        };
        assert!((a.iou(&b) - 50.0 / 150.0).abs() < 1e-6);
        assert_eq!(a.iou(&a), 1.0);
        assert_eq!(
            a.union(&b),
            Rect {
                x: 0.0,
                y: 0.0,
                w: 15.0,
                h: 10.0
            }
        );
    }

    #[test]
    fn ink_lies_inside_gt_box_and_margins_stay_white() {
        let mut r = raster();
        let c = r.render(&["Größe ẞ".into(), "Zwölf".into()], 32, false);
        assert_eq!(c.lines.len(), 2);
        let mut inside = 0;
        for y in 0..CANVAS {
            for x in 0..CANVAS {
                if !dark(&c, x, y) {
                    continue;
                }
                let hit = c.lines.iter().any(|l| {
                    let rr = l.rect;
                    (x as f32) >= rr.x
                        && (x as f32) < rr.x + rr.w
                        && (y as f32) >= rr.y
                        && (y as f32) < rr.y + rr.h
                });
                assert!(hit, "ink outside gt at {x},{y}");
                inside += 1;
                assert!(x >= MARGIN as usize && y >= MARGIN as usize);
            }
        }
        assert!(inside > 100);
        assert!(c.lines[0].rect.y < c.lines[1].rect.y);
    }

    #[test]
    fn width_matches_unifont_cells() {
        let mut r = raster();
        // Unifont: Latein halbe, CJK volle Zelle; Kombinationszeichen 0.
        assert_eq!(r.width("ab", 32), 32);
        assert_eq!(r.width("中文", 32), 64);
        assert_eq!(r.width("กั", 32), r.width("ก", 32));
    }

    #[test]
    fn rtl_line_is_mirrored() {
        let mut r = raster();
        let c = r.render(&["abc".into()], 16, true);
        assert_eq!(c.lines[0].text, "cba");
    }

    #[test]
    fn missing_glyph_detected() {
        let r = raster();
        assert!(r.has('ß') && r.has('中') && r.has('ก'));
        assert!(!r.has('\u{10FFFD}'));
    }

    #[test]
    fn max_lines_follows_size() {
        assert_eq!(Raster::max_lines(32), 9);
        assert_eq!(Raster::max_lines(48), 6);
    }
}
