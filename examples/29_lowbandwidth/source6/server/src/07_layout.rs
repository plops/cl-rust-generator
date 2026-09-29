//! `07_layout` — Text/Bild-Trennung: Farben sampeln, Text maskieren,
//! GUI-Boxen in „Text-Element“ vs. „Icon/Bild“ einteilen.

use lbw_common::{Rect, TextItem};

use crate::image::Rgb;

/// Anteil einer GUI-Box, der von Text bedeckt sein muss → reines Text-Element.
pub const TEXT_COVER: f32 = 0.6;
/// GUI-Boxen größer als dieser Bildanteil sind Container (keine Icons).
pub const MAX_ICON_FRAC: f32 = 0.25;
/// Mindestkonfidenz der Erkennung, damit eine Box als Text gilt.
pub const MIN_TEXT_CONF: f32 = 0.5;

fn dist2(a: [u8; 3], b: [u8; 3]) -> u32 {
    a.iter()
        .zip(b)
        .map(|(x, y)| u32::from(x.abs_diff(y)).pow(2))
        .sum()
}

fn acc(s: &mut [u32; 3], p: [u8; 3]) {
    for (a, v) in s.iter_mut().zip(p) {
        *a += u32::from(v);
    }
}

/// Dominante Hintergrund- und Schriftfarbe einer Textbox.
///
/// `bg` = Mittel der häufigsten (auf 4 bit quantisierten) Randfarbe;
/// `fg` = Mittel der Innenpixel mit ≥ 50 % der maximalen Distanz zu `bg`.
#[must_use]
pub fn sample_colors(img: &Rgb, r: Rect) -> ([u8; 3], [u8; 3]) {
    let r = img.clamp(r);
    if r.w == 0 || r.h == 0 {
        return ([0; 3], [255; 3]);
    }
    let (x0, y0, x1, y1) = (
        r.x as usize,
        r.y as usize,
        r.x2() as usize - 1,
        r.y2() as usize - 1,
    );
    let mut bins: std::collections::HashMap<u16, ([u32; 3], u32)> = Default::default();
    let mut add = |p: [u8; 3]| {
        let k = (u16::from(p[0] >> 4) << 8) | (u16::from(p[1] >> 4) << 4) | u16::from(p[2] >> 4);
        let e = bins.entry(k).or_default();
        acc(&mut e.0, p);
        e.1 += 1;
    };
    for x in x0..=x1 {
        add(img.get(x, y0));
        add(img.get(x, y1));
    }
    for y in y0..=y1 {
        add(img.get(x0, y));
        add(img.get(x1, y));
    }
    let (sum, n) = bins.values().max_by_key(|(_, n)| *n).copied().unwrap();
    let bg = sum.map(|s| ((s + n / 2) / n) as u8);

    let mut maxd = 0;
    for y in y0..=y1 {
        for x in x0..=x1 {
            maxd = maxd.max(dist2(img.get(x, y), bg));
        }
    }
    if maxd < 30 * 30 {
        // Kaum Kontrast: Schrift in Schwarz/Weiß je nach Helligkeit.
        let luma = u32::from(bg[0]) * 3 + u32::from(bg[1]) * 6 + u32::from(bg[2]);
        return (if luma > 1280 { [0; 3] } else { [255; 3] }, bg);
    }
    let (mut s, mut cnt) = ([0u32; 3], 0u32);
    for y in y0..=y1 {
        for x in x0..=x1 {
            let p = img.get(x, y);
            if dist2(p, bg) * 4 >= maxd {
                acc(&mut s, p);
                cnt += 1;
            }
        }
    }
    (s.map(|v| ((v + cnt / 2) / cnt) as u8), bg)
}

/// Rand (px) um jede Textbox, der zusätzlich übermalt wird: DBNet-Boxen
/// sind vertikal knapp, sonst bleiben Unterlängen/Antialiasing als
/// Geisterreste im AV1-Bild.
pub const MASK_PAD: u16 = 2;

/// Übermalt alle Textboxen (plus [`MASK_PAD`]) mit ihrer Hintergrundfarbe.
pub fn mask(img: &mut Rgb, items: &[TextItem]) {
    for t in items {
        let r = t.rect;
        let (x, y) = (r.x.saturating_sub(MASK_PAD), r.y.saturating_sub(MASK_PAD));
        img.fill(
            Rect::new(x, y, r.x2() + MASK_PAD - x, r.y2() + MASK_PAD - y),
            t.bg,
        );
    }
}

/// Anteil von `g`, der von den Textboxen bedeckt ist (Überlappungen der
/// Textboxen untereinander werden ignoriert, Ergebnis ≤ 1).
#[must_use]
pub fn text_coverage(g: &Rect, texts: &[Rect]) -> f32 {
    if g.area() == 0 {
        return 0.0;
    }
    let inter: u32 = texts.iter().map(|t| g.intersection(t)).sum();
    (inter as f32 / g.area() as f32).min(1.0)
}

/// Anteil der Pixel, die nicht die dominante Farbe haben (nach Maskierung).
/// Ein Button mit Label ist danach fast einfarbig, ein Icon nicht.
#[must_use]
pub fn busy_fraction(img: &Rgb, r: Rect) -> f32 {
    let r = img.clamp(r);
    if r.area() == 0 {
        return 0.0;
    }
    let mut hist: std::collections::HashMap<[u8; 3], u32> = Default::default();
    for y in r.y as usize..r.y2() as usize {
        for x in r.x as usize..r.x2() as usize {
            *hist.entry(img.get(x, y).map(|v| v >> 3)).or_default() += 1;
        }
    }
    let top = hist.values().max().copied().unwrap_or(0);
    1.0 - top as f32 / r.area() as f32
}

/// Mindestanteil „unruhiger“ Pixel, damit eine GUI-Box als Icon zählt.
pub const MIN_BUSY: f32 = 0.08;

/// GUI-Boxen, die Bildinhalt (Icons, Fotos) tragen: nicht überwiegend
/// Text, nicht riesig/winzig und nach dem Maskieren nicht einfarbig.
/// `masked` = Frame mit übermalten Textboxen.
#[must_use]
pub fn icon_regions(gui: &[Rect], texts: &[Rect], masked: &Rgb) -> Vec<Rect> {
    let max_area = (masked.w * masked.h) as f32 * MAX_ICON_FRAC;
    gui.iter()
        .filter(|g| g.w >= 8 && g.h >= 8)
        .filter(|g| (g.area() as f32) <= max_area)
        .filter(|g| text_coverage(g, texts) < TEXT_COVER)
        .filter(|g| busy_fraction(masked, **g) >= MIN_BUSY)
        .copied()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Weißer Grund, schwarzer Balken („Schrift“) in der Mitte.
    fn text_like() -> Rgb {
        let mut img = Rgb::filled(40, 20, [250, 250, 250]);
        img.fill(Rect::new(5, 6, 30, 8), [10, 20, 30]);
        img
    }

    #[test]
    fn colors_of_dark_text_on_white() {
        let (fg, bg) = sample_colors(&text_like(), Rect::new(0, 0, 40, 20));
        assert_eq!(bg, [250, 250, 250]);
        assert_eq!(fg, [10, 20, 30]);
    }

    #[test]
    fn low_contrast_box_gets_contrasting_fg() {
        let img = Rgb::filled(10, 10, [30, 30, 30]);
        assert_eq!(
            sample_colors(&img, Rect::new(0, 0, 10, 10)),
            ([255; 3], [30; 3])
        );
        let img = Rgb::filled(10, 10, [220; 3]);
        assert_eq!(sample_colors(&img, Rect::new(2, 2, 5, 5)).0, [0; 3]);
    }

    #[test]
    fn mask_removes_text_pixels_including_fringe() {
        let mut img = text_like();
        img.put(5, 15, [100; 3]); // Unterlänge knapp unter der Box
        let item = TextItem {
            id: 1,
            rect: Rect::new(5, 6, 30, 8),
            fg: [0; 3],
            bg: [250; 3],
            text: "x".into(),
        };
        mask(&mut img, &[item]);
        assert_eq!(img, Rgb::filled(40, 20, [250; 3]));
    }

    #[test]
    fn gui_boxes_are_split_into_text_elements_and_icons() {
        let mut masked = Rgb::filled(640, 640, [240; 3]);
        // Icon: Schachbrett (unruhig); flacher Kasten mit gepolstertem Label.
        for y in 10..42 {
            for x in 10..42 {
                masked.put(
                    x,
                    y,
                    if (x + y) % 2 == 0 {
                        [0; 3]
                    } else {
                        [200, 0, 0]
                    },
                );
            }
        }
        let texts = [Rect::new(100, 100, 80, 16), Rect::new(400, 100, 60, 12)];
        let button = Rect::new(96, 97, 88, 22); // Label deckt 66 %
        let padded = Rect::new(390, 90, 90, 40); // Label deckt 20 %, Rest flach
        let icon = Rect::new(10, 10, 32, 32);
        let panel = Rect::new(0, 0, 640, 400); // Container
        let speck = Rect::new(300, 300, 4, 4);
        let icons = icon_regions(&[button, padded, icon, panel, speck], &texts, &masked);
        assert_eq!(icons, vec![icon]);
        assert!(text_coverage(&button, &texts) >= TEXT_COVER);
        assert!(busy_fraction(&masked, icon) > 0.4);
        assert_eq!(busy_fraction(&masked, padded), 0.0);
    }
}
