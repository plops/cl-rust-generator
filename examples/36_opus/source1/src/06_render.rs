//! Renderer: Bildpuffer, Colormap, 5x7-Font, Layout, Ebenen.
//!
//! Ebenen: Kopfzeilen, Wasserfall (mit Modus-Toenung + Bandgrenze),
//! Modus-Streifen, Bitraten-Lane, Legende, Achsen.

use crate::spectrum::{BINS, DB_FLOOR, HOP_MS};
use crate::types::{Analysis, Mode};

/// RGB-Bild, Zeilen von oben nach unten.
#[derive(Debug, Clone)]
pub struct Image {
    /// Breite in px.
    pub width: u32,
    /// Hoehe in px.
    pub height: u32,
    /// RGB-Tripel, `width*height*3` Bytes.
    pub rgb: Vec<u8>,
}

impl Image {
    /// Neues Bild, gefuellt mit `bg`.
    #[must_use]
    pub fn new(width: u32, height: u32, bg: [u8; 3]) -> Self {
        let mut rgb = vec![0; width as usize * height as usize * 3];
        for px in rgb.as_chunks_mut::<3>().0 {
            *px = bg;
        }
        Self { width, height, rgb }
    }

    /// Pixel setzen (stummes Clipping ausserhalb).
    pub fn set(&mut self, x: i32, y: i32, c: [u8; 3]) {
        if x >= 0 && y >= 0 && (x as u32) < self.width && (y as u32) < self.height {
            let i = (y as usize * self.width as usize + x as usize) * 3;
            self.rgb[i..i + 3].copy_from_slice(&c);
        }
    }

    /// Pixel lesen (fuer Tests).
    #[must_use]
    pub fn get(&self, x: u32, y: u32) -> [u8; 3] {
        let i = (y as usize * self.width as usize + x as usize) * 3;
        [self.rgb[i], self.rgb[i + 1], self.rgb[i + 2]]
    }

    /// Gefuelltes Rechteck (stummes Clipping).
    pub fn rect(&mut self, x: u32, y: u32, w: u32, h: u32, c: [u8; 3]) {
        for dy in 0..h {
            for dx in 0..w {
                self.set(x as i32 + dx as i32, y as i32 + dy as i32, c);
            }
        }
    }

    /// Pixel mit Alpha (0.0–1.0) ueberblenden.
    pub fn blend(&mut self, x: u32, y: u32, c: [u8; 3], a: f32) {
        if x < self.width && y < self.height {
            let i = (y as usize * self.width as usize + x as usize) * 3;
            for (old, target) in self.rgb[i..i + 3].iter_mut().zip(c) {
                let o = f32::from(*old);
                *old = (o + (f32::from(target) - o) * a) as u8;
            }
        }
    }
}

/// Hintergrund, Text, Linie.
pub const BG: [u8; 3] = [13, 17, 23];
/// Siehe `BG`.
pub const FG: [u8; 3] = [220, 225, 235];
/// Siehe `BG`.
pub const GRID: [u8; 3] = [60, 66, 78];

/// Modus-Farbe: SILK blau, Hybrid violett, CELT amber.
#[must_use]
pub fn mode_color(mode: Mode) -> [u8; 3] {
    match mode {
        Mode::Silk => [70, 170, 255],
        Mode::Hybrid => [185, 120, 255],
        Mode::Celt => [255, 178, 70],
    }
}

/// Heatmap: schwarz → indigo → magenta → orange → gelb → weiss.
#[must_use]
pub fn colormap(t: f32) -> [u8; 3] {
    const STOPS: [(f32, [u8; 3]); 6] = [
        (0.0, [10, 10, 16]),
        (0.2, [62, 22, 86]),
        (0.4, [182, 42, 92]),
        (0.6, [242, 122, 42]),
        (0.8, [250, 222, 120]),
        (1.0, [255, 255, 240]),
    ];
    let t = t.clamp(0.0, 1.0);
    let mut s = 0;
    while s + 2 < STOPS.len() && t > STOPS[s + 1].0 {
        s += 1;
    }
    let (t0, c0) = STOPS[s];
    let (t1, c1) = STOPS[s + 1];
    let f = if t1 > t0 { (t - t0) / (t1 - t0) } else { 0.0 };
    [
        (f32::from(c0[0]) + (f32::from(c1[0]) - f32::from(c0[0])) * f) as u8,
        (f32::from(c0[1]) + (f32::from(c1[1]) - f32::from(c0[1])) * f) as u8,
        (f32::from(c0[2]) + (f32::from(c1[2]) - f32::from(c0[2])) * f) as u8,
    ]
}

/// 5x7-Glyphen: A–M (je 7 Zeilen, 5 Bit).
const GLYPH_AM: [(char, [u8; 7]); 13] = [
    ('A', [0x0e, 0x11, 0x11, 0x1f, 0x11, 0x11, 0x11]),
    ('B', [0x1e, 0x11, 0x11, 0x1e, 0x11, 0x11, 0x1e]),
    ('C', [0x0e, 0x11, 0x10, 0x10, 0x10, 0x11, 0x0e]),
    ('D', [0x1e, 0x11, 0x11, 0x11, 0x11, 0x11, 0x1e]),
    ('E', [0x1f, 0x10, 0x10, 0x1e, 0x10, 0x10, 0x1f]),
    ('F', [0x1f, 0x10, 0x10, 0x1e, 0x10, 0x10, 0x10]),
    ('G', [0x0e, 0x11, 0x10, 0x13, 0x11, 0x11, 0x0f]),
    ('H', [0x11, 0x11, 0x11, 0x1f, 0x11, 0x11, 0x11]),
    ('I', [0x0e, 0x04, 0x04, 0x04, 0x04, 0x04, 0x0e]),
    ('J', [0x07, 0x02, 0x02, 0x02, 0x02, 0x12, 0x0c]),
    ('K', [0x11, 0x12, 0x14, 0x18, 0x14, 0x12, 0x11]),
    ('L', [0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x1f]),
    ('M', [0x11, 0x1b, 0x15, 0x15, 0x11, 0x11, 0x11]),
];

/// 5x7-Glyphen: N–Z.
const GLYPH_NZ: [(char, [u8; 7]); 13] = [
    ('N', [0x11, 0x19, 0x19, 0x15, 0x13, 0x13, 0x11]),
    ('O', [0x0e, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0e]),
    ('P', [0x1e, 0x11, 0x11, 0x1e, 0x10, 0x10, 0x10]),
    ('Q', [0x0e, 0x11, 0x11, 0x11, 0x15, 0x12, 0x0d]),
    ('R', [0x1e, 0x11, 0x11, 0x1e, 0x14, 0x12, 0x11]),
    ('S', [0x0f, 0x10, 0x10, 0x0e, 0x01, 0x01, 0x1e]),
    ('T', [0x1f, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04]),
    ('U', [0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0e]),
    ('V', [0x11, 0x11, 0x11, 0x11, 0x11, 0x0a, 0x04]),
    ('W', [0x11, 0x11, 0x11, 0x15, 0x15, 0x1b, 0x11]),
    ('X', [0x11, 0x11, 0x0a, 0x04, 0x0a, 0x11, 0x11]),
    ('Y', [0x11, 0x11, 0x0a, 0x04, 0x04, 0x04, 0x04]),
    ('Z', [0x1f, 0x01, 0x02, 0x04, 0x08, 0x10, 0x1f]),
];

/// 5x7-Glyphen: Ziffern.
const GLYPH_09: [(char, [u8; 7]); 10] = [
    ('0', [0x0e, 0x11, 0x13, 0x15, 0x19, 0x11, 0x0e]),
    ('1', [0x04, 0x0c, 0x04, 0x04, 0x04, 0x04, 0x0e]),
    ('2', [0x0e, 0x11, 0x01, 0x02, 0x04, 0x08, 0x1f]),
    ('3', [0x1f, 0x02, 0x04, 0x02, 0x01, 0x11, 0x0e]),
    ('4', [0x02, 0x06, 0x0a, 0x12, 0x1f, 0x02, 0x02]),
    ('5', [0x1f, 0x10, 0x1e, 0x01, 0x01, 0x11, 0x0e]),
    ('6', [0x06, 0x08, 0x10, 0x1e, 0x11, 0x11, 0x0e]),
    ('7', [0x1f, 0x01, 0x02, 0x04, 0x08, 0x08, 0x08]),
    ('8', [0x0e, 0x11, 0x11, 0x0e, 0x11, 0x11, 0x0e]),
    ('9', [0x0e, 0x11, 0x11, 0x0f, 0x01, 0x02, 0x0c]),
];

/// 5x7-Glyphen: Satzzeichen (Rest: `?`, klein → gross).
const GLYPH_SYM: [(char, [u8; 7]); 12] = [
    (' ', [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ('.', [0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06]),
    (',', [0x00, 0x00, 0x00, 0x00, 0x06, 0x04, 0x08]),
    (':', [0x00, 0x06, 0x06, 0x00, 0x06, 0x06, 0x00]),
    ('-', [0x00, 0x00, 0x00, 0x1f, 0x00, 0x00, 0x00]),
    ('/', [0x01, 0x01, 0x02, 0x04, 0x08, 0x10, 0x10]),
    ('%', [0x18, 0x19, 0x02, 0x04, 0x08, 0x13, 0x03]),
    ('(', [0x02, 0x04, 0x08, 0x08, 0x08, 0x04, 0x02]),
    (')', [0x08, 0x04, 0x02, 0x02, 0x02, 0x04, 0x08]),
    ('_', [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1f]),
    ('?', [0x0e, 0x11, 0x01, 0x02, 0x04, 0x00, 0x04]),
    ('=', [0x00, 0x00, 0x1f, 0x00, 0x1f, 0x00, 0x00]),
];

/// Glyphe suchen (klein → gross, unbekannt → `?`).
fn glyph(c: char) -> [u8; 7] {
    let c = c.to_ascii_uppercase();
    for table in [&GLYPH_AM[..], &GLYPH_NZ[..], &GLYPH_09[..], &GLYPH_SYM[..]] {
        for (k, g) in table {
            if *k == c {
                return *g;
            }
        }
    }
    glyph('?')
}

impl Image {
    /// Text (6 px/Zeichen), Rueckgabe = Breite in px.
    pub fn text(&mut self, x: u32, y: u32, s: &str, fg: [u8; 3]) -> u32 {
        let mut dx = 0;
        for c in s.chars() {
            let g = glyph(c);
            for (row, bits) in g.iter().enumerate() {
                for col in 0..5 {
                    if bits & (0x10 >> col) != 0 {
                        self.set(x as i32 + dx + col, y as i32 + row as i32, fg);
                    }
                }
            }
            dx += 6;
        }
        dx as u32
    }
}

/// Layout-Masse (alle px).
#[derive(Debug, Clone, Copy)]
pub struct Layout {
    /// Gesamtbreite/-hoehe.
    pub width: u32,
    /// Siehe `width`.
    pub height: u32,
    /// Wasserfall links/oben.
    pub plot_x: u32,
    /// Siehe `plot_x`.
    pub plot_y: u32,
    /// Wasserfall-Breite (Hoehe = `BINS`).
    pub plot_w: u32,
    /// Modus-Streifen oben.
    pub strip_y: u32,
    /// Bitraten-Lane oben/Hoehe.
    pub lane_y: u32,
    /// Siehe `lane_y`.
    pub lane_h: u32,
    /// Legende oben.
    pub legend_y: u32,
}

/// Layout aus Plot-Breite berechnen.
#[must_use]
pub fn layout(plot_w: u32) -> Layout {
    let plot_w = plot_w.max(64);
    let plot_x = 56;
    let plot_y = 30;
    let strip_y = plot_y + BINS as u32 + 4;
    let lane_y = strip_y + 14 + 4;
    let lane_h = 56;
    let legend_y = lane_y + lane_h + 16;
    Layout {
        width: plot_x + plot_w + 8,
        height: legend_y + 22,
        plot_x,
        plot_y,
        plot_w,
        strip_y,
        lane_y,
        lane_h,
        legend_y,
    }
}

/// Log-Bin-Index zu Frequenz in Hz (Umkehrung der Achse).
fn bin_of(hz: f32) -> usize {
    let b = BINS as f32 * (hz / 50.0).ln() / 400.0f32.ln();
    (b.round() as usize).min(BINS - 1)
}

/// Rendert Analyse + Spektrum-Spalten (ein Hop = 20 ms).
#[must_use]
pub fn render(analysis: &Analysis, cols: &[[f32; BINS]], plot_w: u32) -> Image {
    let lay = layout(plot_w);
    let mut img = Image::new(lay.width, lay.height, BG);
    header(&mut img, analysis);
    if cols.is_empty() || analysis.frames.is_empty() {
        img.text(lay.plot_x + 8, lay.plot_y + 8, "KEINE FRAMES", FG);
        return img;
    }
    waterfall(&mut img, &lay, analysis, cols);
    strip(&mut img, &lay, analysis, cols.len());
    lane(&mut img, &lay, analysis);
    axes(&mut img, &lay, analysis);
    legend(&mut img, &lay);
    img
}

/// Kopfzeilen: Kennzahlen der Summary.
fn header(img: &mut Image, a: &Analysis) {
    let s = &a.summary;
    let l1 = format!(
        "FRAMES {} DAUER {:.1}S AVG {:.1}KBPS ROH {:.1}X CH {}",
        s.frames,
        s.duration_ms / 1000.0,
        s.avg_kbps,
        s.ratio,
        s.channels,
    );
    let l2 = format!(
        "SILK {:.0}% HYBRID {:.0}% CELT {:.0}% OVERHEAD {:.1}%",
        s.frac_silk * 100.0,
        s.frac_hybrid * 100.0,
        s.frac_celt * 100.0,
        s.overhead_pct,
    );
    img.text(8, 5, &l1, FG);
    img.text(8, 15, &l2, FG);
}

/// Wasserfall: dB → Farbe, Modus-Toenung, Bandgrenze weiss.
fn waterfall(img: &mut Image, lay: &Layout, a: &Analysis, cols: &[[f32; BINS]]) {
    let n = cols.len() as u32;
    for x in 0..lay.plot_w {
        // Hops dieses Pixels (Max-Pooling = spitzenerhaltend).
        let h0 = (u64::from(x) * u64::from(n) / u64::from(lay.plot_w)) as usize;
        let h1 =
            (u64::from(x + 1) * u64::from(n) / u64::from(lay.plot_w)).max(h0 as u64 + 1) as usize;
        let mut db = [DB_FLOOR; BINS];
        for col in &cols[h0..h1.min(cols.len())] {
            for (d, v) in db.iter_mut().zip(col) {
                *d = d.max(*v);
            }
        }
        let t_ms = (h0 + h1) as f64 / 2.0 * HOP_MS;
        let frame = a.frame_at(t_ms);
        let tint = frame.map(|f| mode_color(f.mode));
        for (b, d) in db.iter().enumerate() {
            let c = colormap((d - DB_FLOOR) / -DB_FLOOR);
            let y = lay.plot_y + BINS as u32 - 1 - b as u32;
            img.set(lay.plot_x as i32 + x as i32, y as i32, c);
            if let Some(t) = tint {
                img.blend(lay.plot_x + x, y, t, 0.22);
            }
        }
        // Bandgrenze des Frames als weisser Punkt.
        if let Some(f) = frame {
            let bw = f.bandwidth.hz() as f32;
            let y = lay.plot_y + BINS as u32 - 1 - bin_of(bw) as u32;
            img.set(lay.plot_x as i32 + x as i32, y as i32, [255; 3]);
        }
    }
}

/// Modus-Streifen: Vollfarbe pro Pixel (Mitte des hops entscheidet).
fn strip(img: &mut Image, lay: &Layout, a: &Analysis, hops: usize) {
    for x in 0..lay.plot_w {
        let h = (u64::from(x) * hops as u64 / u64::from(lay.plot_w)) as usize;
        if let Some(f) = a.frame_at(h as f64 * HOP_MS) {
            img.rect(lay.plot_x + x, lay.strip_y, 1, 12, mode_color(f.mode));
        }
    }
}

/// Bitraten-Lane: Balken pro Frame + AVG-Linie.
fn lane(img: &mut Image, lay: &Layout, a: &Analysis) {
    let s = &a.summary;
    let max = a.frames.iter().map(|f| f.kbps).fold(0.0f64, f64::max);
    let scale = (2.0 * s.avg_kbps).max(max).max(1.0);
    let dur = s.duration_ms.max(1.0);
    img.rect(lay.plot_x, lay.lane_y, lay.plot_w, lay.lane_h, [20, 24, 32]);
    for f in &a.frames {
        let x0 = (f.t_start_ms / dur * lay.plot_w as f64) as u32;
        let x1 = ((f.t_start_ms + f.duration_ms) / dur * lay.plot_w as f64).ceil() as u32;
        let h = (f.kbps / scale * lay.lane_h as f64).ceil() as u32;
        let h = h.clamp(1, lay.lane_h);
        img.rect(
            lay.plot_x + x0.min(lay.plot_w - 1),
            lay.lane_y + lay.lane_h - h,
            (x1 - x0).clamp(1, lay.plot_w - x0.min(lay.plot_w - 1)),
            h,
            [255, 190, 90],
        );
    }
    // AVG-Linie gestrichelt weiss.
    let y = lay.lane_y + lay.lane_h - (s.avg_kbps / scale * lay.lane_h as f64) as u32;
    for x in (0..lay.plot_w).step_by(4) {
        img.set(lay.plot_x as i32 + x as i32, y as i32, [255; 3]);
    }
    img.text(2, lay.lane_y, &format!("{:.0}K", scale), FG);
}

/// Achsen: Frequenz-Ticks links, Zeit-Ticks unter der Lane.
fn axes(img: &mut Image, lay: &Layout, a: &Analysis) {
    for (hz, label) in [
        (100.0, "100"),
        (500.0, "500"),
        (1000.0, "1K"),
        (5000.0, "5K"),
        (10000.0, "10K"),
        (20000.0, "20K"),
    ] {
        let y = lay.plot_y + BINS as u32 - 1 - bin_of(hz) as u32;
        img.rect(0, y, lay.plot_x - 2, 1, GRID);
        img.text(2, y.saturating_sub(3), label, FG);
    }
    let dur_s = a.summary.duration_ms / 1000.0;
    let step = nice_step(dur_s, lay.plot_w);
    let mut t = 0.0;
    while t <= dur_s {
        let x = lay.plot_x + (t / dur_s * lay.plot_w as f64) as u32;
        img.rect(x, lay.lane_y + lay.lane_h + 2, 1, 4, GRID);
        img.text(x + 2, lay.lane_y + lay.lane_h + 3, &fmt_time(t), FG);
        t += step;
    }
}

/// Zeitschritt, sodass hoechstens ~1 Tick pro 60 px steht.
fn nice_step(dur_s: f64, plot_w: u32) -> f64 {
    for s in [1.0, 2.0, 5.0, 10.0, 15.0, 30.0, 60.0, 120.0, 300.0] {
        if dur_s / s <= f64::from(plot_w) / 60.0 {
            return s;
        }
    }
    600.0
}

/// Sekunden → "NS" bzw. "M:SS".
fn fmt_time(t: f64) -> String {
    if t < 60.0 {
        format!("{t:.0}S")
    } else {
        format!("{}:{:02.0}", t / 60.0, t % 60.0)
    }
}

/// Legende: Modus-Farben + dB-Verlauf.
fn legend(img: &mut Image, lay: &Layout) {
    let mut x = 8;
    for (mode, label) in [
        (Mode::Silk, "SILK SPRACHE"),
        (Mode::Hybrid, "HYBRID"),
        (Mode::Celt, "CELT MUSIK"),
    ] {
        img.rect(x, lay.legend_y, 10, 7, mode_color(mode));
        x += 12;
        x += img.text(x, lay.legend_y, label, FG);
        x += 10;
    }
    img.text(x, lay.legend_y, "DB", FG);
    x += 18;
    for i in 0..60 {
        let c = colormap(i as f32 / 59.0);
        img.rect(x + i, lay.legend_y, 1, 7, c);
    }
    x += 62;
    img.text(x, lay.legend_y, "-72/0", FG);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Bandwidth, FrameMeta, Summary};

    #[test]
    fn colormap_spans_dark_to_bright() {
        let dark = colormap(0.0);
        let bright = colormap(1.0);
        assert!(dark[0] + dark[1] + dark[2] < 60);
        assert!(bright[0] as u16 + bright[1] as u16 > 500);
        // Mitte ist bunt (nicht grau).
        let mid = colormap(0.5);
        assert!(mid[0].abs_diff(mid[2]) > 40);
    }

    #[test]
    fn text_advances_and_marks_pixels() {
        let mut img = Image::new(40, 10, BG);
        let w = img.text(0, 0, "AB", FG);
        assert_eq!(w, 12);
        assert!(img.rgb.iter().any(|&v| v > 200));
        // Unbekanntes Zeichen → Ersatzglyphe (kein Panic, kein Leer).
        let mut img2 = Image::new(40, 10, BG);
        img2.text(0, 0, "ä", FG);
        assert!(img2.rgb.iter().any(|&v| v > 0));
    }

    fn two_frames() -> Analysis {
        let mk = |index: usize, t: f64, mode: Mode| FrameMeta {
            index,
            t_start_ms: t,
            duration_ms: 20.0,
            mode,
            bandwidth: Bandwidth::Full,
            channels: 1,
            bytes: 80,
            kbps: 32.0,
        };
        Analysis {
            frames: vec![mk(0, 0.0, Mode::Silk), mk(1, 20.0, Mode::Celt)],
            summary: Summary {
                frames: 2,
                duration_ms: 40.0,
                audio_bytes: 160,
                total_bytes: 200,
                avg_kbps: 32.0,
                raw_kbps: 768.0,
                ratio: 24.0,
                overhead_pct: 20.0,
                frac_silk: 0.5,
                frac_hybrid: 0.0,
                frac_celt: 0.5,
                channels: 1,
            },
        }
    }

    #[test]
    fn render_places_modes_and_bars() {
        let a = two_frames();
        let cols = vec![[DB_FLOOR; BINS], [-6.0; BINS]];
        let lay = layout(64);
        let img = render(&a, &cols, 64);
        assert_eq!((img.width, img.height), (lay.width, lay.height));
        // Streifen: links SILK-blau, rechts CELT-amber.
        assert_eq!(img.get(lay.plot_x, lay.strip_y), mode_color(Mode::Silk));
        assert_eq!(
            img.get(lay.plot_x + lay.plot_w - 1, lay.strip_y),
            mode_color(Mode::Celt)
        );
        // Lane enthaelt Balken (nicht nur Hintergrund).
        let mut bars = 0;
        for x in 0..lay.plot_w {
            let p = img.get(lay.plot_x + x, lay.lane_y + lay.lane_h - 1);
            if p != [20, 24, 32] {
                bars += 1;
            }
        }
        assert!(bars > lay.plot_w / 2);
        // Kopfzeile enthaelt helle Textpixel.
        assert!(
            img.rgb[..lay.width as usize * 30 * 3]
                .iter()
                .any(|&v| v > 200)
        );
    }
}
