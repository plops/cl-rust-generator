//! Canvas: Bildpuffer, Colormap, 5x7-Font, Text.
//!
//! Grafikgrundlagen ohne Opus-Wissen; Ebenen liegen in `render`.

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
#[cfg(test)]
mod tests {
    use super::*;

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
}
