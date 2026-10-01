//! Bitmap-Text: font8x8-Atlas und Zeilen-Layout für die Headerzeile.
//!
//! Die 96 druckbaren ASCII-Zeichen (32..128) werden einmalig in eine
//! RGBA-Atlas-Textur gerastert (16×6-Grid aus 8×8-Zellen). Pro Zeichen
//! erzeugt [`layout_line`] eine GPU-Instanz ([`GlyphInstance`]).

use font8x8::{BASIC_FONTS, UnicodeFonts};

/// Kantenlänge einer Glyphe in Pixeln.
pub const GLYPH_PX: f32 = 8.0;
/// Atlas-Raster: 16 Spalten × 6 Zeilen aus 8×8-Zellen.
pub const ATLAS_COLS: u32 = 16;
pub const ATLAS_ROWS: u32 = 6;
pub const ATLAS_W: u32 = 128;
pub const ATLAS_H: u32 = 48;
const FIRST_CHAR: u32 = 32;
const GLYPH_COUNT: u32 = 96;

/// Erstes Byte einer Zeile in [`build_atlas_rgba`] (Bit 0 = Pixel links).
pub fn build_atlas_rgba() -> Vec<u8> {
    let mut rgba = vec![0u8; (ATLAS_W * ATLAS_H * 4) as usize];
    for cell in 0..GLYPH_COUNT {
        let glyph = BASIC_FONTS
            .get(char::from_u32(FIRST_CHAR + cell).unwrap_or('?'))
            .unwrap_or([0; 8]);
        let col = cell % ATLAS_COLS;
        let row = cell / ATLAS_COLS;
        for (gy, bits) in glyph.iter().enumerate() {
            for gx in 0..8 {
                if bits >> gx & 1 == 1 {
                    let px = col * 8 + gx;
                    let py = row * 8 + gy as u32;
                    let off = ((py * ATLAS_W + px) * 4) as usize;
                    rgba[off..off + 4].copy_from_slice(&[255, 255, 255, 255]);
                }
            }
        }
    }
    rgba
}

/// Atlas-Zelle (0..96) für `c` oder `None` außerhalb von ASCII 32..128.
pub fn glyph_cell(c: char) -> Option<u32> {
    let code = u32::from(c);
    if (FIRST_CHAR..FIRST_CHAR + GLYPH_COUNT).contains(&code) {
        Some(code - FIRST_CHAR)
    } else {
        None
    }
}

/// Häufige Latin-1-Zeichen (z. B. Umlaute in Pfaden) auf ASCII abbilden.
fn transliterate(c: char) -> char {
    match c {
        'ä' => 'a',
        'ö' => 'o',
        'ü' => 'u',
        'Ä' => 'A',
        'Ö' => 'O',
        'Ü' => 'U',
        'ß' => 's',
        'é' | 'è' | 'ê' | 'ë' => 'e',
        'á' | 'à' | 'â' | 'å' => 'a',
        'ó' | 'ò' | 'ô' => 'o',
        'ú' | 'ù' | 'û' => 'u',
        'ñ' => 'n',
        'ç' => 'c',
        _ => c,
    }
}

/// Eine Text-Instanz für den Shader: Position (Pixel, oben links), Zelle, Skalierung.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct GlyphInstance {
    pub x: f32,
    pub y: f32,
    pub cell: u32,
    pub scale: f32,
}

/// Legt `text` als Instanz-Zeile ab `(x, y)`; bricht bei `max_width_px` ab.
/// Unbekannte Zeichen werden `?`, Steuerzeichen (außer Space) übersprungen.
pub fn layout_line(
    text: &str,
    x: f32,
    y: f32,
    scale: f32,
    max_width_px: f32,
) -> Vec<GlyphInstance> {
    let mut out = Vec::new();
    let mut cx = x;
    for mut c in text.chars() {
        if c.is_control() {
            continue;
        }
        c = transliterate(c);
        let cell = glyph_cell(c).unwrap_or(glyph_cell('?').unwrap_or(0));
        if cx + GLYPH_PX * scale - x > max_width_px {
            break;
        }
        out.push(GlyphInstance {
            x: cx,
            y,
            cell,
            scale,
        });
        cx += GLYPH_PX * scale;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atlas_dims_and_content() {
        let rgba = build_atlas_rgba();
        assert_eq!(rgba.len(), (ATLAS_W * ATLAS_H * 4) as usize);
        // 'A' (Zelle 33): erste Zeile hat Pixel 2 und 3 gesetzt (LSB = links).
        let cell = glyph_cell('A').unwrap();
        let (col, row) = (cell % ATLAS_COLS, cell / ATLAS_COLS);
        let alpha = |gx: u32| {
            let off = (((row * 8) * ATLAS_W + col * 8 + gx) * 4 + 3) as usize;
            rgba[off]
        };
        assert_eq!((alpha(0), alpha(1)), (0, 0));
        assert_eq!((alpha(2), alpha(3)), (255, 255));
        assert_eq!((alpha(4), alpha(7)), (0, 0));
        // Space ist leer.
        let space = glyph_cell(' ').unwrap();
        let (col, row) = (space % ATLAS_COLS, space / ATLAS_COLS);
        let off = (((row * 8) * ATLAS_W + col * 8) * 4 + 3) as usize;
        assert!(rgba[off..off + 8 * 4].iter().all(|&b| b == 0));
    }

    #[test]
    fn cells_cover_ascii_printable() {
        assert_eq!(glyph_cell(' '), Some(0));
        assert_eq!(glyph_cell('~'), Some(94));
        assert_eq!(glyph_cell('€'), None);
        assert_eq!(glyph_cell('ä'), None);
    }

    #[test]
    fn line_layout_truncates_and_transliterates() {
        let glyphs = layout_line("aäb", 0.0, 0.0, 2.0, 1000.0);
        assert_eq!(glyphs.len(), 3);
        assert_eq!(glyphs[0].x, 0.0);
        assert_eq!(glyphs[1].x, 16.0);
        // 'ä' -> 'a'.
        assert_eq!(glyphs[1].cell, glyph_cell('a').unwrap());
        // Trunkierung: nur 2 Zeichen à 16 px passen in 40 px.
        let short = layout_line("abcdef", 10.0, 5.0, 2.0, 40.0);
        assert_eq!(short.len(), 2);
        assert_eq!(short[0].y, 5.0);
    }

    #[test]
    fn instance_is_16_bytes() {
        assert_eq!(std::mem::size_of::<GlyphInstance>(), 16);
    }
}
