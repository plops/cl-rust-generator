//! `01_image` — minimales RGB-Bild: PPM (P6) lesen/schreiben, BGRA-Import
//! aus X11, Rechtecke zeichnen. Bewusst ohne Bild-Crate.

use std::io::{self, Read, Write};

/// Interleaved RGB8, Zeilen ohne Padding.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rgb {
    pub w: usize,
    pub h: usize,
    pub data: Vec<u8>,
}

impl Rgb {
    /// Einfarbiges Bild.
    #[must_use]
    pub fn filled(w: usize, h: usize, c: [u8; 3]) -> Self {
        Self {
            w,
            h,
            data: c.repeat(w * h),
        }
    }

    /// X11-Z-Pixmap (BGRX, 4 Byte/Pixel) → RGB.
    #[must_use]
    pub fn from_bgra(w: usize, h: usize, bgra: &[u8]) -> Self {
        let (px, _) = bgra.as_chunks::<4>();
        let mut data = Vec::with_capacity(w * h * 3);
        for p in px.iter().take(w * h) {
            data.extend_from_slice(&[p[2], p[1], p[0]]);
        }
        Self { w, h, data }
    }

    /// Pixel an `(x, y)`.
    #[must_use]
    pub fn get(&self, x: usize, y: usize) -> [u8; 3] {
        let i = (y * self.w + x) * 3;
        [self.data[i], self.data[i + 1], self.data[i + 2]]
    }

    /// Setzt Pixel, ignoriert Koordinaten außerhalb.
    pub fn put(&mut self, x: i64, y: i64, c: [u8; 3]) {
        if x >= 0 && y >= 0 && (x as usize) < self.w && (y as usize) < self.h {
            let i = (y as usize * self.w + x as usize) * 3;
            self.data[i..i + 3].copy_from_slice(&c);
        }
    }

    /// Rechteck-Umriss mit Linienstärke `t` (für annotierte Ausgabe).
    pub fn draw_rect(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, t: i64, c: [u8; 3]) {
        let (x1, y1, x2, y2) = (x1 as i64, y1 as i64, x2 as i64, y2 as i64);
        for k in 0..t {
            for x in x1..=x2 {
                self.put(x, y1 + k, c);
                self.put(x, y2 - k, c);
            }
            for y in y1..=y2 {
                self.put(x1 + k, y, c);
                self.put(x2 - k, y, c);
            }
        }
    }

    /// Liest binäres PPM (P6, maxval 255).
    pub fn read_ppm(mut r: impl Read) -> io::Result<Self> {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        let mut pos = 0;
        let mut fields = [0usize; 3];
        if next_token(&buf, &mut pos) != b"P6" {
            return Err(bad("kein P6-PPM"));
        }
        for f in &mut fields {
            let tok = next_token(&buf, &mut pos);
            *f = std::str::from_utf8(tok)
                .ok()
                .and_then(|s| s.parse().ok())
                .ok_or_else(|| bad("PPM-Header ungültig"))?;
        }
        let [w, h, maxval] = fields;
        if maxval != 255 {
            return Err(bad("nur maxval 255"));
        }
        pos += 1; // genau ein Whitespace nach maxval
        let data = buf
            .get(pos..pos + w * h * 3)
            .ok_or_else(|| bad("PPM zu kurz"))?
            .to_vec();
        Ok(Self { w, h, data })
    }

    /// Schreibt binäres PPM (P6).
    pub fn write_ppm(&self, mut wr: impl Write) -> io::Result<()> {
        write!(wr, "P6\n{} {}\n255\n", self.w, self.h)?;
        wr.write_all(&self.data)
    }
}

fn bad(msg: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, msg)
}

/// Nächstes Header-Token; überspringt Whitespace und `#`-Kommentare.
fn next_token<'a>(buf: &'a [u8], pos: &mut usize) -> &'a [u8] {
    loop {
        while *pos < buf.len() && buf[*pos].is_ascii_whitespace() {
            *pos += 1;
        }
        if *pos < buf.len() && buf[*pos] == b'#' {
            while *pos < buf.len() && buf[*pos] != b'\n' {
                *pos += 1;
            }
        } else {
            break;
        }
    }
    let start = *pos;
    while *pos < buf.len() && !buf[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
    &buf[start..*pos]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ppm_roundtrip_with_comment() {
        let mut img = Rgb::filled(3, 2, [1, 2, 3]);
        img.put(2, 1, [200, 100, 50]);
        let mut out = Vec::new();
        img.write_ppm(&mut out).unwrap();
        // Kommentar einschieben: Parser muss ihn überspringen.
        let mut with_comment = b"P6\n# gimp\n".to_vec();
        with_comment.extend_from_slice(&out[3..]);
        assert_eq!(Rgb::read_ppm(&with_comment[..]).unwrap(), img);
    }

    #[test]
    fn ppm_rejects_truncated() {
        assert!(Rgb::read_ppm(&b"P6 4 4 255\n\x00\x00"[..]).is_err());
        assert!(Rgb::read_ppm(&b"P3 1 1 255\n"[..]).is_err());
    }

    #[test]
    fn bgra_swizzle() {
        let img = Rgb::from_bgra(2, 1, &[10, 20, 30, 0, 40, 50, 60, 0]);
        assert_eq!(img.data, vec![30, 20, 10, 60, 50, 40]);
    }

    #[test]
    fn rect_clips_at_border() {
        let mut img = Rgb::filled(4, 4, [0; 3]);
        img.draw_rect(-2.0, -2.0, 1.0, 1.0, 1, [255; 3]);
        assert_eq!(img.get(1, 0), [255; 3]);
        assert_eq!(img.get(1, 1), [255; 3]);
        assert_eq!(img.get(3, 3), [0; 3]);
    }
}
