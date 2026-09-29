//! `03_image` — minimales RGB8-Bild: BGRA-Import (X11), Ausschneiden,
//! Füllen, Kopieren von Bereichen, PPM-Ein/Ausgabe (Tests/Debug).
//! Bewusst ohne Bild-Crate (Muster aus `26_onnx/source8/src/01_image.rs`).

use lbw_common::Rect;
use std::io::{self, Read, Write};

/// Interleaved RGB8 ohne Zeilen-Padding.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rgb {
    pub w: usize,
    pub h: usize,
    pub data: Vec<u8>,
}

impl Rgb {
    #[must_use]
    pub fn filled(w: usize, h: usize, c: [u8; 3]) -> Self {
        Self {
            w,
            h,
            data: c.repeat(w * h),
        }
    }

    /// X11-Z-Pixmap (BGRX) → RGB.
    #[must_use]
    pub fn from_bgra(w: usize, h: usize, bgra: &[u8]) -> Self {
        let (px, _) = bgra.as_chunks::<4>();
        let mut data = Vec::with_capacity(w * h * 3);
        for p in px.iter().take(w * h) {
            data.extend_from_slice(&[p[2], p[1], p[0]]);
        }
        Self { w, h, data }
    }

    #[must_use]
    pub fn get(&self, x: usize, y: usize) -> [u8; 3] {
        let i = (y * self.w + x) * 3;
        [self.data[i], self.data[i + 1], self.data[i + 2]]
    }

    pub fn put(&mut self, x: usize, y: usize, c: [u8; 3]) {
        if x < self.w && y < self.h {
            let i = (y * self.w + x) * 3;
            self.data[i..i + 3].copy_from_slice(&c);
        }
    }

    /// Rechteck auf das Bild begrenzen.
    #[must_use]
    pub fn clamp(&self, r: Rect) -> Rect {
        let x = (r.x as usize).min(self.w);
        let y = (r.y as usize).min(self.h);
        let w = (r.w as usize).min(self.w - x);
        let h = (r.h as usize).min(self.h - y);
        Rect::new(x as u16, y as u16, w as u16, h as u16)
    }

    /// Füllt `r` (geclippt) mit `c`.
    pub fn fill(&mut self, r: Rect, c: [u8; 3]) {
        let r = self.clamp(r);
        for y in r.y as usize..r.y2() as usize {
            for x in r.x as usize..r.x2() as usize {
                self.put(x, y, c);
            }
        }
    }

    /// Ausschnitt als RGB-Bytes (`r` muss im Bild liegen).
    #[must_use]
    pub fn crop(&self, r: Rect) -> Vec<u8> {
        let mut out = Vec::with_capacity(r.area() as usize * 3);
        for y in r.y as usize..r.y2() as usize {
            let s = (y * self.w + r.x as usize) * 3;
            out.extend_from_slice(&self.data[s..s + r.w as usize * 3]);
        }
        out
    }

    /// Kopiert Bereich `r` aus `src` (gleiche Größe) in dieses Bild.
    pub fn copy_from(&mut self, src: &Rgb, r: Rect) {
        let r = self.clamp(r);
        for y in r.y as usize..r.y2() as usize {
            let s = (y * self.w + r.x as usize) * 3;
            let e = s + r.w as usize * 3;
            self.data[s..e].copy_from_slice(&src.data[s..e]);
        }
    }

    /// Liest binäres PPM (P6, maxval 255, ohne Kommentare).
    pub fn read_ppm(mut r: impl Read) -> io::Result<Self> {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        let bad = || io::Error::new(io::ErrorKind::InvalidData, "PPM");
        let mut fields = Vec::new();
        let mut pos = 0;
        while fields.len() < 4 {
            while pos < buf.len() && buf[pos].is_ascii_whitespace() {
                pos += 1;
            }
            let s = pos;
            while pos < buf.len() && !buf[pos].is_ascii_whitespace() {
                pos += 1;
            }
            if s == pos {
                return Err(bad());
            }
            fields.push(
                std::str::from_utf8(&buf[s..pos])
                    .map_err(|_| bad())?
                    .to_owned(),
            );
        }
        pos += 1;
        let w: usize = fields[1].parse().map_err(|_| bad())?;
        let h: usize = fields[2].parse().map_err(|_| bad())?;
        if fields[0] != "P6" || fields[3] != "255" {
            return Err(bad());
        }
        let data = buf.get(pos..pos + w * h * 3).ok_or_else(bad)?.to_vec();
        Ok(Self { w, h, data })
    }

    /// Schreibt binäres PPM (P6).
    pub fn write_ppm(&self, mut w: impl Write) -> io::Result<()> {
        write!(w, "P6\n{} {}\n255\n", self.w, self.h)?;
        w.write_all(&self.data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fill_crop_copy() {
        let mut a = Rgb::filled(8, 6, [0; 3]);
        a.fill(Rect::new(2, 1, 3, 2), [9, 8, 7]);
        assert_eq!(a.get(2, 1), [9, 8, 7]);
        assert_eq!(a.get(5, 1), [0; 3]);
        assert_eq!(a.crop(Rect::new(2, 1, 3, 2)), [9, 8, 7].repeat(6));
        let mut b = Rgb::filled(8, 6, [1; 3]);
        b.copy_from(&a, Rect::new(0, 0, 4, 2));
        assert_eq!(b.get(3, 1), [9, 8, 7]);
        assert_eq!(b.get(4, 1), [1; 3]);
    }

    #[test]
    fn clamp_and_fill_outside_is_safe() {
        let mut a = Rgb::filled(4, 4, [0; 3]);
        assert_eq!(a.clamp(Rect::new(3, 3, 10, 10)), Rect::new(3, 3, 1, 1));
        a.fill(Rect::new(10, 10, 5, 5), [1; 3]);
        assert!(a.data.iter().all(|&v| v == 0));
    }

    #[test]
    fn ppm_roundtrip_and_bgra() {
        let img = Rgb::from_bgra(2, 1, &[10, 20, 30, 0, 40, 50, 60, 0]);
        assert_eq!(img.data, vec![30, 20, 10, 60, 50, 40]);
        let mut out = Vec::new();
        img.write_ppm(&mut out).unwrap();
        assert_eq!(Rgb::read_ppm(&out[..]).unwrap(), img);
        assert!(Rgb::read_ppm(&b"P6 4 4 255\n\0"[..]).is_err());
    }
}
