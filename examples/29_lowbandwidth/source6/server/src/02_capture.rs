//! `02_capture` — Bildquellen: X11-Ausschnitt (`GetImage`) oder
//! synthetisch (Tests). Die Pipeline kennt nur den Trait [`FrameSource`].

use std::sync::{Arc, Mutex};

use x11rb::connection::Connection;
use x11rb::protocol::xproto::{ConnectionExt, ImageFormat};
use x11rb::rust_connection::RustConnection;

use crate::image::Rgb;

/// Liefert Frames fester Größe.
pub trait FrameSource: Send {
    /// Breite/Höhe jedes Frames.
    fn size(&self) -> (usize, usize);
    /// Aktuelles Bild.
    fn grab(&mut self) -> Result<Rgb, String>;
}

/// Rechteckiger Ausschnitt des X11-Root-Fensters.
pub struct X11Source {
    conn: RustConnection,
    root: u32,
    x: i16,
    y: i16,
    w: usize,
    h: usize,
}

impl X11Source {
    /// Verbindet zu `display` (None = `$DISPLAY`) und prüft den Ausschnitt.
    pub fn open(
        display: Option<&str>,
        x: usize,
        y: usize,
        w: usize,
        h: usize,
    ) -> Result<Self, String> {
        let (conn, idx) = x11rb::connect(display).map_err(|e| format!("X11: {e}"))?;
        let s = &conn.setup().roots[idx];
        let (sw, sh) = (s.width_in_pixels as usize, s.height_in_pixels as usize);
        if x + w > sw || y + h > sh {
            return Err(format!("Ausschnitt {w}x{h}@{x},{y} außerhalb {sw}x{sh}"));
        }
        let root = s.root;
        Ok(Self {
            conn,
            root,
            x: x as i16,
            y: y as i16,
            w,
            h,
        })
    }
}

impl FrameSource for X11Source {
    fn size(&self) -> (usize, usize) {
        (self.w, self.h)
    }

    fn grab(&mut self) -> Result<Rgb, String> {
        let r = self
            .conn
            .get_image(
                ImageFormat::Z_PIXMAP,
                self.root,
                self.x,
                self.y,
                self.w as u16,
                self.h as u16,
                u32::MAX,
            )
            .map_err(|e| format!("GetImage: {e}"))?
            .reply()
            .map_err(|e| format!("GetImage: {e}"))?;
        if r.data.len() < self.w * self.h * 4 {
            return Err(format!(
                "Tiefe {} nicht unterstützt (24/32 bpp nötig)",
                r.depth
            ));
        }
        Ok(Rgb::from_bgra(self.w, self.h, &r.data))
    }
}

/// Synthetische Quelle: liefert das Bild, das zuletzt per Handle gesetzt wurde.
#[derive(Clone)]
pub struct SharedSource {
    img: Arc<Mutex<Rgb>>,
    w: usize,
    h: usize,
}

impl SharedSource {
    #[must_use]
    pub fn new(img: Rgb) -> Self {
        let (w, h) = (img.w, img.h);
        Self {
            img: Arc::new(Mutex::new(img)),
            w,
            h,
        }
    }

    /// Ersetzt das Bild (Test simuliert Bildschirmänderung).
    pub fn set(&self, img: Rgb) {
        assert_eq!((img.w, img.h), (self.w, self.h));
        *self.img.lock().unwrap() = img;
    }
}

impl FrameSource for SharedSource {
    fn size(&self) -> (usize, usize) {
        (self.w, self.h)
    }

    fn grab(&mut self) -> Result<Rgb, String> {
        Ok(self.img.lock().unwrap().clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_source_follows_updates() {
        let s = SharedSource::new(Rgb::filled(4, 4, [0; 3]));
        let mut reader = s.clone();
        assert_eq!(reader.grab().unwrap().get(0, 0), [0; 3]);
        s.set(Rgb::filled(4, 4, [5; 3]));
        assert_eq!(reader.grab().unwrap().get(3, 3), [5; 3]);
        assert_eq!(reader.size(), (4, 4));
    }
}
