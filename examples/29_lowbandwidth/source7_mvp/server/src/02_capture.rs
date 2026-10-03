//! `02_capture` — Bildquellen: Scrap-Ausschnitt oder synthetisch (Tests).
//! Die Session kennt nur den Trait [`FrameSource`].

use std::sync::{Arc, Mutex};

use image::RgbImage;

/// Liefert RGB-Frames fester Größe. Läuft im Session-Thread (kein `Send`:
/// `scrap::Capturer` ist `!Send`).
pub trait FrameSource {
    /// Breite/Höhe jedes Frames.
    fn size(&self) -> (u32, u32);
    /// Aktuelles Bild.
    fn grab(&mut self) -> Result<RgbImage, String>;
}

/// Rechteckiger Ausschnitt der primären Anzeige (via `scrap`, X11/MIT-SHM).
/// Das Display kommt aus `$DISPLAY`.
pub struct ScrapSource {
    cap: scrap::Capturer,
    fw: u32,
    fh: u32,
    x: u32,
    y: u32,
    w: u32,
    h: u32,
}

impl ScrapSource {
    /// Öffnet die primäre Anzeige und prüft den Ausschnitt.
    pub fn open(x: u32, y: u32, w: u32, h: u32) -> Result<Self, String> {
        let d = scrap::Display::primary().map_err(|e| format!("scrap: {e}"))?;
        let (fw, fh) = (d.width() as u32, d.height() as u32);
        if x + w > fw || y + h > fh {
            return Err(format!("Ausschnitt {w}x{h}@{x},{y} außerhalb {fw}x{fh}"));
        }
        let cap = scrap::Capturer::new(d).map_err(|e| format!("scrap: {e}"))?;
        Ok(Self {
            cap,
            fw,
            fh,
            x,
            y,
            w,
            h,
        })
    }

    /// Wartet auf einen frischen Frame (scrap liefert anfangs `WouldBlock`).
    fn frame(&mut self) -> Result<Vec<u8>, String> {
        for _ in 0..300 {
            match self.cap.frame() {
                Ok(f) => return Ok(f.to_vec()),
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                Err(e) => return Err(format!("capture: {e}")),
            }
        }
        Err("capture: kein Frame nach 3 s".into())
    }
}

impl FrameSource for ScrapSource {
    fn size(&self) -> (u32, u32) {
        (self.w, self.h)
    }

    fn grab(&mut self) -> Result<RgbImage, String> {
        let f = self.frame()?;
        Ok(crop_bgrx_to_rgb(
            &f, self.fw, self.fh, self.x, self.y, self.w, self.h,
        ))
    }
}

/// Schneidet `(x, y, w, h)` aus einem BGRX-Vollbild (`fw*4` Stride) und
/// wandelt nach RGB.
fn crop_bgrx_to_rgb(f: &[u8], fw: u32, _fh: u32, x: u32, y: u32, w: u32, h: u32) -> RgbImage {
    let mut out = RgbImage::new(w, h);
    for dy in 0..h {
        for dx in 0..w {
            let s = (((y + dy) * fw + (x + dx)) * 4) as usize;
            out.put_pixel(dx, dy, image::Rgb([f[s + 2], f[s + 1], f[s]]));
        }
    }
    out
}

/// Synthetische Quelle: liefert das Bild, das zuletzt per Handle gesetzt wurde.
#[derive(Clone)]
pub struct SharedSource {
    img: Arc<Mutex<RgbImage>>,
    w: u32,
    h: u32,
}

impl SharedSource {
    #[must_use]
    pub fn new(img: RgbImage) -> Self {
        let (w, h) = (img.width(), img.height());
        Self {
            img: Arc::new(Mutex::new(img)),
            w,
            h,
        }
    }

    /// Ersetzt das Bild (Test simuliert Bildschirmänderung).
    pub fn set(&self, img: RgbImage) {
        assert_eq!((img.width(), img.height()), (self.w, self.h));
        *self.img.lock().unwrap() = img;
    }
}

impl FrameSource for SharedSource {
    fn size(&self) -> (u32, u32) {
        (self.w, self.h)
    }

    fn grab(&mut self) -> Result<RgbImage, String> {
        Ok(self.img.lock().unwrap().clone())
    }
}

/// Einfarbiges Testbild.
#[must_use]
pub fn solid(w: u32, h: u32, c: [u8; 3]) -> RgbImage {
    RgbImage::from_fn(w, h, |_, _| image::Rgb(c))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_source_follows_updates() {
        let s = SharedSource::new(solid(4, 4, [0; 3]));
        let mut reader = s.clone();
        assert_eq!(reader.grab().unwrap().get_pixel(0, 0).0, [0; 3]);
        s.set(solid(4, 4, [5; 3]));
        assert_eq!(reader.grab().unwrap().get_pixel(3, 3).0, [5; 3]);
        assert_eq!(reader.size(), (4, 4));
    }

    #[test]
    fn bgrx_crop_swaps_channels() {
        // 2×2 BGRX: Pixel (1,0) = R1 G2 B3.
        let f = vec![
            0, 0, 0, 0, 3, 2, 1, 0, //
            0, 0, 0, 0, 0, 0, 0, 0, //
        ];
        let img = crop_bgrx_to_rgb(&f, 2, 2, 1, 0, 1, 1);
        assert_eq!(img.get_pixel(0, 0).0, [1, 2, 3]);
    }
}
