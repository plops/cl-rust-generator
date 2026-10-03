//! `04_scene` — Client-Modell des entfernten Bildschirms: RGBA-Canvas
//! (aus AV1-Kacheln) plus Textelemente (aus Clear/Add-Nachrichten).
//! Fest 640×640, ohne Skalierungscode. Rein, ohne Grafik-Kontext
//! testbar; `05_app` zeichnet daraus.

use std::time::Instant;

use lbw_common::{SIZE, TextItem};

use crate::net::Event;

/// Verbindungsstatus fürs HUD.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Link {
    Connecting,
    Up,
    /// Getrennt seit … (Grund).
    Down(Instant, String),
}

/// Zustand der Anzeige (immer 640×640 RGBA).
pub struct Scene {
    pub canvas: Vec<u8>,
    pub texts: Vec<TextItem>,
    /// Canvas seit dem letzten Upload verändert.
    pub dirty: bool,
    pub link: Link,
    pub last_rx: Instant,
    pub tiles: u32,
    pub tile_bytes: u64,
}

const N: usize = SIZE as usize;

impl Scene {
    #[must_use]
    pub fn new() -> Self {
        Self {
            canvas: [24, 24, 32, 255].repeat(N * N),
            texts: Vec::new(),
            dirty: true,
            link: Link::Connecting,
            last_rx: Instant::now(),
            tiles: 0,
            tile_bytes: 0,
        }
    }

    /// Kopiert eine `w`×`h`-RGBA-Box an (`x`, `y`). Kaputte Boxen
    /// werden ignoriert statt den Client abstürzen zu lassen.
    pub fn blit(&mut self, x: u16, y: u16, w: usize, h: usize, rgba: &[u8]) {
        let (x0, y0) = (x as usize, y as usize);
        if x0 + w > N || y0 + h > N || rgba.len() < w * h * 4 {
            return;
        }
        for row in 0..h {
            let s = row * w * 4;
            let d = ((y0 + row) * N + x0) * 4;
            self.canvas[d..d + w * 4].copy_from_slice(&rgba[s..s + w * 4]);
        }
        self.dirty = true;
    }

    /// Wendet ein Netz-Ereignis an.
    pub fn apply(&mut self, e: Event) {
        self.last_rx = Instant::now();
        match e {
            Event::Connected => self.link = Link::Up,
            Event::Disconnected(why) => {
                if !matches!(self.link, Link::Down(..)) {
                    self.link = Link::Down(Instant::now(), why);
                }
            }
            Event::ClearText => self.texts.clear(),
            Event::AddText(t) => self.texts.push(t),
            Event::Tile {
                x,
                y,
                w,
                h,
                rgba,
                bytes,
            } => {
                self.blit(x, y, w, h, &rgba);
                self.tiles += 1;
                self.tile_bytes += bytes as u64;
            }
        }
    }

    /// Farbe des Canvas an `(x, y)` (Tests/Debug).
    #[must_use]
    pub fn pixel(&self, x: usize, y: usize) -> [u8; 3] {
        let i = (y * N + x) * 4;
        [self.canvas[i], self.canvas[i + 1], self.canvas[i + 2]]
    }
}

impl Default for Scene {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::Rect;

    fn item(text: &str) -> TextItem {
        TextItem {
            rect: Rect::new(0, 0, 10, 10),
            fg: [0; 3],
            bg: [255; 3],
            text: text.into(),
        }
    }

    #[test]
    fn clear_and_add_texts() {
        let mut s = Scene::new();
        s.apply(Event::AddText(item("a")));
        s.apply(Event::AddText(item("b")));
        assert_eq!(s.texts.len(), 2);
        s.apply(Event::ClearText);
        assert!(s.texts.is_empty());
        s.apply(Event::AddText(item("c")));
        assert_eq!(s.texts[0].text, "c");
    }

    #[test]
    fn blit_places_tile_and_ignores_garbage() {
        let mut s = Scene::new();
        s.dirty = false;
        s.blit(64, 0, 64, 64, &[200, 100, 50, 255].repeat(64 * 64));
        assert!(s.dirty);
        assert_eq!(s.pixel(64, 0), [200, 100, 50]);
        assert_eq!(s.pixel(127, 63), [200, 100, 50]);
        assert_eq!(s.pixel(63, 0), [24, 24, 32]);
        // Außerhalb und zu kurz: ignoriert.
        s.blit(640, 0, 64, 64, &[0; 64 * 64 * 4]);
        s.blit(0, 0, 64, 64, &[0; 10]);
        assert_eq!(s.pixel(0, 0), [24, 24, 32]);
    }

    #[test]
    fn blit_handles_arbitrary_box_sizes() {
        let mut s = Scene::new();
        s.blit(100, 100, 16, 92, &[10, 20, 30, 255].repeat(16 * 92));
        assert_eq!(s.pixel(100, 100), [10, 20, 30]);
        assert_eq!(s.pixel(115, 191), [10, 20, 30]);
        assert_eq!(s.pixel(116, 100), [24, 24, 32]);
        assert_eq!(s.pixel(100, 192), [24, 24, 32]);
    }

    #[test]
    fn link_state_follows_events() {
        let mut s = Scene::new();
        s.apply(Event::Connected);
        assert_eq!(s.link, Link::Up);
        s.apply(Event::Disconnected("x".into()));
        let Link::Down(t, _) = s.link.clone() else {
            panic!()
        };
        s.apply(Event::Disconnected("y".into()));
        assert_eq!(s.link, Link::Down(t, "x".into()), "erste Trennung zählt");
    }
}
