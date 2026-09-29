//! `04_scene` — Client-Modell des entfernten Bildschirms: RGBA-Canvas
//! (aus AV1-Kacheln) plus Textelemente (aus Text-Deltas). Rein, ohne
//! Grafik-Kontext testbar; `07_render` zeichnet daraus.

use std::collections::BTreeMap;
use std::time::Instant;

use lbw_common::{Rect, TextItem};

use crate::net::Event;

/// Verbindungsstatus fürs HUD.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Link {
    Connecting,
    Up,
    /// Getrennt seit … (Grund).
    Down(Instant, String),
}

/// Zustand der Anzeige.
pub struct Scene {
    pub w: usize,
    pub h: usize,
    pub canvas: Vec<u8>,
    pub texts: BTreeMap<u32, TextItem>,
    /// Canvas seit dem letzten Upload verändert.
    pub dirty: bool,
    pub link: Link,
    pub last_rx: Instant,
    /// Serverseitige Statistik (Rate B/s, Bild-Backlog, Kacheln).
    pub stats: (u32, u32, u32),
    pub tiles: u32,
    pub tile_bytes: u64,
}

impl Scene {
    #[must_use]
    pub fn new(w: usize, h: usize) -> Self {
        Self {
            w,
            h,
            canvas: [24, 24, 32, 255].repeat(w * h),
            texts: BTreeMap::new(),
            dirty: true,
            link: Link::Connecting,
            last_rx: Instant::now(),
            stats: (0, 0, 0),
            tiles: 0,
            tile_bytes: 0,
        }
    }

    /// Kopiert eine RGBA-Kachel an `r` (auf den Canvas begrenzt).
    pub fn blit(&mut self, r: Rect, rgba: &[u8]) {
        let (x0, y0) = (r.x as usize, r.y as usize);
        if x0 >= self.w || y0 >= self.h {
            return;
        }
        let tw = r.w as usize;
        let cw = tw.min(self.w - x0);
        for y in 0..(r.h as usize).min(self.h - y0) {
            let s = y * tw * 4;
            let d = ((y0 + y) * self.w + x0) * 4;
            self.canvas[d..d + cw * 4].copy_from_slice(&rgba[s..s + cw * 4]);
        }
        self.dirty = true;
    }

    /// Wendet ein Netz-Ereignis an.
    pub fn apply(&mut self, e: Event) {
        self.last_rx = Instant::now();
        match e {
            Event::Connected { .. } => self.link = Link::Up,
            Event::Disconnected(why) => {
                if !matches!(self.link, Link::Down(..)) {
                    self.link = Link::Down(Instant::now(), why);
                }
            }
            Event::Clear => {
                self.texts.clear();
                self.canvas = [24, 24, 32, 255].repeat(self.w * self.h);
                self.dirty = true;
            }
            Event::Text { remove, add } => {
                for id in remove {
                    self.texts.remove(&id);
                }
                for t in add {
                    self.texts.insert(t.id, t);
                }
            }
            Event::Tile { rect, rgba, bytes } => {
                self.blit(rect, &rgba);
                self.tiles += 1;
                self.tile_bytes += bytes as u64;
            }
            Event::Stats {
                rate,
                backlog,
                tiles,
            } => self.stats = (rate, backlog, tiles),
        }
    }

    /// Farbe des Canvas an `(x, y)` (Tests/Debug).
    #[must_use]
    pub fn pixel(&self, x: usize, y: usize) -> [u8; 3] {
        let i = (y * self.w + x) * 4;
        [self.canvas[i], self.canvas[i + 1], self.canvas[i + 2]]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn item(id: u32, text: &str) -> TextItem {
        TextItem {
            id,
            rect: Rect::new(0, 0, 10, 10),
            fg: [0; 3],
            bg: [255; 3],
            text: text.into(),
        }
    }

    #[test]
    fn text_deltas_and_clear() {
        let mut s = Scene::new(32, 32);
        s.apply(Event::Text {
            remove: vec![],
            add: vec![item(1, "a"), item(2, "b")],
        });
        s.apply(Event::Text {
            remove: vec![1, 77],
            add: vec![item(3, "c")],
        });
        assert_eq!(s.texts.keys().copied().collect::<Vec<_>>(), vec![2, 3]);
        s.apply(Event::Clear);
        assert!(s.texts.is_empty());
    }

    #[test]
    fn blit_places_and_clips() {
        let mut s = Scene::new(32, 32);
        s.dirty = false;
        s.blit(Rect::new(24, 24, 16, 16), &[200, 100, 50, 255].repeat(256));
        assert!(s.dirty);
        assert_eq!(s.pixel(24, 24), [200, 100, 50]);
        assert_eq!(s.pixel(31, 31), [200, 100, 50]);
        assert_eq!(s.pixel(23, 24), [24, 24, 32]);
        s.blit(Rect::new(40, 0, 16, 16), &[0; 1024]); // außerhalb: ignoriert
    }

    #[test]
    fn link_state_follows_events() {
        let mut s = Scene::new(16, 16);
        s.apply(Event::Connected {
            w: 16,
            h: 16,
            resumed: false,
        });
        assert_eq!(s.link, Link::Up);
        s.apply(Event::Disconnected("x".into()));
        let Link::Down(t, _) = s.link.clone() else {
            panic!()
        };
        s.apply(Event::Disconnected("y".into()));
        assert_eq!(s.link, Link::Down(t, "x".into()), "erste Trennung zählt");
    }
}
