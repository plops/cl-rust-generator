//! `01_engine` — eine Verbindung samt Szene, von Kotlin im UI-Takt gepollt.
//!
//! Der Netz-Thread (`lbw_client::net`) empfängt, setzt Kacheln zusammen und
//! dekodiert AV1; `poll` wendet die Ereignisse auf die Szene an und kopiert
//! den Canvas nur bei Änderung in den Puffer der UI (Direct-`ByteBuffer`).
//! Die Canvasgröße folgt dem `Hello` des Servers.

use std::time::{Duration, Instant};

use lbw_client::input::MouseThrottle;
use lbw_client::net::{Event, Net, NetCfg, spawn};
use lbw_client::scene::{Link, Scene};
use lbw_client::select::{drag_rect, paste_chunks, selected_text};
use lbw_common::Input;

use crate::blob::{encode_texts, status_line};

/// Bits im Rückgabewert von [`Engine::poll`].
pub mod flags {
    /// Canvas wurde in den Puffer kopiert.
    pub const FRAME: i32 = 1;
    /// Texte geändert → `texts()` abholen.
    pub const TEXT: i32 = 2;
    /// Canvasgröße geändert → Puffer neu anlegen (`size()`), nichts kopiert.
    pub const SIZE: i32 = 4;
    /// Verbindung steht.
    pub const UP: i32 = 8;
}

/// Startgröße bis zum ersten `Hello`.
pub const DEFAULT_SIZE: usize = 640;

pub struct Engine {
    net: Net,
    pub scene: Scene,
    texts_changed: bool,
    size_changed: bool,
    mouse: MouseThrottle,
}

impl Engine {
    /// Startet den Netz-Thread zu `addr` (`host:port`).
    #[must_use]
    pub fn new(addr: &str, dead_after: Duration) -> Self {
        let net = spawn(NetCfg {
            addr: addr.to_owned(),
            dead_after,
            verbose: false,
        });
        Self {
            net,
            scene: Scene::new(DEFAULT_SIZE, DEFAULT_SIZE),
            texts_changed: true,
            size_changed: false,
            mouse: MouseThrottle::new(30),
        }
    }

    /// Ereignisse anwenden; Canvas nach `out` kopieren, falls geändert
    /// und `out` groß genug ist. Liefert [`flags`].
    pub fn poll(&mut self, out: Option<&mut [u8]>) -> i32 {
        while let Ok(e) = self.net.events.try_recv() {
            match &e {
                Event::Connected { w, h, .. }
                    if (usize::from(*w), usize::from(*h)) != (self.scene.w, self.scene.h) =>
                {
                    let (tiles, bytes) = (self.scene.tiles, self.scene.tile_bytes);
                    self.scene = Scene::new(usize::from(*w).max(1), usize::from(*h).max(1));
                    (self.scene.tiles, self.scene.tile_bytes) = (tiles, bytes);
                    self.size_changed = true;
                    self.texts_changed = true;
                }
                Event::Text { .. } | Event::Clear => self.texts_changed = true,
                _ => {}
            }
            self.scene.apply(e);
        }
        let mut f = 0;
        if self.size_changed {
            self.size_changed = false;
            f |= flags::SIZE;
        } else if self.scene.dirty
            && let Some(buf) = out
            && buf.len() >= self.scene.canvas.len()
        {
            buf[..self.scene.canvas.len()].copy_from_slice(&self.scene.canvas);
            self.scene.dirty = false;
            f |= flags::FRAME;
        }
        if self.texts_changed {
            f |= flags::TEXT;
        }
        if self.scene.link == Link::Up {
            f |= flags::UP;
        }
        f
    }

    /// Canvasgröße `(w, h)` in Server-Pixeln.
    #[must_use]
    pub fn size(&self) -> (usize, usize) {
        (self.scene.w, self.scene.h)
    }

    /// Alle Texte als Blob (siehe `02_blob`); setzt das TEXT-Flag zurück.
    pub fn texts(&mut self) -> Vec<u8> {
        self.texts_changed = false;
        encode_texts(self.scene.texts.values())
    }

    /// HUD-Zeile.
    #[must_use]
    pub fn status(&self) -> String {
        status_line(&self.scene, Instant::now())
    }

    pub fn send(&self, i: Input) {
        self.net.send(i);
    }

    /// Mausposition (Server-Pixel, wird begrenzt); `force` vor Klicks.
    pub fn mouse(&mut self, x: i32, y: i32, force: bool) {
        let cx = x.clamp(0, self.scene.w as i32 - 1) as u16;
        let cy = y.clamp(0, self.scene.h as i32 - 1) as u16;
        if let Some(i) = self.mouse.update(cx, cy, Instant::now(), force) {
            self.send(i);
        }
    }

    /// Zwischenablage tippen lassen (in Frame-taugliche Stücke geteilt).
    pub fn paste(&self, s: &str) {
        for part in paste_chunks(s) {
            self.send(Input::Text(part));
        }
    }

    /// Text im Rechteck zweier Eckpunkte (Server-Pixel).
    #[must_use]
    pub fn select(&self, a: (f32, f32), b: (f32, f32)) -> String {
        selected_text(drag_rect(a, b), self.scene.texts.values())
    }
}
