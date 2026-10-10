//! Gemeinsame Loopback-Helfer (Stub-OCR, Config, Items, Leser).
//! Wird per `mod common` in `loopback.rs` und `reconnect.rs` gezogen.

use std::net::TcpStream;
use std::time::{Duration, Instant};

use clap::Parser;
use image::RgbImage;
use lbw_common::framing::FrameReader;
use lbw_common::{Rect, ServerMsg, TextItem};
use lbw_server::config::Config;
use lbw_server::session::Recognize;

/// Stub-OCR mit von außen wechselbarem Ergebnis (Delta-Tests).
#[derive(Clone)]
pub struct StubOcr(std::sync::Arc<std::sync::Mutex<Vec<TextItem>>>);

impl StubOcr {
    pub fn fixed(v: Vec<TextItem>) -> Self {
        Self(std::sync::Arc::new(std::sync::Mutex::new(v)))
    }

    pub fn set(&self, v: Vec<TextItem>) {
        *self.0.lock().unwrap() = v;
    }
}

impl Recognize for StubOcr {
    fn text(&mut self, _img: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ok(self.0.lock().unwrap().clone())
    }
}

pub fn test_cfg() -> Config {
    // Ohne Display: Injector::open scheitert, die Session läuft ohne Eingabe.
    Config::try_parse_from(["lbw-server"]).unwrap()
}

pub fn item() -> TextItem {
    TextItem {
        id: 0, // vergibt die Session (Stub liefert wie OCR id: 0)
        rect: Rect::new(8, 8, 32, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: "hi".into(),
    }
}

pub fn item_at(x: u16, y: u16, text: &str) -> TextItem {
    TextItem {
        id: 0,
        rect: Rect::new(x, y, 32, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: text.into(),
    }
}

/// Liest bis zur Deadline; `want` zählt relevante Nachrichten.
pub fn read_until(
    fr: &mut FrameReader,
    s: &mut TcpStream,
    until: Instant,
    want: &mut dyn FnMut(&ServerMsg) -> bool,
) {
    s.set_read_timeout(Some(Duration::from_millis(200)))
        .unwrap();
    while Instant::now() < until {
        if let Some(m) = fr.read_msg::<ServerMsg>(s).unwrap()
            && want(&m)
        {
            return;
        }
    }
}
