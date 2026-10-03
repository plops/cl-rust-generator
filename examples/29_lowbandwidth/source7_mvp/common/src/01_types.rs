//! `01_types` — alle Nachrichtentypen des MVP-Protokolls plus Konstanten.
//!
//! Serialisierung per `serde` + `bincode` (siehe `02_framing`); deshalb hier
//! keine einzige Zeile Hand-Codec. Koordinaten sind `u16` im Capture-Raum
//! (MVP: `size`×`size`, Default 640). Farben sind RGB8.

use serde::{Deserialize, Serialize};

/// Protokollversion (Client-`Hello`; Server lehnt Abweichungen ab).
pub const PROTO_VERSION: u16 = 1;
/// Default-TCP-Port.
pub const DEFAULT_PORT: u16 = 7878;
/// Kantenlänge einer festen AV1-Kachel.
pub const TILE: u16 = 64;
/// Feste Kantenlänge des quadratischen Bildes (MVP: immer 640×640).
pub const SIZE: u32 = 640;
/// Max. Nachrichtengröße in Byte (Schutz vor OOM bei korrupten Längen).
pub const MAX_MSG: usize = 8 * 1024 * 1024;

/// Achsenparalleles Rechteck.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Rect {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub h: u16,
}

impl Rect {
    #[must_use]
    pub const fn new(x: u16, y: u16, w: u16, h: u16) -> Self {
        Self { x, y, w, h }
    }

    /// Rechte Kante (exklusiv).
    #[must_use]
    pub fn x2(&self) -> u16 {
        self.x + self.w
    }

    /// Untere Kante (exklusiv).
    #[must_use]
    pub fn y2(&self) -> u16 {
        self.y + self.h
    }

    #[must_use]
    pub fn area(&self) -> u32 {
        u32::from(self.w) * u32::from(self.h)
    }
}

/// Ein erkanntes Textelement (Box, Farben, String). Keine ID: der Server
/// sendet bei jeder Textänderung `ClearText` + alle `AddText` neu.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextItem {
    pub rect: Rect,
    /// Vordergrund (Schrift).
    pub fg: [u8; 3],
    /// Hintergrund unter der Schrift.
    pub bg: [u8; 3],
    pub text: String,
}

/// Maustaste (X11-Nummerierung: 1 links, 2 mitte, 3 rechts).
pub type Button = u8;

/// Nachrichten Server → Client.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ServerMsg {
    Hello {
        w: u16,
        h: u16,
    },
    /// Alle bisherigen Texte verwerfen.
    ClearText,
    AddText(TextItem),
    /// Ganze AV1-Kachel (Still-Picture, rohe OBUs) in einer Nachricht.
    Tile {
        x: u16,
        y: u16,
        w: u16,
        h: u16,
        data: Vec<u8>,
    },
}

/// Nachrichten Client → Server. Positionen im Capture-Raum.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ClientMsg {
    Hello {
        version: u16,
    },
    MouseMove {
        x: u16,
        y: u16,
    },
    Button {
        button: Button,
        down: bool,
    },
    /// Getippter Text (Zeichen oder Paste).
    Text(String),
    /// Sondertaste als Name („Enter“, „Esc“, „Tab“, „Left“, …).
    Key {
        key: String,
        down: bool,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rect_geometry() {
        let a = Rect::new(0, 0, 10, 10);
        assert_eq!((a.x2(), a.y2(), a.area()), (10, 10, 100));
        assert_eq!(
            a,
            Rect {
                x: 0,
                y: 0,
                w: 10,
                h: 10
            }
        );
    }

    #[test]
    fn tile_divides_fixed_size() {
        assert_eq!(SIZE % u32::from(TILE), 0);
    }
}
