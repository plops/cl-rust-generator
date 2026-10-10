//! `01_types` — alle Nachrichtentypen des GPU-Protokolls plus Konstanten.
//!
//! Serialisierung per `serde` + `bincode` (siehe `02_framing`); deshalb hier
//! keine einzige Zeile Hand-Codec. Koordinaten sind `u16` im Capture-Raum
//! (GPU: immer 1280×720). Farben sind RGB8.

use serde::{Deserialize, Serialize};

/// Protokollversion (Client-`Hello`; Server lehnt Abweichungen ab).
/// v2: 1280×720 statt 640×640 (inkompatibel zu v1).
/// v3: stabile Text-IDs + `RemoveText` (Delta statt Komplett-Resend).
pub const PROTO_VERSION: u16 = 3;
/// Default-TCP-Port.
pub const DEFAULT_PORT: u16 = 7878;
/// Feste Bildbreite (GPU: immer 1280×720).
pub const WIDTH: u32 = 1280;
/// Feste Bildhöhe (GPU: immer 1280×720).
pub const HEIGHT: u32 = 720;
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

/// Ein erkanntes Textelement (ID, Box, Farben, String). Die `id` vergibt der
/// Server stabil über Frames hinweg (Positions-Matching, s. `09_textids`);
/// der Client ersetzt per `AddText` gleichen IDs und löscht per `RemoveText`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextItem {
    /// Stabile Zeilen-ID (Zähler ab 1; 0 = noch unvergeben).
    pub id: u64,
    pub rect: Rect,
    /// Vordergrund (Schrift).
    pub fg: [u8; 3],
    /// Hintergrund unter der Schrift.
    pub bg: [u8; 3],
    pub text: String,
}

/// Maustaste (X11-Nummerierung: 1 links, 2 mitte, 3 rechts).
pub type Button = u8;

/// Nachrichten Server → Client. Das Bild ist immer [`WIDTH`]×[`HEIGHT`];
/// die AV1-Box hat variable Größe (steht im Bitstrom, nicht im Protokoll).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ServerMsg {
    Hello,
    /// Alle bisherigen Texte verwerfen (nur 1. Frame je Verbindung).
    ClearText,
    /// Text hinzufügen oder gleichen `id` ersetzen.
    AddText(TextItem),
    /// AV1-Box (Still-Picture, rohe OBUs) an Position (`x`, `y`).
    Tile {
        x: u16,
        y: u16,
        data: Vec<u8>,
    },
    /// Text mit `id` entfernen (Delta; hinten angehängt, damit alte
    /// Varianten-Indizes für `.lbwlog`-Kompat stabil bleiben).
    RemoveText(u64),
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
}
