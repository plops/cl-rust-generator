//! `01_types` — alle Nachrichtentypen des v4-Protokolls plus Konstanten.
//!
//! Serialisierung per `serde` + `bincode` (siehe `02_framing`).
//! Koordinaten sind `u16` im Capture-Raum (immer 1280×720). Farben RGB8.

use serde::{Deserialize, Serialize};

/// Protokollversion (Auth-Handshake lehnt Abweichungen ab).
pub const PROTO_VERSION: u16 = 4;
/// Default-TCP-Port (nur localhost — dahinter steht immer SSH).
pub const DEFAULT_PORT: u16 = 7878;
/// Feste Bildbreite.
pub const WIDTH: u32 = 1280;
/// Feste Bildhöhe.
pub const HEIGHT: u32 = 720;
/// Max. Nachrichtengröße in Byte (Schutz vor OOM bei korrupten Längen).
pub const MAX_MSG: usize = 8 * 1024 * 1024;
/// Domain-Trennung für den Auth-MAC.
pub const AUTH_DOMAIN: &[u8] = b"lbw11";

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
/// Server inhaltsverankert über Frames hinweg (s. `09_stabilize`); bekannter
/// Inhalt an neuer Position kommt als `MoveText` ohne String-Retransmit.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextItem {
    /// Stabile Zeilen-ID (Zähler ab 1; 0 = noch unvergeben).
    pub id: u64,
    pub rect: Rect,
    /// Vordergrund (Schrift, quantisiert).
    pub fg: [u8; 3],
    /// Hintergrund unter der Schrift (quantisiert).
    pub bg: [u8; 3],
    pub text: String,
}

/// Maustaste (X11-Nummerierung: 1 links, 2 mitte, 3 rechts).
pub type Button = u8;

/// Nachrichten Server → Client.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ServerMsg {
    /// Verbindungs-Nonce für das Token-Verfahren (Zufall pro Verbindung).
    Challenge {
        version: u16,
        nonce: [u8; 16],
    },
    Hello,
    /// Alle bisherigen Texte verwerfen (nur 1. Frame je Verbindung).
    ClearText,
    /// Wirklich neuer Inhalt (ID, Box, Farben, String).
    NewText(TextItem),
    /// Bekannter Inhalt an neuer Position (nur ID + Box).
    MoveText {
        id: u64,
        rect: Rect,
    },
    /// Text mit `id` entfernen.
    RemoveText(u64),
    /// AV1-Box (Still-Picture, rohe OBUs) an Position (`x`, `y`).
    Tile {
        x: u16,
        y: u16,
        data: Vec<u8>,
    },
}

/// Nachrichten Client → Server.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ClientMsg {
    /// HMAC-Beweis über die Server-Nonce (s. `auth`).
    Auth {
        mac: [u8; 32],
    },
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
    fn move_text_roundtrip_keeps_id_and_rect() {
        let m = ServerMsg::MoveText {
            id: 7,
            rect: Rect::new(10, 200, 100, 16),
        };
        let back: ServerMsg = bincode::serde::decode_from_slice(
            &bincode::serde::encode_to_vec(&m, bincode::config::standard()).unwrap(),
            bincode::config::standard(),
        )
        .unwrap()
        .0;
        assert_eq!(back, m);
    }

    #[test]
    fn proto_version_is_4() {
        assert_eq!(PROTO_VERSION, 4);
    }
}
