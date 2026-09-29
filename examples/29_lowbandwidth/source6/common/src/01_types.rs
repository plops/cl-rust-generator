//! `01_types` — alle Nachrichtentypen des Protokolls plus Konstanten.
//!
//! Koordinaten sind `u16` im Capture-Raum (MVP: 640×640). Farben sind RGB8.

/// Protokollversion (Client-Hello; Server lehnt Abweichungen ab).
pub const PROTO_VERSION: u16 = 1;
/// Default-TCP-Port.
pub const DEFAULT_PORT: u16 = 7878;
/// Max. Nutzdaten eines `TileData`-Stücks (Text kann dazwischen überholen).
pub const CHUNK: usize = 512;
/// Max. Framegröße (u16-Längenfeld).
pub const MAX_FRAME: usize = u16::MAX as usize;

/// Achsenparalleles Rechteck.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
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

    /// Schnittfläche zweier Rechtecke.
    #[must_use]
    pub fn intersection(&self, o: &Rect) -> u32 {
        let w = self.x2().min(o.x2()).saturating_sub(self.x.max(o.x));
        let h = self.y2().min(o.y2()).saturating_sub(self.y.max(o.y));
        u32::from(w) * u32::from(h)
    }

    /// Kleinstes umschließendes Rechteck.
    #[must_use]
    pub fn union(&self, o: &Rect) -> Rect {
        let (x, y) = (self.x.min(o.x), self.y.min(o.y));
        Rect::new(x, y, self.x2().max(o.x2()) - x, self.y2().max(o.y2()) - y)
    }
}

/// Ein erkanntes Textelement (Box, Farben, String).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextItem {
    /// Stabile ID (Server vergibt, Client indexiert danach).
    pub id: u32,
    pub rect: Rect,
    /// Vordergrund (Schrift).
    pub fg: [u8; 3],
    /// Hintergrund unter der Schrift.
    pub bg: [u8; 3],
    pub text: String,
}

/// Maustaste (X11-Nummerierung: 1 links, 2 mitte, 3 rechts).
pub type Button = u8;

/// Eingabeereignis Client → Server.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Input {
    /// Absolute Mausposition im Capture-Raum.
    MouseMove {
        x: u16,
        y: u16,
    },
    Button {
        button: Button,
        down: bool,
    },
    /// Mausrad: positive Werte = hoch.
    Wheel {
        dy: i8,
    },
    /// Sondertaste/Kombination: tippen (drücken+loslassen) mit Modifiern.
    Key {
        keysym: u32,
        mods: u8,
    },
    /// Ein druckbares Zeichen (Unicode-Codepoint).
    Char {
        ch: u32,
    },
    /// Längerer Text (Paste), wird serverseitig getippt.
    Text(String),
}

/// Nachrichten Server → Client.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ServerMsg {
    Hello {
        server_id: u64,
        w: u16,
        h: u16,
        /// `true`: Client-Zustand ist aktuell, kein Refresh folgt.
        resumed: bool,
    },
    /// Text-Delta: erst `remove`, dann `add` anwenden.
    Text {
        seq: u32,
        remove: Vec<u32>,
        add: Vec<TextItem>,
    },
    /// Beginn einer AV1-Kachel mit `len` Byte (folgt in `TileData`).
    TileStart {
        tile_id: u32,
        seq: u32,
        rect: Rect,
        len: u32,
    },
    TileData {
        tile_id: u32,
        offset: u32,
        data: Vec<u8>,
    },
    /// Client soll alles verwerfen (vor Voll-Refresh).
    Clear,
    Ping {
        t: u32,
    },
    Pong {
        t: u32,
    },
    /// Status für das HUD.
    Stats {
        rate: u32,
        backlog: u32,
        tiles: u32,
    },
}

/// Nachrichten Client → Server.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClientMsg {
    /// `server_id`/`seq` des zuletzt angewandten Zustands (0 = keiner).
    Hello {
        version: u16,
        server_id: u64,
        seq: u32,
    },
    Input(Input),
    /// Bisher empfangene Frame-Bytes und zuletzt angewandte `seq`.
    Ack {
        rx_bytes: u64,
        seq: u32,
    },
    Ping {
        t: u32,
    },
    Pong {
        t: u32,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rect_geometry() {
        let a = Rect::new(0, 0, 10, 10);
        let b = Rect::new(5, 5, 10, 10);
        assert_eq!(a.intersection(&b), 25);
        assert_eq!(a.intersection(&Rect::new(20, 20, 5, 5)), 0);
        assert_eq!(a.union(&b), Rect::new(0, 0, 15, 15));
        assert_eq!((b.x2(), b.y2(), b.area()), (15, 15, 100));
    }
}
