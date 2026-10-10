//! `01_record` — alle Record-Typen des `.lbwlog`-Formats.
//!
//! Eine Aufzeichnung ist ein Strom von [`LogRecord`]s (s. `02_io`), jeder mit
//! [`Stamp`] (Wall- + Mono-Zeit). Nachrichten-Bodies sind `bincode` wie auf der
//! Leitung — `bincode::standard` ist deterministisch, daher genügt Nach-Kodieren
//! der dekodierten Nachricht (kein Eingriff ins Protokoll nötig). Hashes sind
//! FNV-1a/64 (stabil über Läufe — `std`-Hash wäre es nicht).

use lbw_common::{ClientMsg, ServerMsg};
use serde::{Deserialize, Serialize};

/// Datei-Magic (8 Byte am Dateianfang).
pub const LOG_MAGIC: [u8; 8] = *b"LBWLOG10";
/// Record-Format-Version (steht im `Session`-Record).
pub const LOG_VERSION: u16 = 1;

/// Leitungsrichtung einer Nachricht.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Dir {
    SrvToCli,
    CliToSrv,
}

/// Nachrichtenart — kompakte Zusammenfassung für Analyse ohne Dekodierung.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MsgKind {
    SrvHello,
    ClearText,
    AddText,
    Tile,
    CliHello,
    MouseMove,
    Button,
    Text,
    Key,
}

impl MsgKind {
    #[must_use]
    pub const fn of_server(m: &ServerMsg) -> Self {
        match m {
            ServerMsg::Hello => Self::SrvHello,
            ServerMsg::ClearText => Self::ClearText,
            ServerMsg::AddText(_) => Self::AddText,
            ServerMsg::Tile { .. } => Self::Tile,
        }
    }

    #[must_use]
    pub const fn of_client(m: &ClientMsg) -> Self {
        match m {
            ClientMsg::Hello { .. } => Self::CliHello,
            ClientMsg::MouseMove { .. } => Self::MouseMove,
            ClientMsg::Button { .. } => Self::Button,
            ClientMsg::Text(_) => Self::Text,
            ClientMsg::Key { .. } => Self::Key,
        }
    }
}

/// Zeitstempel: Wall-Clock (µs seit Unix-Epoche, für Datei-übergreifende
/// Korrelation) + monotone Uhr (µs seit Recorder-Start, für präzise Intervalle).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Stamp {
    pub wall_us: u64,
    pub mono_us: u64,
}

/// AV1-Kachel-Statistik eines Server-Frames.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TileStat {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub h: u16,
    pub bytes: usize,
    pub hash: u64,
}

/// Pipeline-Stufen eines Server-Frames in ms (Detail s. `Frame`-Record).
#[derive(Clone, Copy, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct FrameMs {
    /// Bildschirm-Grab (`FrameSource::grab`).
    pub capture: f32,
    /// DBNet-Detektion (aus `Ocr::last_ms`, GPU).
    pub det: f32,
    /// CTC-Erkennung aller Zeilen (aus `Ocr::last_ms`, CPU).
    pub rec: f32,
    /// Text-Maskierung + `dirty_bbox`-Vergleich.
    pub mask_diff: f32,
    /// AV1-Encode der Box (0 bei Stille).
    pub encode: f32,
    /// Alle `write_msg` des Frames zusammen.
    pub send: f32,
}

impl FrameMs {
    /// Summe aller Stufen (ohne Sleep zwischen Frames).
    #[must_use]
    pub fn total(&self) -> f32 {
        self.capture + self.det + self.rec + self.mask_diff + self.encode + self.send
    }
}

/// Verbindungswechsel (beide Seiten; Down-Dauer wird live gemessen, damit sie
/// auch bei gesplitteten/abgebrochenen Dateien stimmt).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum GapEvent {
    Up {
        peer: String,
        /// ms seit dem letzten `Down` (None beim allerersten Connect).
        down_ms: Option<f32>,
    },
    Down {
        reason: String,
    },
}

/// Ein Eintrag der Aufzeichnung.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum LogRecord {
    /// Dateikopf je Lauf (einmal, direkt nach der Magic).
    Session {
        app: String,
        version: u16,
        args: Vec<String>,
    },
    /// Eine Protokollnachricht; `body` ist der Leitungs-Body, `hash` darüber
    /// (Dedup-Analyse), `wire_bytes` inkl. 4-B-Frame-Header.
    Msg {
        stamp: Stamp,
        dir: Dir,
        kind: MsgKind,
        wire_bytes: usize,
        hash: u64,
        body: Vec<u8>,
    },
    /// Server-Pipeline pro Frame — auch bei Standbild-Stille (`tile: None`),
    /// damit „gesunde Stille“ von „Ausfall“ unterscheidbar bleibt.
    Frame {
        stamp: Stamp,
        frame: u64,
        texts: usize,
        text_changed: bool,
        text_bytes: usize,
        tile: Option<TileStat>,
        ms: FrameMs,
    },
    /// Client: AV1-Decode einer Kachel (`ok: false` = defekter Bitstrom).
    Decode {
        stamp: Stamp,
        tile_bytes: usize,
        ms: f32,
        ok: bool,
    },
    /// Server: Eingabe-Empfang → Injektion (`ok: false` = z. B. Taste unbekannt).
    Inject {
        stamp: Stamp,
        kind: MsgKind,
        ms: f32,
        ok: bool,
    },
    Gap {
        stamp: Stamp,
        event: GapEvent,
    },
    /// Ordentliches Dateiende (fehlt bei Absturz — s. `02_io::Reader`).
    End {
        stamp: Stamp,
    },
}

/// FNV-1a/64 über `b` (Dedup-Hash; stabil über Prozesse hinweg).
#[must_use]
pub fn fnv1a64(b: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for &x in b {
        h ^= u64::from(x);
        h = h.wrapping_mul(0x100_0000_01b3);
    }
    h
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fnv_matches_offset_basis_and_is_stable() {
        assert_eq!(fnv1a64(b""), 0xcbf2_9ce4_8422_2325);
        assert_eq!(fnv1a64(b"hi"), fnv1a64(b"hi"));
        assert_ne!(fnv1a64(b"hi"), fnv1a64(b"ho"));
    }

    #[test]
    fn kinds_cover_all_protocol_variants() {
        use lbw_common::{Rect, TextItem};
        let item = TextItem {
            rect: Rect::new(0, 0, 1, 1),
            fg: [0; 3],
            bg: [0; 3],
            text: String::new(),
        };
        assert_eq!(MsgKind::of_server(&ServerMsg::Hello), MsgKind::SrvHello);
        assert_eq!(
            MsgKind::of_server(&ServerMsg::ClearText),
            MsgKind::ClearText
        );
        assert_eq!(
            MsgKind::of_server(&ServerMsg::AddText(item)),
            MsgKind::AddText
        );
        assert_eq!(
            MsgKind::of_server(&ServerMsg::Tile {
                x: 0,
                y: 0,
                data: vec![]
            }),
            MsgKind::Tile
        );
        assert_eq!(
            MsgKind::of_client(&ClientMsg::Hello { version: 2 }),
            MsgKind::CliHello
        );
        assert_eq!(
            MsgKind::of_client(&ClientMsg::MouseMove { x: 0, y: 0 }),
            MsgKind::MouseMove
        );
        assert_eq!(
            MsgKind::of_client(&ClientMsg::Button {
                button: 1,
                down: true
            }),
            MsgKind::Button
        );
        assert_eq!(
            MsgKind::of_client(&ClientMsg::Text(String::new())),
            MsgKind::Text
        );
        assert_eq!(
            MsgKind::of_client(&ClientMsg::Key {
                key: String::new(),
                down: true
            }),
            MsgKind::Key
        );
    }

    #[test]
    fn frame_ms_total_is_sum() {
        let ms = FrameMs {
            capture: 1.0,
            det: 2.0,
            rec: 4.0,
            mask_diff: 8.0,
            encode: 16.0,
            send: 32.0,
        };
        assert_eq!(ms.total(), 63.0);
    }
}
