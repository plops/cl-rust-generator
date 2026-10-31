//! `07_legacy` — Decoder für alte `.lbwlog`-Dateien.
//!
//! Zwei Altlasten: Nachrichten-Bodies im v2-Protokoll (Log-Version 1) und
//! Records ohne `FrameMs.pace` (Log-Versionen 1 und 2 — z. B. das
//! Referenz-Log `20261017_0614.lbwlog` des Bunching-Plans, das source10
//! seit dem Versions-Bump nicht mehr lesen konnte, Review B5).
//! Gehört bewusst nach `lbw-log`, nicht nach `lbw-common`: das Protokoll-Crate
//! definiert nur das aktuelle Format (v3); Altlasten leben bei der Auswertung.

use lbw_common::framing::decode_msg;
use lbw_common::{Rect, ServerMsg, TextItem};
use serde::Deserialize;

use crate::record::{Dir, FrameMs, GapEvent, LogRecord, MsgKind, Stamp, TileStat};

/// `FrameMs` vor Log-Version 3 (ohne `pace`).
#[derive(Deserialize)]
struct FrameMsV2 {
    capture: f32,
    det: f32,
    rec: f32,
    mask_diff: f32,
    encode: f32,
    send: f32,
}

/// `LogRecord` vor Log-Version 3: gleiche Varianten-Reihenfolge, nur
/// `Frame.ms` ist kürzer (bincode ist positionsbasiert — darum Spiegel-Enum).
#[derive(Deserialize)]
enum LogRecordV2 {
    Session {
        app: String,
        version: u16,
        args: Vec<String>,
    },
    Msg {
        stamp: Stamp,
        dir: Dir,
        kind: MsgKind,
        wire_bytes: usize,
        hash: u64,
        body: Vec<u8>,
    },
    Frame {
        stamp: Stamp,
        frame: u64,
        texts: usize,
        text_changed: bool,
        text_bytes: usize,
        tile: Option<TileStat>,
        ms: FrameMsV2,
    },
    Decode {
        stamp: Stamp,
        tile_bytes: usize,
        ms: f32,
        ok: bool,
    },
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
    End {
        stamp: Stamp,
    },
}

impl From<LogRecordV2> for LogRecord {
    fn from(r: LogRecordV2) -> Self {
        use LogRecordV2 as V;
        match r {
            V::Session { app, version, args } => Self::Session { app, version, args },
            V::Msg {
                stamp,
                dir,
                kind,
                wire_bytes,
                hash,
                body,
            } => Self::Msg {
                stamp,
                dir,
                kind,
                wire_bytes,
                hash,
                body,
            },
            V::Frame {
                stamp,
                frame,
                texts,
                text_changed,
                text_bytes,
                tile,
                ms: m,
            } => Self::Frame {
                stamp,
                frame,
                texts,
                text_changed,
                text_bytes,
                tile,
                ms: FrameMs {
                    capture: m.capture,
                    det: m.det,
                    rec: m.rec,
                    mask_diff: m.mask_diff,
                    encode: m.encode,
                    send: m.send,
                    pace: 0.0,
                },
            },
            V::Decode {
                stamp,
                tile_bytes,
                ms,
                ok,
            } => Self::Decode {
                stamp,
                tile_bytes,
                ms,
                ok,
            },
            V::Inject {
                stamp,
                kind,
                ms,
                ok,
            } => Self::Inject {
                stamp,
                kind,
                ms,
                ok,
            },
            V::Gap { stamp, event } => Self::Gap { stamp, event },
            V::End { stamp } => Self::End { stamp },
        }
    }
}

/// Dekodiert einen Record-Body der Datei-Version `log_version` (strikt:
/// < 3 = ohne `pace`, sonst aktuelles Format). Der `Session`-Record ist in
/// allen Versionen gleich — der Leser kennt die Version also ab Record 1.
pub fn decode_record(b: &[u8], log_version: u16) -> Result<LogRecord, String> {
    let cfg = bincode::config::standard();
    if log_version >= 3 {
        let (r, _): (LogRecord, usize) =
            bincode::serde::decode_from_slice(b, cfg).map_err(|e| e.to_string())?;
        Ok(r)
    } else {
        let (r, _): (LogRecordV2, usize) =
            bincode::serde::decode_from_slice(b, cfg).map_err(|e| e.to_string())?;
        Ok(r.into())
    }
}

/// V2-`TextItem` ohne `id` (nur für alte `.lbwlog`-Bodies).
#[derive(serde::Serialize, serde::Deserialize)]
struct TextItemV2 {
    rect: Rect,
    fg: [u8; 3],
    bg: [u8; 3],
    text: String,
}

/// V2-`ServerMsg` (nur für alte `.lbwlog`-Bodies; `RemoveText` gab es nicht).
#[derive(serde::Serialize, serde::Deserialize)]
enum ServerMsgV2 {
    Hello,
    ClearText,
    AddText(TextItemV2),
    Tile { x: u16, y: u16, data: Vec<u8> },
}

/// Dekodiert einen v2-`ServerMsg`-Body (altes `AddText` ohne `id` → `id: 0`).
/// Nur für `.lbwlog`-Dateien mit Log-Version 1 (s. [`decode_server_logged`]).
fn decode_server_v2(b: &[u8]) -> Result<ServerMsg, String> {
    let (m, _): (ServerMsgV2, usize) =
        bincode::serde::decode_from_slice(b, bincode::config::standard())
            .map_err(|e| e.to_string())?;
    Ok(match m {
        ServerMsgV2::Hello => ServerMsg::Hello,
        ServerMsgV2::ClearText => ServerMsg::ClearText,
        ServerMsgV2::AddText(t) => ServerMsg::AddText(TextItem {
            id: 0,
            rect: t.rect,
            fg: t.fg,
            bg: t.bg,
            text: t.text,
        }),
        ServerMsgV2::Tile { x, y, data } => ServerMsg::Tile { x, y, data },
    })
}

/// Dekodiert `ServerMsg`-Bodies aus Logs: strikt je Log-Version (kein Raten —
/// Bincode-Layouts könnten sonst mehrdeutig parsen). `log_version` steht im
/// `Session`-Record der Datei (1 = v2-Bodies, ≥ 2 = v3-Bodies).
/// Live-Verkehr nutzt `decode_msg` (strikt, eine Version).
pub fn decode_server_logged(b: &[u8], log_version: u16) -> Result<ServerMsg, String> {
    if log_version >= 2 {
        decode_msg::<ServerMsg>(b)
    } else {
        decode_server_v2(b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::framing::encode_msg;

    /// Echter v2-`Frame`-Record (6 Stufen, ohne `pace`) — von Hand kodiert
    /// wie ihn source10 vor dem Bunching-Commit schrieb.
    #[test]
    fn v2_frame_record_decodes_with_zero_pace() {
        #[derive(serde::Serialize)]
        struct Ms6(f32, f32, f32, f32, f32, f32);
        #[derive(serde::Serialize)]
        enum Rec {
            _Session,
            _Msg,
            Frame {
                stamp: Stamp,
                frame: u64,
                texts: usize,
                text_changed: bool,
                text_bytes: usize,
                tile: Option<TileStat>,
                ms: Ms6,
            },
        }
        let stamp = Stamp {
            wall_us: 5,
            mono_us: 6,
        };
        let body = encode_msg(&Rec::Frame {
            stamp,
            frame: 9,
            texts: 2,
            text_changed: true,
            text_bytes: 40,
            tile: None,
            ms: Ms6(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
        })
        .unwrap();
        // Aktuelles Format scheitert (genau der source10-Fehler) ...
        assert!(decode_record(&body, 3).is_err());
        // ... der Legacy-Pfad liest ihn mit pace = 0.
        let LogRecord::Frame { frame, ms, .. } = decode_record(&body, 2).unwrap() else {
            panic!("kein Frame");
        };
        assert_eq!((frame, ms.send, ms.pace), (9, 6.0, 0.0));
    }

    /// Das echte Referenz-Log (falls vorhanden, gitignored) ist komplett lesbar.
    #[test]
    fn real_0614_log_is_readable_again() {
        let p = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../source10_instrumentation/20261017_0614.lbwlog"
        );
        if !std::path::Path::new(p).exists() {
            eprintln!("übersprungen: {p} fehlt");
            return;
        }
        let (recs, _torn) = crate::io::load_lenient(p).unwrap();
        let frames = recs
            .iter()
            .filter(|r| matches!(r, LogRecord::Frame { .. }))
            .count();
        assert!(
            recs.len() > 3000 && frames > 600,
            "{} / {frames}",
            recs.len()
        );
    }

    #[test]
    fn logged_decode_is_strict_per_log_version() {
        // Echter v2-Body (ohne id): Version 1 → id 0, Version 2 → Fehler.
        let v2 = encode_msg(&ServerMsgV2::AddText(TextItemV2 {
            rect: Rect::new(1, 2, 3, 4),
            fg: [0; 3],
            bg: [1; 3],
            text: "alt".into(),
        }))
        .unwrap();
        assert!(matches!(
            decode_server_logged(&v2, 1).unwrap(),
            ServerMsg::AddText(t) if t.id == 0 && t.text == "alt"
        ));
        assert!(decode_server_logged(&v2, 2).is_err());
        // V3-Body: Version 2 ok, Version 1 scheitert (id-Bytes stören).
        let v3 = encode_msg(&ServerMsg::AddText(TextItem {
            id: 7,
            rect: Rect::new(1, 2, 300, 16),
            fg: [0, 0, 0],
            bg: [255, 255, 250],
            text: "neu".into(),
        }))
        .unwrap();
        assert!(matches!(
            decode_server_logged(&v3, 2).unwrap(),
            ServerMsg::AddText(t) if t.id == 7
        ));
        assert!(decode_server_logged(&v3, 1).is_err());
    }
}
