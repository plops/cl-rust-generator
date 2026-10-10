//! `07_legacy` — Decoder für alte `.lbwlog`-Bodies (Log-Version 1).
//!
//! Gehört bewusst nach `lbw-log`, nicht nach `lbw-common`: das Protokoll-Crate
//! definiert nur das aktuelle Format (v3); Altlasten leben bei der Auswertung.

use lbw_common::framing::decode_msg;
use lbw_common::{Rect, ServerMsg, TextItem};

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
