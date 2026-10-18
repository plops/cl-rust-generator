//! `02_blob` — Daten für Kotlin: Texte als kompakter Binärblob, HUD-Zeile.
//!
//! Blob (Little Endian): `u32 anzahl`, dann je Element
//! `u32 id | u16 x y w h | u8 fg[3] | u8 bg[3] | u16 len | len Byte UTF-8`.
//! Kotlin liest ihn mit `ByteBuffer.order(LITTLE_ENDIAN)` (`03_TextItems.kt`).

use lbw_client::scene::{Link, Scene};
use lbw_common::TextItem;

/// Kodiert Textelemente; Texte > 65535 Byte werden an einer Zeichengrenze gekürzt.
#[must_use]
pub fn encode_texts<'a>(items: impl ExactSizeIterator<Item = &'a TextItem>) -> Vec<u8> {
    let mut b = Vec::with_capacity(4 + items.len() * 40);
    b.extend_from_slice(&(items.len() as u32).to_le_bytes());
    for t in items {
        let r = t.rect;
        b.extend_from_slice(&t.id.to_le_bytes());
        for v in [r.x, r.y, r.w, r.h] {
            b.extend_from_slice(&v.to_le_bytes());
        }
        b.extend_from_slice(&t.fg);
        b.extend_from_slice(&t.bg);
        let mut n = t.text.len().min(usize::from(u16::MAX));
        while !t.text.is_char_boundary(n) {
            n -= 1;
        }
        b.extend_from_slice(&(n as u16).to_le_bytes());
        b.extend_from_slice(&t.text.as_bytes()[..n]);
    }
    b
}

/// Statuszeile wie das HUD des Desktop-Clients (F1).
#[must_use]
pub fn status_line(s: &Scene) -> String {
    let link = match &s.link {
        Link::Connecting => "verbinde …".to_owned(),
        Link::Up => "verbunden".to_owned(),
        Link::Down(why) => format!("getrennt ({why}), verbinde neu …"),
    };
    format!(
        "{link} | {} Kacheln {} kB | {} Texte",
        s.tiles,
        s.tile_bytes / 1000,
        s.texts.len()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::Rect;

    #[test]
    fn blob_layout_is_stable() {
        let items = [TextItem {
            id: 0x0102_0304,
            rect: Rect::new(1, 2, 300, 16),
            fg: [9, 8, 7],
            bg: [1, 2, 3],
            text: "Hä".into(),
        }];
        let b = encode_texts(items.iter());
        #[rustfmt::skip]
        let want = [
            1, 0, 0, 0,
            4, 3, 2, 1, 0, 0, 0, 0,
            1, 0, 2, 0, 44, 1, 16, 0,
            9, 8, 7, 1, 2, 3,
            3, 0, b'H', 0xc3, 0xa4,
        ];
        assert_eq!(b, want);
        assert_eq!(encode_texts([].iter()), [0, 0, 0, 0]);
    }

    #[test]
    fn long_text_is_cut_on_char_boundary() {
        let t = TextItem {
            id: 1,
            rect: Rect::new(0, 0, 1, 1),
            fg: [0; 3],
            bg: [0; 3],
            text: "ä".repeat(40_000),
        };
        let b = encode_texts([t].iter());
        let n = u16::from_le_bytes([b[26], b[27]]) as usize;
        assert_eq!(n, 65_534);
        assert!(std::str::from_utf8(&b[28..28 + n]).is_ok());
    }

    #[test]
    fn status_reports_link_and_counts() {
        let mut s = Scene::new();
        assert!(status_line(&s).starts_with("verbinde"));
        s.link = Link::Up;
        s.tiles = 3;
        let l = status_line(&s);
        assert!(l.contains("verbunden") && l.contains("3 Kacheln"), "{l}");
    }
}
