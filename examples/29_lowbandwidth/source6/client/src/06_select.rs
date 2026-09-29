//! `06_select` — Text kopieren/einfügen ohne zusätzliche Dependency.
//!
//! Da Text als Vektordaten ankommt, ist „Kopieren“ trivial: alle
//! Textelemente, die das Auswahlrechteck schneiden, in Lesereihenfolge
//! (Zeilen, dann x) zusammenfügen. Einfügen schickt die Zwischenablage
//! als `Input::Text` in Stücken, die in einen Frame passen.

use lbw_common::{Rect, TextItem};

/// Max. Bytes je `Input::Text`.
pub const PASTE_CHUNK: usize = 1000;

/// Rechteck aus zwei Eckpunkten (beliebige Zugrichtung).
#[must_use]
pub fn drag_rect(a: (f32, f32), b: (f32, f32)) -> Rect {
    let (x0, x1) = (a.0.min(b.0).max(0.0), a.0.max(b.0).max(0.0));
    let (y0, y1) = (a.1.min(b.1).max(0.0), a.1.max(b.1).max(0.0));
    Rect::new(
        x0 as u16,
        y0 as u16,
        (x1 - x0) as u16 + 1,
        (y1 - y0) as u16 + 1,
    )
}

/// Texte im Rechteck; Elemente mit überlappender vertikaler Mitte bilden
/// eine Zeile (durch Leerzeichen getrennt), Zeilen durch `\n`.
#[must_use]
pub fn selected_text<'a>(sel: Rect, items: impl IntoIterator<Item = &'a TextItem>) -> String {
    let mut hit: Vec<&TextItem> = items
        .into_iter()
        .filter(|t| t.rect.intersection(&sel) > 0)
        .collect();
    hit.sort_by_key(|t| t.rect.y + t.rect.h / 2);
    // Zeilen bilden (vertikale Mitten nah beieinander), dann je Zeile nach x.
    let mut lines: Vec<(u16, Vec<&TextItem>)> = Vec::new();
    for t in hit {
        let mid = t.rect.y + t.rect.h / 2;
        match lines.last_mut() {
            Some((m, l)) if mid.abs_diff(*m) <= t.rect.h / 2 => l.push(t),
            _ => lines.push((mid, vec![t])),
        }
    }
    lines
        .into_iter()
        .map(|(_, mut l)| {
            l.sort_by_key(|t| t.rect.x);
            l.iter()
                .map(|t| t.text.as_str())
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Teilt Text an Zeichengrenzen in Stücke ≤ [`PASTE_CHUNK`] Byte.
#[must_use]
pub fn paste_chunks(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    for c in s.chars() {
        if cur.len() + c.len_utf8() > PASTE_CHUNK {
            out.push(std::mem::take(&mut cur));
        }
        cur.push(c);
    }
    if !cur.is_empty() {
        out.push(cur);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(x: u16, y: u16, s: &str) -> TextItem {
        TextItem {
            id: u32::from(x) * 1000 + u32::from(y),
            rect: Rect::new(x, y, 50, 16),
            fg: [0; 3],
            bg: [255; 3],
            text: s.into(),
        }
    }

    #[test]
    fn selection_joins_lines_in_reading_order() {
        let items = [
            t(100, 10, "Welt"),
            t(0, 12, "Hallo"),
            t(0, 40, "zweite"),
            t(0, 200, "weg"),
        ];
        let sel = drag_rect((180.0, 60.0), (5.0, 5.0));
        assert_eq!(selected_text(sel, &items), "Hallo Welt\nzweite");
        assert_eq!(
            selected_text(drag_rect((0.0, 300.0), (1.0, 301.0)), &items),
            ""
        );
    }

    #[test]
    fn paste_is_chunked_on_char_boundaries() {
        let s = "ä".repeat(700); // 1400 Byte
        let c = paste_chunks(&s);
        assert_eq!(c.len(), 2);
        assert!(c.iter().all(|x| x.len() <= PASTE_CHUNK));
        assert_eq!(c.concat(), s);
        assert!(paste_chunks("").is_empty());
    }
}
