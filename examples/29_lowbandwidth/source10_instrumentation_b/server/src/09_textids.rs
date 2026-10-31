//! `09_textids` — stabile Text-IDs und Frame-Delta fürs Protokoll v3.
//!
//! Statt `ClearText` + Komplett-Resend bei jeder Änderung (v2, ~95 %
//! doppelte Bytes in realen Sessions) matcht der Server erkannte Zeilen per
//! Position auf den Vorframe und sendet nur das Delta: `RemoveText(id)` für
//! verschwundene, `AddText` für neue oder geänderte Zeilen (gleiche `id` =
//! Ersetzen beim Client). `ClearText` kommt nur im 1. Frame je Verbindung.

use lbw_common::TextItem;

/// Max. Mittelpunkts-Abstand (px) für ein Box-Match über Frames.
const MATCH_DIST: f32 = 24.0;
/// Max. Größenänderung (|dw|+|dh|, px) für ein Box-Match.
const MATCH_SIZE: u16 = 16;

fn center(r: &lbw_common::Rect) -> (f32, f32) {
    (
        f32::from(r.x) + f32::from(r.w) / 2.0,
        f32::from(r.y) + f32::from(r.h) / 2.0,
    )
}

/// Vergibt stabile IDs: Jede aktuelle Zeile erbt die `id` der nächsten
/// ungenutzten Vorframe-Zeile im [`MATCH_DIST`]-Fenster (bei ähnlicher Größe),
/// sonst eine frische aus `next_id` (Zähler, startet bei 1).
pub fn assign_ids(prev: &[TextItem], mut cur: Vec<TextItem>, next_id: &mut u64) -> Vec<TextItem> {
    let mut used = vec![false; prev.len()];
    for c in &mut cur {
        let (cx, cy) = center(&c.rect);
        let mut best: Option<(usize, f32)> = None;
        for (i, p) in prev.iter().enumerate() {
            if used[i] {
                continue;
            }
            let dw = c.rect.w.abs_diff(p.rect.w);
            let dh = c.rect.h.abs_diff(p.rect.h);
            if dw + dh > MATCH_SIZE {
                continue;
            }
            let (ox, oy) = center(&p.rect);
            let d = (cx - ox).hypot(cy - oy);
            if d <= MATCH_DIST && best.is_none_or(|(_, bd)| d < bd) {
                best = Some((i, d));
            }
        }
        match best {
            Some((i, _)) => {
                used[i] = true;
                c.id = prev[i].id;
            }
            None => {
                c.id = *next_id;
                *next_id += 1;
            }
        }
    }
    cur
}

/// Gleicher Inhalt ohne `id`-Vergleich (OCR liefert `id: 0`).
#[must_use]
pub fn same_content(a: &TextItem, b: &TextItem) -> bool {
    a.rect == b.rect && a.fg == b.fg && a.bg == b.bg && a.text == b.text
}

/// Delta Vorframe → aktuell: (entfernte IDs, neue/geänderte Zeilen).
/// Geändert = gleiche `id`, anderer Inhalt (z. B. getipptes Zeichen).
pub fn diff_texts(prev: &[TextItem], cur: &[TextItem]) -> (Vec<u64>, Vec<TextItem>) {
    let removed = prev
        .iter()
        .filter(|p| !cur.iter().any(|c| c.id == p.id))
        .map(|p| p.id)
        .collect();
    let changed = cur
        .iter()
        .filter(|c| match prev.iter().find(|p| p.id == c.id) {
            None => true,
            Some(p) => !same_content(p, c),
        })
        .cloned()
        .collect();
    (removed, changed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::Rect;

    fn item(id: u64, x: u16, y: u16, text: &str) -> TextItem {
        TextItem {
            id,
            rect: Rect::new(x, y, 100, 16),
            fg: [0; 3],
            bg: [255; 3],
            text: text.into(),
        }
    }

    #[test]
    fn stillstand_keeps_ids_and_has_no_diff() {
        let mut next = 1;
        let a = assign_ids(
            &[],
            vec![item(0, 0, 0, "a"), item(0, 0, 20, "b")],
            &mut next,
        );
        assert_eq!((a[0].id, a[1].id, next), (1, 2, 3));
        let b = assign_ids(&a, vec![item(0, 0, 0, "a"), item(0, 0, 20, "b")], &mut next);
        assert_eq!((b[0].id, b[1].id, next), (1, 2, 3));
        assert_eq!(diff_texts(&a, &b), (vec![], vec![]));
    }

    #[test]
    fn typing_reuses_id_and_sends_replace() {
        let mut next = 1;
        let a = assign_ids(&[], vec![item(0, 0, 0, "a")], &mut next);
        // Gleiches Rechteck, neuer Text (getippt): gleiche id, Inhalt ändert.
        let b = assign_ids(&a, vec![item(0, 0, 0, "ab")], &mut next);
        assert_eq!((b[0].id, next), (1, 2));
        let (removed, changed) = diff_texts(&a, &b);
        assert!(removed.is_empty());
        assert_eq!(changed, vec![item(1, 0, 0, "ab")]);
    }

    #[test]
    fn added_and_removed_lines() {
        let mut next = 1;
        let a = assign_ids(
            &[],
            vec![item(0, 0, 0, "bleibt"), item(0, 0, 20, "weg")],
            &mut next,
        );
        let b = assign_ids(
            &a,
            vec![item(0, 0, 0, "bleibt"), item(0, 0, 72, "neu")],
            &mut next,
        );
        assert_eq!((b[0].id, b[1].id), (1, 3));
        let (removed, changed) = diff_texts(&a, &b);
        assert_eq!(removed, vec![2]);
        assert_eq!(changed, vec![item(3, 0, 72, "neu")]);
    }

    #[test]
    fn scroll_beyond_threshold_is_full_resend() {
        let mut next = 1;
        let a = assign_ids(&[], vec![item(0, 0, 100, "x")], &mut next);
        // 100 px tiefer (Scroll): kein Match → frische id, remove+add.
        let b = assign_ids(&a, vec![item(0, 0, 200, "x")], &mut next);
        assert_eq!(b[0].id, 2);
        let (removed, changed) = diff_texts(&a, &b);
        assert_eq!(removed, vec![1]);
        assert_eq!(changed.len(), 1);
    }

    #[test]
    fn size_jump_breaks_match() {
        let mut next = 1;
        let a = assign_ids(&[], vec![item(0, 0, 0, "x")], &mut next);
        let mut wide = item(0, 0, 0, "x");
        wide.rect.w = 200; // +100 px: kein Match trotz gleicher Position.
        let b = assign_ids(&a, vec![wide], &mut next);
        assert_eq!(b[0].id, 2);
    }
}
