//! `10_text_diff` — Textzustand mit stabilen IDs und minimalen Deltas.
//!
//! Neu erkannte Zeilen werden alten zugeordnet, wenn Text gleich, Box
//! höchstens [`JITTER`] px verschoben und Farben ähnlich sind. Zugeordnete
//! Zeilen behalten ID *und* alte Box → kein Delta, und die Maske im Bild
//! bleibt pixelgleich (kein Box-Zittern → keine unnötigen AV1-Kacheln).

use lbw_common::{Rect, TextItem};

/// Erlaubte Box-Abweichung (px) für „dieselbe Zeile“.
pub const JITTER: u16 = 3;
/// Erlaubte Farbabweichung je Kanal.
pub const COLOR_TOL: u8 = 40;

/// Kandidat aus der OCR (noch ohne ID).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Detected {
    pub rect: Rect,
    pub fg: [u8; 3],
    pub bg: [u8; 3],
    pub text: String,
}

/// Änderung gegenüber dem vorherigen Zustand.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Delta {
    pub remove: Vec<u32>,
    pub add: Vec<TextItem>,
}

impl Delta {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.remove.is_empty() && self.add.is_empty()
    }
}

/// Aktueller Textzustand des Servers (entspricht dem Client-Modell).
#[derive(Default)]
pub struct TextState {
    items: Vec<TextItem>,
    next_id: u32,
}

fn near(a: &Rect, b: &Rect) -> bool {
    a.x.abs_diff(b.x) <= JITTER
        && a.y.abs_diff(b.y) <= JITTER
        && a.w.abs_diff(b.w) <= JITTER
        && a.h.abs_diff(b.h) <= JITTER
}

fn similar(a: [u8; 3], b: [u8; 3]) -> bool {
    a.iter().zip(b).all(|(x, y)| x.abs_diff(y) <= COLOR_TOL)
}

impl TextState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            next_id: 1,
        }
    }

    /// Alle aktuellen Elemente (für Maske und Voll-Refresh).
    #[must_use]
    pub fn items(&self) -> &[TextItem] {
        &self.items
    }

    /// Übernimmt eine neue Erkennung und liefert das Delta.
    pub fn update(&mut self, new: Vec<Detected>) -> Delta {
        let mut used = vec![false; self.items.len()];
        let mut next = Vec::with_capacity(new.len());
        let mut delta = Delta::default();
        for d in new {
            let hit = self.items.iter().enumerate().find(|(i, o)| {
                !used[*i]
                    && o.text == d.text
                    && near(&o.rect, &d.rect)
                    && similar(o.fg, d.fg)
                    && similar(o.bg, d.bg)
            });
            if let Some((i, o)) = hit {
                used[i] = true;
                next.push(o.clone());
            } else {
                let item = TextItem {
                    id: self.next_id,
                    rect: d.rect,
                    fg: d.fg,
                    bg: d.bg,
                    text: d.text,
                };
                self.next_id = self.next_id.wrapping_add(1).max(1);
                delta.add.push(item.clone());
                next.push(item);
            }
        }
        for (i, o) in self.items.iter().enumerate() {
            if !used[i] {
                delta.remove.push(o.id);
            }
        }
        self.items = next;
        delta
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn det(x: u16, text: &str) -> Detected {
        Detected {
            rect: Rect::new(x, 10, 100, 16),
            fg: [0; 3],
            bg: [255; 3],
            text: text.into(),
        }
    }

    #[test]
    fn first_update_adds_everything() {
        let mut s = TextState::new();
        let d = s.update(vec![det(0, "a"), det(200, "b")]);
        assert_eq!(d.add.len(), 2);
        assert!(d.remove.is_empty());
        assert_ne!(d.add[0].id, d.add[1].id);
    }

    #[test]
    fn jitter_keeps_id_and_old_box() {
        let mut s = TextState::new();
        let id = s.update(vec![det(50, "hello")]).add[0].id;
        let d = s.update(vec![det(52, "hello")]);
        assert!(d.is_empty());
        assert_eq!(s.items()[0].id, id);
        assert_eq!(s.items()[0].rect.x, 50);
    }

    #[test]
    fn changed_text_replaces_item() {
        let mut s = TextState::new();
        let old = s.update(vec![det(0, "hell"), det(300, "x")]).add[0].id;
        let d = s.update(vec![det(0, "hello"), det(300, "x")]);
        assert_eq!(d.remove, vec![old]);
        assert_eq!(d.add.len(), 1);
        assert_eq!(d.add[0].text, "hello");
    }

    #[test]
    fn colour_change_is_a_change_and_duplicates_match_once() {
        let mut s = TextState::new();
        s.update(vec![det(0, "a"), det(1, "a")]);
        let mut sel = det(0, "a");
        sel.bg = [0, 0, 200]; // Markierung
        let d = s.update(vec![sel, det(1, "a")]);
        assert_eq!((d.remove.len(), d.add.len()), (1, 1));
        assert_eq!(s.update(vec![]).remove.len(), 2);
    }
}
