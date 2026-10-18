//! `09_stabilize` — OCR-Stabilisierung gegen Flacker-Spam (Plan §2.2a).
//!
//! PP-OCR liefert pro Frame leicht andere Ergebnisse (Box ±wenige Pixel,
//! Farben ±wenige Stufen, gelegentlich ein falsches Zeichen, kurzes
//! Aussetzen). Ohne Dämpfung erzeugt das `NewText`/`RemoveText`-Spam.
//! Verfahren: Normalisieren → Ähnlichkeits-Matching → zeitbasiertes Debounce
//! (neu/geändert nach 250 ms gleichem Befund, weg nach 700 ms) →
//! Move-Totband. Zeit- statt framebasiert: Die Frame-Zeit schwankt je nach
//! OCR-Last zwischen 0,1 und 2 s — frame-gezählt würde Verschieben
//! sekundenlang Geistertext stehen lassen. Dazu das Still-da-Urteil:
//! Eine bekannte Zeile, deren Box noch detektiert wird (Erkennung hängt
//! im Budget hinterher), gilt als anwesend statt als verschwunden.
//! Rein funktional über Frame-Folgen, ohne Display/Modelle testbar.

use std::collections::HashMap;
use std::time::{Duration, Instant};

use lbw_common::{Rect, ServerMsg, TextItem};

/// Gleicher Befund über diese Zeit, bevor Neu/Änderung gesendet wird.
const CONFIRM_AFTER: Duration = Duration::from_millis(250);
/// Ohne Befund (und ohne Box) über diese Zeit, bevor gelöscht wird.
const REMOVE_AFTER: Duration = Duration::from_millis(700);
/// Mindest-Ähnlichkeit für ein ID-Match (Text trägt die Identität).
const MATCH_SIM: f32 = 0.8;
/// Darunter gilt eine Zeile als verschwunden — außer die Box sitzt noch
/// (Tippen ändert kurzen Text stark, aber nicht die Position).
const KEEP_SIM: f32 = 0.5;
/// Positionsfenster für das Tipp-Gate (wie bisheriges MATCH_DIST).
const KEEP_DIST: f32 = 24.0;
/// Move-Totband: Mittelpunkt-Wanderung in px.
const MOVE_DIST: f32 = 6.0;
/// Move-Totband: Größenänderung (|dw|+|dh|) in px.
const MOVE_SIZE: u16 = 4;

/// Normalisierte Zeile (quantisiert — kleinere Abweichungen existieren
/// danach gar nicht mehr).
#[derive(Clone, Debug, PartialEq, Eq)]
struct Norm {
    text: String,
    fg: [u8; 3],
    bg: [u8; 3],
    rect: Rect,
}

fn norm_text(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn norm_color(c: [u8; 3]) -> [u8; 3] {
    [c[0] & 0xF0, c[1] & 0xF0, c[2] & 0xF0]
}

fn norm_rect(r: Rect) -> Rect {
    Rect::new(r.x & !1, r.y & !1, r.w & !1, r.h & !1)
}

fn norm(t: &TextItem) -> Norm {
    Norm {
        text: norm_text(&t.text),
        fg: norm_color(t.fg),
        bg: norm_color(t.bg),
        rect: norm_rect(t.rect),
    }
}

/// Normalisierte Levenshtein-Ähnlichkeit in [0, 1].
fn similarity(a: &str, b: &str) -> f32 {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    if a.is_empty() && b.is_empty() {
        return 1.0;
    }
    if a.is_empty() || b.is_empty() {
        return 0.0;
    }
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0usize; b.len() + 1];
    for (i, ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            cur[j + 1] = (prev[j] + usize::from(ca != cb))
                .min(prev[j + 1] + 1)
                .min(cur[j] + 1);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    let dist = prev[b.len()] as f32;
    1.0 - dist / (a.len().max(b.len()) as f32)
}

fn center(r: Rect) -> (f32, f32) {
    (
        f32::from(r.x) + f32::from(r.w) / 2.0,
        f32::from(r.y) + f32::from(r.h) / 2.0,
    )
}

/// Kosten cur→prev (kleiner = besser): Text-Distanz dominiert, Position
/// und Größe entscheiden bei gleichem Text (Scroll!).
fn cost(c: &Norm, p: &Norm) -> f32 {
    let (cx, cy) = center(c.rect);
    let (px, py) = center(p.rect);
    let d = (cx - px).hypot(cy - py);
    let dw = c.rect.w.abs_diff(p.rect.w) + c.rect.h.abs_diff(p.rect.h);
    let same_colors = (c.fg == p.fg && c.bg == p.bg) as u8 as f32;
    (1.0 - similarity(&c.text, &p.text)) * 1000.0 + d + dw as f32 * 4.0 + (1.0 - same_colors) * 50.0
}

/// Stabilisierter Zustand einer Zeile.
#[derive(Clone, Debug)]
struct Line {
    /// Zuletzt *gesendeter* Inhalt (Referenz für Deltas).
    sent: TextItem,
    /// Ausstehender neuer Inhalt + Zeitpunkt des ersten gleichen Befunds.
    pending: Option<(TextItem, Instant)>,
    /// Seit wann ohne Befund (`None` = anwesend).
    missing_since: Option<Instant>,
}

/// Box noch da? Mittelpunkt der gesendeten Box liegt in einer detektierten
/// Box (Erkennung hängt im Budget hinterher — kein Verschwinden).
fn still_there(sent: &TextItem, seen: &[Rect]) -> bool {
    let (cx, cy) = center(sent.rect);
    seen.iter().any(|b| {
        let (x0, y0) = (f32::from(b.x), f32::from(b.y));
        cx >= x0 && cx <= x0 + f32::from(b.w) && cy >= y0 && cy <= y0 + f32::from(b.h)
    })
}

/// Anti-Flacker-Zustand über Frames hinweg.
#[derive(Default)]
pub struct Stabilizer {
    lines: HashMap<u64, Line>,
    next_id: u64,
}

impl Stabilizer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            lines: HashMap::new(),
            next_id: 1,
        }
    }

    /// Verarbeitet einen OCR-Frame, liefert das sendefertige Delta.
    /// Erster Aufruf je Verbindung: `ClearText` + alle Zeilen als `NewText`.
    /// `seen` sind alle Detektions-Boxen (auch unerkannte) fürs
    /// Still-da-Urteil; `now` steuert das zeitbasierte Debounce.
    pub fn step(
        &mut self,
        cur: Vec<TextItem>,
        seen_boxes: &[Rect],
        first: bool,
        now: Instant,
    ) -> Vec<ServerMsg> {
        let cur: Vec<(TextItem, Norm)> = cur
            .into_iter()
            .map(|t| {
                let n = norm(&t);
                (t, n)
            })
            .collect();
        // Greedy-Matching cur → prev (used-Flags: Duplikate als Multimenge).
        let mut used: HashMap<u64, bool> = HashMap::new();
        let mut matched: Vec<(usize, Option<u64>)> = Vec::with_capacity(cur.len());
        for (i, (_, n)) in cur.iter().enumerate() {
            let mut best: Option<(u64, f32)> = None;
            for (id, line) in &self.lines {
                if used.get(id).is_some_and(|u| *u) {
                    continue;
                }
                let pn = norm(&line.sent);
                if similarity(&n.text, &pn.text) < KEEP_SIM {
                    continue;
                }
                let c = cost(n, &pn);
                if best.is_none_or(|(_, bc)| c < bc) {
                    best = Some((*id, c));
                }
            }
            match best {
                Some((id, _)) => {
                    used.insert(id, true);
                    matched.push((i, Some(id)));
                }
                None => matched.push((i, None)),
            }
        }

        let mut out = Vec::new();
        if first {
            out.push(ServerMsg::ClearText);
        }
        let mut seen: HashMap<u64, bool> = HashMap::new();
        for (i, id) in matched {
            let (mut t, n) = (cur[i].0.clone(), cur[i].1.clone());
            match id {
                Some(id) => {
                    seen.insert(id, true);
                    let line = self.lines.get_mut(&id).unwrap();
                    line.missing_since = None;
                    let pn = norm(&line.sent);
                    let sim = similarity(&n.text, &pn.text);
                    let (cx, cy) = center(n.rect);
                    let (px, py) = center(pn.rect);
                    let dist = (cx - px).hypot(cy - py);
                    // Gleiche Zeile: Text fast gleich ODER Box noch da
                    // (Tippen/Flackern bei kurzem Text).
                    let same = sim >= MATCH_SIM || (dist <= KEEP_DIST && sim >= KEEP_SIM);
                    if same && n == pn {
                        // Identisch (nach Normalisierung): Stille.
                        line.pending = None;
                    } else if same {
                        // Kandidat für Neu/Änderung/Move: erst nach
                        // CONFIRM_AFTER gleichem Befund senden.
                        t.id = id;
                        let since = match &line.pending {
                            Some((p, since)) if norm(p) == n => *since,
                            _ => now,
                        };
                        if now.duration_since(since) >= CONFIRM_AFTER {
                            line.pending = None;
                            if norm_text(&n.text) == norm_text(&pn.text)
                                && n.fg == pn.fg
                                && n.bg == pn.bg
                            {
                                // Nur Box neu (Totband schon via Raster +
                                // Mindestwanderung prüfen).
                                let (cx, cy) = center(n.rect);
                                let (px, py) = center(pn.rect);
                                let dw =
                                    n.rect.w.abs_diff(pn.rect.w) + n.rect.h.abs_diff(pn.rect.h);
                                if (cx - px).hypot(cy - py) >= MOVE_DIST || dw >= MOVE_SIZE {
                                    t.id = id;
                                    out.push(ServerMsg::MoveText { id, rect: t.rect });
                                    line.sent = t;
                                }
                            } else {
                                t.id = id;
                                out.push(ServerMsg::NewText(t.clone()));
                                line.sent = t;
                            }
                        } else {
                            line.pending = Some((t, since));
                        }
                    } else {
                        // Im Hysterese-Band: alten Inhalt behalten, kein Spam.
                        line.pending = None;
                    }
                }
                None => {
                    let id = self.next_id;
                    self.next_id += 1;
                    seen.insert(id, true);
                    t.id = id;
                    if first {
                        out.push(ServerMsg::NewText(t.clone()));
                        self.lines.insert(
                            id,
                            Line {
                                sent: t,
                                pending: None,
                                missing_since: None,
                            },
                        );
                    } else {
                        // Neue Zeile: erst nach CONFIRM_AFTER senden.
                        self.lines.insert(
                            id,
                            Line {
                                sent: t.clone(),
                                pending: Some((t, now)),
                                missing_since: None,
                            },
                        );
                    }
                }
            }
        }
        // Verschwundene erst nach REMOVE_AFTER löschen — außer die Box ist
        // noch detektiert (Still-da: Erkennung hängt im Budget hinterher).
        let mut gone = Vec::new();
        for (id, line) in self.lines.iter_mut() {
            if seen.get(id).is_some_and(|s| *s) {
                continue;
            }
            if still_there(&line.sent, seen_boxes) {
                line.missing_since = None;
                line.pending = None;
                continue;
            }
            let since = *line.missing_since.get_or_insert(now);
            line.pending = None;
            if now.duration_since(since) >= REMOVE_AFTER {
                gone.push(*id);
            }
        }
        for id in gone {
            self.lines.remove(&id);
            if !first {
                out.push(ServerMsg::RemoveText(id));
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::Rect;

    fn item(x: u16, y: u16, text: &str) -> TextItem {
        TextItem {
            id: 0,
            rect: Rect::new(x, y, 100, 16),
            fg: [10; 3],
            bg: [250; 3],
            text: text.into(),
        }
    }

    fn t0() -> Instant {
        Instant::now()
    }

    fn step_texts(
        s: &mut Stabilizer,
        items: Vec<TextItem>,
        seen: &[Rect],
        first: bool,
        now: Instant,
    ) -> Vec<ServerMsg> {
        s.step(items, seen, first, now)
    }

    #[test]
    fn jitter_produces_no_messages_after_first() {
        let mut s = Stabilizer::new();
        let t = t0();
        let first = step_texts(&mut s, vec![item(10, 20, "Hallo Welt")], &[], true, t);
        assert!(matches!(first[0], ServerMsg::ClearText));
        assert_eq!(first.len(), 2);
        // Jitter: ±3 px, ±10 Farben, abwechselnd 1 falsches Zeichen.
        let frames = vec![
            vec![TextItem {
                rect: Rect::new(11, 22, 100, 16),
                fg: [12; 3],
                ..item(0, 0, "Hallo Welt")
            }],
            vec![TextItem {
                rect: Rect::new(9, 19, 100, 16),
                fg: [4; 3],
                ..item(0, 0, "Hallo Velt")
            }],
            vec![item(12, 21, "Hallo Welt")],
            vec![item(10, 20, "Hallo Welt")],
        ];
        for (i, f) in frames.into_iter().enumerate() {
            assert_eq!(
                step_texts(
                    &mut s,
                    f,
                    &[],
                    false,
                    t + Duration::from_millis(i as u64 * 100)
                ),
                vec![],
                "Jitter muss still sein"
            );
        }
    }

    #[test]
    fn typing_updates_after_confirm_time() {
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(&mut s, vec![item(10, 20, "a")], &[], true, t);
        // Frisch getippt: noch nichts.
        assert_eq!(
            step_texts(&mut s, vec![item(10, 20, "ab")], &[], false, t),
            vec![]
        );
        // Gleicher Befund nach 300 ms: Update.
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "ab")],
            &[],
            false,
            t + Duration::from_millis(300),
        );
        assert_eq!(d.len(), 1);
        match &d[0] {
            ServerMsg::NewText(t) => assert_eq!(t.text, "ab"),
            m => panic!("NewText erwartet, {m:?}"),
        }
    }

    #[test]
    fn scroll_sends_only_movetext() {
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(
            &mut s,
            vec![
                item(10, 20, "eins"),
                item(10, 40, "zwei"),
                item(10, 60, "drei"),
            ],
            &[],
            true,
            t,
        );
        // Alle Zeilen −20 px (Scroll): nur MoveText, keine Strings neu.
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "zwei"), item(10, 40, "drei")],
            &[],
            false,
            t,
        );
        // Erster Scroll-Frame: noch nichts (Debounce), +300 ms: Moves.
        assert_eq!(d, vec![]);
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "zwei"), item(10, 40, "drei")],
            &[],
            false,
            t + Duration::from_millis(300),
        );
        assert_eq!(d.len(), 2);
        for m in &d {
            assert!(
                matches!(m, ServerMsg::MoveText { .. }),
                "nur MoveText, {m:?}"
            );
        }
        assert!(!d.iter().any(|m| matches!(m, ServerMsg::NewText(_))));
    }

    #[test]
    fn missing_briefly_keeps_after_timeout_removes() {
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(
            &mut s,
            vec![item(10, 20, "bleibt"), item(10, 40, "geht")],
            &[],
            true,
            t,
        );
        // 400 ms ohne "geht": kein RemoveText.
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "bleibt")],
            &[],
            false,
            t + Duration::from_millis(400),
        );
        assert!(!d.iter().any(|m| matches!(m, ServerMsg::RemoveText(_))));
        // Ab 700 ms ohne Befund (hier: +1100 ms): genau ein RemoveText.
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "bleibt")],
            &[],
            false,
            t + Duration::from_millis(1200),
        );
        assert_eq!(
            d.iter()
                .filter(|m| matches!(m, ServerMsg::RemoveText(_)))
                .count(),
            1
        );
    }

    #[test]
    fn box_still_seen_holds_line_despite_missing_text() {
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(&mut s, vec![item(10, 40, "geht")], &[], true, t);
        // Erkennung hängt hinterher (Budget), Box aber detektiert:
        // selbst nach 2 s kein RemoveText, kein Re-New.
        let seen = [Rect::new(8, 38, 104, 20)];
        for ms in [500, 1000, 2000] {
            let d = step_texts(&mut s, vec![], &seen, false, t + Duration::from_millis(ms));
            assert_eq!(d, vec![], "Still-da muss halten ({ms} ms)");
        }
        // Box wirklich weg → 700 ms später genau ein RemoveText.
        let d = step_texts(&mut s, vec![], &[], false, t + Duration::from_millis(2700));
        assert!(!d.iter().any(|m| matches!(m, ServerMsg::RemoveText(_))));
        let d = step_texts(&mut s, vec![], &[], false, t + Duration::from_millis(3400));
        assert_eq!(
            d.iter()
                .filter(|m| matches!(m, ServerMsg::RemoveText(_)))
                .count(),
            1
        );
    }

    #[test]
    fn duplicates_match_as_multiset() {
        let mut s = Stabilizer::new();
        let t = t0();
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "x"), item(10, 40, "x"), item(10, 60, "x")],
            &[],
            true,
            t,
        );
        assert_eq!(
            d.iter()
                .filter(|m| matches!(m, ServerMsg::NewText(_)))
                .count(),
            3
        );
        // Gleicher Frame: Stille, keine IDs neu vergeben.
        let d = step_texts(
            &mut s,
            vec![item(10, 20, "x"), item(10, 40, "x"), item(10, 60, "x")],
            &[],
            false,
            t + Duration::from_millis(100),
        );
        assert_eq!(d, vec![]);
    }
}
