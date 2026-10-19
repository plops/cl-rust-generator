//! `09_stabilize` — OCR-Stabilisierung gegen Flacker-Spam (Plan §2.2a).
//!
//! PP-OCR liefert pro Frame leicht andere Ergebnisse (Box ±wenige Pixel,
//! Farben ±wenige Stufen, gelegentlich ein falsches Zeichen, kurzes
//! Aussetzen). Ohne Dämpfung erzeugt das `NewText`/`RemoveText`-Spam.
//! Verfahren: Normalisieren → Ähnlichkeits-Matching → Folgeframe-Bestätigung
//! (neu/geändert erst, wenn derselbe Befund in zwei aufeinanderfolgenden
//! Frames steht; weg nach 700 ms ohne Befund) → Move-Totband.
//! Die Bestätigung zählt Frames statt Wanduhr: Ein einzelner Flacker-Frame
//! wird geschluckt, aber es gibt keine künstliche Mindest-Verzögerung —
//! Text-Latenz ist damit wie ohne Stabilisator (ein Frame), statt 250 ms
//! plus Frame-Takt. Nur das Entfernen bleibt zeitbasiert, weil dort die
//! Frame-Zeit je nach OCR-Last zwischen 0,1 und 2 s schwankt und
//! frame-gezählt sekundenlang Geistertext stehen ließe.
//! Dazu das Still-da-Urteil mit Szenenwechsel-Ausnahme:
//! Eine bekannte Zeile, deren Box noch detektiert wird (Erkennung hängt
//! im Budget hinterher), gilt als anwesend statt als verschwunden.
//! Rein funktional über Frame-Folgen, ohne Display/Modelle testbar.

use std::collections::HashMap;
use std::time::{Duration, Instant};

use lbw_common::{Rect, ServerMsg, TextItem};

/// Ohne Befund (und ohne Box) über diese Zeit, bevor gelöscht wird.
const REMOVE_AFTER: Duration = Duration::from_millis(700);
/// Mindest-Ähnlichkeit für ein ID-Match (Text trägt die Identität).
const MATCH_SIM: f32 = 0.8;
/// Scroll-Freeze: so viele Zeilen müssen denselben Shift teilen …
const SCROLL_MIN_LINES: usize = 3;
/// … von mindestens einer Zeilenhöhe, damit Tipp-Getrippel und Jitter nie
/// als Scroll gelten (Move-Totband: 6 px, Zeilenhöhe: ~16+ px).
const SCROLL_MIN_SHIFT: f32 = 16.0;
/// Toleranz beim Shift-Vergleich (OCR-Jitter ±wenige Pixel).
const SCROLL_TOL: f32 = 3.0;
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
    /// Ausstehender neuer Inhalt + Zahl der Wiederholungen in Folge
    /// (1 = in zwei aufeinanderfolgenden Frames gesehen = senden).
    pending: Option<(TextItem, u32)>,
    /// Ging der aktuelle Inhalt je als `NewText` raus? Neue Zeilen werden
    /// erst bestätigt und brauchen dann ein `NewText` (kein `MoveText`
    /// für eine dem Client unbekannte ID).
    live: bool,
    /// Seit wann ohne Befund (`None` = anwesend).
    missing_since: Option<Instant>,
}

/// Fläche eines Rechtecks in px².
fn rect_area(r: Rect) -> i64 {
    i64::from(r.w) * i64::from(r.h)
}

/// Überlappung zweier Boxen in px² (0 ohne Überlappung).
fn overlap(a: Rect, b: Rect) -> i64 {
    let x0 = u32::from(a.x).max(u32::from(b.x));
    let y0 = u32::from(a.y).max(u32::from(b.y));
    let x1 = (u32::from(a.x) + u32::from(a.w)).min(u32::from(b.x) + u32::from(b.w));
    let y1 = (u32::from(a.y) + u32::from(a.h)).min(u32::from(b.y) + u32::from(b.h));
    i64::from(x1.saturating_sub(x0)) * i64::from(y1.saturating_sub(y0))
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
    scrolling: bool,
    was_scrolling: bool,
}

impl Stabilizer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            lines: HashMap::new(),
            next_id: 1,
            scrolling: false,
            was_scrolling: false,
        }
    }

    /// Läuft gerade ein Scroll (uniformer Shift im letzten Schritt)? Die
    /// Session hält dann Kacheln zurück, bis das Bild steht.
    #[must_use]
    pub fn scrolling(&self) -> bool {
        self.scrolling
    }

    /// Verarbeitet einen OCR-Frame, liefert das sendefertige Delta.
    /// Erster Aufruf je Verbindung: `ClearText` + alle Zeilen als `NewText`.
    /// `seen` sind alle Detektions-Boxen (auch unerkannte) fürs
    /// Still-da-Urteil; `now` steuert nur noch das zeitbasierte Entfernen
    /// (Neu/Änderung bestätigt der Folgeframe, nicht die Wanduhr).
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

        // Scroll-Freeze: Teilen sich genug Zeilen denselben großen Shift,
        // fährt das Bild — Inhalt ist dann nicht lesbar, also fließen nur
        // Positionen (Move) und Entfernungen; alle News (Inhalts-Updates wie
        // neue IDs) warten auf Stillstand und kommen dann genau einmal.
        // Das streicht OCR-Wackel-Resends fahrender Zeilen und
        // Kanten-Fragmente, ohne Latenz im lesbaren Betrieb zu kosten.
        let mut votes: Vec<(f32, f32)> = Vec::new();
        for (i, id) in matched
            .iter()
            .filter_map(|(i, id)| (*id).map(|id| (*i, id)))
        {
            if let Some(line) = self.lines.get(&id) {
                let n = &cur[i].1;
                if similarity(&n.text, &norm_text(&line.sent.text)) >= MATCH_SIM {
                    let (cx, cy) = center(n.rect);
                    let pn = norm(&line.sent);
                    let (px, py) = center(pn.rect);
                    votes.push((cx - px, cy - py));
                }
            }
        }
        let mut scrolling = false;
        // Jede Stimme zählt sich selbst mit (Abstand 0 <= Toleranz).
        for v in &votes {
            if v.0.abs().max(v.1.abs()) < SCROLL_MIN_SHIFT {
                continue;
            }
            let supporters = votes
                .iter()
                .filter(|u| (u.0 - v.0).abs() <= SCROLL_TOL && (u.1 - v.1).abs() <= SCROLL_TOL)
                .count();
            if supporters >= SCROLL_MIN_LINES {
                scrolling = true;
                break;
            }
        }
        self.scrolling = scrolling;

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
                    if same && n == pn && line.pending.is_none() {
                        // Identisch (nach Normalisierung) und bestätigt: Stille.
                    } else if same {
                        // Kandidat für Neu/Änderung/Move: senden, sobald
                        // derselbe Befund im Folgeframe steht. Ein einzelner
                        // Flacker-Frame bleibt stumm — aber keine Wanduhr
                        // bestimmt mehr die Latenz (ein Frame statt 250 ms).
                        t.id = id;
                        let repeats = match &line.pending {
                            Some((p, r)) if norm(p) == n => *r + 1,
                            _ => 0,
                        };
                        // Scroll-Freeze: Inhalt (NewText) wartet auf Stillstand,
                        // Position (MoveText) läuft weiter; pending sammelt,
                        // der Settle-Flusch unten sendet genau einmal nach.
                        if repeats >= 1 {
                            if !line.live {
                                // Erstbestätigung einer neuen Zeile: Inhalt
                                // geht erstmals raus (kein Move für eine dem
                                // Client unbekannte ID).
                                if scrolling {
                                    line.pending = Some((t, repeats));
                                } else {
                                    line.pending = None;
                                    t.id = id;
                                    out.push(ServerMsg::NewText(t.clone()));
                                    line.sent = t;
                                    line.live = true;
                                }
                            } else if norm_text(&n.text) == norm_text(&pn.text)
                                && n.fg == pn.fg
                                && n.bg == pn.bg
                            {
                                // Nur Box neu (Totband schon via Raster +
                                // Mindestwanderung prüfen).
                                line.pending = None;
                                let (cx, cy) = center(n.rect);
                                let (px, py) = center(pn.rect);
                                let dw =
                                    n.rect.w.abs_diff(pn.rect.w) + n.rect.h.abs_diff(pn.rect.h);
                                if (cx - px).hypot(cy - py) >= MOVE_DIST || dw >= MOVE_SIZE {
                                    t.id = id;
                                    out.push(ServerMsg::MoveText { id, rect: t.rect });
                                    line.sent = t;
                                }
                            } else if scrolling {
                                line.pending = Some((t, repeats));
                            } else {
                                line.pending = None;
                                t.id = id;
                                out.push(ServerMsg::NewText(t.clone()));
                                line.sent = t;
                            }
                        } else {
                            line.pending = Some((t, repeats));
                        }
                    } else {
                        // Im Hysterese-Band: alten Inhalt behalten, kein Spam —
                        // aber nicht ewig: Wiederholt sich derselbe
                        // abweichende Befund, ist es kein Flackern, sondern
                        // neuer Inhalt auf derselben ID (z. B. Tab-Wechsel auf
                        // ähnliche, aber andere Zeile). Sonst bliebe die ID
                        // stumm blockiert und der wahre Inhalt käme nie.
                        t.id = id;
                        let h = match &line.pending {
                            Some((p, r)) if norm(p) == n => *r + 1,
                            _ => 0,
                        };
                        if h >= 2 && !scrolling {
                            line.pending = None;
                            out.push(ServerMsg::NewText(t.clone()));
                            line.sent = t;
                            line.live = true;
                        } else {
                            line.pending = Some((t, h));
                        }
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
                                live: true,
                                missing_since: None,
                            },
                        );
                    } else {
                        // Neue Zeile: senden, sobald der Folgeframe sie
                        // bestätigt (statt nach fester Wartezeit).
                        self.lines.insert(
                            id,
                            Line {
                                sent: t.clone(),
                                pending: Some((t, 0)),
                                live: false,
                                missing_since: None,
                            },
                        );
                    }
                }
            }
        }
        // Settle-Flusch: Was der Scroll-Freeze angehalten hat, kommt beim
        // ersten stehenden Frame genau einmal nach (neueste Fassung, nur
        // für Zeilen, die jetzt auch wirklich da sind — Reste fahrender
        // Fragmente laufen in den normalen Weg-Timer).
        if !scrolling && self.was_scrolling {
            for (id, line) in self.lines.iter_mut() {
                if !seen.get(id).is_some_and(|s| *s) {
                    continue;
                }
                if let Some((t, _)) = line.pending.take()
                    && norm(&t) != norm(&line.sent)
                {
                    let mut t = t;
                    t.id = *id;
                    out.push(ServerMsg::NewText(t.clone()));
                    line.sent = t;
                    line.live = true;
                }
            }
        }
        self.was_scrolling = scrolling;
        // Verschwundene erst nach REMOVE_AFTER löschen — außer die Box ist
        // noch detektiert (Still-da: Erkennung hängt im Budget hinterher).
        // Ausnahme: Anderslautender Inhalt belegt die alte Box deutlich —
        // dort steht jetzt wirklich etwas anderes (z. B. neuer Browser-Tab),
        // kein hinterherhinkendes Budget. Dann läuft der Weg-Timer, statt
        // die Zeile ewig festzuhalten.
        let mut gone = Vec::new();
        for (id, line) in self.lines.iter_mut() {
            if seen.get(id).is_some_and(|s| *s) {
                continue;
            }
            let area = rect_area(line.sent.rect);
            let sent_norm = norm_text(&line.sent.text);
            let replaced = area > 0
                && cur.iter().any(|(t, _)| {
                    overlap(t.rect, line.sent.rect) * 2 >= area
                        && similarity(&norm_text(&t.text), &sent_norm) < MATCH_SIM
                });
            if !replaced && still_there(&line.sent, seen_boxes) {
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
    fn repeat_confirms_without_wall_clock() {
        // Parität mit source10: Keine 250-ms-Wanduhr mehr — derselbe Befund
        // im Folgeframe genügt, auch wenn keine einzige Millisekunde
        // vergangen ist. Mit Wanduhr-Debounce bliebe der zweite Schritt
        // stumm (Dauer 0 < 250 ms).
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(&mut s, vec![item(10, 20, "Hallo")], &[], true, t);
        assert_eq!(
            step_texts(&mut s, vec![item(10, 20, "Hallo!")], &[], false, t),
            vec![]
        );
        let d = step_texts(&mut s, vec![item(10, 20, "Hallo!")], &[], false, t);
        assert_eq!(d.len(), 1);
        match &d[0] {
            ServerMsg::NewText(t) => assert_eq!(t.text, "Hallo!"),
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
    fn tab_switch_replaces_old_with_new() {
        // Browser-Tab-Wechsel: gleiche Positionen, anderer Inhalt.
        // Erwartung: neue Zeilen kommen per NewText, alte gehen per
        // RemoveText — kein ewiges Festhalten, kein Verschlucken.
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(
            &mut s,
            vec![item(10, 20, "Sonnenaufgang"), item(10, 40, "Mietvertrag")],
            &[],
            true,
            t,
        );
        let beta = vec![
            item(10, 20, "Kartoffelsalat"),
            item(10, 40, "Zahnarzttermin"),
        ];
        let seen = [Rect::new(10, 20, 100, 16), Rect::new(10, 40, 100, 16)];
        let mut news = 0;
        let mut removes = 0;
        for i in 1..=12 {
            let d = step_texts(
                &mut s,
                beta.clone(),
                &seen,
                false,
                t + Duration::from_millis(i * 100),
            );
            news += d
                .iter()
                .filter(|m| matches!(m, ServerMsg::NewText(_)))
                .count();
            removes += d
                .iter()
                .filter(|m| matches!(m, ServerMsg::RemoveText(_)))
                .count();
        }
        assert_eq!(news, 2, "neuer Tab-Inhalt muss ankommen");
        assert_eq!(removes, 2, "alter Tab-Inhalt muss gehen");
    }

    #[test]
    fn scroll_freezes_content_but_moves_flow() {
        // Vier Zeilen fahren gemeinsam (−63 px): Positionen laufen als Move
        // (sobald stabil), der wackelnde Inhalt wartet; Stillstand flusht
        // genau einmal. Moves brauchen wie Inhalte einen stabilen
        // Folgeframe — deshalb ist der erste Scroll-Frame still.
        let mut s = Stabilizer::new();
        let t = t0();
        let base = vec![
            item(10, 200, "eins"),
            item(10, 220, "zwei"),
            item(10, 240, "drei"),
            item(10, 260, "vier"),
        ];
        let first = step_texts(&mut s, base, &[], true, t);
        assert_eq!(first.len(), 5); // ClearText + 4 News
        assert!(!s.scrolling());

        let scrolled = vec![
            item(10, 137, "eins"),
            item(10, 157, "zwei"),
            item(10, 177, "drei"),
            item(10, 197, "vier?"),
        ];
        let d = step_texts(&mut s, scrolled.clone(), &[], false, t);
        assert!(s.scrolling(), "uniformer Shift muss Scroll melden");
        assert_eq!(d, vec![], "erste Sichtung: noch alles pending");

        // Stabile Position: drei Moves, Inhalt weiter eingefroren.
        let d = step_texts(&mut s, scrolled.clone(), &[], false, t);
        assert!(s.scrolling());
        assert_eq!(
            d.iter()
                .filter(|m| matches!(m, ServerMsg::MoveText { .. }))
                .count(),
            3,
            "stabile Zeilen ziehen als Move mit, {d:?}"
        );
        assert!(
            d.iter().all(|m| !matches!(m, ServerMsg::NewText(_))),
            "Inhalt wartet auf Stillstand, {d:?}"
        );

        // Stillstand mit frischem Wackeln: genau ein Flusch mit Fassung.
        let settled: Vec<TextItem> = scrolled
            .iter()
            .map(|it| {
                let mut n = it.clone();
                if n.text == "vier?" {
                    n.text = "vier?!".into();
                }
                n
            })
            .collect();
        let d = step_texts(&mut s, settled.clone(), &[], false, t);
        assert!(!s.scrolling());
        let news: Vec<&str> = d
            .iter()
            .filter_map(|m| match m {
                ServerMsg::NewText(t) => Some(t.text.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(news, vec!["vier?!"], "genau einmal nachreichen, {d:?}");
        // … danach wieder Stille.
        let d = step_texts(&mut s, settled, &[], false, t);
        assert_eq!(d, vec![]);
    }

    #[test]
    fn small_uniform_shift_is_no_scroll() {
        // Drei Zeilen, gemeinsam nur −8 px (unter der Zeilenhöhe): kein
        // Freeze — Inhalts-Update bestätigt normal im Folgeframe.
        let mut s = Stabilizer::new();
        let t = t0();
        step_texts(
            &mut s,
            vec![
                item(10, 100, "alpha"),
                item(10, 120, "beta"),
                item(10, 140, "gamma"),
            ],
            &[],
            true,
            t,
        );
        let moved = vec![
            item(10, 92, "alphax"),
            item(10, 112, "betax"),
            item(10, 132, "gammax"),
        ];
        let d = step_texts(&mut s, moved.clone(), &[], false, t);
        assert!(!s.scrolling(), "8 px sind Tippen, kein Scroll");
        assert!(
            d.iter().all(|m| !matches!(m, ServerMsg::NewText(_))),
            "erste Sichtung noch still"
        );
        let d = step_texts(&mut s, moved, &[], false, t);
        assert_eq!(
            d.iter()
                .filter(|m| matches!(m, ServerMsg::NewText(_)))
                .count(),
            3,
            "Folgeframe bestätigt normal, {d:?}"
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
