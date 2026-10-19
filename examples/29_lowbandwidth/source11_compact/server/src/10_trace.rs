//! `10_trace` — optionale Ablaufverfolgung zur Live-Diagnose (Plan: „wer
//! sieht was pro Frame?"). Ohne `--trace` ein No-op (eine
//! `Option`-Prüfung pro Frame, kein messbarer Overhead); mit Flag schreibt
//! die Session pro Frame je eine Zeile für OCR-Befund und Sende-Delta in
//! eine Textdatei. Format bewusst simpel (grep-bar, keine Deps):
//! `t=<ms> frame=<n> <stufe> ...`.
//!
//! Diagnose-Rezept Tab-Wechsel: Server mit `--trace spur.log` starten, Tab
//! wechseln, Log lesen. Pro Frame stehen OCR-Befund und Delta nebeneinander:
//! - OCR-Zeile da, aber kein `NewText` → Stabilisator hält (pending,
//!   Hysterese, Still-da: Begründung steht im selben Frame am Delta).
//! - OCR-Zeile fehlt → Detektor/Erkennung (keine Box oder Gatter).
//! - `NewText`/`RemoveText` im Log, Client zeigt trotzdem alt → Draht oder
//!   Client (dort weiter suchen, nicht in der Pipeline).
//!
//! Zum Entfernen: Datei löschen, `mod trace` aus `lib.rs`, `--trace` aus
//! der Config und die drei `Tracer`-Aufrufe in `07_session.rs` streichen.

use std::fs::File;
use std::io::{BufWriter, Write};
use std::time::Instant;

use lbw_common::{Rect, ServerMsg, TextItem};

/// Ablaufverfolgung; ohne Datei vollständig stumm.
pub struct Tracer {
    wr: Option<BufWriter<File>>,
    t0: Instant,
}

impl Tracer {
    /// Keine Verfolgung (Default ohne `--trace`).
    #[must_use]
    pub fn disabled() -> Self {
        Self {
            wr: None,
            t0: Instant::now(),
        }
    }

    /// Verfolgung in `path` (wird angelegt/überschrieben).
    pub fn open(path: &str) -> Result<Self, String> {
        let f = File::create(path).map_err(|e| format!("Trace-Datei: {e}"))?;
        Ok(Self {
            wr: Some(BufWriter::new(f)),
            t0: Instant::now(),
        })
    }

    fn ms(&self) -> u64 {
        self.t0.elapsed().as_millis() as u64
    }

    fn line(&mut self, s: String) {
        if let Some(w) = self.wr.as_mut() {
            let _ = writeln!(w, "{s}");
        }
    }

    /// Frame-Kopf (immer zuerst, auch ohne Befund — Lücken im Log sind
    /// stehende Schleifen, keine fehlenden Frames).
    pub fn frame(&mut self, n: u64) {
        if self.wr.is_none() {
            return;
        }
        let ms = self.ms();
        self.line(format!("t={ms} frame={n}"));
    }

    /// OCR-Befund des Frames: Boxen (Detektor, inkl. unerkannter) und
    /// gelesene Zeilen (nach Gatter/Budget).
    pub fn ocr(&mut self, n: u64, boxes: &[Rect], texts: &[TextItem]) {
        if self.wr.is_none() {
            return;
        }
        let ms = self.ms();
        self.line(format!("t={ms} frame={n} ocr boxen={}", boxes.len()));
        for t in texts {
            let r = t.rect;
            self.line(format!(
                "t={ms} frame={n} text id={} box={},{},{}x{} {:?}",
                t.id, r.x, r.y, r.w, r.h, t.text
            ));
        }
    }

    /// Sende-Delta des Frames (Stabilisator-Entscheidung, 1:1 was rausgeht).
    pub fn delta(&mut self, n: u64, msgs: &[ServerMsg]) {
        if self.wr.is_none() {
            return;
        }
        let ms = self.ms();
        if msgs.is_empty() {
            self.line(format!("t={ms} frame={n} delta still"));
        }
        for m in msgs {
            self.line(format!("t={ms} frame={n} delta {m:?}"));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file(dir: &std::path::Path, name: &str) -> String {
        dir.join(name).to_string_lossy().into_owned()
    }

    #[test]
    fn schreibt_frames_und_deltas() {
        let dir = std::env::temp_dir();
        let p = file(&dir, "lbw_trace_unit.log");
        let _ = std::fs::remove_file(&p);
        let mut tr = Tracer::open(&p).unwrap();
        tr.frame(1);
        tr.ocr(
            1,
            &[Rect::new(0, 0, 10, 10)],
            &[TextItem {
                id: 7,
                rect: Rect::new(0, 0, 10, 10),
                fg: [0; 3],
                bg: [255; 3],
                text: "Hallo".into(),
            }],
        );
        tr.delta(1, &[]);
        tr.delta(
            2,
            &[ServerMsg::NewText(TextItem {
                id: 7,
                rect: Rect::new(0, 0, 10, 10),
                fg: [0; 3],
                bg: [255; 3],
                text: "Hallo".into(),
            })],
        );
        drop(tr);
        let log = std::fs::read_to_string(&p).unwrap();
        assert!(log.contains("frame=1\n"), "{log}");
        assert!(log.contains("boxen=1"), "{log}");
        assert!(log.contains("Hallo"), "{log}");
        assert!(log.contains("delta still"), "{log}");
        assert!(log.contains("NewText"), "{log}");
        let _ = std::fs::remove_file(&p);
    }

    #[test]
    fn abgeschaltet_schreibt_nichts_und_panikt_nicht() {
        let mut tr = Tracer::disabled();
        tr.frame(1);
        tr.ocr(1, &[], &[]);
        tr.delta(1, &[]);
    }
}
