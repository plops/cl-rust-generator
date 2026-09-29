//! `15_pipeline` — Capture → Analyse → Text-Delta → Maske → AV1-Kacheln.
//!
//! Adaptive Bildrate: OCR läuft bei jeder Bildänderung (Text hat Vorrang),
//! aber neue Bildkacheln werden erst kodiert, wenn der Bild-Backlog des
//! Schedulers leer ist. Bei 0,5 fps Leitungskapazität wird also auch nur
//! mit ~0,5 fps kodiert. Ohne Client ruht die Pipeline komplett.

use std::sync::Arc;
use std::time::{Duration, Instant};

use lbw_common::{Rect, ServerMsg, TextItem};

use crate::analyze::{Analyzer, icons};
use crate::av1::{Av1Params, encode_rgb};
use crate::capture::FrameSource;
use crate::dirty::{align, dirty_rects};
use crate::image::Rgb;
use crate::layout::mask;
use crate::session::Shared;
use crate::text_diff::{Delta, Detected, TextState};

/// Max. Textelemente pro `Text`-Nachricht (Frame < 64 KB).
const ITEMS_PER_MSG: usize = 100;

/// Was die Pipeline von den Modellen braucht (Tests nutzen Attrappen).
pub trait Analyze: Send {
    fn text(&mut self, img: &Rgb) -> Result<Vec<Detected>, String>;
    fn gui_boxes(&mut self, img: &Rgb) -> Result<Vec<Rect>, String>;
    /// Kurzer Zeitbericht fürs Log.
    fn report(&self) -> String {
        String::new()
    }
}

impl Analyze for Analyzer {
    fn text(&mut self, img: &Rgb) -> Result<Vec<Detected>, String> {
        Analyzer::text(self, img)
    }
    fn gui_boxes(&mut self, img: &Rgb) -> Result<Vec<Rect>, String> {
        Analyzer::gui_boxes(self, img)
    }
    fn report(&self) -> String {
        let t = self.timings;
        format!("det {:.0} rec {:.0} gui {:.0} ms", t.det, t.rec, t.gui)
    }
}

/// Pipeline-Parameter.
#[derive(Clone, Debug)]
pub struct PipeCfg {
    /// Abfrageintervall für Bildänderungen.
    pub poll: Duration,
    /// Mindestabstand zweier OCR-Läufe.
    pub min_ocr: Duration,
    /// Quantizer Hintergrund / Icons.
    pub q_bg: usize,
    pub q_icon: usize,
    pub speed: u8,
    pub enc_threads: usize,
    /// Max. Hintergrund-Kacheln je Bild.
    pub max_rects: usize,
    pub verbose: bool,
}

impl Default for PipeCfg {
    fn default() -> Self {
        Self {
            poll: Duration::from_millis(100),
            min_ocr: Duration::from_millis(250),
            q_bg: 180,
            q_icon: 110,
            speed: 10,
            enc_threads: 4,
            max_rects: 4,
            verbose: false,
        }
    }
}

/// Zustand der Pipeline.
pub struct Pipeline {
    src: Box<dyn FrameSource>,
    an: Box<dyn Analyze>,
    sh: Arc<Shared>,
    cfg: PipeCfg,
    text: TextState,
    last: Option<Rgb>,
    masked: Option<Rgb>,
    /// Stand des Clients (letzte gesendete maskierte Bereiche).
    reference: Option<Rgb>,
    pending_image: bool,
    last_ocr: Option<Instant>,
    seq: u32,
}

impl Pipeline {
    #[must_use]
    pub fn new(
        src: Box<dyn FrameSource>,
        an: Box<dyn Analyze>,
        sh: Arc<Shared>,
        cfg: PipeCfg,
    ) -> Self {
        Self {
            src,
            an,
            sh,
            cfg,
            text: TextState::new(),
            last: None,
            masked: None,
            reference: None,
            pending_image: false,
            last_ocr: None,
            seq: 0,
        }
    }

    fn next_seq(&mut self) -> u32 {
        self.seq = self.seq.wrapping_add(1).max(1);
        self.seq
    }

    /// Text-Delta in Frame-taugliche Nachrichten teilen und einreihen.
    fn push_delta(&mut self, d: Delta) {
        let mut remove = d.remove;
        let mut chunks: Vec<Vec<TextItem>> =
            d.add.chunks(ITEMS_PER_MSG).map(<[_]>::to_vec).collect();
        if chunks.is_empty() {
            chunks.push(Vec::new());
        }
        for add in chunks {
            let seq = self.next_seq();
            let m = ServerMsg::Text {
                seq,
                remove: std::mem::take(&mut remove),
                add,
            };
            self.sh.outbox.with(|q| q.push_text(seq, m));
        }
    }

    /// Ein Durchlauf; liefert `true`, wenn etwas verarbeitet wurde.
    pub fn step(&mut self) -> Result<bool, String> {
        if !self.sh.connected() {
            return Ok(false);
        }
        let refresh = self.sh.take_refresh();
        if refresh {
            // Client ist leer: alles neu (Seq 0 = „Zustand gelöscht“).
            self.text = TextState::new();
            self.reference = None;
            self.last = None;
            self.sh.outbox.with(|q| q.push_text(0, ServerMsg::Clear));
        }
        let mut worked = false;
        let ocr_due = self
            .last_ocr
            .is_none_or(|t| t.elapsed() >= self.cfg.min_ocr);
        if ocr_due {
            let frame = self.src.grab()?;
            if self.last.as_ref() != Some(&frame) {
                self.analyze(frame)?;
                worked = true;
            }
        }
        if self.pending_image && self.sh.outbox.with(|q| q.image_backlog()) == 0 {
            self.encode()?;
            worked = true;
        }
        Ok(worked)
    }

    fn analyze(&mut self, frame: Rgb) -> Result<(), String> {
        self.last_ocr = Some(Instant::now());
        let delta = self.text.update(self.an.text(&frame)?);
        let (nadd, nrem) = (delta.add.len(), delta.remove.len());
        if !delta.is_empty() {
            self.push_delta(delta);
        }
        let mut m = frame.clone();
        mask(&mut m, self.text.items());
        self.masked = Some(m);
        self.last = Some(frame);
        self.pending_image = true;
        if self.cfg.verbose {
            eprintln!(
                "[pipe] Text +{nadd} -{nrem} ({} Zeilen) | {}",
                self.text.items().len(),
                self.an.report()
            );
        }
        Ok(())
    }

    fn encode(&mut self) -> Result<(), String> {
        self.pending_image = false;
        let (Some(masked), Some(frame)) = (self.masked.as_ref(), self.last.as_ref()) else {
            return Ok(());
        };
        let t0 = Instant::now();
        let rects = dirty_rects(masked, self.reference.as_ref(), self.cfg.max_rects);
        if rects.is_empty() {
            return Ok(());
        }
        let gui = self.an.gui_boxes(frame)?;
        let icon_rects: Vec<Rect> = icons(&gui, self.text.items(), masked)
            .into_iter()
            .map(|r| align(r, masked.w, masked.h))
            .filter(|r| rects.iter().any(|d| d.intersection(r) > 0))
            .collect();
        let p = |q| Av1Params {
            quantizer: q,
            speed: self.cfg.speed,
            threads: self.cfg.enc_threads,
        };
        let mut tiles = Vec::new();
        for r in &rects {
            tiles.push((
                *r,
                encode_rgb(&masked.crop(*r), r.w.into(), r.h.into(), p(self.cfg.q_bg))?,
            ));
        }
        for r in &icon_rects {
            tiles.push((
                *r,
                encode_rgb(&masked.crop(*r), r.w.into(), r.h.into(), p(self.cfg.q_icon))?,
            ));
        }
        let bytes: usize = tiles.iter().map(|t| t.1.len()).sum();
        let (nr, ni) = (rects.len(), icon_rects.len());
        self.reference = Some(masked.clone());
        for (r, data) in tiles {
            let seq = self.next_seq();
            self.sh.outbox.with(|q| q.push_tile(seq, r, data));
        }
        if self.cfg.verbose {
            eprintln!(
                "[pipe] Bild: {nr} Kacheln + {ni} Icons, {bytes} B, {:.0} ms",
                t0.elapsed().as_secs_f64() * 1e3
            );
        }
        Ok(())
    }

    /// Endlosschleife (eigener Thread).
    pub fn run(mut self) {
        loop {
            match self.step() {
                Ok(true) => {}
                Ok(false) => std::thread::sleep(self.cfg.poll),
                Err(e) => {
                    eprintln!("[pipe] Fehler: {e}");
                    std::thread::sleep(Duration::from_secs(1));
                }
            }
        }
    }
}
