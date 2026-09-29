//! `11_analyze` — bündelt die Modelle: OCR (Detektion + Erkennung +
//! Farben) und optional GUI-Detektion → Textkandidaten und Icon-Bereiche.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::time::Instant;

use lbw_common::{Rect, TextItem};

use crate::gui_detect::GuiDetector;
use crate::image::Rgb;
use crate::layout::{MIN_TEXT_CONF, icon_regions, sample_colors};
use crate::ocr_detect::Detector;
use crate::ocr_recognize::Recognizer;
use crate::text_diff::Detected;

/// Max. erkannte Zeilen je Frame (Rechenzeit-Deckel).
pub const MAX_LINES: usize = 160;

/// Modellpfade.
#[derive(Clone, Debug)]
pub struct ModelPaths {
    pub det: String,
    pub rec: String,
    pub dict: String,
    /// `None` = ohne GUI-Detektor (keine Icon-Kacheln).
    pub gui: Option<String>,
    pub threads: usize,
}

/// Laufzeiten der letzten Analyse (ms).
#[derive(Clone, Copy, Debug, Default)]
pub struct Timings {
    pub det: f64,
    pub rec: f64,
    pub gui: f64,
}

/// Geladene Modelle.
pub struct Analyzer {
    det: Detector,
    rec: Recognizer,
    gui: Option<GuiDetector>,
    /// Erkennungs-Cache des letzten Frames: (Box, Pixel-Hash) → (Text, Konfidenz).
    cache: HashMap<(Rect, u64), (String, f32)>,
    pub timings: Timings,
}

/// Hash der Pixel einer Box (std-`DefaultHasher`, deterministisch je Prozess).
#[must_use]
pub fn crop_hash(img: &Rgb, r: Rect) -> u64 {
    let mut h = std::hash::DefaultHasher::new();
    img.crop(img.clamp(r)).hash(&mut h);
    h.finish()
}

fn ms(t: Instant) -> f64 {
    t.elapsed().as_secs_f64() * 1e3
}

impl Analyzer {
    pub fn load(p: &ModelPaths) -> Result<Self, String> {
        Ok(Self {
            det: Detector::new(&p.det, p.threads)?,
            rec: Recognizer::new(&p.rec, &p.dict, p.threads)?,
            gui: p
                .gui
                .as_deref()
                .map(|g| GuiDetector::new(g, p.threads))
                .transpose()?,
            cache: HashMap::new(),
            timings: Timings::default(),
        })
    }

    /// Größe, die der GUI-Detektor verlangt (falls geladen).
    #[must_use]
    pub fn gui_size(&self) -> Option<(usize, usize)> {
        self.gui.as_ref().map(|g| (g.in_w, g.in_h))
    }

    /// Textzeilen mit Farben; unsichere/leere Erkennungen fallen weg
    /// (die bleiben dann Bildinhalt).
    pub fn text(&mut self, img: &Rgb) -> Result<Vec<Detected>, String> {
        let t = Instant::now();
        let boxes = self.det.detect(img)?;
        self.timings.det = ms(t);
        let t = Instant::now();
        let mut out = Vec::new();
        let mut cache = HashMap::new();
        for r in boxes.into_iter().take(MAX_LINES) {
            // Unveränderte Zeile (gleiche Box, gleiche Pixel) → alter Text.
            let key = (r, crop_hash(img, r));
            let (text, conf) = match self.cache.get(&key) {
                Some(v) => v.clone(),
                None => {
                    let (t, c) = self.rec.recognize(img, r)?;
                    (t.trim().to_owned(), c)
                }
            };
            cache.insert(key, (text.clone(), conf));
            if text.is_empty() || conf < MIN_TEXT_CONF {
                continue;
            }
            let (fg, bg) = sample_colors(img, r);
            out.push(Detected {
                rect: r,
                fg,
                bg,
                text,
            });
        }
        self.cache = cache;
        self.timings.rec = ms(t);
        Ok(out)
    }

    /// GUI-Boxen im Original-Frame (Modell sieht die echte Oberfläche).
    pub fn gui_boxes(&mut self, img: &Rgb) -> Result<Vec<Rect>, String> {
        let Some(g) = self.gui.as_mut() else {
            return Ok(Vec::new());
        };
        let t = Instant::now();
        let gui = g
            .detect(img)?
            .iter()
            .map(|d| d.rect(img.w, img.h))
            .collect();
        self.timings.gui = ms(t);
        Ok(gui)
    }
}

/// Icon-/Bildbereiche: GUI-Boxen, die nach dem Maskieren Inhalt tragen.
#[must_use]
pub fn icons(gui: &[Rect], texts: &[TextItem], masked: &Rgb) -> Vec<Rect> {
    let rects: Vec<Rect> = texts.iter().map(|t| t.rect).collect();
    icon_regions(gui, &rects, masked)
}
