//! `12_engine` — ein Roundtrip-Sample: erzeugen → rendern → OCR → messen.
//!
//! `Settings` lebt hier (zentral für `bench` und UI); `15_ui_state`
//! bedient es per Tasten. Zeiten werden je Stufe mit `Instant` gemessen.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::corpus::{Charset, Corpus};
use crate::detect::TextBox;
use crate::generate::{GenInput, GenMode};
use crate::lang::LANGS;
use crate::markov::Markov;
use crate::metrics::{SampleEval, evaluate};
use crate::models::{ModelChoice, Models, rec_model_for};
use crate::render::{CANVAS, Raster};
use crate::rng::Rng;
use crate::stats::Times;

/// Lauf-Einstellung eines Samples.
#[derive(Clone, Debug)]
pub struct Settings {
    /// Index in `LANGS`.
    pub lang: usize,
    /// Textgenerator.
    pub mode: GenMode,
    /// Schriftgröße in Pixeln (16/24/32/48).
    pub px: u32,
    /// Zeilen pro Sample.
    pub lines: usize,
    /// Modellwahl.
    pub model: ModelChoice,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            lang: 0,
            mode: GenMode::Pangram,
            px: 32,
            lines: 4,
            model: ModelChoice::Auto,
        }
    }
}

/// Ein vermessenes Sample.
#[derive(Debug)]
pub struct Sample {
    /// Sprachcode.
    pub lang: String,
    /// Generator.
    pub mode: GenMode,
    /// Schriftgröße.
    pub px: u32,
    /// Erkennungsmodell (Ordnername).
    pub model: String,
    /// Ground-Truth-Zeilen (visuelle Reihenfolge).
    pub gt: Vec<String>,
    /// Detektions-Boxen mit erkanntem Text.
    pub boxes: Vec<TextBox>,
    /// Auswertung.
    pub eval: SampleEval,
    /// Stufen-Zeiten.
    pub times: Times,
    /// Canvas-RGBA (`CANVAS²×4`, für die UI-Textur).
    pub rgba: Vec<u8>,
}

/// Roundtrip-Engine: Schrift + Modelle, wiederverwendbar.
pub struct Engine {
    raster: Raster,
    models: Models,
    charsets: HashMap<(usize, String), Charset>,
    corpus_dir: Option<PathBuf>,
    corpora: HashMap<usize, Corpus>,
    markovs: HashMap<usize, Markov>,
}

impl Engine {
    /// Öffnet Schrift (`None` = Suchliste), Modelle und Korpus (`None` =
    /// kein Korpus; `Words`/`Markov` fallen auf Pangramme zurück).
    pub fn open(
        models_dir: &Path,
        font: Option<&Path>,
        corpus_dir: Option<&Path>,
    ) -> Result<Self, String> {
        Ok(Self {
            raster: Raster::load(font)?,
            models: Models::open(models_dir),
            charsets: HashMap::new(),
            corpus_dir: corpus_dir.map(Path::to_path_buf),
            corpora: HashMap::new(),
            markovs: HashMap::new(),
        })
    }

    /// Führt ein Sample mit deterministischem `seed` aus.
    pub fn run(&mut self, settings: &Settings, seed: u64) -> Result<Sample, String> {
        let lang = &LANGS[settings.lang];
        let model = rec_model_for(lang, settings.model).to_string();
        let dict = self.models.dict(&model)?.to_vec();
        let charset = self
            .charsets
            .entry((settings.lang, model.clone()))
            .or_insert_with(|| Charset::build(lang, &dict, &self.raster));
        let empty_corpus = Corpus::empty();
        let empty_markov = Markov::train(&[]);
        let dir = self.corpus_dir.clone();
        let corpus = if matches!(settings.mode, GenMode::Words | GenMode::Markov) {
            self.corpora
                .entry(settings.lang)
                .or_insert_with(|| Corpus::load(dir.as_deref(), lang, charset))
        } else {
            &empty_corpus
        };
        let markov = if settings.mode == GenMode::Markov && !corpus.is_empty() {
            self.markovs
                .entry(settings.lang)
                .or_insert_with(|| Markov::train(corpus.tokens()))
        } else {
            &empty_markov
        };
        let input = GenInput {
            charset,
            corpus,
            markov,
        };
        let mut rng = Rng::new(seed);
        let raster = &mut self.raster;
        let mut fits = |s: &str| raster.width(s, settings.px) <= Raster::max_width();
        let lines = crate::generate::generate(
            settings.mode,
            lang,
            &mut rng,
            settings.lines,
            &input,
            &mut fits,
        );

        let t = Instant::now();
        let canvas = self.raster.render(&lines, settings.px, lang.rtl);
        let render_ms = t.elapsed().as_secs_f64() * 1000.0;

        let t = Instant::now();
        let mut boxes = self.models.detector()?.detect(&canvas.rgba)?;
        let det_ms = t.elapsed().as_secs_f64() * 1000.0;

        let t = Instant::now();
        let confs = self
            .models
            .recognizer(&model)?
            .recognize(&canvas.rgba, &mut boxes)?;
        let rec_ms = t.elapsed().as_secs_f64() * 1000.0;

        debug_assert_eq!(canvas.rgba.len(), CANVAS * CANVAS * 4);
        let eval = evaluate(&canvas.lines, &boxes, &confs);
        let gt: Vec<String> = canvas.lines.iter().map(|l| l.text.clone()).collect();
        Ok(Sample {
            lang: lang.code.to_string(),
            mode: settings.mode,
            px: settings.px,
            rgba: canvas.rgba,
            model,
            gt,
            boxes,
            eval,
            times: Times {
                render_ms,
                det_ms,
                rec_ms,
            },
        })
    }
}
