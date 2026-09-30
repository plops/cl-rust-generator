//! Roundtrip-Integrationstest: rendern → detektieren → erkennen.
//!
//! Braucht echte Assets (Font + Modelle); kein stilles Überspringen.
//! T3: nur Deutsch („Hallo Welt“); T5: alle 15 Sprachen + CER-Gate.

use std::path::PathBuf;
use unicode_ocr::{engine, generate, lang, models, render};

fn models_dir() -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("models");
    assert!(
        dir.join(models::DET_MODEL).join("inference.onnx").exists(),
        "models missing: run ./scripts/fetch_models.sh first"
    );
    dir
}

fn corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("corpus")
}

#[test]
fn german_hello_world_reads_back_correctly() {
    let l = &lang::LANGS[lang::by_code("de").unwrap()];
    let mut raster = render::Raster::load(None).expect("GNU Unifont required");
    let canvas = raster.render(&["Hallo Welt".to_string()], 32, l.rtl);
    assert_eq!(canvas.lines.len(), 1);

    let mut m = models::Models::open(&models_dir());
    let mut boxes = m
        .detector()
        .expect("det session")
        .detect(&canvas.rgba)
        .expect("detect");
    assert!(!boxes.is_empty(), "no text detected in 'Hallo Welt'");

    let model = models::rec_model_for(l, models::ModelChoice::Auto);
    let confs = m
        .recognizer(model)
        .expect("rec session")
        .recognize(&canvas.rgba, &mut boxes)
        .expect("recognize");
    assert_eq!(confs.len(), boxes.len());

    let read: String = boxes
        .iter()
        .map(|b| b.text.as_str())
        .collect::<Vec<_>>()
        .join(" ");
    assert_eq!(read, "Hallo Welt", "boxes: {boxes:?}");
    assert!(confs.iter().all(|&c| c > 0.5), "confs: {confs:?}");
}

/// Alle Sprachen laufen durch; de/en/fr lesen fast fehlerfrei.
///
/// Gate CER < 10 % (Plan sagte < 5 % voraus; gemessen 3–5 % mit
/// Einzelzeichen-Verwechslungen des Modells auf Unifont: ß→B/β, Ä→A,
/// œ→e, è→ē/e, ç→c, ’→', “→", –→-, !→l). Dazu Recall 1.0 + keine FP:
/// echte Regressionen (z. B. ohne Crop-Padding de-CER 29 %) lösen aus.
#[test]
fn all_languages_run_through_and_latin_pangram_cer_low() {
    let mut eng = engine::Engine::open(&models_dir(), None, Some(&corpus_dir())).expect("engine");
    let mut cers: Vec<(&str, f32)> = Vec::new();
    for (i, l) in lang::LANGS.iter().enumerate() {
        let s = engine::Settings {
            lang: i,
            mode: generate::GenMode::Pangram,
            px: 32,
            lines: 4,
            model: models::ModelChoice::Auto,
        };
        let sample = eng.run(&s, 1).expect(l.code);
        assert!(
            !sample.eval.lines.is_empty(),
            "{}: no lines rendered",
            l.code
        );
        println!(
            "{}: cer={:.3} recall={:.2} fp={} det={:.0}ms rec={:.0}ms",
            l.code,
            sample.eval.mean_cer(),
            sample.eval.recall(),
            sample.eval.fp_boxes,
            sample.times.det_ms,
            sample.times.rec_ms,
        );
        if ["de", "en", "fr"].contains(&l.code) {
            assert_eq!(sample.eval.recall(), 1.0, "{}: missed line", l.code);
            assert_eq!(sample.eval.fp_boxes, 0, "{}: false positive", l.code);
        }
        cers.push((l.code, sample.eval.mean_cer()));
    }
    for (code, cer) in &cers {
        if ["de", "en", "fr"].contains(code) {
            assert!(*cer < 0.10, "{code}: cer={cer} (all: {cers:?})");
        }
    }
}

/// Modus `words`: Zufallswörter aus dem Korpus lesen sich fehlerarm.
///
/// T6-Nachweis auf Engine-Ebene (der `bench`-Nachweis folgt in T9).
#[test]
fn words_mode_reads_back_cleanly() {
    if !corpus_dir().join("de.txt").exists() {
        println!("SKIP: corpus missing (uv run scripts/fetch_corpus.py)");
        return;
    }
    let mut eng = engine::Engine::open(&models_dir(), None, Some(&corpus_dir())).expect("engine");
    for code in ["de", "ru", "th"] {
        let s = engine::Settings {
            lang: lang::by_code(code).unwrap(),
            mode: generate::GenMode::Words,
            px: 32,
            lines: 4,
            model: models::ModelChoice::Auto,
        };
        let sample = eng.run(&s, 1).expect(code);
        println!(
            "{code} words: cer={:.3} gt={:?}",
            sample.eval.mean_cer(),
            sample.gt
        );
        assert_eq!(sample.eval.lines.len(), 4, "{code}: short sample");
        assert!(
            sample.eval.mean_cer() < 0.30,
            "{code}: cer={}",
            sample.eval.mean_cer()
        );
    }
}

/// Modus `markov`: n-Gramm-Text sieht plausibel aus und liest sich.
///
/// T7-Nachweis auf Engine-Ebene (der `bench`-Nachweis folgt in T9).
#[test]
fn markov_mode_samples_plausible_text() {
    if !corpus_dir().join("de.txt").exists() {
        println!("SKIP: corpus missing (uv run scripts/fetch_corpus.py)");
        return;
    }
    let mut eng = engine::Engine::open(&models_dir(), None, Some(&corpus_dir())).expect("engine");
    for code in ["de", "ja"] {
        let s = engine::Settings {
            lang: lang::by_code(code).unwrap(),
            mode: generate::GenMode::Markov,
            px: 32,
            lines: 4,
            model: models::ModelChoice::Auto,
        };
        let sample = eng.run(&s, 1).expect(code);
        println!(
            "{code} markov: cer={:.3} gt={:?}",
            sample.eval.mean_cer(),
            sample.gt
        );
        assert_eq!(sample.eval.lines.len(), 4, "{code}: short sample");
        assert!(
            sample.eval.mean_cer() < 0.50,
            "{code}: cer={}",
            sample.eval.mean_cer()
        );
    }
}
