//! Roundtrip-Integrationstest: rendern → detektieren → erkennen.
//!
//! Braucht echte Assets (Font + Modelle); kein stilles Überspringen.
//! T3: nur Deutsch („Hallo Welt“); T5 erweitert auf alle 15 Sprachen.

use std::path::PathBuf;
use unicode_ocr::{lang, models, render};

fn models_dir() -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("models");
    assert!(
        dir.join(models::DET_MODEL).join("inference.onnx").exists(),
        "models missing: run ./scripts/fetch_models.sh first"
    );
    dir
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
