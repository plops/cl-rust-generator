//! unicode_ocr — Schriftproben in vielen Sprachen mit GNU Unifont rendern und
//! mit PaddleOCR (ONNX) wieder einlesen; Fehler + Geschwindigkeit messen.
//!
//! Nur Modul-Deklarationen; Nummern folgen dem Datenfluss.

#[path = "01_rng.rs"]
pub mod rng;

#[path = "02_lang.rs"]
pub mod lang;

#[path = "06_render.rs"]
pub mod render;

#[path = "07_detect.rs"]
pub mod detect;

#[path = "08_recognize.rs"]
pub mod recognize;

#[path = "09_models.rs"]
pub mod models;

#[path = "10_metrics.rs"]
pub mod metrics;

#[path = "11_stats.rs"]
pub mod stats;
