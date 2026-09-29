//! `lbw-server` — Bildschirm-Capture, OCR/GUI-Detektion, AV1-Kacheln und
//! priorisierter Versand über schmalbandiges TCP. Nur Modul-Deklarationen.

#[path = "01_config.rs"]
pub mod config;

#[path = "02_capture.rs"]
pub mod capture;

#[path = "03_image.rs"]
pub mod image;

#[path = "04_ocr_detect.rs"]
pub mod ocr_detect;

#[path = "05_ocr_recognize.rs"]
pub mod ocr_recognize;

#[path = "06_gui_detect.rs"]
pub mod gui_detect;

#[path = "07_layout.rs"]
pub mod layout;

#[path = "08_dirty.rs"]
pub mod dirty;

#[path = "09_av1.rs"]
pub mod av1;

#[path = "10_text_diff.rs"]
pub mod text_diff;

#[path = "11_analyze.rs"]
pub mod analyze;

#[path = "12_scheduler.rs"]
pub mod scheduler;

#[path = "13_input.rs"]
pub mod input;

#[path = "14_session.rs"]
pub mod session;

#[path = "15_pipeline.rs"]
pub mod pipeline;
