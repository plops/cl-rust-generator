//! `lbw-server` — Bildschirm-Capture, OCR-Text, AV1-Kacheln, direktes TCP.
//! Nur Modul-Deklarationen.

#[path = "01_config.rs"]
pub mod config;

#[path = "02_capture.rs"]
pub mod capture;

#[path = "03_detect.rs"]
pub mod detect;

#[path = "04_recognize.rs"]
pub mod recognize;

#[path = "05_ocr.rs"]
pub mod ocr;

#[path = "06_tiles.rs"]
pub mod tiles;

#[path = "07_av1.rs"]
pub mod av1;

#[path = "08_input.rs"]
pub mod input;

#[path = "09_textids.rs"]
pub mod textids;

#[path = "10_session.rs"]
pub mod session;
