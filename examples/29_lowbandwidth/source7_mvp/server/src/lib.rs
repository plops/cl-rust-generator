//! `lbw-server` — Bildschirm-Capture, OCR-Text, AV1-Kacheln, direktes TCP.
//! Nur Modul-Deklarationen.

#[path = "01_config.rs"]
pub mod config;

#[path = "02_capture.rs"]
pub mod capture;

#[path = "03_ocr.rs"]
pub mod ocr;

#[path = "04_tiles.rs"]
pub mod tiles;

#[path = "05_av1.rs"]
pub mod av1;
