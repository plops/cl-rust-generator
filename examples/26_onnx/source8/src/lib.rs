//! gui_detect — PoC für Salesforce/GPA-GUI-Detector (YOLO11m, ONNX).
//! Nur Modul-Deklarationen; Reihenfolge = Datenfluss.

#[path = "01_image.rs"]
pub mod image;

#[path = "02_capture.rs"]
pub mod capture;

#[path = "03_letterbox.rs"]
pub mod letterbox;

#[path = "04_session.rs"]
pub mod session;

#[path = "05_decode.rs"]
pub mod decode;

#[path = "06_detector.rs"]
pub mod detector;

#[path = "07_cli.rs"]
pub mod cli;

#[path = "08_bench.rs"]
pub mod bench;

#[path = "09_window.rs"]
pub mod window;

#[path = "10_live.rs"]
pub mod live;

#[path = "11_run.rs"]
pub mod run;
