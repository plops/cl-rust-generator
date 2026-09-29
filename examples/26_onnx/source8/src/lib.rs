//! gui_detect — PoC für Salesforce/GPA-GUI-Detector (YOLO11m, ONNX).
//! Nur Modul-Deklarationen; Reihenfolge = Datenfluss.

#[path = "01_image.rs"]
pub mod image;

#[path = "02_capture.rs"]
pub mod capture;

#[path = "03_letterbox.rs"]
pub mod letterbox;

#[path = "07_cli.rs"]
pub mod cli;
