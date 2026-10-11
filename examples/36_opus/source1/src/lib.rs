//! `opus-vis`: Opus-Dateien visualisieren.
//!
//! Nur Modul-Deklarationen und Re-Exporte; Verhalten steckt in den
//! nummerierten Modulen in Datenfluss-Reihenfolge (siehe `plan.md`).

#![forbid(unsafe_code)]

#[path = "03_analyze.rs"]
pub mod analyze;
#[path = "08_cli.rs"]
pub mod cli;
#[path = "04_decode.rs"]
pub mod decode;
#[path = "07_png.rs"]
pub mod png;
#[path = "06_render.rs"]
pub mod render;
#[path = "09_show.rs"]
#[cfg(feature = "show")]
pub mod show;
#[path = "05_spectrum.rs"]
pub mod spectrum;
#[path = "02_toc.rs"]
pub mod toc;
#[path = "01_types.rs"]
pub mod types;
