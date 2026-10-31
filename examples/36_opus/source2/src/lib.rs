//! `opus-vis` (source2): Opus-Dateien bis zum letzten Bit verstehen.
//!
//! Nur Modul-Deklarationen; Verhalten steckt in den nummerierten Modulen,
//! deren Nummern dem Datenfluss folgen: Typen → TOC/Split → Ogg →
//! Range-Coder → CELT → SILK → Opus-Walk → Decode → Modell → Fakten →
//! Bild → CLI → Fenster (Details: `../plan/20261031_07_review/plan.md`).

#![forbid(unsafe_code)]

#[path = "05b_celt_alloc.rs"]
pub mod alloc;
#[path = "03_analyze.rs"]
pub mod analyze;
#[path = "05c_celt_bands.rs"]
pub mod bands;
#[path = "11_canvas.rs"]
pub mod canvas;
#[path = "05d_celt_frame.rs"]
pub mod celt;
#[path = "05a_celt_tab.rs"]
pub mod celt_tab;
#[path = "12_cli.rs"]
pub mod cli;
#[path = "08_decode.rs"]
pub mod decode;
#[path = "10_fakten.rs"]
pub mod fakten;
#[path = "10c_json.rs"]
pub mod json;
#[path = "11b_lanes.rs"]
pub mod lanes;
#[path = "09b_model.rs"]
pub mod model;
#[path = "11d_png.rs"]
pub mod png;
#[path = "04_range.rs"]
pub mod range;
#[path = "11c_render.rs"]
pub mod render;
#[path = "10b_report.rs"]
pub mod report;
#[path = "13_show.rs"]
#[cfg(feature = "show")]
pub mod show;
#[path = "06d_silk_frame.rs"]
pub mod silk;
#[path = "06e_silk_dec.rs"]
pub mod silk_dec;
#[path = "06b_silk_dq.rs"]
pub mod silk_dq;
#[path = "06c_silk_pulse.rs"]
pub(crate) mod silk_pulse;
#[path = "06a_silk_tab.rs"]
pub mod silk_tab;
#[path = "09_spectrum.rs"]
pub mod spectrum;
#[path = "02b_split.rs"]
pub mod split;
#[path = "02_toc.rs"]
pub mod toc;
#[path = "01_types.rs"]
pub mod types;
#[path = "07_walk.rs"]
pub mod walk;
