//! SAR Time-Domain Backprojection (TDBP) mit `cuda-oxide`.
//!
//! Module sind in Datenfluss-Reihenfolge nummeriert; diese Datei enthält nur
//! die Verdrahtung.

#[path = "01_types.rs"]
pub mod types;

#[path = "02_phantom.rs"]
pub mod phantom;

#[path = "03_simulator.rs"]
pub mod simulator;

#[path = "04_kernel.rs"]
pub mod kernel;

#[path = "05_pipeline.rs"]
pub mod pipeline;

#[path = "06_gui.rs"]
pub mod gui;

pub use phantom::{
    PhantomKind, PointTarget, build as build_phantom, grid_5x5, rust_text, single_point,
};
pub use types::{Complex32, RadarParams, SceneGeometry, Vec3};
