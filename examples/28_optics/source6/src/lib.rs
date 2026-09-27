//! optics - headless differentiable 3D optical ray tracer.
//!
//! `lib.rs` only declares modules plus re-exports (file rule, see prompt).
//! Data flow: `dual` -> `linalg` -> `ray` -> `system` -> `trace` ->
//! `optimize` -> `export` / `tui`.

#[path = "01_dual.rs"]
pub mod dual;
#[path = "07_export.rs"]
pub mod export;
#[path = "02_vec.rs"]
pub mod linalg;
#[path = "06_optimize.rs"]
pub mod optimize;
#[path = "03_ray.rs"]
pub mod ray;
#[path = "04_system.rs"]
pub mod system;
#[path = "05_trace.rs"]
pub mod trace;
#[path = "08_tui.rs"]
pub mod tui;

pub use dual::Dual;
pub use export::{
    ExportSurface, ExportVar, SCHEMA_VERSION, SystemJson, profile_points, sag, to_document, to_json,
};
pub use linalg::{Point3, Vec3};
pub use optimize::{
    Var, VarKey, descend, get_var, gradient, loss_for, set_var, spot_loss, to_toml, variables,
};
pub use ray::{Hit, Ray, intersect_plane_z, intersect_surface, point_at, refract};
pub use system::{
    D_LINE, OpticalSetup, OptimizeCfg, SourceCfg, Surface, bundle, image_z, index_at, load_toml,
    pupil_radius, vertices,
};
pub use trace::{
    DualSurface, IntersectionResult, MARGINAL_H, RayEnd, back_focal_z, efl, layout_for, trace_dual,
    trace_ray, trace_system,
};
pub use tui::{AppState, render, run_tui};
