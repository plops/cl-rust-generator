//! x11_yolo_viewer — X11-Region capturen, YOLOv8-Inferenz, Boxen anzeigen.
//! `lib.rs` enthaelt nur Modul-Deklarationen + Re-Exporte (Dateiregel s. Prompt).

#[path = "02_capture.rs"]
mod capture;
#[path = "01_cli.rs"]
mod cli;
#[path = "04_draw.rs"]
mod draw;
#[path = "03_infer.rs"]
mod infer;
#[path = "05_view.rs"]
mod view;

pub use capture::{
    capture_clamped, capture_region_clamped, clamp_region, monitor_geom, rgba_image_to_rgb,
    rgba_to_rgb, select_monitor, ClampedRegion, MonitorGeom,
};
pub use cli::{Args, DEFAULT_FPS, MAX_FPS, MIN_FPS, YOLOV8M_URL};
