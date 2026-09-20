//! x11_rb_mq_viewer — X11-Region via x11rb capturen, YOLOv8-Inferenz,
//! Boxen via macroquad anzeigen (source1: schlanke Variante ohne
//! xcap/pixels/winit/wgpu).
//! `lib.rs` enthaelt nur Modul-Deklarationen + Re-Exporte.

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
    bgrx_to_rgb, clamp_region, rgba_image_to_rgb, Capturer, ClampedRegion, ScreenGeom,
};
pub use cli::{Args, DEFAULT_FPS, MAX_FPS, MIN_FPS, YOLOV8M_URL};
pub use draw::{color_for_label, draw_box, draw_detections, LABEL_BAR, THICKNESS};
pub use infer::{
    decode, extract_rows, infer_image, intersection, iou, load_session, preprocess, union,
    BoundingBox, Detection, Letterbox, CLASS_LABELS, FALLBACK_INPUT, LETTERBOX_PAD, MODEL_SIZE,
    OUTPUT_NAME,
};
pub use view::{blit_nearest, frame_period, run_window_mq, save_frame_png, zoomed_size};
