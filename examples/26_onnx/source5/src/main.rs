//! main.rs — nur Modul-Deklaration + Verdrahtung (kein Verhalten).
//!
//! Ablauf pro Frame: Tasten → ROI-Capture → Change-Detect → Inferenz
//! (1:1 direkt, sonst Nearest-Resize) → Dedup-Druck → Anzeige
//! (`draw_texture` bei 1:1, sonst `draw_texture_ex` mit Nearest).

#[path = "01_view.rs"]
mod view;

#[path = "02_capture.rs"]
mod capture;

#[path = "03_detect.rs"]
mod detect;

#[path = "04_recognize.rs"]
mod recognize;

#[path = "05_overlay.rs"]
mod overlay;

use macroquad::prelude::*;
use std::time::Instant;
use x11rb::connection::Connection;

use capture::{ConvertPath, screen_size, view_changed};
use capture::{bgra_to_rgba, capture_roi, convert_path, prepare_native, resize_nearest_planar};
use detect::{Detector, TextBox};
use overlay::{draw_boxes, draw_hud, load_font_bytes};
use recognize::Recognizer;
use view::{MODEL_SIZE, View};

/// Anzeigefenster (fix; ROI-Bild wird per Nearest eingepasst).
const DISPLAY: u32 = MODEL_SIZE;

fn window_conf() -> Conf {
    Conf {
        window_title: "PP-OCRv6 Live OCR".into(),
        window_width: DISPLAY as i32,
        window_height: DISPLAY as i32,
        ..Default::default()
    }
}

// ----------------------------------------------------------------------------
// Main Loop
// ----------------------------------------------------------------------------

#[macroquad::main(window_conf)]
async fn main() {
    let font_bytes = load_font_bytes();
    let font = load_ttf_font_from_bytes(&font_bytes).expect("Failed to parse font");
    let (conn, screen_idx) = x11rb::connect(None).expect("Failed to connect to X11");
    let root = conn.setup().roots[screen_idx].root;
    let screen = screen_size(&conn, screen_idx);

    let mut detector = Detector::new();
    let mut recognizer = Recognizer::new();
    let mut view = View::default();
    let mut prev_view = view;

    let mut img = Image::gen_image_color(view.size as u16, view.size as u16, BLACK);
    let mut tex = Texture2D::from_image(&img);
    tex.set_filter(FilterMode::Nearest);

    // Frame cache & deduplication buffers
    let mut prev_screen_bytes = Vec::new();
    let mut prev_printed_lines: Vec<String> = Vec::new();
    let mut cached_boxes: Vec<TextBox> = Vec::new();

    let mut det_ms = 0.0;
    let mut rec_ms = 0.0;

    while !is_key_down(KeyCode::Escape) {
        // 0. Tasten: Pan mit gehaltenen Pfeiltasten, Zoom stufenweise.
        if is_key_down(KeyCode::Left) {
            view.pan(-1, 0, screen);
        }
        if is_key_down(KeyCode::Right) {
            view.pan(1, 0, screen);
        }
        if is_key_down(KeyCode::Up) {
            view.pan(0, -1, screen);
        }
        if is_key_down(KeyCode::Down) {
            view.pan(0, 1, screen);
        }
        if is_key_pressed(KeyCode::Key1) {
            view.zoom_in(screen);
        }
        if is_key_pressed(KeyCode::Key2) {
            view.zoom_out(screen);
        }

        // ROI-Wechsel invalidiert alle Caches der alten Region.
        if view_changed(&prev_view, &view) {
            prev_screen_bytes.clear();
            cached_boxes.clear();
            prev_printed_lines.clear();
            if view.size != prev_view.size {
                img = Image::gen_image_color(view.size as u16, view.size as u16, BLACK);
                tex = Texture2D::from_image(&img);
                tex.set_filter(FilterMode::Nearest);
            }
            prev_view = view;
        }

        let bgra = capture_roi(&conn, root, &view);

        // 1. Change Detector
        // High-throughput raw slice equality (SIMD-accelerated memcmp)
        let frame_changed = prev_screen_bytes != bgra;
        let is_idle = !frame_changed;

        if frame_changed {
            // X11 BGRA -> Modell-Input + RGBA-Textur. Bei 1:1 läuft nur der
            // direkte Copy-Loop (kein Resize, keine Interpolation).
            match convert_path(&view) {
                ConvertPath::Native => {
                    prepare_native(&bgra, &mut detector.det_input, &mut img.bytes);
                }
                ConvertPath::Scaled => {
                    resize_nearest_planar(
                        &bgra,
                        view.size as usize,
                        &mut detector.det_input,
                        MODEL_SIZE as usize,
                    );
                    bgra_to_rgba(&bgra, &mut img.bytes);
                }
            }
            tex.update(&img);

            // Run detection
            let t0 = Instant::now();
            let mut boxes = detector.detect();
            det_ms = t0.elapsed().as_secs_f64() * 1000.0;

            // Run recognition
            let t1 = Instant::now();
            recognizer.recognize(&img.bytes, &mut boxes, view.size as usize);
            rec_ms = t1.elapsed().as_secs_f64() * 1000.0;

            // 2. Print output without duplicates
            let current_lines: Vec<String> = boxes
                .iter()
                .map(|b| b.text.trim())
                .filter(|t| !t.is_empty())
                .map(|t| t.to_string())
                .collect();

            if !current_lines.is_empty() && current_lines != prev_printed_lines {
                println!(
                    "--- [{}] Detected ({} lines) ---",
                    get_time() as u64,
                    current_lines.len()
                );
                for line in &current_lines {
                    println!("{line}");
                }
                prev_printed_lines = current_lines;
            }

            cached_boxes = boxes;
            prev_screen_bytes = bgra; // O(1) buffer move
        }

        // 3. Render (1:1 ohne Skalierung, sonst Nearest auf Fenstergröße).
        clear_background(BLACK);
        if view.is_native() {
            draw_texture(&tex, 0.0, 0.0, WHITE);
        } else {
            draw_texture_ex(
                &tex,
                0.0,
                0.0,
                WHITE,
                DrawTextureParams {
                    dest_size: Some(vec2(DISPLAY as f32, DISPLAY as f32)),
                    ..Default::default()
                },
            );
        }

        draw_boxes(
            &cached_boxes,
            &font,
            View::overlay_scale(DISPLAY),
            DISPLAY as f32,
        );

        // HUD overlay
        draw_hud(
            &font,
            DISPLAY as f32,
            is_idle,
            view.x,
            view.y,
            view.size,
            cached_boxes.len(),
            det_ms,
            rec_ms,
        );

        next_frame().await;
    }
}
