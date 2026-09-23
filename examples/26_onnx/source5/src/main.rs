//! main.rs — nur Modul-Deklaration + Verdrahtung (kein Verhalten).
//!
//! S3: Detektion/Erkennung laufen über `Detector`/`Recognizer`
//! (bit-identisch zu vorher); Tasten-Verdrahtung folgt in S4.

/// S1: ROI-State; Verdrahtung in den Loop erfolgt in S4.
#[allow(dead_code)]
#[path = "01_view.rs"]
mod view;

/// S2: Capture-Helfer; `prepare_native` ist hier schon verdrahtet,
/// der Rest (ROI-Capture, Resize-Pfad) folgt in S4.
#[allow(dead_code)]
#[path = "02_capture.rs"]
mod capture;

#[path = "03_detect.rs"]
mod detect;

#[path = "04_recognize.rs"]
mod recognize;

use macroquad::prelude::*;
use std::time::Instant;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

use capture::prepare_native;
use detect::{Detector, TextBox};
use recognize::Recognizer;

const SIZE: usize = 640;

/// Sucht die Unifont-Datei an den bekannten System-Pfaden (APT-Paket
/// `fonts-unifont` installiert unter `opentype/`, nicht `unifont/`).
fn load_font_bytes() -> Vec<u8> {
    const CANDIDATES: &[&str] = &[
        "/usr/share/fonts/opentype/unifont/unifont.otf",
        "/usr/share/fonts/unifont/unifont.otf",
        "/usr/share/fonts/truetype/unifont/unifont.ttf",
    ];
    for path in CANDIDATES {
        if let Ok(bytes) = std::fs::read(path) {
            return bytes;
        }
    }
    panic!(
        "GNU Unifont not found (tried: {}); install it via `apt-get install fonts-unifont`",
        CANDIDATES.join(", ")
    );
}

fn window_conf() -> Conf {
    Conf {
        window_title: "PP-OCRv6 Live OCR".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
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
    let (conn, screen) = x11rb::connect(None).expect("Failed to connect to X11");
    let root = conn.setup().roots[screen].root;

    let mut detector = Detector::new();
    let mut recognizer = Recognizer::new();
    let mut img = Image::gen_image_color(SIZE as u16, SIZE as u16, BLACK);
    let tex = Texture2D::from_image(&img);

    // Frame cache & deduplication buffers
    let mut prev_screen_bytes = Vec::new();
    let mut prev_printed_lines: Vec<String> = Vec::new();
    let mut cached_boxes: Vec<TextBox> = Vec::new();

    let mut det_ms = 0.0;
    let mut rec_ms = 0.0;

    while !is_key_down(KeyCode::Escape) {
        let reply = xproto::get_image(
            &conn,
            ImageFormat::Z_PIXMAP,
            root,
            0,
            0,
            SIZE as u16,
            SIZE as u16,
            u32::MAX,
        )
        .unwrap()
        .reply()
        .unwrap();

        // 1. Change Detector
        // High-throughput raw slice equality (SIMD-accelerated memcmp)
        let frame_changed = prev_screen_bytes != reply.data;
        let is_idle = !frame_changed;

        if frame_changed {
            // Convert X11 BGRA -> Planar RGB & RGBA texture
            prepare_native(&reply.data, &mut detector.det_input, &mut img.bytes);
            tex.update(&img);

            // Run detection
            let t0 = Instant::now();
            let mut boxes = detector.detect();
            det_ms = t0.elapsed().as_secs_f64() * 1000.0;

            // Run recognition
            let t1 = Instant::now();
            recognizer.recognize(&img.bytes, &mut boxes, SIZE);
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
            prev_screen_bytes = reply.data; // O(1) buffer move
        }

        // 3. Render
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        for b in &cached_boxes {
            draw_rectangle_lines(b.x, b.y, b.w, b.h, 2.0, GREEN);

            if !b.text.is_empty() {
                let dims = measure_text(&b.text, Some(&font), 16, 1.0);
                let (pad, bw, bh) = (3.0, dims.width + 6.0, dims.height + 6.0);
                let bx = b.x.clamp(0.0, (SIZE as f32 - bw).max(0.0));
                let by = if b.y >= bh + 2.0 {
                    b.y - bh - 2.0
                } else {
                    b.y + b.h + 2.0
                };

                draw_rectangle(bx, by, bw, bh, Color::new(0.0, 0.0, 0.0, 0.85));
                draw_rectangle_lines(bx, by, bw, bh, 1.0, YELLOW);

                draw_text_ex(
                    &b.text,
                    bx + pad,
                    by + bh - pad - 2.0,
                    TextParams {
                        font: Some(&font),
                        font_size: 16,
                        color: WHITE,
                        ..Default::default()
                    },
                );
            }
        }

        // HUD overlay
        let status_color = if is_idle {
            Color::new(0.4, 0.8, 1.0, 1.0)
        } else {
            GREEN
        };
        let status_text = if is_idle { "PAUSED (STATIC)" } else { "ACTIVE" };

        draw_rectangle(0.0, 0.0, SIZE as f32, 24.0, Color::new(0.0, 0.0, 0.0, 0.75));
        draw_text_ex(
            format!(
                "[{status_text}] Lines: {} | Det: {det_ms:.1}ms | Rec: {rec_ms:.1}ms | FPS: {}",
                cached_boxes.len(),
                get_fps()
            ),
            10.0,
            17.0,
            TextParams {
                font: Some(&font),
                font_size: 16,
                color: status_color,
                ..Default::default()
            },
        );

        next_frame().await;
    }
}
