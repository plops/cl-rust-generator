//! main.rs — CLI, EP-Aufbau mit Fallback, Macroquad-Loop, Sidebar-Rendering.
//!
//! Fenster 820×640: links 640×640-Feed (Boxen, Keypoints, IDs), rechts
//! 180px-Sidebar (Live-Crop, Galerie, HUD). Exit-Codes: 0 ok, 1 X11-Fehler,
//! 2 Konfig/Modell fehlt.

#[path = "01_types.rs"]
mod types;

#[path = "02_screen_capture.rs"]
mod capture;

#[path = "03_alignment.rs"]
mod align;

#[path = "04_scrfd_detector.rs"]
mod scrfd;

#[path = "05_arcface_embed.rs"]
mod arcface;

#[path = "06_face_database.rs"]
mod db;

#[path = "07_engine.rs"]
mod engine;

use macroquad::prelude::*;
use std::time::Instant;

use capture::{CAPTURE_SIZE, ScreenCapture};
use db::FaceDatabase;
use engine::Engine;
use types::{CROP_SIZE, THUMB_BYTES};

/// Fensterbreite: 640 Feed + 180 Sidebar.
const WIN_W: i32 = 820;
/// Fensterhöhe.
const WIN_H: i32 = 640;
/// Sidebar-Breite.
const SIDE_W: f32 = 180.0;

/// CLI-Konfiguration (Handparse, kein clap).
struct Args {
    models: String,
    db_path: String,
    conf: f32,
    max_frames: usize,
}

fn parse_args() -> Result<Args, String> {
    let mut a = Args {
        models: ".".into(),
        db_path: "faces_db.bin".into(),
        conf: 0.5,
        max_frames: 0,
    };
    let mut it = std::env::args().skip(1);
    while let Some(f) = it.next() {
        match f.as_str() {
            "--models" => a.models = it.next().ok_or("--models braucht Wert")?,
            "--db" => a.db_path = it.next().ok_or("--db braucht Wert")?,
            "--conf" => {
                a.conf = it
                    .next()
                    .ok_or("--conf braucht Wert")?
                    .parse()
                    .map_err(|_| "conf?")?
            }
            "--max-frames" => a.max_frames = it.next().ok_or("n?")?.parse().map_err(|_| "n?")?,
            "--help" | "-h" => return Err("help".into()),
            x => return Err(format!("unbekannt: {x}")),
        }
    }
    Ok(a)
}

fn usage() -> &'static str {
    "x11_face_reid [--models DIR] [--db PATH] [--conf F] [--max-frames N]"
}

fn window_conf() -> Conf {
    Conf {
        window_title: "Face Re-ID".into(),
        window_width: WIN_W,
        window_height: WIN_H,
        ..Default::default()
    }
}

/// RGB in RGBA-Image-Puffer (Feed links, Sidebar-Hintergrund rechts).
fn compose_rgba(feed_rgb: &[u8]) -> Vec<u8> {
    let mut rgba = vec![26u8; WIN_W as usize * WIN_H as usize * 4];
    for y in 0..CAPTURE_SIZE {
        for x in 0..CAPTURE_SIZE {
            let si = (y * CAPTURE_SIZE + x) * 3;
            let di = (y * WIN_W as usize + x) * 4;
            rgba[di..di + 3].copy_from_slice(&feed_rgb[si..si + 3]);
            rgba[di + 3] = 255;
        }
    }
    rgba
}

/// Nearest-Skalierung eines 112-Thumbnails auf `size` (Galerie).
fn thumb_scaled(thumb: &[u8], size: usize) -> Vec<u8> {
    let mut out = vec![0u8; size * size * 4];
    for y in 0..size {
        for x in 0..size {
            let si = ((y * CROP_SIZE / size) * CROP_SIZE + (x * CROP_SIZE / size)) * 3;
            let di = (y * size + x) * 4;
            out[di..di + 3].copy_from_slice(&thumb[si..si + 3]);
            out[di + 3] = 255;
        }
    }
    out
}

#[macroquad::main(window_conf)]
async fn main() {
    let args = parse_args().unwrap_or_else(|e| {
        if e == "help" {
            println!("{usage}", usage = usage());
            std::process::exit(0);
        }
        eprintln!("{e}\n{usage}", usage = usage());
        std::process::exit(2);
    });
    let det_path = format!("{}/det_500m.onnx", args.models);
    let emb_path = format!("{}/w600k_mbf.onnx", args.models);
    for p in [&det_path, &emb_path] {
        if !std::path::Path::new(p).exists() {
            eprintln!(
                "Modell fehlt: {p} — erst ./download_models.sh {}",
                args.models
            );
            std::process::exit(2);
        }
    }
    let cap = ScreenCapture::connect().unwrap_or_else(|e| {
        eprintln!("X11-Fehler: {e}");
        std::process::exit(1);
    });
    let mut detector = scrfd::ScrfdDetector::open(&det_path);
    detector.conf_thres = args.conf;
    let provider = detector.provider;
    let embedder = arcface::ArcfaceEmbed::open(&emb_path);
    let database = FaceDatabase::load(&args.db_path);
    let mut eng = Engine::new(detector, embedder, database, provider);

    let mut feed = Image {
        width: WIN_W as u16,
        height: WIN_H as u16,
        bytes: vec![0; 820 * 640 * 4],
    };
    let feed_tex = Texture2D::from_image(&feed);
    feed_tex.set_filter(FilterMode::Nearest);
    let mut preview = Image {
        width: 112,
        height: 112,
        bytes: vec![0; 112 * 112 * 4],
    };
    let preview_tex = Texture2D::from_image(&preview);
    preview_tex.set_filter(FilterMode::Nearest);
    let mut gal = Image {
        width: 56,
        height: 56,
        bytes: vec![0; 56 * 56 * 4],
    };
    let gal_tex = Texture2D::from_image(&gal);
    gal_tex.set_filter(FilterMode::Nearest);

    let mut fps = 60.0f32;
    let mut frame = 0usize;
    let mut faces_total = 0usize;
    loop {
        if is_key_down(KeyCode::Escape) || (args.max_frames > 0 && frame >= args.max_frames) {
            break;
        }
        let t0 = Instant::now();
        let rgb = cap.capture_rgb();
        let tracked = eng.process_frame(&rgb, CAPTURE_SIZE, CAPTURE_SIZE);
        faces_total += tracked.len();

        feed.bytes = compose_rgba(&rgb);
        feed_tex.update(&feed);
        clear_background(BLACK);
        draw_texture(&feed_tex, 0.0, 0.0, WHITE);
        for t in &tracked {
            let b = t.detection.bbox;
            draw_rectangle_lines(b.x1, b.y1, b.width(), b.height(), 2.0, GREEN);
            for p in t.detection.landmarks.points {
                draw_circle(p[0], p[1], 2.0, YELLOW);
            }
            let label = match t.person_id {
                Some(id) if t.sim.is_finite() => format!("ID {id} {sim:.2}", sim = t.sim),
                Some(id) => format!("ID {id}"),
                None => "?".to_string(),
            };
            draw_text(&label, b.x1, (b.y1 - 6.0).max(10.0), 16.0, GREEN);
        }
        // Sidebar: Preview, Galerie, HUD.
        let sx = CAPTURE_SIZE as f32;
        draw_rectangle(
            sx,
            0.0,
            SIDE_W,
            WIN_H as f32,
            Color::from_rgba(20, 20, 28, 255),
        );
        if let Some(t) = tracked.first() {
            assert_eq!(t.crop.len(), THUMB_BYTES);
            let (dst4, _) = preview.bytes.as_chunks_mut::<4>();
            let (src3, _) = t.crop.as_chunks::<3>();
            for (d, s) in dst4.iter_mut().zip(src3.iter()) {
                d[..3].copy_from_slice(s);
                d[3] = 255;
            }
            preview_tex.update(&preview);
            draw_text("live", sx + 8.0, 16.0, 16.0, WHITE);
            draw_texture(&preview_tex, sx + 34.0, 24.0, WHITE);
        }
        draw_text("galerie", sx + 8.0, 160.0, 16.0, WHITE);
        for (i, p) in eng.db().persons().iter().take(6).enumerate() {
            if let Some(ex) = p.exemplars.first() {
                gal.bytes = thumb_scaled(&ex.thumbnail, 56);
                gal_tex.update(&gal);
                let gy = 170.0 + i as f32 * 62.0;
                draw_texture(&gal_tex, sx + 12.0, gy, WHITE);
                draw_text(format!("ID {}", p.id), sx + 74.0, gy + 32.0, 16.0, WHITE);
            }
        }
        let hud_y = WIN_H as f32 - 76.0;
        for (i, line) in [
            format!("personen: {}", eng.db().persons().len()),
            format!("exemplare: {}", eng.db().exemplar_count()),
            format!("fps: {fps:.0}"),
            format!("provider: {}", eng.provider()),
        ]
        .iter()
        .enumerate()
        {
            draw_text(line, sx + 8.0, hud_y + i as f32 * 18.0, 15.0, LIGHTGRAY);
        }
        next_frame().await;
        let dt = t0.elapsed().as_secs_f32().max(1e-3);
        fps = fps * 0.9 + (1.0 / dt) * 0.1;
        frame += 1;
        if frame.is_multiple_of(300) {
            let _ = eng.db().save(&args.db_path);
        }
    }
    let _ = eng.db().save(&args.db_path);
    println!(
        "stats frames={frame} faces={faces_total} persons={} exemplars={} provider={}",
        eng.db().persons().len(),
        eng.db().exemplar_count(),
        eng.provider()
    );
}
