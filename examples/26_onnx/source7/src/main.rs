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

#[path = "08_latent.rs"]
#[allow(dead_code)] // Modul wird geteilt; main nutzt nur Layout+Projektion, nicht HDBSCAN.
mod latent;

use macroquad::prelude::*;
use std::time::Instant;

use capture::{CAPTURE_SIZE, ScreenCapture};
use db::FaceDatabase;
use engine::Engine;
use latent::{LatentData, project_into_2d, umap_embed};
use types::{CROP_SIZE, THUMB_BYTES};

/// Fensterbreite: 640 Feed + 180 Sidebar + 400 Latent-Panel.
const WIN_W: i32 = 1220;
/// Fensterhöhe.
const WIN_H: i32 = 640;
/// Sidebar-Breite.
const SIDE_W: f32 = 180.0;
/// Latent-Panel-Breite (rechts neben der Sidebar).
const LAT_W: f32 = 400.0;
/// k für die k-NN-Projektion eines Live-Embeddings ins 2D-Layout.
const PROJECT_K: usize = 8;
/// Maximale Länge einer Live-Trajektorie (Ringpuffer je Person).
const TRAIL_MAX: usize = 48;
/// Galerie: Kantenlänge eines Thumbnails in px.
const GAL_CELL: usize = 40;
/// Galerie: obere Kante (unter dem Live-Preview).
const GAL_TOP: f32 = 176.0;
/// Galerie: Zeilenhöhe (Thumbnail + Label + Abstand).
const GAL_ROW: f32 = 56.0;

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

/// Deterministische, gut unterscheidbare Farbe je Personen-ID (Golden-Angle).
fn person_color(id: u32) -> Color {
    let h = (id as f32 * 137.508).rem_euclid(360.0) / 360.0;
    let (r, g, b) = hsv_to_rgb(h, 0.72, 0.98);
    Color::new(r, g, b, 1.0)
}

fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (f32, f32, f32) {
    let i = (h * 6.0).floor();
    let f = h * 6.0 - i;
    let p = v * (1.0 - s);
    let q = v * (1.0 - f * s);
    let t = v * (1.0 - (1.0 - f) * s);
    match (i as i32).rem_euclid(6) {
        0 => (v, t, p),
        1 => (q, v, p),
        2 => (p, v, t),
        3 => (p, q, v),
        4 => (t, p, v),
        _ => (v, p, q),
    }
}

/// Ein Punkt einer Live-Spur im Layout-Koordinatensystem.
#[derive(Clone, Copy)]
struct TrailPoint {
    pos: [f32; 2],
    person_id: Option<u32>,
}

/// Vorberechnetes 2D-UMAP-Layout der DB + Live-Trajektorien fürs Panel.
///
/// Das Layout ist teuer (UMAP über alle Exemplare, ~Sekunden), daher wird es
/// einmalig beim Start und danach nur auf Tastendruck (`M`) neu berechnet. Die
/// 512D-Matrix bleibt gespeichert, damit Live-Embeddings per k-NN-Baryzentrik
/// (`project_into_2d`) ins bestehende Layout projiziert werden können — das ist
/// pro Frame billig (O(n·d)) und hält die Punkte konsistent zur Wolke.
struct LatentPanel {
    data: ndarray::Array2<f32>,
    person_ids: Vec<u32>,
    layout: Vec<[f32; 2]>,
    bounds: (f32, f32, f32, f32),
    n_persons: usize,
    n_neighbors: usize,
    trail: std::collections::VecDeque<TrailPoint>,
    built_secs: f64,
}

impl LatentPanel {
    /// Baut das Layout aus der aktuellen DB (leeres Panel bei leerer DB).
    fn build(db: &FaceDatabase, n_neighbors: usize) -> Self {
        let ld = LatentData::from_database(db);
        let t0 = Instant::now();
        let (layout, data, person_ids, n_persons) = if ld.is_empty() {
            (Vec::new(), ndarray::Array2::zeros((0, 0)), Vec::new(), 0)
        } else {
            let emb = umap_embed(&ld.data, n_neighbors, 2);
            let layout: Vec<[f32; 2]> = (0..ld.len()).map(|i| [emb[[i, 0]], emb[[i, 1]]]).collect();
            let ids: Vec<u32> = ld.meta.iter().map(|m| m.person_id).collect();
            (layout, ld.data, ids, ld.n_persons)
        };
        let bounds = layout_bounds(&layout);
        Self {
            data,
            person_ids,
            layout,
            bounds,
            n_persons,
            n_neighbors,
            trail: std::collections::VecDeque::with_capacity(TRAIL_MAX),
            built_secs: t0.elapsed().as_secs_f64(),
        }
    }

    /// Projiziert ein Live-Embedding ins Layout und hängt es an die Spur.
    fn push_live(&mut self, emb: &[f32], person_id: Option<u32>) {
        if self.layout.is_empty() {
            return;
        }
        let (pos, _sim) = project_into_2d(&self.data, &self.layout, emb, PROJECT_K);
        if self.trail.len() >= TRAIL_MAX {
            self.trail.pop_front();
        }
        self.trail.push_back(TrailPoint { pos, person_id });
    }
}

/// Achsenparallele Bounds der 2D-Punkte (min_x, min_y, max_x, max_y).
fn layout_bounds(pts: &[[f32; 2]]) -> (f32, f32, f32, f32) {
    let mut mn = [f32::INFINITY; 2];
    let mut mx = [f32::NEG_INFINITY; 2];
    for p in pts {
        for c in 0..2 {
            mn[c] = mn[c].min(p[c]);
            mx[c] = mx[c].max(p[c]);
        }
    }
    if !mn[0].is_finite() {
        return (-1.0, -1.0, 1.0, 1.0);
    }
    (mn[0], mn[1], mx[0], mx[1])
}

/// Zeichnet das Latent-Panel: statische Wolke (nach Person gefärbt) plus die
/// Live-Trajektorie (nach zugeordneter Person gefärbt, verblassend).
///
/// `px0` ist die linke Kante des Panels; das Panel ist `LAT_W × WIN_H`.
fn draw_latent_panel(panel: &LatentPanel, px0: f32) {
    draw_rectangle(
        px0,
        0.0,
        LAT_W,
        WIN_H as f32,
        Color::from_rgba(14, 14, 20, 255),
    );
    draw_text("latent space (UMAP)", px0 + 8.0, 18.0, 18.0, WHITE);
    if panel.layout.is_empty() {
        draw_text("keine exemplare", px0 + 8.0, 44.0, 16.0, GRAY);
        return;
    }
    // Layout-Koordinaten -> Panel-Pixel (mit Rand), Y invertiert.
    let (mnx, mny, mxx, mxy) = panel.bounds;
    let pad = 24.0f32;
    let span_x = (mxx - mnx).max(1e-3);
    let span_y = (mxy - mny).max(1e-3);
    let top = 28.0f32;
    let w = LAT_W - 2.0 * pad;
    let h = WIN_H as f32 - top - pad;
    let map = |p: [f32; 2]| -> Vec2 {
        let fx = (p[0] - mnx) / span_x;
        let fy = (p[1] - mny) / span_y;
        vec2(px0 + pad + fx * w, top + (1.0 - fy) * h)
    };
    // Statische Punktwolke (klein, halbtransparent, nach Person gefärbt).
    for (p, &id) in panel.layout.iter().zip(panel.person_ids.iter()) {
        let s = map(*p);
        let mut c = person_color(id);
        c.a = 0.5;
        draw_circle(s.x, s.y, 1.6, c);
    }
    // Live-Trajektorie: verblassende Polylinie, Farbe = zugeordnete Person.
    let n = panel.trail.len();
    let mut prev: Option<Vec2> = None;
    for (k, tp) in panel.trail.iter().enumerate() {
        let age = (k + 1) as f32 / n.max(1) as f32;
        let s = map(tp.pos);
        let mut col = tp
            .person_id
            .map_or(Color::new(1.0, 1.0, 1.0, 1.0), person_color);
        col.a = age;
        if let Some(pv) = prev {
            let mut lc = col;
            lc.a = age * 0.7;
            draw_line(pv.x, pv.y, s.x, s.y, 2.0, lc);
        }
        draw_circle(s.x, s.y, 2.0 + age * 2.0, col);
        prev = Some(s);
    }
    if let Some(head) = panel.trail.back() {
        let s = map(head.pos);
        draw_circle_lines(s.x, s.y, 9.0, 2.5, WHITE);
        let hc = head.person_id.map_or(WHITE, person_color);
        draw_circle(s.x, s.y, 5.0, hc);
        let tag = head
            .person_id
            .map_or_else(|| "?".to_string(), |id| format!("ID {id}"));
        draw_text(&tag, s.x + 11.0, s.y - 8.0, 16.0, WHITE);
    }
    // Panel-HUD unten.
    for (i, line) in [
        format!(
            "punkte: {}  personen: {}  nn: {}",
            panel.layout.len(),
            panel.n_persons,
            panel.n_neighbors
        ),
        format!(
            "spur: {} pkt   build: {:.1}s   M=neu berechnen",
            panel.trail.len(),
            panel.built_secs
        ),
    ]
    .iter()
    .enumerate()
    {
        draw_text(
            line,
            px0 + 8.0,
            WIN_H as f32 - 34.0 + i as f32 * 16.0,
            15.0,
            LIGHTGRAY,
        );
    }
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

    // Latent-Panel: Layout einmal beim Start berechnen (nn=8, validiert).
    println!("latent: berechne UMAP-Layout ...");
    let mut panel = LatentPanel::build(eng.db(), 8);
    println!(
        "latent: {} Punkte, {} Personen in {:.1}s",
        panel.layout.len(),
        panel.n_persons,
        panel.built_secs
    );

    let mut feed = Image {
        width: WIN_W as u16,
        height: WIN_H as u16,
        bytes: vec![0; WIN_W as usize * WIN_H as usize * 4],
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
        width: GAL_CELL as u16,
        height: GAL_CELL as u16,
        bytes: vec![0; GAL_CELL * GAL_CELL * 4],
    };
    let gal_tex = Texture2D::from_image(&gal);
    gal_tex.set_filter(FilterMode::Nearest);

    let mut fps = 60.0f32;
    let mut frame = 0usize;
    let mut faces_total = 0usize;
    let mut gal_scroll = 0.0f32;
    loop {
        if is_key_down(KeyCode::Escape) || (args.max_frames > 0 && frame >= args.max_frames) {
            break;
        }
        let t0 = Instant::now();
        let rgb = cap.capture_rgb();
        let tracked = eng.process_frame(&rgb, CAPTURE_SIZE, CAPTURE_SIZE);
        faces_total += tracked.len();

        // Live-Gesichter in die Latent-Trajektorie projizieren (erstes/
        // prominentestes Gesicht treibt die Spur, gefärbt nach Person).
        if let Some(f) = tracked.first() {
            panel.push_live(&f.embedding.v, f.person_id);
        }

        // M: Layout aus aktueller (gewachsener) DB neu berechnen. Blockiert
        // kurz (UMAP), daher nur auf Tastendruck statt pro Frame/automatisch.
        if is_key_pressed(KeyCode::M) {
            println!("latent: berechne Layout neu ...");
            panel = LatentPanel::build(eng.db(), panel.n_neighbors);
            println!(
                "latent: {} Punkte in {:.1}s",
                panel.layout.len(),
                panel.built_secs
            );
        }

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
        // Galerie: alle Personen + alle Exemplare, scrollbar (Mausrad).
        let hud_top = WIN_H as f32 - 84.0;
        draw_text("galerie", sx + 8.0, GAL_TOP - 12.0, 16.0, WHITE);
        // Scroll-Offset aus dem Mausrad (nur wenn Cursor über der Sidebar).
        let (_, wheel_y) = mouse_wheel();
        let (mx, _) = mouse_position();
        if mx >= sx {
            gal_scroll -= wheel_y * 24.0;
        }
        let persons = eng.db().persons();
        let total_h = persons.len() as f32 * GAL_ROW;
        let view_h = hud_top - GAL_TOP;
        let max_scroll = (total_h - view_h).max(0.0);
        gal_scroll = gal_scroll.clamp(0.0, max_scroll);
        for (i, p) in persons.iter().enumerate() {
            let row_y = GAL_TOP + i as f32 * GAL_ROW - gal_scroll;
            // Zeilen außerhalb des sichtbaren Bereichs überspringen.
            if row_y + GAL_CELL as f32 <= GAL_TOP || row_y >= hud_top {
                continue;
            }
            draw_text(
                format!("ID {} ({})", p.id, p.exemplars.len()),
                sx + 8.0,
                row_y - 2.0,
                14.0,
                LIGHTGRAY,
            );
            for (k, ex) in p.exemplars.iter().enumerate() {
                let tx = sx + 8.0 + k as f32 * (GAL_CELL as f32 + 2.0);
                if tx + GAL_CELL as f32 > sx + SIDE_W {
                    break; // Zeile voll
                }
                gal.bytes = thumb_scaled(&ex.thumbnail, GAL_CELL);
                gal_tex.update(&gal);
                draw_texture(&gal_tex, tx, row_y + 2.0, WHITE);
            }
        }
        // Sidebar-Ränder überzeichnen (einfaches Clipping oben/unten).
        draw_rectangle(
            sx,
            GAL_TOP - 28.0,
            SIDE_W,
            16.0,
            Color::from_rgba(20, 20, 28, 255),
        );
        draw_rectangle(
            sx,
            hud_top,
            SIDE_W,
            WIN_H as f32 - hud_top,
            Color::from_rgba(20, 20, 28, 255),
        );
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
        // Latent-Space-Panel rechts neben der Sidebar.
        draw_latent_panel(&panel, CAPTURE_SIZE as f32 + SIDE_W);
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
