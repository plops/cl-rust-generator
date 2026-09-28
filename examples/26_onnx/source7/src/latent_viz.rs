//! latent_viz.rs — UMAP-Visualisierung des ArcFace-Latent-Space.
//!
//! Liest `faces_db.bin`, reduziert die 512D-Exemplar-Embeddings per `umap-rs`
//! auf 2D und rendert eine zoom-/schwenkbare Streuwolke: ein Punkt je Exemplar,
//! eingefärbt nach Personen-ID. Beim Hovern erscheint das zugehörige Thumbnail.
//!
//! Zusätzlich `--live`: greift live Gesichter vom X11-Desktop ab (dieselbe
//! SCRFD→ArcFace-Pipeline wie `x11_face_reid`, ohne DB-Update), projiziert jedes
//! Embedding per k-NN-Baryzentrik in genau dieses 2D-Layout und zeichnet die
//! einlaufenden Punkte als verblassende **Trajektorie** — man sieht, wie sich
//! ein Gesicht durch den Latent-Space bewegt und an welchem Cluster es andockt.
//!
//! Bedienung: Mausrad = Zoom (auf Cursor), linke Maustaste ziehen = schwenken,
//! `T` = Thumbnails aller Punkte ein/aus, `C` = Färbung Person↔HDBSCAN-Cluster,
//! `L` = Trajektorie ein/aus (nur `--live`), `R` = Ansicht zurücksetzen,
//! `Esc` = beenden. `--frames N` beendet nach N Frames (Headless-Smoke).

// Dieses Binary teilt sich die DB-/Typ-Module mit `main.rs`, nutzt davon aber
// nur einen Teil (Laden + Personen lesen). Ungenutzte Re-ID-Logik ist daher
// hier erwartbar tot — kein Signal für echten toten Code.
#![allow(dead_code)]

#[path = "03_alignment.rs"]
mod align;
#[path = "05_arcface_embed.rs"]
mod arcface;
#[path = "02_screen_capture.rs"]
mod capture;
#[path = "06_face_database.rs"]
mod db;
#[path = "08_latent.rs"]
mod latent;
#[path = "10_live.rs"]
mod live;
#[path = "04_scrfd_detector.rs"]
mod scrfd;
#[path = "01_types.rs"]
mod types;

use db::FaceDatabase;
use latent::{
    ClusterResult, LatentData, hdbscan_cluster_ms, project_into_2d, thumb_scaled_rgba, umap_embed,
};
use live::LiveFeed;
use macroquad::prelude::*;

/// Fensterbreite.
const WIN_W: i32 = 1000;
/// Fensterhöhe.
const WIN_H: i32 = 760;
/// Thumbnail-Kantenlänge beim Hover/Overlay in px.
const THUMB_PX: usize = 48;
/// Maximale Länge der Live-Trajektorie (Ringpuffer je Spur).
const TRAIL_MAX: usize = 64;
/// k für die k-NN-Projektion eines Live-Embeddings ins 2D-Layout.
const PROJECT_K: usize = 8;

/// CLI-Konfiguration (Handparse, kein clap — wie `main.rs`).
struct Args {
    db_path: String,
    n_neighbors: usize,
    min_cluster_size: usize,
    min_samples: usize,
    cluster_dim: usize,
    frames: usize,
    live: bool,
    models: String,
    conf: f32,
}

fn parse_args() -> Result<Args, String> {
    let mut a = Args {
        db_path: "faces_db.bin".into(),
        n_neighbors: 15,
        min_cluster_size: 4,
        min_samples: 3,
        cluster_dim: 6,
        frames: 0,
        live: false,
        models: "models".into(),
        conf: 0.5,
    };
    let mut it = std::env::args().skip(1);
    while let Some(f) = it.next() {
        match f.as_str() {
            "--db" => a.db_path = it.next().ok_or("--db braucht Wert")?,
            "--neighbors" => {
                a.n_neighbors = it
                    .next()
                    .ok_or("--neighbors braucht Wert")?
                    .parse()
                    .map_err(|_| "neighbors?")?;
            }
            "--min-cluster-size" => {
                a.min_cluster_size = it
                    .next()
                    .ok_or("--min-cluster-size braucht Wert")?
                    .parse()
                    .map_err(|_| "min-cluster-size?")?;
            }
            "--min-samples" => {
                a.min_samples = it
                    .next()
                    .ok_or("--min-samples braucht Wert")?
                    .parse()
                    .map_err(|_| "min-samples?")?;
            }
            "--cluster-dim" => {
                a.cluster_dim = it
                    .next()
                    .ok_or("--cluster-dim braucht Wert")?
                    .parse()
                    .map_err(|_| "cluster-dim?")?;
            }
            "--frames" => a.frames = it.next().ok_or("n?")?.parse().map_err(|_| "n?")?,
            "--live" => a.live = true,
            "--models" => a.models = it.next().ok_or("--models braucht Wert")?,
            "--conf" => {
                a.conf = it
                    .next()
                    .ok_or("--conf braucht Wert")?
                    .parse()
                    .map_err(|_| "conf?")?;
            }
            "--help" | "-h" => return Err("help".into()),
            x => return Err(format!("unbekannt: {x}")),
        }
    }
    Ok(a)
}

fn usage() -> &'static str {
    "latent_viz [--db PATH] [--neighbors K] [--min-cluster-size M] \
     [--min-samples S] [--cluster-dim D] [--frames N] \
     [--live [--models DIR] [--conf F]]   (min-samples 0 = an mcs gekoppelt)"
}

/// Deterministische, gut unterscheidbare Farbe je Personen-ID (Golden-Angle-Hue).
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

/// Farbe je HDBSCAN-Label: Rauschen (`-1`) grau, sonst distinkter Hue.
fn cluster_color(label: i32) -> Color {
    if label < 0 {
        return Color::new(0.45, 0.45, 0.5, 0.6); // Rauschen: gedämpftes Grau
    }
    let h = (label as f32 * 137.508).rem_euclid(360.0) / 360.0;
    let (r, g, b) = hsv_to_rgb(h, 0.75, 1.0);
    Color::new(r, g, b, 1.0)
}

/// 2D-UMAP-Projektion für die Darstellung (Layout, nicht Clustering).
fn compute_embedding(ld: &LatentData, n_neighbors: usize) -> Vec<[f32; 2]> {
    let emb = umap_embed(&ld.data, n_neighbors, 2);
    (0..ld.len()).map(|i| [emb[[i, 0]], emb[[i, 1]]]).collect()
}

/// Achsenparallele Bounds der 2D-Punkte (min_x, min_y, max_x, max_y).
fn bounds(pts: &[[f32; 2]]) -> (f32, f32, f32, f32) {
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

/// Kamera: Weltkoordinaten → Bildschirm über Zoom + Pan.
struct View {
    zoom: f32,
    pan: Vec2,
}

impl View {
    fn to_screen(&self, w: [f32; 2]) -> Vec2 {
        vec2(w[0], w[1]) * self.zoom + self.pan
    }
    fn to_world(&self, s: Vec2) -> Vec2 {
        (s - self.pan) / self.zoom
    }
}

fn window_conf() -> Conf {
    Conf {
        window_title: "Latent Space (UMAP)".into(),
        window_width: WIN_W,
        window_height: WIN_H,
        ..Default::default()
    }
}

/// Ein Punkt der Live-Trajektorie im Layout-Koordinatensystem.
#[derive(Clone, Copy)]
struct TrailPoint {
    /// 2D-Position im Layout (wie die statische Wolke).
    pos: [f32; 2],
    /// Beste Similarity zu einem Exemplar (`S_max`) — steuert Färbung.
    sim: f32,
}

/// Ringpuffer der letzten Live-Positionen eines Gesichts (eine Spur).
struct Trajectory {
    pts: std::collections::VecDeque<TrailPoint>,
}

impl Trajectory {
    fn new() -> Self {
        Self {
            pts: std::collections::VecDeque::with_capacity(TRAIL_MAX),
        }
    }

    /// Hängt eine neue Position an; deckelt die Länge (FIFO).
    fn push(&mut self, pos: [f32; 2], sim: f32) {
        if self.pts.len() >= TRAIL_MAX {
            self.pts.pop_front();
        }
        self.pts.push_back(TrailPoint { pos, sim });
    }
}

/// Farbe eines Live-Punkts nach `S_max`: grün (sicher) → gelb → rot (fremd),
/// Schwellen wie in der Re-ID (0.65 bekannt, 0.45 neu).
fn sim_color(sim: f32, alpha: f32) -> Color {
    let (r, g) = if sim >= 0.65 {
        (0.2, 1.0)
    } else if sim >= 0.45 {
        (1.0, 0.85)
    } else {
        (1.0, 0.3)
    };
    Color::new(r, g, 0.35, alpha)
}

#[macroquad::main(window_conf)]
async fn main() {
    let args = parse_args().unwrap_or_else(|e| {
        if e == "help" {
            println!("{}", usage());
            std::process::exit(0);
        }
        eprintln!("{e}\n{}", usage());
        std::process::exit(2);
    });

    let database = FaceDatabase::load(&args.db_path);
    let ld = LatentData::from_database(&database);
    println!(
        "geladen: {} Exemplare, {} Personen aus {}",
        ld.len(),
        ld.n_persons,
        args.db_path
    );
    if ld.is_empty() {
        eprintln!("keine Exemplare in {} — nichts zu zeigen", args.db_path);
        std::process::exit(2);
    }

    // Live-Zufuhr früh öffnen (vor dem teuren UMAP), damit eine Fehlkonfig
    // (kein Display, fehlende Modelle) sofort abbricht statt erst nach Minuten.
    let mut feed: Option<LiveFeed> = if args.live {
        match LiveFeed::open(&args.models, args.conf) {
            Ok(f) => {
                println!("live: Provider {}", f.provider);
                Some(f)
            }
            Err(e) => {
                eprintln!("--live nicht möglich: {e}");
                std::process::exit(2);
            }
        }
    } else {
        None
    };
    let live_provider = feed.as_ref().map_or("-", |f| f.provider);

    let t0 = std::time::Instant::now();
    let pts = compute_embedding(&ld, args.n_neighbors);
    println!(
        "UMAP fertig: {} Punkte in {:.2}s",
        pts.len(),
        t0.elapsed().as_secs_f64()
    );

    // HDBSCAN im UMAP-`cluster_dim`-D-Subraum (nicht in 2D, nicht in vollen
    // 512D): ein mittleres `d` reduziert Cluster-Überlapp gegenüber 2D und
    // entrauscht gegenüber 512D — die per DoE (`doe`-Binary) gefundene Balance.
    // Euklidisch auf UMAP-Ausgabe; das 2D-Display bleibt davon unberührt.
    let tc = std::time::Instant::now();
    let cluster_space = if args.cluster_dim <= 2 {
        // d≤2: dasselbe Layout wie die Anzeige clustern (kein Extra-UMAP).
        let mut a = ndarray::Array2::<f32>::zeros((pts.len(), 2));
        for (i, p) in pts.iter().enumerate() {
            a[[i, 0]] = p[0];
            a[[i, 1]] = p[1];
        }
        a
    } else {
        umap_embed(&ld.data, args.n_neighbors, args.cluster_dim)
    };
    let ms = if args.min_samples == 0 {
        None
    } else {
        Some(args.min_samples)
    };
    let cluster = hdbscan_cluster_ms(&cluster_space, args.min_cluster_size, ms);
    println!(
        "HDBSCAN in {}D: {} Cluster, {:.1}% Rauschen in {:.2}s",
        args.cluster_dim,
        cluster.n_clusters,
        cluster.noise_ratio * 100.0,
        tc.elapsed().as_secs_f64()
    );

    // Startansicht: Punktwolke bildschirmfüllend zentrieren.
    let (mnx, mny, mxx, mxy) = bounds(&pts);
    let span = (mxx - mnx).max(mxy - mny).max(1e-3);
    let base_zoom = 0.8 * WIN_H as f32 / span;
    let center = vec2((mnx + mxx) * 0.5, (mny + mxy) * 0.5);
    let mut view = View {
        zoom: base_zoom,
        pan: vec2(WIN_W as f32 * 0.5, WIN_H as f32 * 0.5) - center * base_zoom,
    };

    // Vorberechnete Thumbnail-Texturen (klein, RGBA), lazy pro Punkt.
    let mut thumb_tex: Vec<Option<Texture2D>> = vec![None; ld.len()];
    let mut show_all_thumbs = false;
    let mut color_by_cluster = true;
    let mut last_drag = None::<Vec2>;
    let mut frame = 0usize;

    // Trajektorie + aktuelle Live-Vorschau (letzter alignter Crop).
    let mut trail = Trajectory::new();
    let mut show_trail = true;
    let mut live_preview: Option<Texture2D> = None;
    let mut live_sim = f32::NEG_INFINITY;
    let mut live_seen = 0usize;

    loop {
        if is_key_pressed(KeyCode::Escape) || (args.frames > 0 && frame >= args.frames) {
            break;
        }
        if is_key_pressed(KeyCode::T) {
            show_all_thumbs = !show_all_thumbs;
        }
        if is_key_pressed(KeyCode::C) {
            color_by_cluster = !color_by_cluster;
        }
        if is_key_pressed(KeyCode::L) {
            show_trail = !show_trail;
        }
        if is_key_pressed(KeyCode::R) {
            view.zoom = base_zoom;
            view.pan = vec2(WIN_W as f32 * 0.5, WIN_H as f32 * 0.5) - center * base_zoom;
        }

        // Live-Frame abgreifen und ins statische 2D-Layout projizieren.
        if let Some(f) = feed.as_mut() {
            let faces = f.poll();
            live_seen = faces.len();
            // Erstes (prominentestes) Gesicht treibt Trajektorie + Vorschau.
            if let Some(face) = faces.first() {
                let (pos, sim) = project_into_2d(&ld.data, &pts, &face.embedding.v, PROJECT_K);
                trail.push(pos, sim);
                live_sim = sim;
                let rgba = thumb_scaled_rgba(&face.crop, THUMB_PX);
                let img = Image {
                    width: THUMB_PX as u16,
                    height: THUMB_PX as u16,
                    bytes: rgba,
                };
                let tex = Texture2D::from_image(&img);
                tex.set_filter(FilterMode::Nearest);
                live_preview = Some(tex);
            }
        }

        // Zoom auf Cursor.
        let (_, wheel) = mouse_wheel();
        if wheel.abs() > 0.0 {
            let m = vec2(mouse_position().0, mouse_position().1);
            let before = view.to_world(m);
            view.zoom = (view.zoom * (1.0 + wheel.signum() * 0.1)).clamp(1.0, 5000.0);
            let after = view.to_world(m);
            view.pan += (after - before) * view.zoom;
        }
        // Pan per linke Maustaste.
        if is_mouse_button_down(MouseButton::Left) {
            let m = vec2(mouse_position().0, mouse_position().1);
            if let Some(prev) = last_drag {
                view.pan += m - prev;
            }
            last_drag = Some(m);
        } else {
            last_drag = None;
        }

        clear_background(Color::from_rgba(16, 16, 22, 255));

        // Punkte zeichnen; nächsten zum Cursor für Hover-Thumbnail merken.
        let mouse = vec2(mouse_position().0, mouse_position().1);
        let mut hover: Option<usize> = None;
        let mut hover_d2 = f32::INFINITY;
        for (i, p) in pts.iter().enumerate() {
            let s = view.to_screen(*p);
            if s.x < -20.0 || s.x > WIN_W as f32 + 20.0 || s.y < -20.0 || s.y > WIN_H as f32 + 20.0
            {
                continue;
            }
            let col = if color_by_cluster {
                cluster_color(cluster.labels[i])
            } else {
                person_color(ld.meta[i].person_id)
            };
            draw_circle(s.x, s.y, 3.5, col);
            let d2 = (s - mouse).length_squared();
            if d2 < hover_d2 {
                hover_d2 = d2;
                hover = Some(i);
            }
            if show_all_thumbs && view.zoom > 60.0 {
                draw_thumb(&mut thumb_tex, &ld, i, s.x + 5.0, s.y - 5.0, THUMB_PX);
            }
        }

        // Hover-Thumbnail groß + Label.
        if hover_d2 < 20.0 * 20.0
            && let Some(i) = hover
        {
            let s = view.to_screen(pts[i]);
            draw_circle_lines(s.x, s.y, 6.0, 2.0, WHITE);
            draw_thumb(&mut thumb_tex, &ld, i, mouse.x + 12.0, mouse.y + 12.0, 96);
            let m = &ld.meta[i];
            let clabel = cluster.labels[i];
            let ctxt = if clabel < 0 {
                "Rauschen".to_string()
            } else {
                format!("Cluster {clabel}")
            };
            draw_text(
                format!("ID {} · Ex {} · {ctxt}", m.person_id, m.exemplar_idx),
                mouse.x + 12.0,
                mouse.y + 12.0 + 96.0 + 14.0,
                18.0,
                WHITE,
            );
        }

        // Live-Trajektorie: verblassende Polylinie + heller Kopf-Marker.
        if show_trail && !trail.pts.is_empty() {
            let n = trail.pts.len();
            let mut prev: Option<Vec2> = None;
            for (k, tp) in trail.pts.iter().enumerate() {
                // Ältere Punkte transparenter (linear verblassend).
                let age = (k + 1) as f32 / n as f32;
                let s = view.to_screen(tp.pos);
                if let Some(pv) = prev {
                    draw_line(pv.x, pv.y, s.x, s.y, 2.0, sim_color(tp.sim, age * 0.8));
                }
                draw_circle(s.x, s.y, 2.0 + age * 2.0, sim_color(tp.sim, age));
                prev = Some(s);
            }
            // Kopf der Spur (aktuellste Position) betonen.
            if let Some(head) = trail.pts.back() {
                let s = view.to_screen(head.pos);
                draw_circle_lines(s.x, s.y, 9.0, 2.5, WHITE);
                draw_circle(s.x, s.y, 5.0, sim_color(head.sim, 1.0));
                let tag = if head.sim.is_finite() {
                    format!("live {:.2}", head.sim)
                } else {
                    "live".to_string()
                };
                draw_text(&tag, s.x + 11.0, s.y - 8.0, 18.0, WHITE);
            }
        }

        // Live-Vorschau (aktueller alignter Crop) oben rechts + Sim-Zeile.
        if let Some(tex) = &live_preview {
            let px = WIN_W as f32 - 96.0 - 10.0;
            draw_texture_ex(
                tex,
                px,
                10.0,
                WHITE,
                DrawTextureParams {
                    dest_size: Some(vec2(96.0, 96.0)),
                    ..Default::default()
                },
            );
            draw_rectangle_lines(px, 10.0, 96.0, 96.0, 2.0, sim_color(live_sim, 1.0));
            draw_text("live crop", px, 122.0, 18.0, LIGHTGRAY);
        }

        draw_hud(&ld, &cluster, view.zoom, show_all_thumbs, color_by_cluster);
        if feed.is_some() {
            draw_live_hud(
                live_seen,
                live_sim,
                trail.pts.len(),
                show_trail,
                live_provider,
            );
        }
        next_frame().await;
        frame += 1;
    }
    println!(
        "stats frames={frame} points={} persons={} clusters={} noise={:.1}% live_trail={}",
        ld.len(),
        ld.n_persons,
        cluster.n_clusters,
        cluster.noise_ratio * 100.0,
        trail.pts.len()
    );
}

/// Zeichnet (und cached) das Thumbnail von Punkt `i` an `(x, y)`.
fn draw_thumb(
    cache: &mut [Option<Texture2D>],
    ld: &LatentData,
    i: usize,
    x: f32,
    y: f32,
    size: usize,
) {
    if cache[i].is_none() {
        let rgba = thumb_scaled_rgba(&ld.thumbs[i], THUMB_PX);
        let img = Image {
            width: THUMB_PX as u16,
            height: THUMB_PX as u16,
            bytes: rgba,
        };
        let tex = Texture2D::from_image(&img);
        tex.set_filter(FilterMode::Nearest);
        cache[i] = Some(tex);
    }
    if let Some(tex) = &cache[i] {
        draw_texture_ex(
            tex,
            x,
            y,
            WHITE,
            DrawTextureParams {
                dest_size: Some(vec2(size as f32, size as f32)),
                ..Default::default()
            },
        );
        draw_rectangle_lines(x, y, size as f32, size as f32, 1.0, DARKGRAY);
    }
}

/// Info-HUD oben links.
fn draw_hud(ld: &LatentData, cluster: &ClusterResult, zoom: f32, thumbs: bool, by_cluster: bool) {
    draw_rectangle(0.0, 0.0, 360.0, 116.0, Color::from_rgba(0, 0, 0, 160));
    for (i, line) in [
        format!("exemplare: {}  personen: {}", ld.len(), ld.n_persons),
        format!(
            "cluster: {}  rauschen: {:.1}%",
            cluster.n_clusters,
            cluster.noise_ratio * 100.0
        ),
        format!(
            "C farbe: {}   zoom: {zoom:.0}",
            if by_cluster { "cluster" } else { "person" }
        ),
        format!(
            "T thumbnails: {}   (rad=zoom, ziehen=pan, R=reset, L=spur)",
            if thumbs { "an" } else { "aus" }
        ),
    ]
    .iter()
    .enumerate()
    {
        draw_text(line, 8.0, 22.0 + i as f32 * 22.0, 18.0, LIGHTGRAY);
    }
}

/// Live-HUD unten links: einlaufende Gesichter, S_max, Trajektorie, Provider.
fn draw_live_hud(seen: usize, sim: f32, trail_len: usize, show_trail: bool, provider: &str) {
    let y0 = WIN_H as f32 - 92.0;
    draw_rectangle(0.0, y0, 360.0, 92.0, Color::from_rgba(0, 0, 0, 160));
    let sim_txt = if sim.is_finite() {
        format!("{sim:.2}")
    } else {
        "—".into()
    };
    for (i, line) in [
        format!("LIVE  gesichter: {seen}   provider: {provider}"),
        format!("S_max: {sim_txt}   trajektorie: {trail_len} pkt"),
        format!(
            "spur (L): {}   grün=bekannt gelb=ambig rot=neu",
            if show_trail { "an" } else { "aus" }
        ),
    ]
    .iter()
    .enumerate()
    {
        draw_text(line, 8.0, y0 + 24.0 + i as f32 * 22.0, 18.0, LIGHTGRAY);
    }
}
