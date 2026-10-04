//! Macroquad-GUI: interaktives dB-Bild, Apertur-Animation, PSF-Schnitte.
//!
//! `←/→` verändert die Pulszahl (Re-Fokussierung live), `R` stellt die volle
//! Apertur wieder her, ein Mausklick setzt das Fadenkreuz für Range- und
//! Azimuth-Schnitt. Mit `--frames N --screenshot F` rendert die GUI ohne
//! Interaktion N Frames, speichert einen Screenshot und beendet sich
//! (Headless-Test unter `xvfb`).

use crate::kernel::peak_power;
use crate::phantom::{PhantomKind, build as build_phantom};
use crate::pipeline::{DYN_RANGE_DB, SarPipeline, image_to_rgb};
use crate::simulator::simulate;
use crate::types::{Complex32, RadarParams, SceneGeometry, mag_to_unit_db};
use macroquad::prelude::*;
use std::path::PathBuf;

/// GUI-Konfiguration (aus CLI-Argumenten).
pub struct GuiConfig {
    pub phantom: PhantomKind,
    pub size: u32,
    pub num_pulses: u32,
    pub limit: Option<u32>,
    pub frames: Option<u32>,
    pub screenshot: Option<PathBuf>,
}

/// Bildposition und -größe im Fenster (800×600, links).
pub const IMG_X: f32 = 20.0;
pub const IMG_Y: f32 = 20.0;
pub const IMG_S: f32 = 560.0;

/// Bildschirmkoordinate → Bildpixel (`None` außerhalb des Bildes).
pub fn screen_to_pixel(mx: f32, my: f32, n: u32) -> Option<(u32, u32)> {
    if mx < IMG_X || my < IMG_Y || mx >= IMG_X + IMG_S || my >= IMG_Y + IMG_S {
        return None;
    }
    let px = ((mx - IMG_X) / IMG_S * n as f32) as u32;
    let py = ((my - IMG_Y) / IMG_S * n as f32) as u32;
    if px < n && py < n {
        Some((px, py))
    } else {
        None
    }
}

/// Range-Schnitt (Spalte `x`, Länge `height`) und Azimuth-Schnitt
/// (Zeile `y`, Länge `width`), jeweils dB-normiert auf `[0, 1]`.
pub fn profiles_db(
    img: &[Complex32],
    width: u32,
    height: u32,
    x: u32,
    y: u32,
) -> (Vec<f32>, Vec<f32>) {
    let max = img.iter().fold(0.0f32, |m, c| m.max(c.norm()));
    let w = width as usize;
    let range = (0..height)
        .map(|yy| mag_to_unit_db(img[yy as usize * w + x as usize].norm(), max, DYN_RANGE_DB))
        .collect();
    let azimuth = (0..width)
        .map(|xx| mag_to_unit_db(img[y as usize * w + xx as usize].norm(), max, DYN_RANGE_DB))
        .collect();
    (range, azimuth)
}

fn to_colors(rgb: &[u8]) -> Vec<Color> {
    rgb.as_chunks::<3>()
        .0
        .iter()
        .map(|c| Color::from_rgba(c[0], c[1], c[2], 255))
        .collect()
}

/// Zeichnet einen dB-Schnitt als Linienzug mit −3-dB-Marke.
fn draw_profile(x0: f32, y0: f32, w: f32, h: f32, label: &str, cut: &[f32]) {
    draw_rectangle(x0, y0, w, h, Color::new(0.1, 0.1, 0.12, 1.0));
    // −3-dB-Linie (0 dB oben).
    let y3 = y0 + h * (1.0 - (DYN_RANGE_DB - 3.0) / DYN_RANGE_DB);
    draw_line(x0, y3, x0 + w, y3, 1.0, Color::new(1.0, 0.3, 0.3, 0.6));
    let n = cut.len().max(2);
    for i in 1..cut.len() {
        let xa = x0 + (i - 1) as f32 / (n - 1) as f32 * w;
        let xb = x0 + i as f32 / (n - 1) as f32 * w;
        draw_line(
            xa,
            y0 + h * (1.0 - cut[i - 1]),
            xb,
            y0 + h * (1.0 - cut[i]),
            1.5,
            YELLOW,
        );
    }
    draw_text(label, x0 + 4.0, y0 + 16.0, 15.0, LIGHTGRAY);
    draw_text("0dB", x0 + w - 34.0, y0 + 14.0, 12.0, GRAY);
}

/// GUI-Hauptschleife (als Future für `macroquad::Window::new`).
pub async fn run_gui(cfg: GuiConfig) {
    let geo = SceneGeometry::default_scene(cfg.size, cfg.size, cfg.num_pulses);
    let radar = RadarParams::x_band();
    println!("Simuliere {:?}-Phantom …", cfg.phantom);
    let targets = build_phantom(cfg.phantom, geo);
    let raw = simulate(geo, radar, &targets);
    let mut pipe = match SarPipeline::new(geo, radar, &raw) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Fehler: {e}");
            return;
        }
    };
    let step = (cfg.num_pulses / 64).max(1);
    let mut limit = cfg.limit.unwrap_or(cfg.num_pulses).clamp(1, cfg.num_pulses);
    let mut cross: Option<(u32, u32)> = None;

    let n16 = cfg.size.min(1024) as u16;
    let mut image = Image::gen_image_color(n16, n16, BLACK);
    let texture = Texture2D::from_image(&image);
    texture.set_filter(FilterMode::Nearest);

    // Erstes Bild rechnen (kann je nach Größe dauern).
    let mut current = match pipe.run(limit) {
        Ok(img) => img,
        Err(e) => {
            eprintln!("Fehler: {e}");
            return;
        }
    };
    println!("Apertur-Pulse: {limit}/{}", cfg.num_pulses);

    let mut frame: u32 = 0;
    loop {
        // — Eingaben —
        if is_key_pressed(KeyCode::Right) {
            limit = (limit + step).min(cfg.num_pulses);
            match pipe.run(limit) {
                Ok(img) => {
                    current = img;
                    println!("Apertur-Pulse: {limit}/{}", cfg.num_pulses);
                }
                Err(e) => eprintln!("Fehler: {e}"),
            }
        }
        if is_key_pressed(KeyCode::Left) {
            limit = limit.saturating_sub(step).max(1);
            match pipe.run(limit) {
                Ok(img) => {
                    current = img;
                    println!("Apertur-Pulse: {limit}/{}", cfg.num_pulses);
                }
                Err(e) => eprintln!("Fehler: {e}"),
            }
        }
        if is_key_pressed(KeyCode::R) {
            limit = cfg.num_pulses;
            match pipe.run(limit) {
                Ok(img) => {
                    current = img;
                    println!("Apertur-Pulse: {limit}/{}", cfg.num_pulses);
                }
                Err(e) => eprintln!("Fehler: {e}"),
            }
        }
        if is_mouse_button_pressed(MouseButton::Left) {
            let (mx, my) = mouse_position();
            cross = screen_to_pixel(mx, my, cfg.size);
        }
        if is_key_pressed(KeyCode::Escape) {
            break;
        }

        // — Zeichnen —
        clear_background(DARKGRAY);
        image.update(&to_colors(&image_to_rgb(&current, cfg.size, cfg.size)));
        texture.update(&image);
        draw_texture_ex(
            &texture,
            IMG_X,
            IMG_Y,
            WHITE,
            DrawTextureParams {
                dest_size: Some(vec2(IMG_S, IMG_S)),
                ..Default::default()
            },
        );
        // Peak-Markierung (gelb).
        let (peak, _) = peak_power(&current);
        let (pkx, pky) = (peak as u32 % cfg.size, peak as u32 / cfg.size);
        let sx = IMG_X + (pkx as f32 + 0.5) / cfg.size as f32 * IMG_S;
        let sy = IMG_Y + (pky as f32 + 0.5) / cfg.size as f32 * IMG_S;
        draw_line(sx - 8.0, sy, sx + 8.0, sy, 2.0, YELLOW);
        draw_line(sx, sy - 8.0, sx, sy + 8.0, 2.0, YELLOW);

        // Fadenkreuz.
        if let Some((cx, cy)) = cross {
            let fx = IMG_X + (cx as f32 + 0.5) / cfg.size as f32 * IMG_S;
            let fy = IMG_Y + (cy as f32 + 0.5) / cfg.size as f32 * IMG_S;
            draw_line(IMG_X, fy, IMG_X + IMG_S, fy, 1.0, RED);
            draw_line(fx, IMG_Y, fx, IMG_Y + IMG_S, 1.0, RED);
            draw_circle(fx, fy, 4.0, RED);
        }

        // Rechte Spalte: Status + Schnitte.
        let px = 592.0;
        draw_text("SAR TDBP", px, 40.0, 22.0, WHITE);
        draw_text(
            format!("Phantom: {:?}", cfg.phantom),
            px,
            64.0,
            16.0,
            LIGHTGRAY,
        );
        draw_text(
            format!("Pulse: {limit}/{}", cfg.num_pulses),
            px,
            86.0,
            16.0,
            LIGHTGRAY,
        );
        draw_text(
            format!("Bild: {}x{}", cfg.size, cfg.size),
            px,
            108.0,
            16.0,
            LIGHTGRAY,
        );
        if let Some((cx, cy)) = cross {
            let (range, azimuth) = profiles_db(&current, cfg.size, cfg.size, cx, cy);
            draw_profile(px, 126.0, 196.0, 150.0, "Azimuth", &azimuth);
            draw_profile(px, 292.0, 196.0, 150.0, "Range", &range);
            draw_text(format!("x={cx} y={cy}"), px, 462.0, 15.0, WHITE);
        } else {
            draw_text("Klick: Schnitt", px, 150.0, 16.0, GRAY);
        }
        draw_text("<-/->: Pulse", px, 512.0, 15.0, LIGHTGRAY);
        draw_text("R: voll  Esc: Ende", px, 532.0, 15.0, LIGHTGRAY);

        // Screenshot-Modus für Headless-Tests.
        if let (Some(max_frames), Some(path)) = (cfg.frames, &cfg.screenshot) {
            frame += 1;
            if frame >= max_frames {
                let s = path.to_string_lossy().to_string();
                get_screen_data().export_png(&s);
                println!("Screenshot: {s} (Apertur-Pulse: {limit})");
                return;
            }
        }

        next_frame().await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn impulse(n: u32) -> Vec<Complex32> {
        let mut img = vec![Complex32::zero(); (n * n) as usize];
        img[(n as usize / 2 * n as usize) + n as usize / 2] = Complex32::new(1.0, 0.0);
        img
    }

    #[test]
    fn schnitte_lage_und_norm() {
        let (range, azimuth) = profiles_db(&impulse(16), 16, 16, 8, 8);
        assert_eq!((range.len(), azimuth.len()), (16, 16));
        // Peak an Index 8 beider Schnitte, Rest 0 (dB-Minimum).
        assert_eq!(range[8], 1.0);
        assert_eq!(azimuth[8], 1.0);
        assert!(range.iter().enumerate().all(|(i, &v)| i == 8 || v == 0.0));
        assert!(azimuth.iter().enumerate().all(|(i, &v)| i == 8 || v == 0.0));
        // Alle Werte in [0, 1].
        let img = impulse(16);
        let (r, a) = profiles_db(&img, 16, 16, 3, 5);
        assert!(r.iter().chain(a.iter()).all(|&v| (0.0..=1.0).contains(&v)));
    }

    #[test]
    fn maus_mapping() {
        assert_eq!(screen_to_pixel(20.0, 20.0, 256), Some((0, 0)));
        assert_eq!(screen_to_pixel(579.9, 579.9, 256), Some((255, 255)));
        assert_eq!(screen_to_pixel(300.0, 300.0, 256), Some((128, 128)));
        assert_eq!(screen_to_pixel(0.0, 0.0, 256), None);
        assert_eq!(screen_to_pixel(700.0, 300.0, 256), None);
    }
}
