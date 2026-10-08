//! macroquad-Renderer: Partikel, Hindernis, HUD.
//!
//! Reine Umrechnungsfunktionen sind unit-getestet (Farben + Sprite in
//! `07a_water_style.rs`); `draw_frame` zeichnet einen Backend-Schnappschuss
//! (Welt: Meter, y nach oben) als weiche Sprites mit Gischt + Trails.

use macroquad::models::Vertex;
use macroquad::prelude::*;
use macroquad::window::get_internal_gl;

use crate::backend::Backend;
use crate::water_style::{
    TRAIL_FADE_ALPHA, WATER_BG, foam_color, is_foam, water_color, water_density_color,
};

/// Partikel-Farbmodus (Taste C schaltet um).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorMode {
    /// Tiefblau→Türkis→Weiß nach Geschwindigkeitsbetrag.
    Velocity,
    /// Tiefblau→Türkis→Weiß nach Dichte relativ zu ρ₀.
    Density,
}

impl ColorMode {
    /// Wechselt den Modus, gibt den Kurznamen zurück.
    pub fn toggle(&mut self) -> &'static str {
        *self = match self {
            ColorMode::Velocity => ColorMode::Density,
            ColorMode::Density => ColorMode::Velocity,
        };
        self.name()
    }

    /// Deutscher Kurzname fürs HUD.
    pub fn name(&self) -> &'static str {
        match self {
            ColorMode::Velocity => "Geschwindigkeit",
            ColorMode::Density => "Dichte",
        }
    }
}

/// Kamera-/Domänenzustand (Letterbox-Skalierung).
#[derive(Clone, Copy, Debug)]
pub struct ViewState {
    /// Domänenbreite in m.
    pub domain_w: f32,
    /// Domänenhöhe in m.
    pub domain_h: f32,
    /// Aktiver Farbmodus.
    pub color_mode: ColorMode,
    /// Ruhedichte ρ₀ für den Dichte-Farbmodus.
    pub rest_density: f32,
    /// Partikelradius in m (aus Anfangsabstand, siehe Stilkonstante).
    pub particle_r: f32,
    /// Trails an (Fade) statt hartem Clear (Taste T).
    pub trails: bool,
}

/// HUD-Zustand (Text links oben).
pub struct HudState {
    /// Render-FPS.
    pub fps: i32,
    /// Physikschritte seit Reset.
    pub steps: u64,
    /// Pause aktiv.
    pub paused: bool,
    /// Gravitation an.
    pub gravity_on: bool,
    /// Backend-Name ("GPU"/"CPU").
    pub backend: &'static str,
    /// Partikelanzahl N.
    pub particles: usize,
}

/// Welt (m, y oben) → Bildschirm (px, y unten), Letterbox.
pub fn world_to_screen(
    p: [f32; 2],
    view_w: f32,
    view_h: f32,
    domain_w: f32,
    domain_h: f32,
) -> (f32, f32) {
    let scale = (view_w / domain_w).min(view_h / domain_h);
    let ox = (view_w - domain_w * scale) * 0.5;
    let oy = (view_h - domain_h * scale) * 0.5;
    (ox + p[0] * scale, view_h - (oy + p[1] * scale))
}

/// Bildschirm (px) → Welt (m), Umkehrung von `world_to_screen`.
pub fn screen_to_world(
    sx: f32,
    sy: f32,
    view_w: f32,
    view_h: f32,
    domain_w: f32,
    domain_h: f32,
) -> [f32; 2] {
    let scale = (view_w / domain_w).min(view_h / domain_h);
    let ox = (view_w - domain_w * scale) * 0.5;
    let oy = (view_h - domain_h * scale) * 0.5;
    [(sx - ox) / scale, (view_h - sy - oy) / scale]
}

/// Eingefärbtes Sprite-Quad in den laufenden Batch schieben.
///
/// Der Aufrufer setzt Textur + Draw-Mode einmalig (State-Hoisting wie bei
/// der Letterbox-Skalierung); pro Partikel bleibt nur der Geometrie-Push.
/// `draw_texture_ex` wäre ~80 ms (Trigonometrie + Lookups pro Aufruf).
fn push_sprite_quad(
    gl: &mut macroquad::window::InternalGlContext,
    x: f32,
    y: f32,
    size: f32,
    color: Color,
) {
    #[rustfmt::skip]
    let vertices = [
        Vertex::new(x,        y,        0., 0.0, 0.0, color),
        Vertex::new(x + size, y,        0., 1.0, 0.0, color),
        Vertex::new(x + size, y + size, 0., 1.0, 1.0, color),
        Vertex::new(x,        y + size, 0., 0.0, 1.0, color),
    ];
    let indices: [u16; 6] = [0, 1, 2, 0, 2, 3];
    gl.quad_gl.geometry(&vertices, &indices);
}

/// Zeichnet Partikel, Hindernis, Mausindikator und HUD.
///
/// `sprite` ist das einmal erzeugte Soft-Sprite (weißer Radialverlauf);
/// Partikel werden als eingefärbte, skalierte Quads gebatcht.
#[allow(clippy::too_many_arguments)] // Frame-Kontext: 8 explizite Handles statt God-Struct.
pub fn draw_frame(
    backend: &dyn Backend,
    view: &ViewState,
    hud: &HudState,
    sprite: &Texture2D,
    obstacle: [f32; 2],
    obstacle_r: f32,
    mouse_world: [f32; 2],
    mouse_mode: u32,
) {
    let (vw, vh) = (screen_width(), screen_height());
    let (br, bg, bb) = WATER_BG;
    if view.trails {
        // Kein hartes Clear: halbtransparentes Übermalen lässt Schweife stehen.
        draw_rectangle(
            0.0,
            0.0,
            vw,
            vh,
            Color::from_rgba(br, bg, bb, TRAIL_FADE_ALPHA),
        );
    } else {
        clear_background(Color::from_rgba(br, bg, bb, 255));
    }
    // Letterbox-Skalierung einmal pro Frame statt pro Partikel (Divisionen!).
    let scale = (vw / view.domain_w).min(vh / view.domain_h);
    let ox = (vw - view.domain_w * scale) * 0.5;
    let oy = (vh - view.domain_h * scale) * 0.5;
    // Becken: massiver Außenrahmen + feine Innenkante.
    let (x0, y1) = world_to_screen([0.0, 0.0], vw, vh, view.domain_w, view.domain_h);
    let (x1, y0) = world_to_screen(
        [view.domain_w, view.domain_h],
        vw,
        vh,
        view.domain_w,
        view.domain_h,
    );
    draw_rectangle_lines(
        x0 - 5.0,
        y0 - 5.0,
        x1 - x0 + 10.0,
        y1 - y0 + 10.0,
        5.0,
        Color::from_rgba(28, 36, 52, 255),
    );
    draw_rectangle_lines(
        x0,
        y0,
        x1 - x0,
        y1 - y0,
        1.5,
        Color::new(0.35, 0.55, 0.85, 0.9),
    );
    // Partikel als weiche Sprites (Überlappung → Fläche); Gischt kleiner+weiß.
    // Welt→Bild-Transform inline (identische Reihenfolge wie world_to_screen).
    let size = (2.0 * view.particle_r * scale).clamp(3.0, 16.0);
    let foam_size = (size * 0.55).max(2.0);
    let pos = backend.positions();
    let vel = backend.velocities();
    let dens = backend.densities();
    // Textur-State einmalig setzen, dann 16k reine Geometrie-Pushes.
    // SAFETY: Handle lebt nur bis Schleifenende, Main-Thread, keine
    // Reentrancy (Schleifenkörper ruft keine Draw-Funktionen auf).
    let mut gl = unsafe { get_internal_gl() };
    gl.quad_gl.texture(Some(sprite));
    gl.quad_gl.draw_mode(DrawMode::Triangles);
    match view.color_mode {
        ColorMode::Velocity => {
            for i in 0..pos.len() {
                let p = pos[i];
                let sx = ox + p[0] * scale;
                let sy = vh - (oy + p[1] * scale);
                let v = vel[i];
                if is_foam(dens[i], view.rest_density, v[1]) {
                    let h = foam_size * 0.5;
                    push_sprite_quad(&mut gl, sx - h, sy - h, foam_size, foam_color());
                } else {
                    let color = water_color((v[0] * v[0] + v[1] * v[1]).sqrt());
                    let h = size * 0.5;
                    push_sprite_quad(&mut gl, sx - h, sy - h, size, color);
                }
            }
        }
        ColorMode::Density => {
            for i in 0..pos.len() {
                let p = pos[i];
                let sx = ox + p[0] * scale;
                let sy = vh - (oy + p[1] * scale);
                if is_foam(dens[i], view.rest_density, vel[i][1]) {
                    let h = foam_size * 0.5;
                    push_sprite_quad(&mut gl, sx - h, sy - h, foam_size, foam_color());
                } else {
                    let color = water_density_color(dens[i], view.rest_density);
                    let h = size * 0.5;
                    push_sprite_quad(&mut gl, sx - h, sy - h, size, color);
                }
            }
        }
    }
    // `gl` ist hier tot (NLL) — Hindernis/HUD nutzen wieder High-Level-Calls.
    // Hindernis: Schatten + Körper + Glanzpunkt.
    let (ox, oy) = world_to_screen(obstacle, vw, vh, view.domain_w, view.domain_h);
    let orad = obstacle_r * scale;
    draw_circle(ox + 4.0, oy + 4.0, orad, Color::from_rgba(0, 0, 0, 80));
    draw_circle(ox, oy, orad, Color::new(1.0, 0.3, 0.3, 0.25));
    draw_circle_lines(ox, oy, orad, 2.0, RED);
    draw_circle(
        ox - 0.35 * orad,
        oy - 0.35 * orad,
        0.18 * orad,
        Color::new(1.0, 1.0, 1.0, 0.55),
    );
    if mouse_mode == 1 {
        let (mx, my) = world_to_screen(mouse_world, vw, vh, view.domain_w, view.domain_h);
        draw_circle_lines(mx, my, 0.2 * scale, 2.0, SKYBLUE);
    }
    // HUD.
    let hud_color = WHITE;
    let mut y = 24.0;
    for line in [
        format!(
            "SPH cuda-oxide | {} | N={} | {} FPS",
            hud.backend, hud.particles, hud.fps
        ),
        format!(
            "Schritte: {} | {} | Gravitation: {} (G)",
            hud.steps,
            if hud.paused {
                "PAUSE (Space)"
            } else {
                "läuft"
            },
            if hud.gravity_on { "an" } else { "aus" }
        ),
        format!(
            "Farbe: {} (C) | Schweif: {} (T) | Reset: R | Schritt: S",
            view.color_mode.name(),
            if view.trails { "an" } else { "aus" }
        ),
        "Links: Wirbel | Rechts: Strahl | Hindernis folgt Maus | Esc: Ende".to_string(),
    ] {
        draw_text(&line, 12.0, y, 20.0, hud_color);
        y += 22.0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn welt_bildschirm_transformation_ist_invers() {
        let (dw, dh, vw, vh) = (1.6, 1.0, 1280.0, 800.0);
        for p in [[0.0, 0.0], [1.6, 1.0], [0.8, 0.5], [0.13, 0.71]] {
            let (sx, sy) = world_to_screen(p, vw, vh, dw, dh);
            let back = screen_to_world(sx, sy, vw, vh, dw, dh);
            assert!((back[0] - p[0]).abs() < 1e-4, "{back:?} ≈ {p:?}");
            assert!((back[1] - p[1]).abs() < 1e-4, "{back:?} ≈ {p:?}");
        }
        // Ecken landen im sichtbaren Rahmen.
        let (sx, sy) = world_to_screen([0.0, 0.0], vw, vh, dw, dh);
        assert!(sx >= 0.0 && sx <= vw && sy >= 0.0 && sy <= vh);
    }
}
