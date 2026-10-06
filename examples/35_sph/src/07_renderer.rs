//! macroquad-Renderer: Partikel, Hindernis, HUD.
//!
//! Reine Umrechnungs-/Farbfunktionen sind unit-getestet; `draw_frame`
//! zeichnet einen Backend-Schnappschuss (Welt: Meter, y nach oben).

use macroquad::prelude::*;

use crate::backend::Backend;

/// Partikel-Farbmodus (Taste C schaltet um).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorMode {
    /// Blau→Rot nach Geschwindigkeitsbetrag.
    Velocity,
    /// Grün→Gelb→Rot nach Dichte relativ zu ρ₀.
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

/// Lineare Farbmischung (t = 0 → a, t = 1 → b).
fn mix(a: Color, b: Color, t: f32) -> Color {
    Color::new(
        a.r + (b.r - a.r) * t,
        a.g + (b.g - a.g) * t,
        a.b + (b.b - a.b) * t,
        1.0,
    )
}

/// Blau→Cyan→Gelb→Rot über 0–6 m/s.
pub fn velocity_color(speed: f32) -> Color {
    let t = (speed / 6.0).clamp(0.0, 1.0);
    if t < 0.33 {
        mix(BLUE, SKYBLUE, t / 0.33)
    } else if t < 0.66 {
        mix(SKYBLUE, YELLOW, (t - 0.33) / 0.33)
    } else {
        mix(YELLOW, RED, (t - 0.66) / 0.34)
    }
}

/// Grün (0.3ρ₀) → Gelb (ρ₀) → Rot (1.1ρ₀+).
pub fn density_color(rho: f32, rho0: f32) -> Color {
    let t = ((rho / rho0 - 0.3) / 0.8).clamp(0.0, 1.0);
    if t < 0.875 {
        mix(GREEN, YELLOW, t / 0.875)
    } else {
        mix(YELLOW, RED, (t - 0.875) / 0.125)
    }
}

/// Zeichnet Partikel, Hindernis, Mausindikator und HUD.
pub fn draw_frame(
    backend: &dyn Backend,
    view: &ViewState,
    hud: &HudState,
    obstacle: [f32; 2],
    obstacle_r: f32,
    mouse_world: [f32; 2],
    mouse_mode: u32,
) {
    clear_background(Color::from_rgba(8, 10, 18, 255));
    let (vw, vh) = (screen_width(), screen_height());
    let scale = (vw / view.domain_w).min(view.domain_h);
    // Domänenrahmen.
    let (x0, y1) = world_to_screen([0.0, 0.0], vw, vh, view.domain_w, view.domain_h);
    let (x1, y0) = world_to_screen(
        [view.domain_w, view.domain_h],
        vw,
        vh,
        view.domain_w,
        view.domain_h,
    );
    draw_rectangle_lines(x0, y0, x1 - x0, y1 - y0, 2.0, GRAY);
    // Partikel als gebatchte Rechtecke.
    let size = (0.006 * scale).clamp(2.0, 6.0);
    let half = size * 0.5;
    let pos = backend.positions();
    let vel = backend.velocities();
    let dens = backend.densities();
    for i in 0..pos.len() {
        let (sx, sy) = world_to_screen(pos[i], vw, vh, view.domain_w, view.domain_h);
        let color = match view.color_mode {
            ColorMode::Velocity => {
                velocity_color((vel[i][0] * vel[i][0] + vel[i][1] * vel[i][1]).sqrt())
            }
            ColorMode::Density => density_color(dens[i], view.rest_density),
        };
        draw_rectangle(sx - half, sy - half, size, size, color);
    }
    // Hindernis + Wirbelradius.
    let (ox, oy) = world_to_screen(obstacle, vw, vh, view.domain_w, view.domain_h);
    draw_circle(ox, oy, obstacle_r * scale, Color::new(1.0, 0.3, 0.3, 0.25));
    draw_circle_lines(ox, oy, obstacle_r * scale, 2.0, RED);
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
            "Farbe: {} (C) | Reset: R | Schritt: S",
            view.color_mode.name()
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

    #[test]
    fn farbrampen_sind_gueltig() {
        for v in [0.0, 1.5, 3.0, 6.0, 20.0] {
            let c = velocity_color(v);
            assert!([c.r, c.g, c.b, c.a].iter().all(|x| (0.0..=1.0).contains(x)));
        }
        assert_eq!(velocity_color(0.0), BLUE);
        for r in [0.0, 300.0, 1000.0, 1100.0, 5000.0] {
            let c = density_color(r, 1000.0);
            assert!([c.r, c.g, c.b, c.a].iter().all(|x| (0.0..=1.0).contains(x)));
        }
    }
}
