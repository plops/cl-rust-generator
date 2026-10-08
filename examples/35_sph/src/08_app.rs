//! Interaktive GUI-App: Event-Loop, Dam Break, Maus/Tasten.
//!
//! Start synchron via `macroquad::Window` (kein `#[macroquad::main]`, damit
//! `--headless` ohne Fenster bleibt). Weltmathematik mit `glam::Vec2`:
//! Hindernis folgt der Maus, Linksklick = Wirbel, Rechtsklick = Strahl,
//! R/Space/G/C/S/Esc wie im HUD beschrieben.

use glam::Vec2;
use macroquad::prelude::*;

use crate::backend::{Backend, CpuBackend};
use crate::params::Cli;
use crate::renderer::{ColorMode, HudState, ViewState, draw_frame, screen_to_world};
use crate::water_style::{PARTICLE_R_FACTOR, soft_sprite_image};

#[cfg(feature = "gpu")]
use crate::backend::GpuBackend;

/// Fensterkonfiguration (Titel + 1280×800).
fn window_conf() -> macroquad::conf::Conf {
    macroquad::conf::Conf {
        miniquad_conf: macroquad::miniquad::conf::Conf {
            window_title: "SPH Fluid – cuda-oxide".to_string(),
            window_width: 1280,
            window_height: 800,
            ..Default::default()
        },
        ..Default::default()
    }
}

/// Startet die GUI (blockiert bis Esc/Fenster-Schließen).
pub fn run(cli: Cli) {
    macroquad::Window::from_config(window_conf(), async_main(cli));
}

/// Wählt GPU (außer `--cpu`), fällt bei GPU-Fehler auf CPU zurück.
fn make_backend(
    cfg: &crate::params::SimConfig,
    #[cfg_attr(not(feature = "gpu"), allow(unused_variables))] cli: &Cli,
) -> (Box<dyn Backend>, &'static str) {
    #[cfg(feature = "gpu")]
    if !cli.cpu {
        match GpuBackend::new(cfg) {
            Ok(gpu) => return (Box::new(gpu), "GPU"),
            Err(e) => eprintln!("GPU-Backend fehlgeschlagen ({e}), falle auf CPU zurück."),
        }
    }
    (Box::new(CpuBackend::new(cfg)), "CPU")
}

/// Ereignisschleife: Eingaben → Sub-Steps → Zeichnen.
async fn async_main(cli: Cli) {
    let cfg = cli.sim_config();
    let (mut backend, backend_name) = make_backend(&cfg, &cli);
    backend.reset(&cfg.dam_break());

    let mut obstacle = Vec2::new(0.7 * cfg.domain_w, 0.45 * cfg.domain_h);
    let obstacle_r = 0.08;
    let mut color_mode = ColorMode::Velocity;
    let mut paused = false;
    let mut gravity_on = true;
    let mut step_once = false;
    let mut jet_cursor = 0u32;
    let mut steps = 0u64;
    let mut frames = 0u64;
    // FPS-Messung: erste 5 Frames (Warmup) ausschließen.
    let mut t_start = std::time::Instant::now();
    let mut phys_total = std::time::Duration::ZERO;
    let mut draw_total = std::time::Duration::ZERO;
    let mut present_total = std::time::Duration::ZERO;
    let view = ViewState {
        domain_w: cfg.domain_w,
        domain_h: cfg.domain_h,
        color_mode,
        rest_density: cfg.rest_density,
        particle_r: PARTICLE_R_FACTOR * cfg.initial_spacing(),
        trails: true,
    };
    let mut view = view;
    // Soft-Sprite einmal erzeugen (weißer Radialverlauf, linear gefiltert).
    let sprite = Texture2D::from_image(&soft_sprite_image());
    sprite.set_filter(FilterMode::Linear);

    loop {
        // Tasten (kantengetriggert).
        if is_key_pressed(KeyCode::Escape) {
            break;
        }
        if is_key_pressed(KeyCode::R) {
            backend.reset(&cfg.dam_break());
            steps = 0;
        }
        if is_key_pressed(KeyCode::Space) {
            paused = !paused;
        }
        if is_key_pressed(KeyCode::S) && paused {
            step_once = true;
        }
        if is_key_pressed(KeyCode::G) {
            gravity_on = !gravity_on;
        }
        if is_key_pressed(KeyCode::C) {
            color_mode.toggle();
            view.color_mode = color_mode;
        }
        if is_key_pressed(KeyCode::T) {
            view.trails = !view.trails;
        }
        // Maus → Welt; Hindernis folgt dem Cursor (in Domäne geklemmt).
        let (mx, my) = mouse_position();
        let mw = screen_to_world(
            mx,
            my,
            screen_width(),
            screen_height(),
            cfg.domain_w,
            cfg.domain_h,
        );
        let mouse = Vec2::new(mw[0], mw[1]);
        // Hindernis folgt dem Cursor, solange er in der Domäne liegt.
        if mouse.cmpge(Vec2::ZERO).all() && mouse.cmplt(Vec2::new(cfg.domain_w, cfg.domain_h)).all()
        {
            obstacle = mouse.clamp(
                Vec2::new(obstacle_r, obstacle_r),
                Vec2::new(cfg.domain_w - obstacle_r, cfg.domain_h - obstacle_r),
            );
        }
        let mut inter = crate::types::InteractParams {
            mouse: mouse.to_array(),
            mouse_mode: 0,
            obstacle: obstacle.to_array(),
            obstacle_r,
            gravity_on: if gravity_on { 1.0 } else { 0.0 },
            jet_start: 0,
            jet_count: 0,
            jet_vel: [2.5, 0.5],
        };
        if is_mouse_button_down(MouseButton::Left) {
            inter.mouse_mode = 1;
        }
        if is_mouse_button_down(MouseButton::Right) {
            inter.mouse_mode = 2;
            inter.jet_start = jet_cursor;
            inter.jet_count = 96;
            jet_cursor = (jet_cursor + 96) % cfg.particles as u32;
        }
        backend.set_interact(inter);
        // Physik (inkl. Download für den Renderer).
        let t_phys = std::time::Instant::now();
        if !paused || step_once {
            for _ in 0..cfg.substeps {
                backend.step();
                steps += 1;
            }
            step_once = false;
        }
        backend.sync_host();
        if frames >= 5 {
            phys_total += t_phys.elapsed();
        }
        // Zeichnen (CPU-Schleife) vs. Present (GL + X11) getrennt messen.
        let t_draw = std::time::Instant::now();
        // Zeichnen.
        let hud = HudState {
            fps: get_fps(),
            steps,
            paused,
            gravity_on,
            backend: backend_name,
            particles: cfg.particles,
        };
        draw_frame(
            backend.as_ref(),
            &view,
            &hud,
            &sprite,
            obstacle.to_array(),
            obstacle_r,
            mouse.to_array(),
            inter.mouse_mode,
        );
        if frames >= 5 {
            draw_total += t_draw.elapsed();
        }
        let t_present = std::time::Instant::now();
        next_frame().await;
        if frames >= 5 {
            present_total += t_present.elapsed();
        }
        frames += 1;
        if frames == 5 {
            t_start = std::time::Instant::now();
        }
        if frames.is_multiple_of(60) {
            println!(
                "frame={frames} fps={} steps={steps} backend={backend_name}",
                get_fps()
            );
        }
        if cli.frames.is_some_and(|max| frames >= max) {
            let m = (frames - 5).max(1) as f64;
            let wall_s = t_start.elapsed().as_secs_f64();
            let frame_ms = wall_s * 1000.0 / m;
            let phys_ms = phys_total.as_secs_f64() * 1000.0 / m;
            let draw_ms = draw_total.as_secs_f64() * 1000.0 / m;
            let present_ms = present_total.as_secs_f64() * 1000.0 / m;
            println!(
                "Smoke-Test: {frames} Frames gerendert, beende. Ø {frame_ms:.2} ms/Frame ({:.1} FPS), Physik Ø {phys_ms:.2} ms, Draw Ø {draw_ms:.2} ms, Present Ø {present_ms:.2} ms.",
                m / wall_s,
            );
            break;
        }
    }
}
