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

#[cfg(not(test))]
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
fn make_backend(cfg: &crate::params::SimConfig, cli: &Cli) -> (Box<dyn Backend>, &'static str) {
    if !cli.cpu {
        #[cfg(not(test))]
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
    let view = ViewState {
        domain_w: cfg.domain_w,
        domain_h: cfg.domain_h,
        color_mode,
        rest_density: cfg.rest_density,
    };
    let mut view = view;

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
        // Maus → Welt; Hindernis folgt dem Cursor (in Domäne geklemmt).
        let (mx, my) = mouse_position();
        let mw = screen_to_world(mx, my, screen_width(), screen_height(), cfg.domain_w, cfg.domain_h);
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
        // Physik.
        if !paused || step_once {
            for _ in 0..cfg.substeps {
                backend.step();
                steps += 1;
            }
            step_once = false;
        }
        backend.sync_host();
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
            obstacle.to_array(),
            obstacle_r,
            mouse.to_array(),
            inter.mouse_mode,
        );
        next_frame().await;
        frames += 1;
        if frames % 60 == 0 {
            println!("frame={frames} fps={} steps={steps} backend={backend_name}", get_fps());
        }
        if cli.frames.is_some_and(|max| frames >= max) {
            println!("Smoke-Test: {frames} Frames gerendert, beende.");
            break;
        }
    }
}
