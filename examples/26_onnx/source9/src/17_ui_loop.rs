//! `17_ui_loop` — Fenster-Schleife, Tasten, Worker-Thread.
//!
//! Die `Engine` (ONNX-Sessions) lebt im Worker-Thread; die Schleife
//! schickt höchstens ein Sample voraus (E8: UI bleibt reaktiv).
//! Start nur mit intakten Assets (Warmup-Sample vor dem Fenster).
//! Ende per `Q`/gehaltenem `Escape` → Report auf stdout, Exit 0.

use std::path::Path;
use std::sync::mpsc::{Receiver, Sender, channel};

use macroquad::prelude::*;

use crate::engine::{Engine, Sample, Settings};
use crate::lang::LANGS;
use crate::render::{CANVAS, font_bytes};
use crate::rng::Rng;
use crate::stats::Stats;
use crate::ui_draw::{draw_hud, draw_sample, draw_waiting};
use crate::ui_state::{Action, UiState, apply};

/// Fenster-Titel (sucht `scripts/smoke_xvfb.sh` per `xdotool`).
pub const TITLE: &str = "Unicode OCR Round Trip";

struct Req {
    settings: Settings,
    seed: u64,
}

fn worker(mut engine: Engine, rx: Receiver<Req>, tx: Sender<Result<Sample, String>>) {
    while let Ok(req) = rx.recv() {
        if tx.send(engine.run(&req.settings, req.seed)).is_err() {
            break;
        }
    }
}

fn window_conf() -> Conf {
    Conf {
        window_title: TITLE.to_string(),
        window_width: CANVAS as i32,
        window_height: CANVAS as i32,
        ..Default::default()
    }
}

/// Öffnet das Fenster (blockiert bis `Q`/`Escape`).
pub fn run_window() -> Result<(), String> {
    let bytes = font_bytes(None)?;
    let mut engine = Engine::open(Path::new("models"), None, Some(Path::new("corpus")))?;
    engine.run(&Settings::default(), 0)?; // Warmup: lädt erste Sessions
    let (req_tx, req_rx) = channel();
    let (resp_tx, resp_rx) = channel();
    std::thread::spawn(move || worker(engine, req_rx, resp_tx));
    macroquad::Window::from_config(window_conf(), ui_main(req_tx, resp_rx, bytes));
    Ok(())
}

async fn ui_main(
    req_tx: Sender<Req>,
    resp_rx: Receiver<Result<Sample, String>>,
    font_bytes: Vec<u8>,
) {
    let font = load_ttf_font_from_bytes(&font_bytes).expect("hud font parses");
    let mut img = Image::gen_image_color(CANVAS as u16, CANVAS as u16, WHITE);
    let tex = Texture2D::from_image(&img);
    tex.set_filter(FilterMode::Nearest);

    let mut state = UiState::default();
    let mut stats = Stats::new();
    let mut current: Option<Sample> = None;
    let mut pending = false;
    let mut want_step = false;
    let mut seed = 1u64;
    let mut rng = Rng::new(0x5EED);

    loop {
        let mut quit = false;
        for &(key, action) in keys() {
            if is_key_pressed(key) {
                let fx = apply(&mut state, action);
                quit |= fx.quit;
                want_step |= fx.step;
                if fx.clear {
                    stats = Stats::new();
                }
            }
        }
        if is_key_down(KeyCode::Escape) || is_key_down(KeyCode::Q) {
            quit = true; // gehalten trifft garantiert (kein Frame-Lücken-Problem)
        }
        if quit {
            break;
        }

        if (!state.paused || want_step) && !pending {
            let mut settings = state.settings.clone();
            if state.random_lang {
                settings.lang = rng.below(LANGS.len());
            }
            if req_tx.send(Req { settings, seed }).is_err() {
                break; // Worker weg
            }
            pending = true;
            want_step = false;
            seed = seed.wrapping_add(1);
        }
        while let Ok(resp) = resp_rx.try_recv() {
            pending = false;
            match resp {
                Ok(sample) => {
                    img.bytes.copy_from_slice(&sample.rgba);
                    tex.update(&img);
                    stats.add(&sample.lang, &sample.model, &sample.eval, sample.times);
                    current = Some(sample);
                }
                Err(e) => {
                    eprintln!("worker: {e}");
                    quit = true;
                }
            }
        }
        if quit {
            break;
        }

        clear_background(BLACK);
        match &current {
            Some(s) => {
                draw_sample(&tex, s, state.view, &font);
                draw_hud(s, &stats, &state, &font);
            }
            None => draw_waiting(&font),
        }
        next_frame().await;
    }
    print!("{}", stats.markdown());
}

/// Taste → Aktion.
fn keys() -> &'static [(KeyCode, Action)] {
    use Action::*;
    use KeyCode::*;
    &[
        (Right, NextLang),
        (Left, PrevLang),
        (R, ToggleRandom),
        (G, NextGen),
        (V, NextView),
        (Up, Bigger),
        (Down, Smaller),
        (M, ToggleModel),
        (Space, TogglePause),
        (N, Step),
        (C, Clear),
        (Q, Quit),
    ]
}
