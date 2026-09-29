//! `08_app` — Hauptschleife: Netz-Ereignisse anwenden, Eingaben senden,
//! lokale Tasten (F1 HUD, F2 Auswahl, F3 Einfügen), zeichnen.

use std::time::{Duration, Instant};

use lbw_common::Input;
use macroquad::prelude::*;

use crate::config::Config;
use crate::input::{MouseThrottle, char_input, key_input, mod_bits, special_keysym};
use crate::net::{Event, Net, NetCfg, spawn};
use crate::render::{Renderer, load_font};
use crate::scene::Scene;
use crate::select::{drag_rect, paste_chunks, selected_text};

/// Tastenwiederholung für gehaltene Sondertasten (macroquad meldet nur den ersten Druck).
const REPEAT_DELAY: Duration = Duration::from_millis(450);
const REPEAT_EVERY: Duration = Duration::from_millis(60);

/// Protokollzeile je Ereignis (`--dump-text`), mit Sekunden seit Start.
fn log_event(e: &Event, t0: Instant) {
    let t = t0.elapsed().as_secs_f64();
    match e {
        Event::Text { remove, add } => {
            for id in remove {
                println!("TEXT - {id} t={t:.3}");
            }
            for x in add {
                let r = x.rect;
                println!(
                    "TEXT + {} {} {} {} {} {:?} t={t:.3}",
                    x.id, r.x, r.y, r.w, r.h, x.text
                );
            }
        }
        Event::Tile { rect, bytes, .. } => {
            println!(
                "TILE {} {} {} {} {bytes} t={t:.3}",
                rect.x, rect.y, rect.w, rect.h
            );
        }
        Event::Connected { resumed, .. } => println!("CONNECTED resumed={resumed} t={t:.3}"),
        Event::Disconnected(why) => println!("DISCONNECTED {why} t={t:.3}"),
        Event::Clear => println!("CLEAR t={t:.3}"),
        Event::Stats { rate, backlog, .. } => println!("STATS {rate} {backlog} t={t:.3}"),
    }
}

fn mods_now() -> u8 {
    let d = |a, b| is_key_down(a) || is_key_down(b);
    mod_bits(
        d(KeyCode::LeftShift, KeyCode::RightShift),
        d(KeyCode::LeftControl, KeyCode::RightControl),
        d(KeyCode::LeftAlt, KeyCode::RightAlt),
        d(KeyCode::LeftSuper, KeyCode::RightSuper),
    )
}

/// Lokaler UI-Zustand.
struct Ui {
    hud: bool,
    select: bool,
    drag: Option<(f32, f32)>,
    mouse: MouseThrottle,
    held: Option<(KeyCode, Instant)>,
    scale: f32,
}

fn server_position((x, y): (f32, f32), scale: f32) -> (f32, f32) {
    (x / scale, y / scale)
}

impl Ui {
    fn keys(&mut self, net: &Net, s: &Scene) {
        let m = mods_now();
        for k in get_keys_pressed() {
            match k {
                KeyCode::F1 => self.hud = !self.hud,
                KeyCode::F2 => {
                    self.select = !self.select;
                    self.drag = None;
                }
                KeyCode::F3 => {
                    if let Some(c) = miniquad::window::clipboard_get() {
                        for part in paste_chunks(&c) {
                            net.send(Input::Text(part));
                        }
                    }
                }
                _ => {
                    if let Some(i) = key_input(k, m) {
                        net.send(i);
                        self.held = special_keysym(k).map(|_| (k, Instant::now() + REPEAT_DELAY));
                    }
                }
            }
        }
        // Wiederholung gehaltener Sondertasten (Backspace, Pfeile …).
        if let Some((k, next)) = self.held {
            if !is_key_down(k) {
                self.held = None;
            } else if Instant::now() >= next {
                if let Some(i) = key_input(k, m) {
                    net.send(i);
                }
                self.held = Some((k, Instant::now() + REPEAT_EVERY));
            }
        }
        while let Some(c) = get_char_pressed() {
            if let Some(i) = char_input(c, m) {
                net.send(i);
            }
        }
        let _ = s;
    }

    fn mouse(&mut self, net: &Net, s: &Scene) -> Option<lbw_common::Rect> {
        let (x, y) = server_position(mouse_position(), self.scale);
        let (cx, cy) = (
            x.clamp(0.0, s.w as f32 - 1.0) as u16,
            y.clamp(0.0, s.h as f32 - 1.0) as u16,
        );
        if self.select {
            if is_mouse_button_pressed(MouseButton::Left) {
                self.drag = Some((x, y));
            }
            let start = self.drag?;
            let r = drag_rect(start, (x, y));
            if is_mouse_button_released(MouseButton::Left) {
                let txt = selected_text(r, s.texts.values());
                if !txt.is_empty() {
                    miniquad::window::clipboard_set(&txt);
                    eprintln!("[client] kopiert: {} Zeichen", txt.chars().count());
                }
                self.select = false;
                self.drag = None;
            }
            return Some(r);
        }
        let now = Instant::now();
        let buttons = [
            (MouseButton::Left, 1),
            (MouseButton::Middle, 2),
            (MouseButton::Right, 3),
        ];
        let click = buttons
            .iter()
            .any(|(b, _)| is_mouse_button_pressed(*b) || is_mouse_button_released(*b));
        if let Some(i) = self.mouse.update(cx, cy, now, click) {
            net.send(i);
        }
        for (b, n) in buttons {
            if is_mouse_button_pressed(b) {
                net.send(Input::Button {
                    button: n,
                    down: true,
                });
            }
            if is_mouse_button_released(b) {
                net.send(Input::Button {
                    button: n,
                    down: false,
                });
            }
        }
        let wheel = mouse_wheel().1;
        if wheel != 0.0 {
            net.send(Input::Wheel {
                dy: wheel.signum() as i8,
            });
        }
        None
    }
}

/// Läuft bis das Fenster geschlossen wird.
pub async fn run(cfg: Config) {
    let net = spawn(NetCfg {
        addr: cfg.connect.clone(),
        dead_after: cfg.dead_after,
        verbose: cfg.verbose,
    });
    let mut scene = Scene::new(cfg.size, cfg.size);
    let mut r = Renderer::new(cfg.size, cfg.size, load_font(cfg.font.as_deref()), cfg.zoom);
    let mut ui = Ui {
        hud: false,
        select: false,
        drag: None,
        mouse: MouseThrottle::new(30),
        held: None,
        scale: cfg.scale() as f32,
    };
    let t0 = Instant::now();
    loop {
        while let Ok(e) = net.events.try_recv() {
            if cfg.dump_text {
                log_event(&e, t0);
            }

            scene.apply(e);
        }
        ui.keys(&net, &scene);
        let sel = ui.mouse(&net, &scene);
        r.draw(&mut scene, sel, ui.hud);
        next_frame().await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn window_coordinates_map_to_capture_coordinates() {
        assert_eq!(server_position((638.0, 470.0), 2.0), (319.0, 235.0));
        assert_eq!(server_position((638.0, 470.0), 1.0), (638.0, 470.0));
        assert_eq!(
            drag_rect(
                server_position((20.0, 40.0), 2.0),
                server_position((80.0, 100.0), 2.0)
            ),
            lbw_common::Rect::new(10, 20, 31, 31)
        );
    }
}
