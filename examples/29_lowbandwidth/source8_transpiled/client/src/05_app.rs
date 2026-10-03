//! `05_app` — macroquad-Schleife: Textur + Text + HUD rendern,
//! Maus/Tastatur an den Server schicken. Fest 640×640, ohne Skalierung.

use lbw_common::{ClientMsg, SIZE};
use macroquad::prelude::*;

use crate::config::Config;
use crate::net::Net;
use crate::scene::{Link, Scene};

/// Startet den Client (läuft bis zum Fensterschluss).
pub async fn run(cfg: Config) {
    let net = Net::connect(&cfg.connect);
    {
        let mut scene = Scene::new();
        {
            let texture = Texture2D::from_image(
                &(Image {
                    bytes: scene.canvas.clone(),
                    width: SIZE as u16,
                    height: SIZE as u16,
                }),
            );
            scene.dirty = false;
            {
                let mut show_hud = true;
                {
                    let mut last_mouse = (u16::MAX, u16::MAX);
                    loop {
                        while let Ok(e) = net.events.try_recv() {
                            scene.apply(e)
                        }
                        if scene.dirty {
                            texture.update(
                                &(Image {
                                    bytes: scene.canvas.clone(),
                                    width: SIZE as u16,
                                    height: SIZE as u16,
                                }),
                            );
                            scene.dirty = false;
                        }
                        clear_background(BLACK);
                        draw_texture(&texture, 0.0, 0.0, WHITE);
                        for t in &scene.texts {
                            let r = t.rect;
                            draw_rectangle(
                                r.x as f32,
                                r.y as f32,
                                r.w as f32,
                                r.h as f32,
                                Color::from_rgba(t.bg[0], t.bg[1], t.bg[2], 255),
                            );
                            draw_text(
                                &t.text,
                                r.x as f32,
                                r.y as f32,
                                r.h as f32,
                                Color::from_rgba(t.fg[0], t.fg[1], t.fg[2], 255),
                            );
                        }
                        if show_hud {
                            draw_text(hud(&scene).as_str(), 8.0, 16.0, 16.0, YELLOW);
                        }
                        send_input(&net, &mut last_mouse, &mut show_hud);
                        next_frame().await
                    }
                }
            }
        }
    }
}

fn hud(s: &Scene) -> String {
    let link = match &s.link {
        Link::Connecting => "verbinde…".to_owned(),
        Link::Up => "online".to_owned(),
        Link::Down(why) => {
            format!("offline ({why})")
        }
    };
    format!(
        "{link} | {} Texte | {} Kacheln ({} B) | F1 HUD",
        s.texts.len(),
        s.tiles,
        s.tile_bytes
    )
}

/// Liest macroquad-Eingaben und schickt Deltas an den Server.
fn send_input(net: &Net, last_mouse: &mut (u16, u16), show_hud: &mut bool) {
    let mp = mouse_position();
    {
        let mx = mp.0;
        {
            let my = mp.1;
            {
                let pos = (
                    mx.clamp(0.0, (SIZE - 1) as f32) as u16,
                    my.clamp(0.0, (SIZE - 1) as f32) as u16,
                );
                if pos != *last_mouse {
                    net.send(ClientMsg::MouseMove { x: pos.0, y: pos.1 });
                    *last_mouse = pos;
                }
                for (btn, code) in [
                    (MouseButton::Left, 1),
                    (MouseButton::Middle, 2),
                    (MouseButton::Right, 3),
                ] {
                    if is_mouse_button_pressed(btn) {
                        net.send(ClientMsg::Button {
                            button: code,
                            down: true,
                        })
                    }
                    if is_mouse_button_released(btn) {
                        net.send(ClientMsg::Button {
                            button: code,
                            down: false,
                        })
                    }
                }
                while let Some(c) = get_char_pressed() {
                    if !c.is_control() {
                        net.send(ClientMsg::Text(c.to_string()))
                    }
                }
                for (key, name) in [
                    (KeyCode::Enter, "Enter"),
                    (KeyCode::Escape, "Esc"),
                    (KeyCode::Tab, "Tab"),
                    (KeyCode::Backspace, "Backspace"),
                    (KeyCode::Delete, "Delete"),
                    (KeyCode::Up, "Up"),
                    (KeyCode::Down, "Down"),
                    (KeyCode::Left, "Left"),
                    (KeyCode::Right, "Right"),
                    (KeyCode::Home, "Home"),
                    (KeyCode::End, "End"),
                    (KeyCode::PageUp, "PageUp"),
                    (KeyCode::PageDown, "PageDown"),
                    (KeyCode::LeftShift, "Shift"),
                    (KeyCode::RightShift, "Shift"),
                    (KeyCode::LeftControl, "Control"),
                    (KeyCode::RightControl, "Control"),
                    (KeyCode::LeftAlt, "Alt"),
                    (KeyCode::RightAlt, "Alt"),
                ] {
                    if is_key_pressed(key) {
                        net.send(ClientMsg::Key {
                            key: name.into(),
                            down: true,
                        })
                    }
                    if is_key_released(key) {
                        net.send(ClientMsg::Key {
                            key: name.into(),
                            down: false,
                        })
                    }
                }
                if is_key_pressed(KeyCode::F1) {
                    *show_hud = !*show_hud;
                }
            }
        }
    }
}
