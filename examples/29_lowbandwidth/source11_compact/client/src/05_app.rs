//! `05_app` — macroquad-Schleife: Textur + Text + HUD rendern,
//! Maus/Tastatur an den Server schicken. Fest 1280×720, ohne Skalierung.

use lbw_common::{ClientMsg, HEIGHT, WIDTH};
use macroquad::prelude::*;

use crate::config::Config;
use crate::font::FontStack;
use crate::net::{Metrics, Net};
use crate::scene::{Link, Scene};

/// Startet den Client (läuft bis zum Fensterschluss).
pub async fn run(cfg: Config) {
    let token = match cfg.token_bytes() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("lbw-client: {e}");
            return;
        }
    };
    let net = Net::connect(&cfg.connect, &token, cfg.tap);
    let mut fonts = FontStack::load(cfg.font.as_deref());
    let mut scene = Scene::new();
    let texture = Texture2D::from_image(&Image {
        bytes: scene.canvas.clone(),
        width: WIDTH as u16,
        height: HEIGHT as u16,
    });
    scene.dirty = false;
    let mut show_hud = true;
    let mut last_mouse = (u16::MAX, u16::MAX);
    loop {
        while let Ok(e) = net.events.try_recv() {
            scene.apply(e);
        }
        if scene.dirty {
            texture.update(&Image {
                bytes: scene.canvas.clone(),
                width: WIDTH as u16,
                height: HEIGHT as u16,
            });
            scene.dirty = false;
        }

        clear_background(BLACK);
        draw_texture(&texture, 0.0, 0.0, WHITE);
        for t in &scene.texts {
            draw_text_item(t, &mut fonts);
        }
        if show_hud {
            draw_top_text(hud(&scene, &net.metrics()).as_str(), 8.0, 8.0, 16.0, YELLOW);
        }

        send_input(&net, &mut last_mouse, &mut show_hud);
        next_frame().await;
    }
}

/// Baseline-Y, sodass die Glyphen-Oberkante an `top` sitzt (reine Geometrie).
/// macroquad-Vertrag (`TextDimensions`-Doku): `draw_text` rendert nach
/// `Rect(X, Y − offset_y, …)` — Y ist also die Baseline, nicht die Oberkante.
fn baseline_y(top: f32, offset_y: f32) -> f32 {
    top + offset_y
}

/// Zeichnet Text mit Oberkante an (`x`, `top`) statt macroquad-Baseline.
fn draw_top_text(text: &str, x: f32, top: f32, size: f32, color: Color) {
    let dims = measure_text(text, None, size as u16, 1.0);
    draw_text(text, x, baseline_y(top, dims.offset_y), size, color);
}

/// Zeichnet ein Textelement: Hintergrund-Box + Glyphen in der Box.
/// Jedes Zeichen kommt aus dem ersten Font, der es enthält (Fallback pro
/// Glyphe statt pro Text — gemischtes CJK/Latein funktioniert).
pub fn draw_text_item(t: &lbw_common::TextItem, fonts: &mut FontStack) {
    let r = t.rect;
    draw_rectangle(
        r.x as f32,
        r.y as f32,
        r.w as f32,
        r.h as f32,
        Color::from_rgba(t.bg[0], t.bg[1], t.bg[2], 255),
    );
    let size = r.h.max(1) as f32;
    let color = Color::from_rgba(t.fg[0], t.fg[1], t.fg[2], 255);
    let mut x = r.x as f32;
    for (slot, run) in fonts.runs(&t.text) {
        let font = fonts.font(slot);
        let dims = measure_text(&run, font, size as u16, 1.0);
        draw_text_ex(
            &run,
            x,
            baseline_y(r.y as f32, dims.offset_y),
            TextParams {
                font,
                font_size: size as u16,
                color,
                ..Default::default()
            },
        );
        x += dims.width;
    }
}

fn hud(s: &Scene, m: &Metrics) -> String {
    let link = match &s.link {
        Link::Connecting => "verbinde…".to_owned(),
        Link::Up => "online".to_owned(),
        Link::Down(why) => format!("offline ({why})"),
    };
    let it = m
        .input_to_text_ms()
        .map(|v| format!("{v:.0} ms"))
        .unwrap_or_else(|| "–".into());
    let ik = m
        .input_to_tile_ms()
        .map(|v| format!("{v:.0} ms"))
        .unwrap_or_else(|| "–".into());
    format!(
        "{link} | {} Texte | {} Kacheln ({} B) | ↓{} B ↑{} B | In→Text {it} In→Kachel {ik} | F1 HUD",
        s.texts.len(),
        s.tiles,
        s.tile_bytes,
        m.rx_bytes,
        m.tx_bytes,
    )
}

/// Liest macroquad-Eingaben und schickt Deltas an den Server.
fn send_input(net: &Net, last_mouse: &mut (u16, u16), show_hud: &mut bool) {
    let (mx, my) = mouse_position();
    let pos = (
        mx.clamp(0.0, (WIDTH - 1) as f32) as u16,
        my.clamp(0.0, (HEIGHT - 1) as f32) as u16,
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
            });
        }
        if is_mouse_button_released(btn) {
            net.send(ClientMsg::Button {
                button: code,
                down: false,
            });
        }
    }
    while let Some(c) = get_char_pressed() {
        if !c.is_control() {
            net.send(ClientMsg::Text(c.to_string()));
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
            });
        }
        if is_key_released(key) {
            net.send(ClientMsg::Key {
                key: name.into(),
                down: false,
            });
        }
    }
    if is_key_pressed(KeyCode::F1) {
        *show_hud = !*show_hud;
    }
}

#[cfg(test)]
mod tests {
    use super::baseline_y;

    #[test]
    fn baseline_places_ink_top_at_box_top() {
        // macroquad-Vertrag: Ink-Oberkante = Y − offset_y → Y = top + offset_y.
        // (Dass macroquad den Vertrag einhält, prüft tests/render.rs per Pixel.)
        assert_eq!(baseline_y(200.0, 32.5), 232.5);
        assert_eq!(baseline_y(200.0, 32.5) - 32.5, 200.0);
        assert_eq!(baseline_y(0.0, 0.0), 0.0);
    }
}
