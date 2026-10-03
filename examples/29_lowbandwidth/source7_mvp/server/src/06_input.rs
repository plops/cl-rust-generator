//! `06_input` — Eingabe-Injektion per `enigo` (Maus + Tastatur).
//! Client-Koordinaten sind relativ zum Capture-Ausschnitt; der
//! Ausschnitt-Offset wird hier auf Bildschirm-Koordinaten addiert.

use enigo::{Button, Coordinate, Direction, Enigo, Key, Keyboard, Mouse, Settings};

use lbw_common::ClientMsg;

/// Injiziert Client-Eingaben ins lokale Display (`$DISPLAY`).
pub struct Injector {
    enigo: Enigo,
    ox: i32,
    oy: i32,
}

impl Injector {
    /// `offset`: linke obere Ecke des Capture-Ausschnitts am Bildschirm.
    pub fn open(offset: (u32, u32)) -> Result<Self, String> {
        let enigo = Enigo::new(&Settings::default()).map_err(|e| format!("enigo: {e}"))?;
        Ok(Self {
            enigo,
            ox: offset.0 as i32,
            oy: offset.1 as i32,
        })
    }

    /// Führt eine Client-Nachricht aus (Hello wird ignoriert).
    pub fn handle(&mut self, m: &ClientMsg) -> Result<(), String> {
        match m {
            ClientMsg::Hello { .. } => Ok(()),
            ClientMsg::MouseMove { x, y } => self
                .enigo
                .move_mouse(
                    self.ox + i32::from(*x),
                    self.oy + i32::from(*y),
                    Coordinate::Abs,
                )
                .map_err(|e| format!("mouse: {e}")),
            ClientMsg::Button { button, down } => {
                let b = match button {
                    2 => Button::Middle,
                    3 => Button::Right,
                    _ => Button::Left,
                };
                let d = if *down {
                    Direction::Press
                } else {
                    Direction::Release
                };
                self.enigo.button(b, d).map_err(|e| format!("button: {e}"))
            }
            ClientMsg::Text(s) => self.enigo.text(s).map_err(|e| format!("text: {e}")),
            ClientMsg::Key { key, down } => {
                let d = if *down {
                    Direction::Press
                } else {
                    Direction::Release
                };
                match key_code(key) {
                    Some(k) => self.enigo.key(k, d).map_err(|e| format!("key: {e}")),
                    None => Err(format!("unbekannte Taste {key:?}")),
                }
            }
        }
    }
}

/// Sonder-Tastenname → enigo-Taste (vgl. Client `05_app`).
fn key_code(name: &str) -> Option<Key> {
    Some(match name {
        "Enter" => Key::Return,
        "Esc" => Key::Escape,
        "Tab" => Key::Tab,
        "Backspace" => Key::Backspace,
        "Delete" => Key::Delete,
        "Up" => Key::UpArrow,
        "Down" => Key::DownArrow,
        "Left" => Key::LeftArrow,
        "Right" => Key::RightArrow,
        "Home" => Key::Home,
        "End" => Key::End,
        "PageUp" => Key::PageUp,
        "PageDown" => Key::PageDown,
        "Shift" => Key::Shift,
        "Control" => Key::Control,
        "Alt" => Key::Option,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_keys_resolve() {
        for k in ["Enter", "Esc", "Tab", "Left", "Control", "Alt"] {
            assert!(key_code(k).is_some(), "{k}");
        }
        assert!(key_code("F13").is_none());
    }
}
