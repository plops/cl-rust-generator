//! `06_input` — Eingabe-Injektion per `enigo` (Maus + Tastatur).
//! Client-Koordinaten sind relativ zum Capture-Ausschnitt; Ausschnitt-Offset
//! und Ursprung des primären Monitors (RandR) werden auf absolute
//! Bildschirm-Koordinaten addiert.

use enigo::{Button, Coordinate, Direction, Enigo, Key, Keyboard, Mouse, Settings};
use x11rb::connection::Connection as _;
use x11rb::protocol::randr;

use lbw_common::ClientMsg;

/// Injiziert Client-Eingaben ins lokale Display (`$DISPLAY`).
pub struct Injector {
    enigo: Enigo,
    ox: i32,
    oy: i32,
}

impl Injector {
    /// `offset`: linke obere Ecke des Capture-Ausschnitts im primären
    /// Monitor; dessen RandR-Ursprung kommt dazu (Mehrmonitor-Layouts).
    pub fn open(offset: (u32, u32)) -> Result<Self, String> {
        let enigo = Enigo::new(&Settings::default()).map_err(|e| format!("enigo: {e}"))?;
        {
            let (mx, my) = primary_origin();
            Ok(Self {
                enigo,
                ox: mx + offset.0 as i32,
                oy: my + offset.1 as i32,
            })
        }
    }

    /// Führt eine Client-Nachricht aus (Hello wird ignoriert).
    pub fn handle(&mut self, m: &ClientMsg) -> Result<(), String> {
        match m {
            ClientMsg::Hello { .. } => Ok(()),
            ClientMsg::MouseMove { x, y } => {
                let (ax, ay) = inject_pos(self.ox, self.oy, *x, *y);
                self.enigo
                    .move_mouse(ax, ay, Coordinate::Abs)
                    .map_err(|e| format!("mouse: {e}"))
            }
            ClientMsg::Button { button, down } => {
                let b = match button {
                    2 => Button::Middle,
                    3 => Button::Right,
                    _ => Button::Left,
                };
                {
                    let d = if *down {
                        Direction::Press
                    } else {
                        Direction::Release
                    };
                    self.enigo.button(b, d).map_err(|e| format!("button: {e}"))
                }
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

/// Ursprung des primären Monitors im globalen Bildschirm (RandR
/// `GetMonitors` — dieselbe Quelle, aus der `scrap` den Capture-Monitor
/// wählt). Bei Fehler +0+0 mit Warnung (Ein-Monitor-Verhalten).
fn primary_origin() -> (i32, i32) {
    match query_monitors() {
        Ok(ms) => {
            let (ox, oy) = pick_origin(&ms);
            eprintln!("[input] Monitor-Ursprung {:+}{:+}", ox, oy);
            (ox, oy)
        }
        Err(e) => {
            eprintln!("[input] Monitore nicht abfragbar ({e}) — Ursprung +0+0");
            (0, 0)
        }
    }
}

/// Alle RandR-Monitore als (x, y, primär?) — Display aus `$DISPLAY`.
fn query_monitors() -> Result<Vec<(i32, i32, bool)>, String> {
    let (conn, screen) = x11rb::connect(None).map_err(|e| e.to_string())?;
    {
        let root = conn
            .setup()
            .roots
            .get(screen)
            .ok_or_else(|| format!("Bildschirm {screen} fehlt"))?
            .root;
        {
            let reply = randr::get_monitors(&conn, root, true)
                .map_err(|e| e.to_string())?
                .reply()
                .map_err(|e| e.to_string())?;
            Ok(reply
                .monitors
                .iter()
                .map(|m| (i32::from(m.x), i32::from(m.y), m.primary))
                .collect())
        }
    }
}

/// Wählt den Injektions-Ursprung: primärer Monitor gewinnt, sonst der erste
/// (wie `scrap::Display::primary`), sonst +0+0.
#[must_use]
fn pick_origin(monitors: &[(i32, i32, bool)]) -> (i32, i32) {
    monitors
        .iter()
        .find(|m| m.2)
        .or_else(|| monitors.first())
        .map(|m| (m.0, m.1))
        .unwrap_or((0, 0))
}

/// Client-Punkt → absoluter Bildschirmpunkt (Ursprung + Ausschnitt + Punkt).
#[must_use]
fn inject_pos(ox: i32, oy: i32, x: u16, y: u16) -> (i32, i32) {
    (ox + i32::from(x), oy + i32::from(y))
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
            assert!(key_code(k).is_some(), "{k}")
        }
        assert!(key_code("F13").is_none())
    }

    #[test]
    fn primary_monitor_origin_wins() {
        // Vier-Monitor-Layout: primärer Monitor rechts, hochkant.
        {
            let ms = [
                (4200, 0, true),
                (0, 0, false),
                (3120, 0, false),
                (1920, 0, false),
            ];
            assert_eq!(pick_origin(&ms), (4200, 0))
        }
    }

    #[test]
    fn origin_falls_back_to_first_then_zero() {
        assert_eq!(pick_origin(&[(100, 50, false), (0, 0, false)]), (100, 50));
        assert_eq!(pick_origin(&[]), (0, 0))
    }

    #[test]
    fn injection_adds_origin_and_region() {
        // Monitor +4200+0, Ausschnitt @10,10, Client (550,639).
        assert_eq!(inject_pos(4200 + 10, 10, 550, 639), (4760, 649))
    }
}
