//! `06_keycode` — Macroquad-`KeyCode` → Protokoll-`Input` (nur Desktop).
//!
//! Sondertasten gehen als `Key{keysym, mods}`, Buchstaben/Ziffern nur
//! zusammen mit Ctrl/Alt/Super (sonst liefert der Zeichen-Pfad `Char`).

use lbw_common::Input;
use lbw_common::keys::{f_key, ks};
use macroquad::input::KeyCode;

use crate::input::is_combo;

/// Keysym für Sondertasten (nicht als Zeichen darstellbar).
#[must_use]
pub fn special_keysym(k: KeyCode) -> Option<u32> {
    use KeyCode as K;
    Some(match k {
        K::Enter | K::KpEnter => ks::RETURN,
        K::Backspace => ks::BACKSPACE,
        K::Tab => ks::TAB,
        K::Escape => ks::ESCAPE,
        K::Delete => ks::DELETE,
        K::Insert => ks::INSERT,
        K::Home => ks::HOME,
        K::End => ks::END,
        K::PageUp => ks::PAGE_UP,
        K::PageDown => ks::PAGE_DOWN,
        K::Left => ks::LEFT,
        K::Right => ks::RIGHT,
        K::Up => ks::UP,
        K::Down => ks::DOWN,
        K::F4 => f_key(4),
        K::F5 => f_key(5),
        K::F6 => f_key(6),
        K::F7 => f_key(7),
        K::F8 => f_key(8),
        K::F9 => f_key(9),
        K::F10 => f_key(10),
        K::F11 => f_key(11),
        K::F12 => f_key(12),
        _ => return None,
    })
}

/// Keysym für Buchstaben/Ziffern/Leertaste (für Ctrl/Alt-Kombinationen).
#[must_use]
pub fn plain_keysym(k: KeyCode) -> Option<u32> {
    use KeyCode as K;
    let letters = [
        K::A,
        K::B,
        K::C,
        K::D,
        K::E,
        K::F,
        K::G,
        K::H,
        K::I,
        K::J,
        K::K,
        K::L,
        K::M,
        K::N,
        K::O,
        K::P,
        K::Q,
        K::R,
        K::S,
        K::T,
        K::U,
        K::V,
        K::W,
        K::X,
        K::Y,
        K::Z,
    ];
    let digits = [
        K::Key0,
        K::Key1,
        K::Key2,
        K::Key3,
        K::Key4,
        K::Key5,
        K::Key6,
        K::Key7,
        K::Key8,
        K::Key9,
    ];
    if let Some(i) = letters.iter().position(|&l| l == k) {
        return Some(u32::from(b'a') + i as u32);
    }
    if let Some(i) = digits.iter().position(|&d| d == k) {
        return Some(u32::from(b'0') + i as u32);
    }
    (k == K::Space).then_some(ks::SPACE)
}

/// Übersetzt eine gedrückte Taste. `None` → Zeichen-Pfad (`Char`) zuständig.
#[must_use]
pub fn key_input(k: KeyCode, m: u8) -> Option<Input> {
    if let Some(keysym) = special_keysym(k) {
        return Some(Input::Key { keysym, mods: m });
    }
    // Buchstaben nur mit Ctrl/Alt/Super als Taste (sonst kommen sie als Char).
    if is_combo(m) {
        return plain_keysym(k).map(|keysym| Input::Key { keysym, mods: m });
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::keys::mods;

    #[test]
    fn special_and_combo_keys() {
        assert_eq!(
            key_input(KeyCode::Enter, 0),
            Some(Input::Key {
                keysym: ks::RETURN,
                mods: 0
            })
        );
        assert_eq!(
            key_input(KeyCode::A, 0),
            None,
            "Buchstabe ohne Ctrl → Char-Pfad"
        );
        assert_eq!(
            key_input(KeyCode::C, mods::CTRL),
            Some(Input::Key {
                keysym: u32::from(b'c'),
                mods: mods::CTRL
            })
        );
        assert_eq!(
            key_input(KeyCode::Tab, mods::SHIFT),
            Some(Input::Key {
                keysym: ks::TAB,
                mods: mods::SHIFT
            })
        );
        assert_eq!(plain_keysym(KeyCode::Key7), Some(u32::from(b'7')));
        assert_eq!(special_keysym(KeyCode::F12), Some(0xffc9));
    }
}
