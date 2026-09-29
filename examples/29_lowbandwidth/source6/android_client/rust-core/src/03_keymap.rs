//! `03_keymap` — Android-Tastencodes (`KeyEvent.KEYCODE_*`, `META_*`) und
//! Zeichen der Softwaretastatur → Protokoll-`Input`.
//!
//! Sondertasten gehen als `Key{keysym}`; Buchstaben/Ziffern nur zusammen mit
//! Ctrl/Alt/Meta, sonst liefert Kotlin das Zeichen (`getUnicodeChar`) und
//! [`char_with_mods`] macht daraus `Char`.

use lbw_client::input::{char_input, is_combo};
use lbw_common::Input;
use lbw_common::keys::{char_to_keysym, f_key, ks, mods};

/// Ausgewählte Werte aus `android.view.KeyEvent`.
pub mod kc {
    pub const KEY_0: i32 = 7;
    pub const KEY_9: i32 = 16;
    pub const DPAD_UP: i32 = 19;
    pub const DPAD_DOWN: i32 = 20;
    pub const DPAD_LEFT: i32 = 21;
    pub const DPAD_RIGHT: i32 = 22;
    pub const A: i32 = 29;
    pub const Z: i32 = 54;
    pub const TAB: i32 = 61;
    pub const SPACE: i32 = 62;
    pub const ENTER: i32 = 66;
    pub const DEL: i32 = 67;
    pub const PAGE_UP: i32 = 92;
    pub const PAGE_DOWN: i32 = 93;
    pub const ESCAPE: i32 = 111;
    pub const FORWARD_DEL: i32 = 112;
    pub const MOVE_HOME: i32 = 122;
    pub const MOVE_END: i32 = 123;
    pub const INSERT: i32 = 124;
    pub const F1: i32 = 131;
    pub const F12: i32 = 142;
    pub const NUMPAD_ENTER: i32 = 160;

    pub const META_SHIFT_ON: i32 = 0x1;
    pub const META_ALT_ON: i32 = 0x2;
    pub const META_CTRL_ON: i32 = 0x1000;
    pub const META_META_ON: i32 = 0x1_0000;
}

/// Android-Meta-Zustand → Modifier-Bits des Protokolls.
#[must_use]
pub fn meta_mods(meta: i32) -> u8 {
    let on = |bit: i32| meta & bit != 0;
    u8::from(on(kc::META_SHIFT_ON)) * mods::SHIFT
        + u8::from(on(kc::META_CTRL_ON)) * mods::CTRL
        + u8::from(on(kc::META_ALT_ON)) * mods::ALT
        + u8::from(on(kc::META_META_ON)) * mods::SUPER
}

/// Keysym für Sondertasten.
#[must_use]
pub fn special_keysym(code: i32) -> Option<u32> {
    Some(match code {
        kc::ENTER | kc::NUMPAD_ENTER => ks::RETURN,
        kc::DEL => ks::BACKSPACE,
        kc::FORWARD_DEL => ks::DELETE,
        kc::TAB => ks::TAB,
        kc::ESCAPE => ks::ESCAPE,
        kc::INSERT => ks::INSERT,
        kc::MOVE_HOME => ks::HOME,
        kc::MOVE_END => ks::END,
        kc::PAGE_UP => ks::PAGE_UP,
        kc::PAGE_DOWN => ks::PAGE_DOWN,
        kc::DPAD_LEFT => ks::LEFT,
        kc::DPAD_RIGHT => ks::RIGHT,
        kc::DPAD_UP => ks::UP,
        kc::DPAD_DOWN => ks::DOWN,
        kc::F1..=kc::F12 => f_key((code - kc::F1 + 1) as u8),
        _ => return None,
    })
}

/// Keysym für Buchstaben/Ziffern/Leertaste (für Kombinationen).
#[must_use]
pub fn plain_keysym(code: i32) -> Option<u32> {
    match code {
        kc::A..=kc::Z => Some(u32::from(b'a') + (code - kc::A) as u32),
        kc::KEY_0..=kc::KEY_9 => Some(u32::from(b'0') + (code - kc::KEY_0) as u32),
        kc::SPACE => Some(ks::SPACE),
        _ => None,
    }
}

/// Taste → `Input`; `None` → Kotlin schickt stattdessen das Zeichen.
#[must_use]
pub fn android_key(code: i32, m: u8) -> Option<Input> {
    let keysym = special_keysym(code).or_else(|| plain_keysym(code).filter(|_| is_combo(m)))?;
    Some(Input::Key { keysym, mods: m })
}

/// Zeichen (Softwaretastatur, `getUnicodeChar`) mit aktiven Modifiern.
/// Steuerzeichen (`\n`, `\t`, `\b`) werden zu Tasten; mit Ctrl/Alt/Super
/// wird der Kleinbuchstabe als Taste gesendet (Ctrl+C statt „C“).
#[must_use]
pub fn char_with_mods(c: char, m: u8) -> Option<Input> {
    if c.is_control() || is_combo(m) {
        let lower = c.to_lowercase().next().unwrap_or(c);
        let keysym = char_to_keysym(lower)?;
        return Some(Input::Key { keysym, mods: m });
    }
    char_input(c, m)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn specials_and_function_keys() {
        let k = |keysym, mods| Some(Input::Key { keysym, mods });
        assert_eq!(android_key(kc::ENTER, 0), k(ks::RETURN, 0));
        assert_eq!(android_key(kc::DEL, 0), k(ks::BACKSPACE, 0));
        assert_eq!(android_key(kc::F1, 0), k(0xffbe, 0));
        assert_eq!(android_key(kc::F12, 0), k(0xffc9, 0));
        assert_eq!(android_key(kc::DPAD_LEFT, mods::SHIFT), k(ks::LEFT, 1));
        assert_eq!(android_key(kc::A, 0), None, "Buchstabe → Zeichen-Pfad");
        assert_eq!(android_key(kc::Z, mods::CTRL), k(u32::from(b'z'), 2));
        assert_eq!(android_key(kc::KEY_9, mods::ALT), k(u32::from(b'9'), 4));
        assert_eq!(android_key(999, mods::CTRL), None);
    }

    #[test]
    fn meta_state_maps_to_mod_bits() {
        assert_eq!(meta_mods(0), 0);
        let all = kc::META_SHIFT_ON | kc::META_CTRL_ON | kc::META_ALT_ON | kc::META_META_ON;
        assert_eq!(meta_mods(all), 15);
        assert_eq!(
            meta_mods(0x40 | kc::META_CTRL_ON),
            mods::CTRL,
            "CTRL_LEFT_ON zählt nicht doppelt"
        );
    }

    #[test]
    fn chars_with_sticky_modifiers() {
        assert_eq!(char_with_mods('ß', 0), Some(Input::Char { ch: 'ß' as u32 }));
        assert_eq!(
            char_with_mods('C', mods::CTRL),
            Some(Input::Key {
                keysym: u32::from(b'c'),
                mods: mods::CTRL
            })
        );
        assert_eq!(
            char_with_mods('\n', 0),
            Some(Input::Key {
                keysym: ks::RETURN,
                mods: 0
            })
        );
        assert_eq!(char_with_mods('\u{1}', 0), None);
    }
}
