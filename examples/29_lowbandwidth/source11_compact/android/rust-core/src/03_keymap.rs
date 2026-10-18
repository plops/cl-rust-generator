//! `03_keymap` — Android-Tastencodes (`KeyEvent.KEYCODE_*`, `META_*`)
//! → v4-`ClientMsg` (Sondertasten als Namen, Zeichen als `Text`).
//!
//! Sondertasten gehen als `Key{key}`; Buchstaben/Ziffern nur zusammen mit
//! Ctrl/Alt/Meta, sonst liefert Kotlin das Zeichen (`getUnicodeChar`).

use lbw_common::ClientMsg;

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

/// Sondername für Android-Sondertasten.
#[must_use]
pub fn special_name(code: i32) -> Option<&'static str> {
    Some(match code {
        kc::ENTER | kc::NUMPAD_ENTER => "Enter",
        kc::DEL => "BackSpace",
        kc::FORWARD_DEL => "Delete",
        kc::TAB => "Tab",
        kc::ESCAPE => "Esc",
        kc::INSERT => "Insert",
        kc::MOVE_HOME => "Home",
        kc::MOVE_END => "End",
        kc::PAGE_UP => "PageUp",
        kc::PAGE_DOWN => "PageDown",
        kc::DPAD_LEFT => "Left",
        kc::DPAD_RIGHT => "Right",
        kc::DPAD_UP => "Up",
        kc::DPAD_DOWN => "Down",
        _ => return None,
    })
}

/// F-Tasten-Name (F1..F12).
#[must_use]
pub fn f_name(code: i32) -> Option<String> {
    if (kc::F1..=kc::F12).contains(&code) {
        return Some(format!("F{}", code - kc::F1 + 1));
    }
    None
}

/// Taste → `ClientMsg::Key`; `None` → Kotlin schickt stattdessen das Zeichen.
#[must_use]
pub fn android_key(code: i32) -> Option<ClientMsg> {
    if let Some(name) = f_name(code) {
        return Some(ClientMsg::Key {
            key: name,
            down: true,
        });
    }
    special_name(code).map(|name| ClientMsg::Key {
        key: name.into(),
        down: true,
    })
}

/// Zeichen (Softwaretastatur) mit Modifiern: Steuerzeichen und Combos
/// werden zu Tasten (`\n` → Enter), sonst `Text`.
#[must_use]
pub fn char_msg(c: char) -> Option<ClientMsg> {
    match c {
        '\n' | '\r' => android_key(kc::ENTER),
        '\t' => android_key(kc::TAB),
        '\x08' | '\x7f' => android_key(kc::DEL),
        c if c.is_control() => None,
        c => Some(ClientMsg::Text(c.to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn specials_map_to_names() {
        assert!(matches!(
            android_key(kc::ENTER),
            Some(ClientMsg::Key { key, .. }) if key == "Enter"
        ));
        assert!(matches!(
            android_key(kc::DPAD_LEFT),
            Some(ClientMsg::Key { key, .. }) if key == "Left"
        ));
        assert!(matches!(
            android_key(kc::F12),
            Some(ClientMsg::Key { key, .. }) if key == "F12"
        ));
        assert_eq!(android_key(kc::A), None, "Buchstabe → Zeichen-Pfad");
        assert_eq!(android_key(999), None);
    }

    #[test]
    fn chars_map_to_text_or_keys() {
        assert!(matches!(char_msg('ß'), Some(ClientMsg::Text(_))));
        assert!(matches!(
            char_msg('\n'),
            Some(ClientMsg::Key { key, .. }) if key == "Enter"
        ));
        assert_eq!(char_msg('\u{1}'), None);
    }
}
