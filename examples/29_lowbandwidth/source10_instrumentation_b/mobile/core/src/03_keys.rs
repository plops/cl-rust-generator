//! `03_keys` — Tasten der Mobil-Apps → Tastennamen von `lbw-server`.
//!
//! Der Server (`server/src/08_input.rs`) kennt genau diese Namen:
//! Enter, Esc, Tab, Backspace, Delete, Up, Down, Left, Right, Home, End,
//! PageUp, PageDown, Shift, Control, Alt. Alles andere geht als Zeichen
//! (`ClientMsg::Text`). Android liefert `KeyEvent.KEYCODE_*`, iOS liefert
//! Zeichen bzw. `UIKeyCommand`-Namen — beide landen hier.

/// Ausgewählte Werte aus `android.view.KeyEvent`.
pub mod kc {
    pub const DPAD_UP: i32 = 19;
    pub const DPAD_DOWN: i32 = 20;
    pub const DPAD_LEFT: i32 = 21;
    pub const DPAD_RIGHT: i32 = 22;
    pub const TAB: i32 = 61;
    pub const ENTER: i32 = 66;
    pub const DEL: i32 = 67;
    pub const PAGE_UP: i32 = 92;
    pub const PAGE_DOWN: i32 = 93;
    pub const ESCAPE: i32 = 111;
    pub const FORWARD_DEL: i32 = 112;
    pub const CTRL_LEFT: i32 = 113;
    pub const CTRL_RIGHT: i32 = 114;
    pub const MOVE_HOME: i32 = 122;
    pub const MOVE_END: i32 = 123;
    pub const NUMPAD_ENTER: i32 = 160;
}

/// Android-Tastencode → Servername (`None` → App schickt das Zeichen).
#[must_use]
pub fn android_key(code: i32) -> Option<&'static str> {
    Some(match code {
        kc::ENTER | kc::NUMPAD_ENTER => "Enter",
        kc::DEL => "Backspace",
        kc::FORWARD_DEL => "Delete",
        kc::TAB => "Tab",
        kc::ESCAPE => "Esc",
        kc::MOVE_HOME => "Home",
        kc::MOVE_END => "End",
        kc::PAGE_UP => "PageUp",
        kc::PAGE_DOWN => "PageDown",
        kc::DPAD_LEFT => "Left",
        kc::DPAD_RIGHT => "Right",
        kc::DPAD_UP => "Up",
        kc::DPAD_DOWN => "Down",
        kc::CTRL_LEFT | kc::CTRL_RIGHT => "Control",
        _ => return None,
    })
}

/// Zeichen der Softwaretastatur: Steuerzeichen werden zu Tasten
/// (`\n` → Enter, `\u{8}` → Backspace), sonst Text; `None` = verwerfen.
pub fn char_key(c: char) -> Result<&'static str, Option<String>> {
    match c {
        '\n' | '\r' => Ok("Enter"),
        '\t' => Ok("Tab"),
        '\u{8}' | '\u{7f}' => Ok("Backspace"),
        '\u{1b}' => Ok("Esc"),
        c if c.is_control() => Err(None),
        c => Err(Some(c.to_string())),
    }
}

/// Namen, die der Server versteht (für Tests und die Sondertastenleiste).
pub const SERVER_KEYS: [&str; 16] = [
    "Enter",
    "Esc",
    "Tab",
    "Backspace",
    "Delete",
    "Up",
    "Down",
    "Left",
    "Right",
    "Home",
    "End",
    "PageUp",
    "PageDown",
    "Shift",
    "Control",
    "Alt",
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_mapped_android_key_is_known_to_the_server() {
        for code in 0..300 {
            if let Some(n) = android_key(code) {
                assert!(SERVER_KEYS.contains(&n), "{code} → {n}");
            }
        }
        assert_eq!(android_key(kc::DEL), Some("Backspace"));
        assert_eq!(android_key(29), None, "Buchstabe A → Zeichen-Pfad");
    }

    #[test]
    fn chars_become_text_or_keys() {
        assert_eq!(char_key('\n'), Ok("Enter"));
        assert_eq!(char_key('\u{8}'), Ok("Backspace"));
        assert_eq!(char_key('ß'), Err(Some("ß".into())));
        assert_eq!(char_key('\u{1}'), Err(None));
    }
}
