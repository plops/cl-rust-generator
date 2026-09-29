//! `04_keys` — X11-Keysyms und Modifier-Bits (Client und Server teilen sie).
//!
//! Keysym-Werte aus `X11/keysymdef.h`; Unicode-Zeichen ohne Latin-1-Keysym
//! nutzen die Konvention `0x0100_0000 + Codepoint`.

/// Modifier-Bits in `Input::Key::mods`.
pub mod mods {
    pub const SHIFT: u8 = 1;
    pub const CTRL: u8 = 2;
    pub const ALT: u8 = 4;
    pub const SUPER: u8 = 8;
}

/// Keysyms der Sondertasten.
pub mod ks {
    pub const BACKSPACE: u32 = 0xff08;
    pub const TAB: u32 = 0xff09;
    pub const RETURN: u32 = 0xff0d;
    pub const ESCAPE: u32 = 0xff1b;
    pub const DELETE: u32 = 0xffff;
    pub const HOME: u32 = 0xff50;
    pub const LEFT: u32 = 0xff51;
    pub const UP: u32 = 0xff52;
    pub const RIGHT: u32 = 0xff53;
    pub const DOWN: u32 = 0xff54;
    pub const PAGE_UP: u32 = 0xff55;
    pub const PAGE_DOWN: u32 = 0xff56;
    pub const END: u32 = 0xff57;
    pub const INSERT: u32 = 0xff63;
    pub const F1: u32 = 0xffbe;
    pub const SHIFT_L: u32 = 0xffe1;
    pub const CONTROL_L: u32 = 0xffe3;
    pub const ALT_L: u32 = 0xffe9;
    pub const SUPER_L: u32 = 0xffeb;
    pub const SPACE: u32 = 0x0020;
}

/// Keysym der Funktionstaste `Fn` (1..=12).
#[must_use]
pub fn f_key(n: u8) -> u32 {
    ks::F1 + u32::from(n.clamp(1, 12)) - 1
}

/// Zeichen → Keysym; Steuerzeichen außer `\n`/`\t`/`\b` → `None`.
#[must_use]
pub fn char_to_keysym(c: char) -> Option<u32> {
    let cp = c as u32;
    match c {
        '\n' | '\r' => Some(ks::RETURN),
        '\t' => Some(ks::TAB),
        '\u{8}' => Some(ks::BACKSPACE),
        _ if cp < 0x20 || cp == 0x7f || (0x80..0xa0).contains(&cp) => None,
        _ if cp < 0x100 => Some(cp),
        _ => Some(0x0100_0000 + cp),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn latin1_and_unicode_keysyms() {
        assert_eq!(char_to_keysym('a'), Some(0x61));
        assert_eq!(char_to_keysym('ä'), Some(0xe4));
        assert_eq!(char_to_keysym('€'), Some(0x0100_20ac));
        assert_eq!(char_to_keysym('\n'), Some(ks::RETURN));
        assert_eq!(char_to_keysym('\u{1}'), None);
        assert_eq!(char_to_keysym('\u{7f}'), None);
    }

    #[test]
    fn function_keys() {
        assert_eq!(f_key(1), 0xffbe);
        assert_eq!(f_key(12), 0xffc9);
    }
}
