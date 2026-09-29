//! `13_input` — Eingaben des Clients per XTEST in X11 einspeisen.
//!
//! Maus: absolute Bewegung (Capture-Offset addiert), Tasten 1–3, Rad als
//! Tasten 4/5. Tastatur: Keysym → Keycode über die aktuelle Tastenbelegung
//! (Spalte 0 = ohne, Spalte 1 = mit Shift). Fehlt ein Keysym (z. B. „€“ auf
//! US-Layout), wird es wie bei `xdotool` temporär auf einen freien Keycode
//! gelegt. Keine externen Tools, nur `x11rb` mit Feature `xtest`.

use std::collections::HashMap;

use lbw_common::Input;
use lbw_common::keys::{char_to_keysym, ks, mods};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{
    BUTTON_PRESS_EVENT, BUTTON_RELEASE_EVENT, ConnectionExt as _, KEY_PRESS_EVENT,
    KEY_RELEASE_EVENT, MOTION_NOTIFY_EVENT,
};
use x11rb::protocol::xtest::ConnectionExt as _;
use x11rb::rust_connection::RustConnection;

/// Keysym → (Keycode, Shift nötig) aus einer Tastenbelegung.
#[derive(Debug, Default)]
pub struct Keymap {
    map: HashMap<u32, (u8, bool)>,
    /// Keycodes ohne Belegung (für temporäre Bindungen).
    pub spare: Vec<u8>,
}

impl Keymap {
    /// Aus `GetKeyboardMapping` (`per` Keysyms je Keycode ab `min`).
    #[must_use]
    pub fn from_mapping(min: u8, per: usize, keysyms: &[u32]) -> Self {
        let mut k = Self::default();
        for (i, row) in keysyms.chunks(per.max(1)).enumerate() {
            let kc = min.saturating_add(i as u8);
            if row.iter().all(|&s| s == 0) {
                k.spare.push(kc);
                continue;
            }
            for (col, &sym) in row.iter().take(2).enumerate() {
                if sym != 0 {
                    k.map.entry(sym).or_insert((kc, col == 1));
                }
            }
        }
        k
    }

    #[must_use]
    pub fn lookup(&self, keysym: u32) -> Option<(u8, bool)> {
        self.map.get(&keysym).copied()
    }

    pub fn insert(&mut self, keysym: u32, kc: u8) {
        self.map.retain(|_, v| v.0 != kc);
        self.map.insert(keysym, (kc, false));
    }
}

/// XTEST-Einspeiser für einen Bildschirmausschnitt.
pub struct Injector {
    conn: RustConnection,
    root: u32,
    off: (i16, i16),
    size: (u16, u16),
    keymap: Keymap,
    next_spare: usize,
}

impl Injector {
    /// `off` = linke obere Ecke des Captures, `size` = dessen Größe.
    pub fn open(
        display: Option<&str>,
        off: (usize, usize),
        size: (usize, usize),
    ) -> Result<Self, String> {
        let (conn, idx) = x11rb::connect(display).map_err(|e| format!("X11: {e}"))?;
        let root = conn.setup().roots[idx].root;
        let (min, max) = (conn.setup().min_keycode, conn.setup().max_keycode);
        let m = conn
            .get_keyboard_mapping(min, max - min + 1)
            .map_err(|e| e.to_string())?
            .reply()
            .map_err(|e| e.to_string())?;
        let keymap = Keymap::from_mapping(min, m.keysyms_per_keycode as usize, &m.keysyms);
        if keymap.spare.is_empty() {
            eprintln!("warnung: kein freier Keycode, Sonderzeichen evtl. nicht tippbar");
        }
        Ok(Self {
            conn,
            root,
            off: (off.0 as i16, off.1 as i16),
            size: (size.0 as u16, size.1 as u16),
            keymap,
            next_spare: 0,
        })
    }

    fn fake(&self, ty: u8, detail: u8, x: i16, y: i16) -> Result<(), String> {
        self.conn
            .xtest_fake_input(ty, detail, 0, self.root, x, y, 0)
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    fn key(&self, kc: u8, down: bool) -> Result<(), String> {
        self.fake(
            if down {
                KEY_PRESS_EVENT
            } else {
                KEY_RELEASE_EVENT
            },
            kc,
            0,
            0,
        )
    }

    fn button(&self, b: u8, down: bool) -> Result<(), String> {
        self.fake(
            if down {
                BUTTON_PRESS_EVENT
            } else {
                BUTTON_RELEASE_EVENT
            },
            b,
            0,
            0,
        )
    }

    /// Keycode für ein Keysym; bindet es bei Bedarf an einen freien Keycode.
    fn resolve(&mut self, keysym: u32) -> Result<(u8, bool), String> {
        if let Some(v) = self.keymap.lookup(keysym) {
            return Ok(v);
        }
        if self.keymap.spare.is_empty() {
            return Err(format!("Keysym {keysym:#x} nicht belegbar"));
        }
        let kc = self.keymap.spare[self.next_spare % self.keymap.spare.len()];
        self.next_spare += 1;
        self.conn
            .change_keyboard_mapping(1, kc, 2, &[keysym, keysym])
            .map_err(|e| e.to_string())?;
        // Rundreise: Server hat die Belegung übernommen, bevor wir tippen.
        self.conn
            .get_input_focus()
            .map_err(|e| e.to_string())?
            .reply()
            .map_err(|e| e.to_string())?;
        std::thread::sleep(std::time::Duration::from_millis(20));
        self.keymap.insert(keysym, kc);
        Ok((kc, false))
    }

    /// Tippt ein Keysym mit Modifier-Bits (drücken, loslassen).
    fn tap(&mut self, keysym: u32, m: u8) -> Result<(), String> {
        let (kc, shift) = self.resolve(keysym)?;
        let mut held = Vec::new();
        for (bit, sym) in [
            (mods::CTRL, ks::CONTROL_L),
            (mods::ALT, ks::ALT_L),
            (mods::SUPER, ks::SUPER_L),
            (mods::SHIFT, ks::SHIFT_L),
        ] {
            if m & bit != 0 || (bit == mods::SHIFT && shift) {
                let (mk, _) = self.resolve(sym)?;
                self.key(mk, true)?;
                held.push(mk);
            }
        }
        self.key(kc, true)?;
        self.key(kc, false)?;
        for mk in held.into_iter().rev() {
            self.key(mk, false)?;
        }
        Ok(())
    }

    fn type_char(&mut self, c: char) -> Result<(), String> {
        match char_to_keysym(c) {
            Some(sym) => self.tap(sym, 0),
            None => Ok(()), // Steuerzeichen ignorieren
        }
    }

    /// Verarbeitet ein Eingabeereignis.
    pub fn handle(&mut self, i: &Input) -> Result<(), String> {
        match i {
            Input::MouseMove { x, y } => {
                let x = self.off.0 + (*x).min(self.size.0.saturating_sub(1)) as i16;
                let y = self.off.1 + (*y).min(self.size.1.saturating_sub(1)) as i16;
                self.fake(MOTION_NOTIFY_EVENT, 0, x, y)?;
            }
            Input::Button { button, down } => self.button((*button).clamp(1, 9), *down)?,
            Input::Wheel { dy } => {
                let b = if *dy > 0 { 4 } else { 5 };
                for _ in 0..dy.unsigned_abs().min(10) {
                    self.button(b, true)?;
                    self.button(b, false)?;
                }
            }
            Input::Key { keysym, mods } => self.tap(*keysym, *mods)?,
            Input::Char { ch } => {
                if let Some(c) = char::from_u32(*ch) {
                    self.type_char(c)?;
                }
            }
            Input::Text(s) => {
                for c in s.chars() {
                    self.type_char(c)?;
                }
            }
        }
        self.conn.flush().map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keymap_prefers_unshifted_and_finds_spares() {
        // Keycodes 8..=11, 2 Keysyms je Code.
        let syms = [
            0x61, 0x41, // 8: a A
            0x31, 0x21, // 9: 1 !
            0, 0, // 10: frei
            0x41, 0, // 11: A (ohne Shift) — darf 8/Spalte 1 nicht verdrängen
        ];
        let k = Keymap::from_mapping(8, 2, &syms);
        assert_eq!(k.lookup(0x61), Some((8, false)));
        assert_eq!(k.lookup(0x41), Some((8, true)));
        assert_eq!(k.lookup(0x21), Some((9, true)));
        assert_eq!(k.lookup(0x20ac), None);
        assert_eq!(k.spare, vec![10]);
    }

    #[test]
    fn insert_replaces_previous_binding_of_keycode() {
        let mut k = Keymap::from_mapping(8, 2, &[0, 0]);
        k.insert(0x1000_20ac, 8);
        k.insert(0xe4, 8);
        assert_eq!(k.lookup(0xe4), Some((8, false)));
        assert_eq!(k.lookup(0x1000_20ac), None);
    }
}
