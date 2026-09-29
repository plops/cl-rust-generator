//! `05_input` — plattformneutrale Eingabe-Aufbereitung für das Protokoll.
//!
//! Druckbare Zeichen kommen als `Char` (tastaturlayout-unabhängig, der
//! Server tippt sie). Kombinationen mit Ctrl/Alt/Super gehen als
//! `Key{keysym, mods}`. Mausbewegungen werden auf 30 Hz gedrosselt.
//! Desktop (`06_keycode`) und Android (`lbw-core`) teilen diese Logik.

use std::time::{Duration, Instant};

use lbw_common::Input;
use lbw_common::keys::mods;

/// Modifier-Bits aus dem Zustand der Modifier-Tasten.
#[must_use]
pub fn mod_bits(shift: bool, ctrl: bool, alt: bool, sup: bool) -> u8 {
    u8::from(shift) * mods::SHIFT
        + u8::from(ctrl) * mods::CTRL
        + u8::from(alt) * mods::ALT
        + u8::from(sup) * mods::SUPER
}

/// Sind Ctrl, Alt oder Super aktiv (Taste statt Zeichen)?
#[must_use]
pub fn is_combo(m: u8) -> bool {
    m & (mods::CTRL | mods::ALT | mods::SUPER) != 0
}

/// Zeichen → `Char`, sofern druckbar und keine Kombination aktiv ist.
#[must_use]
pub fn char_input(c: char, m: u8) -> Option<Input> {
    let printable = !c.is_control();
    (printable && !is_combo(m)).then_some(Input::Char { ch: c as u32 })
}

/// Drosselt Mausbewegungen (nur bei Änderung, max. alle `every`).
pub struct MouseThrottle {
    every: Duration,
    sent: Option<(u16, u16)>,
    last: Option<Instant>,
}

impl MouseThrottle {
    #[must_use]
    pub fn new(hz: u32) -> Self {
        Self {
            every: Duration::from_secs(1) / hz.max(1),
            sent: None,
            last: None,
        }
    }

    /// Neue Position; `force` (vor Klicks) sendet sofort.
    pub fn update(&mut self, x: u16, y: u16, now: Instant, force: bool) -> Option<Input> {
        if self.sent == Some((x, y)) {
            return None;
        }
        if !force
            && self
                .last
                .is_some_and(|t| now.duration_since(t) < self.every)
        {
            return None;
        }
        self.sent = Some((x, y));
        self.last = Some(now);
        Some(Input::MouseMove { x, y })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chars_are_filtered() {
        assert_eq!(
            char_input('ä', mods::SHIFT),
            Some(Input::Char { ch: 'ä' as u32 })
        );
        assert_eq!(char_input('\r', 0), None);
        assert_eq!(char_input('c', mods::CTRL), None);
        assert_eq!(mod_bits(true, true, false, true), 11);
    }

    #[test]
    fn mouse_is_throttled_and_deduplicated() {
        let t0 = Instant::now();
        let mut m = MouseThrottle::new(30);
        assert!(m.update(1, 1, t0, false).is_some());
        assert!(
            m.update(1, 1, t0 + Duration::from_secs(1), false).is_none(),
            "gleiche Position"
        );
        assert!(
            m.update(2, 2, t0 + Duration::from_millis(10), false)
                .is_none(),
            "zu früh"
        );
        assert!(
            m.update(2, 2, t0 + Duration::from_millis(10), true)
                .is_some(),
            "force"
        );
        assert!(
            m.update(3, 3, t0 + Duration::from_millis(60), false)
                .is_some()
        );
    }
}
