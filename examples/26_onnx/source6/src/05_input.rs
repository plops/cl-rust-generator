//! `05_input` — X11-Eingabe-Automation per XTEST (S2).
//!
//! `X11Input` baut aus dem Server-Keymap eine ASCII-Tabelle
//! (0x20–0x7e, Shift-Status) und sendet Maus- und Tastatur-Events per
//! `xtest::fake_input`. Alles gibt `Result` zurück (kein `unwrap`/`expect`);
//! nicht abbildbare Zeichen werden übersprungen und gezählt (sichtbarer
//! `skipped`-Zähler statt stiller Falscheingabe). Ohne X11 ist nur der
//! Zähler testbar; Klick/Type brauchen ein Display (Xvfb-Smoke in T1).

use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;

use x11rb::connection::Connection;
use x11rb::protocol::xproto::{
    self, BUTTON_PRESS_EVENT, BUTTON_RELEASE_EVENT, CLIENT_MESSAGE_EVENT, KEY_PRESS_EVENT,
    KEY_RELEASE_EVENT, Keycode, MOTION_NOTIFY_EVENT, Window,
};
use x11rb::protocol::xtest;

/// XTEST-Zeitstempel „jetzt" (CurrentTime, 0 laut X11-Protokoll).
const NOW: u32 = 0;

/// Pause nach Maus-Bewegung (ms), damit der Server folgen kann.
pub const MOTION_SETTLE_MS: u64 = 20;
/// Pause zwischen Tastenschlägen (ms).
pub const KEY_SETTLE_MS: u64 = 15;
/// Pause vor Enter (ms).
pub const ENTER_SETTLE_MS: u64 = 30;
/// Pause zwischen Klick und Tippen (ms, Fokus-Wechsel abwarten).
/// Liegt garantiert DAZWISCHEN (in `Sink::click_and_type`), nicht danach.
pub const FOCUS_SETTLE_MS: u64 = 100;

/// Fehler im Input-Pfad (tragbar, ohne X11-Abhängigkeit im Typ).
#[derive(Debug)]
pub enum InputError {
    /// X11-Verbindungs-/Protokollfehler (Text aus x11rb).
    X11(String),
    /// Kein Keycode für Shift/Return im Server-Keymap gefunden.
    NoKeycode(&'static str),
}

impl std::fmt::Display for InputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::X11(e) => write!(f, "X11-Fehler: {e}"),
            Self::NoKeycode(k) => {
                write!(
                    f,
                    "Keycode fehlt im Server-Keymap: {k} \
                           (Tipp: --dry-run oder US-Layout prüfen)"
                )
            }
        }
    }
}

impl std::error::Error for InputError {}

/// XTEST-Eingabe auf einer bestehenden X11-Verbindung.
pub struct X11Input<'a, C: Connection> {
    conn: &'a C,
    root: Window,
    key_map: HashMap<char, (Keycode, bool)>,
    shift_kc: Keycode,
    return_kc: Keycode,
    skipped: u64,
}

impl<'a, C: Connection> X11Input<'a, C> {
    /// Liest das Server-Keymap und merkt sich Shift-/Return-Keycodes.
    /// Schlägt klar fehl, wenn XTEST-Berechtigung oder Keycodes fehlen.
    pub fn new(conn: &'a C, screen_idx: usize) -> Result<Self, InputError> {
        let root = conn.setup().roots[screen_idx].root;
        let min_kc = conn.setup().min_keycode;
        let max_kc = conn.setup().max_keycode;
        let count = max_kc - min_kc + 1;

        let mapping = xproto::get_keyboard_mapping(conn, min_kc, count)
            .map_err(|e| InputError::X11(e.to_string()))?
            .reply()
            .map_err(|e| InputError::X11(e.to_string()))?;
        let kpk = mapping.keysyms_per_keycode as usize;

        let mut key_map = HashMap::new();
        let mut shift_kc = 0;
        let mut return_kc = 0;

        for (i, syms) in mapping.keysyms.chunks(kpk).enumerate() {
            let kc = min_kc + i as u8;
            let normal = syms.first().copied().unwrap_or(0);
            let shifted = syms.get(1).copied().unwrap_or(0);

            if normal == 0xffe1 || normal == 0xffe2 {
                shift_kc = kc;
            }
            if normal == 0xff0d {
                return_kc = kc;
            }

            // Standard-ASCII-Keysyms (0x20–0x7e), unverschoben + verschoben.
            if (0x20..=0x7e).contains(&normal)
                && let Some(ch) = char::from_u32(normal)
            {
                key_map.entry(ch).or_insert((kc, false));
            }
            if (0x20..=0x7e).contains(&shifted)
                && let Some(ch) = char::from_u32(shifted)
            {
                key_map.entry(ch).or_insert((kc, true));
            }
        }

        Ok(Self {
            conn,
            root,
            key_map,
            shift_kc,
            return_kc,
            skipped: 0,
        })
    }

    /// Top-Level-Fenster unter `(x, y)`: vom gefundenen Kind über
    /// `query_tree` hochklettern bis zum direkten Kind von Root.
    /// Fokus und WM-Aktivierung brauchen das Top-Level — ein tiefes
    /// Unterfenster (z. B. Firefox-Content) nimmt keinen Fokus an.
    fn toplevel_at(&self, x: i16, y: i16) -> Option<Window> {
        let tr = xproto::translate_coordinates(self.conn, self.root, self.root, x, y)
            .ok()?
            .reply()
            .ok()?;
        let mut w = if tr.child != 0 { tr.child } else { self.root };
        // Hochklettern (Deckel gegen Zyklen); Root selbst hat Parent 0.
        for _ in 0..16 {
            let parent = xproto::query_tree(self.conn, w).ok()?.reply().ok()?.parent;
            if parent == self.root || parent == 0 {
                break;
            }
            w = parent;
        }
        Some(w)
    }

    /// Hebt das Top-Level-Fenster unter `(x, y)` und gibt ihm den Fokus
    /// (EWMH `_NET_ACTIVE_WINDOW` + Core-Fokus). Alles best-effort:
    /// Fehler werden geschluckt, damit ein eigenwilliger WM nie einen
    /// Klick verhindert (der XTEST-Klick selbst fokussiert meist eh).
    pub fn focus_at(&self, x: i16, y: i16) {
        let Some(target) = self.toplevel_at(x, y) else {
            return;
        };
        let _ = xproto::set_input_focus(self.conn, xproto::InputFocus::PARENT, target, NOW);

        // EWMH-Meldung an den Window-Manager (Datenlayout `[Quelle, Zeit, …]`).
        if let Ok(atom_cookie) = xproto::intern_atom(self.conn, false, b"_NET_ACTIVE_WINDOW")
            && let Ok(atom) = atom_cookie.reply()
        {
            let ev = xproto::ClientMessageEvent {
                response_type: CLIENT_MESSAGE_EVENT,
                format: 32,
                sequence: 0,
                window: target,
                type_: atom.atom,
                data: xproto::ClientMessageData::from([1u32, NOW, 0, 0, 0]),
            };
            let _ = xproto::send_event(
                self.conn,
                false,
                self.root,
                xproto::EventMask::SUBSTRUCTURE_REDIRECT | xproto::EventMask::SUBSTRUCTURE_NOTIFY,
                ev,
            );
        }
        let _ = self.conn.flush();
    }

    /// Ein XTEST-Event senden (Maus wie Taste, immer mit Koordinaten).
    fn xt(&self, t: u8, d: Keycode, x: i16, y: i16) -> Result<(), InputError> {
        xtest::fake_input(self.conn, t, d, NOW, self.root, x, y, 0)
            .map_err(|e| InputError::X11(e.to_string()))?;
        self.conn
            .flush()
            .map_err(|e| InputError::X11(e.to_string()))
    }

    /// Linksklick auf absolute Bildschirm-Pixel (mit Fokus vorher und
    /// echten Koordinaten in den Button-Events).
    pub fn click(&self, x: i16, y: i16) -> Result<(), InputError> {
        self.focus_at(x, y);
        self.xt(MOTION_NOTIFY_EVENT, 0, x, y)?;
        sleep(Duration::from_millis(MOTION_SETTLE_MS));
        self.xt(BUTTON_PRESS_EVENT, 1, x, y)?;
        sleep(Duration::from_millis(15));
        self.xt(BUTTON_RELEASE_EVENT, 1, x, y)?;
        Ok(())
    }

    /// Tippt ASCII-Text über das Server-Keymap; unbekannte Zeichen werden
    /// übersprungen und in `skipped()` gezählt. `hit_enter` drückt Return.
    pub fn type_text(&mut self, text: &str, hit_enter: bool) -> Result<(), InputError> {
        for ch in text.chars() {
            let Some(&(kc, needs_shift)) = self.key_map.get(&ch) else {
                self.skipped += 1;
                continue;
            };
            if needs_shift {
                if self.shift_kc == 0 {
                    self.skipped += 1;
                    continue;
                }
                self.xt(KEY_PRESS_EVENT, self.shift_kc, 0, 0)?;
            }
            self.xt(KEY_PRESS_EVENT, kc, 0, 0)?;
            self.xt(KEY_RELEASE_EVENT, kc, 0, 0)?;
            if needs_shift {
                self.xt(KEY_RELEASE_EVENT, self.shift_kc, 0, 0)?;
            }
            sleep(Duration::from_millis(KEY_SETTLE_MS));
        }
        if hit_enter {
            if self.return_kc == 0 {
                return Err(InputError::NoKeycode("Return"));
            }
            sleep(Duration::from_millis(ENTER_SETTLE_MS));
            self.xt(KEY_PRESS_EVENT, self.return_kc, 0, 0)?;
            self.xt(KEY_RELEASE_EVENT, self.return_kc, 0, 0)?;
        }
        Ok(())
    }

    /// Anzahl übersprungener (nicht abbildbarer) Zeichen seit `new`.
    #[must_use]
    pub fn skipped_count(&self) -> u64 {
        self.skipped
    }
}

/// Trockenlauf-Sink: loggt nur (kein XTEST-Event).
#[derive(Default)]
pub struct DrySink;

impl crate::rules::Sink for DrySink {
    fn click(&mut self, _x: i16, _y: i16) -> Result<(), InputError> {
        Ok(())
    }

    fn type_text(&mut self, _text: &str, _press_enter: bool) -> Result<(), InputError> {
        Ok(())
    }
}

/// `X11Input` als Regel-`Sink` (Fehler landen im Engine-Log).
/// Das Trait ist in `08_rules` definiert (dort wird es verbraucht);
///
/// dieser Adapter hält die Verdrahtung in `main` schlank.
impl<C: Connection> crate::rules::Sink for X11Input<'_, C> {
    fn click(&mut self, x: i16, y: i16) -> Result<(), InputError> {
        X11Input::click(self, x, y)
    }

    fn type_text(&mut self, text: &str, press_enter: bool) -> Result<(), InputError> {
        X11Input::type_text(self, text, press_enter)
    }

    fn skipped(&self) -> u64 {
        self.skipped_count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Tippfehler-Schutz zur Compile-Zeit: Delays positiv,
    // Fokus-Pause am größten.
    const _: () = {
        assert!(MOTION_SETTLE_MS > 0);
        assert!(KEY_SETTLE_MS > 0);
        assert!(FOCUS_SETTLE_MS >= MOTION_SETTLE_MS);
        assert!(ENTER_SETTLE_MS >= KEY_SETTLE_MS);
    };

    /// XTEST-Pfad gegen echten X-Server (nur unter Xvfb, s. smoke_xvfb.sh).
    /// Beweist über den neuen Pfad: Fokus-Sequenz, Klick mit Koordinaten,
    /// `click_and_type` (Klick → 100 ms → Tippen) ohne Fehler, Skip 0.
    #[test]
    #[ignore = "braucht X-Server (Xvfb), kein Unit-Test"]
    fn xtest_path_against_real_server() {
        let (conn, screen) = x11rb::connect(None).expect("kein X11 (Xvfb läuft?)");
        let mut input =
            super::X11Input::new(&conn, screen).expect("XTEST-Init (XTEST im Server aktiv?)");
        input.focus_at(100, 100);
        input.click(100, 100).expect("click");
        input.type_text("abc XYZ 123", false).expect("type_text");
        crate::rules::Sink::click_and_type(&mut input, 100, 100, "ok", false)
            .expect("click_and_type");
        assert_eq!(input.skipped_count(), 0);
    }

    #[test]
    fn input_error_is_display() {
        let e = InputError::NoKeycode("Return");
        assert!(e.to_string().contains("Return"));
        let e = InputError::X11("conn closed".into());
        assert!(e.to_string().contains("conn closed"));
    }
}
