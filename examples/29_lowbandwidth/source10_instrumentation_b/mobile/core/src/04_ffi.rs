//! `04_ffi` — C-ABI für iOS (Swift über `include/lbw_core.h`) und Tests.
//!
//! Handle = `Box<Mutex<Engine>>` als undurchsichtiger Zeiger. Strings sind
//! nullterminiertes UTF-8; Ausgaben (Texte, Status, Pins) schreibt der Kern
//! in einen Puffer des Aufrufers und liefert die **benötigte** Länge
//! (größer als `cap` → mit größerem Puffer erneut fragen).
//! Die JNI-Brücke (`05_jni`) nutzt dieselben Bausteine.

use std::ffi::{CStr, c_char};
use std::sync::{Mutex, MutexGuard};

use lbw_common::ClientMsg;

use crate::engine::Engine;
use crate::keys::char_key;

/// Undurchsichtiger Griff für C/Swift.
pub struct LbwEngine(Mutex<Engine>);

impl LbwEngine {
    pub(crate) fn lock(&self) -> MutexGuard<'_, Engine> {
        self.0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }
}

pub(crate) fn boxed(e: Engine) -> *mut LbwEngine {
    Box::into_raw(Box::new(LbwEngine(Mutex::new(e))))
}

/// # Safety
/// `h` ist null oder stammt aus `lbw_new`/`boxed` und wurde nicht freigegeben.
pub(crate) unsafe fn handle<'a>(h: *const LbwEngine) -> Option<&'a LbwEngine> {
    // SAFETY: laut Vertrag oben.
    unsafe { h.as_ref() }
}

fn cstr(s: *const c_char) -> String {
    if s.is_null() {
        return String::new();
    }
    // SAFETY: Aufrufer liefert nullterminiertes UTF-8 (Header-Vertrag).
    unsafe { CStr::from_ptr(s) }.to_string_lossy().into_owned()
}

/// Kopiert `b` nach `out` (falls passend) und liefert die benötigte Länge.
fn give(b: &[u8], out: *mut u8, cap: usize) -> usize {
    if !out.is_null() && cap >= b.len() {
        // SAFETY: Aufrufer garantiert `cap` beschreibbare Bytes ab `out`.
        unsafe { std::ptr::copy_nonoverlapping(b.as_ptr(), out, b.len()) };
    }
    b.len()
}

/// Ein Zeichen der Softwaretastatur (Steuerzeichen → Tasten).
pub(crate) fn send_char(e: &Engine, c: char) -> bool {
    match char_key(c) {
        Ok(k) => {
            e.key(k);
            true
        }
        Err(Some(t)) => {
            e.send(ClientMsg::Text(t));
            true
        }
        Err(None) => false,
    }
}

/// Text (IME/Einfügen) zeichenweise wie der Desktop-Client.
pub(crate) fn send_text(e: &Engine, s: &str) {
    for c in s.chars() {
        send_char(e, c);
    }
}

/// Startet eine Verbindung (`json` s. `EngineCfg`); null bei Fehler.
#[unsafe(no_mangle)]
pub extern "C" fn lbw_new(json: *const c_char) -> *mut LbwEngine {
    match Engine::new(&cstr(json)) {
        Ok(e) => boxed(e),
        Err(e) => {
            eprintln!("[lbw] lbw_new: {e}");
            std::ptr::null_mut()
        }
    }
}

/// Beendet die Verbindung und gibt den Griff frei (null erlaubt).
///
/// # Safety
/// `h` stammt aus `lbw_new` und wird danach nicht mehr benutzt.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_free(h: *mut LbwEngine) {
    if !h.is_null() {
        // SAFETY: laut Vertrag.
        drop(unsafe { Box::from_raw(h) });
    }
}

/// Wendet Netz-Ereignisse an; kopiert den RGBA-Canvas (1280×720×4) nach
/// `rgba`, falls geändert. Rückgabe: `LBW_FRAME|LBW_TEXT|LBW_UP`-Bits.
///
/// # Safety
/// `h` gültig; `rgba` null oder `len` beschreibbare Bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_poll(h: *const LbwEngine, rgba: *mut u8, len: usize) -> i32 {
    // SAFETY: laut Vertrag.
    let Some(e) = (unsafe { handle(h) }) else {
        return 0;
    };
    let buf = (!rgba.is_null()).then(|| {
        // SAFETY: laut Vertrag.
        unsafe { std::slice::from_raw_parts_mut(rgba, len) }
    });
    e.lock().poll(buf)
}

/// Texte als Blob (s. `02_blob`); Rückgabe = benötigte Länge.
///
/// # Safety
/// `h` gültig; `out` null oder `cap` beschreibbare Bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_texts(h: *const LbwEngine, out: *mut u8, cap: usize) -> usize {
    // SAFETY: laut Vertrag.
    unsafe { handle(h) }.map_or(0, |e| give(&e.lock().texts(), out, cap))
}

/// Statuszeile (UTF-8, ohne Null); Rückgabe = benötigte Länge.
///
/// # Safety
/// wie `lbw_texts`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_status(h: *const LbwEngine, out: *mut u8, cap: usize) -> usize {
    // SAFETY: laut Vertrag.
    unsafe { handle(h) }.map_or(0, |e| give(e.lock().status().as_bytes(), out, cap))
}

/// Hostkey-Fingerprints pro Hop, eine Zeile je Hop (TOFU speichern).
///
/// # Safety
/// wie `lbw_texts`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_pins(h: *const LbwEngine, out: *mut u8, cap: usize) -> usize {
    // SAFETY: laut Vertrag.
    unsafe { handle(h) }.map_or(0, |e| give(e.lock().pins().join("\n").as_bytes(), out, cap))
}

/// Sondertaste drücken + loslassen (`"Enter"`, `"Backspace"`, …).
///
/// # Safety
/// `h` gültig, `name` nullterminiertes UTF-8.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_key(h: *const LbwEngine, name: *const c_char) {
    // SAFETY: laut Vertrag.
    if let Some(e) = unsafe { handle(h) } {
        e.lock().key(&cstr(name));
    }
}

/// Getippter/eingefügter Text (Steuerzeichen werden zu Tasten).
///
/// # Safety
/// wie `lbw_key`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_text(h: *const LbwEngine, utf8: *const c_char) {
    // SAFETY: laut Vertrag.
    if let Some(e) = unsafe { handle(h) } {
        send_text(&e.lock(), &cstr(utf8));
    }
}

/// Maus an absolute Server-Pixel.
///
/// # Safety
/// `h` gültig.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_mouse(h: *const LbwEngine, x: i32, y: i32) {
    // SAFETY: laut Vertrag.
    if let Some(e) = unsafe { handle(h) } {
        e.lock().send(ClientMsg::MouseMove {
            x: x.clamp(0, i32::from(u16::MAX)) as u16,
            y: y.clamp(0, i32::from(u16::MAX)) as u16,
        });
    }
}

/// Maustaste (1 links, 2 mitte, 3 rechts).
///
/// # Safety
/// `h` gültig.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_button(h: *const LbwEngine, button: i32, down: bool) {
    // SAFETY: laut Vertrag.
    if let Some(e) = unsafe { handle(h) } {
        e.lock().send(ClientMsg::Button {
            button: button.clamp(1, 3) as u8,
            down,
        });
    }
}

/// Rad (dy < 0 hoch) als Bild-auf/-ab.
///
/// # Safety
/// `h` gültig.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lbw_wheel(h: *const LbwEngine, dy: i32) {
    // SAFETY: laut Vertrag.
    if let Some(e) = unsafe { handle(h) } {
        e.lock().wheel(dy);
    }
}
