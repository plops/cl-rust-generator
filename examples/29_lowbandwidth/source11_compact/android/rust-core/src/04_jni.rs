//! `04_jni` — JNI-Exporte für `de.lbw.client.CoreBridge` (Kotlin).
//!
//! Handle = `Box<Mutex<Engine>>` als `long`. Zeichenketten reisen als
//! UTF-8-`byte[]` (JNI-`NewStringUTF` erwartet „modified UTF-8“), der
//! Canvas über einen Direct-`ByteBuffer` (kein Kopieren in den Java-Heap).
//! Alle `unsafe`-Zugriffe auf `JNIEnv` stehen in den Helfern oben.

use std::ptr::null_mut;
use std::sync::{Mutex, MutexGuard};

use jni_sys::{JNIEnv, jboolean, jbyte, jbyteArray, jclass, jint, jlong, jobject};
use lbw_common::ClientMsg;

use crate::engine::Engine;
use crate::keymap::{android_key, char_msg};

type Env = *mut JNIEnv;

/// Engine hinter dem Handle (vergiftete Sperre wird übernommen).
fn engine<'a>(h: jlong) -> Option<MutexGuard<'a, Engine>> {
    // SAFETY: `h` stammt aus `nativeNew` und lebt bis `nativeFree`.
    let m = unsafe { (h as *const Mutex<Engine>).as_ref()? };
    Some(m.lock().unwrap_or_else(std::sync::PoisonError::into_inner))
}

/// `byte[]` → `Vec<u8>` (`null` → leer).
fn bytes(env: Env, a: jbyteArray) -> Vec<u8> {
    if a.is_null() {
        return Vec::new();
    }
    // SAFETY: `env` ist der gültige JNIEnv des aufrufenden Threads, `a` ein byte[].
    unsafe {
        let f = (**env).v1_1;
        let n = (f.GetArrayLength)(env, a).max(0);
        let mut v = vec![0u8; n as usize];
        (f.GetByteArrayRegion)(env, a, 0, n, v.as_mut_ptr().cast::<jbyte>());
        v
    }
}

fn text(env: Env, a: jbyteArray) -> String {
    String::from_utf8_lossy(&bytes(env, a)).into_owned()
}

/// `&[u8]` → neues `byte[]`.
fn new_bytes(env: Env, b: &[u8]) -> jbyteArray {
    // SAFETY: wie oben; Länge passt in `jsize` (Blob ≤ wenige MB).
    unsafe {
        let f = (**env).v1_1;
        let a = (f.NewByteArray)(env, b.len() as jint);
        if !a.is_null() {
            (f.SetByteArrayRegion)(env, a, 0, b.len() as jint, b.as_ptr().cast::<jbyte>());
        }
        a
    }
}

/// Speicher eines Direct-`ByteBuffer` (`null`/Heap-Puffer → `None`).
fn direct<'a>(env: Env, buf: jobject) -> Option<&'a mut [u8]> {
    if buf.is_null() {
        return None;
    }
    // SAFETY: Die JVM garantiert `capacity` gültige Bytes ab `address`,
    // solange der Puffer lebt (Kotlin hält ihn während des Aufrufs).
    unsafe {
        let f = (**env).v1_4;
        let p = (f.GetDirectBufferAddress)(env, buf).cast::<u8>();
        let n = (f.GetDirectBufferCapacity)(env, buf);
        (!p.is_null() && n > 0).then(|| std::slice::from_raw_parts_mut(p, n as usize))
    }
}

fn with(h: jlong, f: impl FnOnce(&mut Engine)) {
    if let Some(mut e) = engine(h) {
        f(&mut e);
    }
}

/// `CoreBridge.version()` — JNI-Namenskonvention, kein Mangling.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_version(env: Env, _cls: jclass) -> jbyteArray {
    new_bytes(env, b"lbw11-android-core")
}

/// `nativeNew(configJsonUtf8, deadAfterS)` → Handle (0 bei Fehler).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeNew(
    env: Env,
    _cls: jclass,
    config: jbyteArray,
    _dead_after_s: jint,
) -> jlong {
    let json = text(env, config);
    let _ = env;
    match Engine::new(&json) {
        Ok(e) => Box::into_raw(Box::new(Mutex::new(e))) as jlong,
        Err(e) => {
            eprintln!("[jni] nativeNew: {e}");
            0
        }
    }
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeFree(_env: Env, _cls: jclass, h: jlong) {
    if h != 0 {
        // SAFETY: wie in `engine`; danach ist das Handle ungültig.
        unsafe {
            drop(Box::from_raw(h as *mut Mutex<Engine>));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePoll(
    env: Env,
    _cls: jclass,
    h: jlong,
    frame: jobject,
) -> jint {
    let Some(mut e) = engine(h) else {
        return 0;
    };
    e.poll(direct(env, frame))
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeSize(
    _env: Env,
    _cls: jclass,
    h: jlong,
) -> jint {
    engine(h)
        .map(|e| {
            let (w, h) = e.size();
            ((w << 16) | h) as jint
        })
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeTexts(
    env: Env,
    _cls: jclass,
    h: jlong,
) -> jbyteArray {
    match engine(h) {
        Some(e) => new_bytes(env, &e.texts()),
        None => null_mut(),
    }
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeStatus(
    env: Env,
    _cls: jclass,
    h: jlong,
) -> jbyteArray {
    match engine(h) {
        Some(e) => new_bytes(env, e.status().as_bytes()),
        None => null_mut(),
    }
}

/// `nativePins(h)` — gelernte Hostkey-Fingerprints pro Hop, ein `SHA256:…`
/// pro Zeile (Reihenfolge = Hop-Reihenfolge); leer, solange die Kette noch
/// nie stand. Kotlin speichert sie unter `pinKey()` (TOFU).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePins(
    env: Env,
    _cls: jclass,
    h: jlong,
) -> jbyteArray {
    match engine(h) {
        Some(e) => new_bytes(env, e.pins().join("\n").as_bytes()),
        None => null_mut(),
    }
}

/// `nativeKey(nameUtf8, down)` — Sondertaste.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeKey(
    env: Env,
    _cls: jclass,
    h: jlong,
    name: jbyteArray,
    down: jboolean,
) {
    let key = text(env, name);
    with(h, |e| {
        e.send(ClientMsg::Key {
            key: key.clone(),
            down,
        });
    });
}

/// `nativeAndroidKey(keyCode, metaState)` → true = behandelt.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeAndroidKey(
    _env: Env,
    _cls: jclass,
    h: jlong,
    code: jint,
    _meta: jint,
) -> jboolean {
    match android_key(code) {
        Some(m) => {
            with(h, |e| e.send(m));
            true
        }
        None => false,
    }
}

/// `nativeChar(codepoint)` — Zeichen der Softwaretastatur.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeChar(
    _env: Env,
    _cls: jclass,
    h: jlong,
    codepoint: jint,
) -> jboolean {
    let Some(c) = char::from_u32(codepoint as u32) else {
        return false;
    };
    match char_msg(c) {
        Some(m) => {
            with(h, |e| e.send(m));
            true
        }
        None => false,
    }
}

/// `nativePaste(utf8)` — Zwischenablage als Text.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePaste(
    env: Env,
    _cls: jclass,
    h: jlong,
    utf8: jbyteArray,
) {
    let s = text(env, utf8);
    if !s.is_empty() {
        with(h, |e| e.send(ClientMsg::Text(s.clone())));
    }
}

/// `nativeMouse(x, y)` — absolute Server-Pixel.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeMouse(
    _env: Env,
    _cls: jclass,
    h: jlong,
    x: jint,
    y: jint,
) {
    with(h, |e| {
        e.send(ClientMsg::MouseMove {
            x: x as u16,
            y: y as u16,
        });
    });
}

/// `nativeButton(button, down)` — 1 links, 2 mitte, 3 rechts.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeButton(
    _env: Env,
    _cls: jclass,
    h: jlong,
    button: jint,
    down: jboolean,
) {
    with(h, |e| {
        e.send(ClientMsg::Button {
            button: button as u8,
            down,
        });
    });
}

/// `nativeWheel(dy)` — Scrollen als PageUp/PageDown (v4 hat kein Rad).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeWheel(
    _env: Env,
    _cls: jclass,
    h: jlong,
    dy: jint,
) {
    let key = if dy < 0 { "PageUp" } else { "PageDown" };
    with(h, |e| {
        e.send(ClientMsg::Key {
            key: key.into(),
            down: true,
        });
        e.send(ClientMsg::Key {
            key: key.into(),
            down: false,
        });
    });
}
