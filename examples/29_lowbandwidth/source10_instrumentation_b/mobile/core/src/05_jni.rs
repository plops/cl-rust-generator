//! `05_jni` — JNI-Exporte für `de.lbw.client.CoreBridge` (Kotlin).
//!
//! Signaturen identisch zu source11 (`android/rust-core/src/04_jni.rs`),
//! damit die dort per Emulator-E2E erprobte Kotlin-App weiterverwendbar
//! ist. Handle = derselbe Griff wie in der C-ABI (`04_ffi::LbwEngine`). Zeichenketten reisen als
//! UTF-8-`byte[]` (JNI-`NewStringUTF` erwartet „modified UTF-8“), der
//! Canvas über einen Direct-`ByteBuffer` (kein Kopieren in den Java-Heap).
//! Alle `unsafe`-Zugriffe auf `JNIEnv` stehen in den Helfern oben.

use std::ptr::null_mut;

use jni_sys::{JNIEnv, jboolean, jbyte, jbyteArray, jclass, jint, jlong, jobject};
use lbw_common::ClientMsg;

use crate::engine::Engine;
use crate::ffi::{LbwEngine, boxed, handle, send_char, send_text};
use crate::keys::android_key;

type Env = *mut JNIEnv;

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

fn with<R>(h: jlong, f: impl FnOnce(&mut Engine) -> R) -> Option<R> {
    // SAFETY: `h` stammt aus `nativeNew` und lebt bis `nativeFree`.
    let e = unsafe { handle(h as *const LbwEngine) }?;
    Some(f(&mut e.lock()))
}

/// `CoreBridge.version()`.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_version(env: Env, _cls: jclass) -> jbyteArray {
    new_bytes(env, b"lbw10b-mobile-core")
}

/// `nativeNew(configJsonUtf8, deadAfterS)` → Handle (0 bei Fehler).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeNew(
    env: Env,
    _cls: jclass,
    config: jbyteArray,
    _dead_after_s: jint,
) -> jlong {
    match Engine::new(&text(env, config)) {
        Ok(e) => boxed(e) as jlong,
        Err(e) => {
            eprintln!("[jni] nativeNew: {e}");
            0
        }
    }
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeFree(_env: Env, _cls: jclass, h: jlong) {
    // SAFETY: `h` stammt aus `nativeNew`, Kotlin ruft das genau einmal.
    unsafe { crate::ffi::lbw_free(h as *mut LbwEngine) };
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePoll(
    env: Env,
    _cls: jclass,
    h: jlong,
    frame: jobject,
) -> jint {
    with(h, |e| e.poll(direct(env, frame))).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeSize(
    _env: Env,
    _cls: jclass,
    h: jlong,
) -> jint {
    with(h, |e| {
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
    with(h, |e| new_bytes(env, &e.texts())).unwrap_or(null_mut())
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeStatus(
    env: Env,
    _cls: jclass,
    h: jlong,
) -> jbyteArray {
    with(h, |e| new_bytes(env, e.status().as_bytes())).unwrap_or(null_mut())
}

/// Gelernte Hostkey-Fingerprints pro Hop, ein `SHA256:…` pro Zeile.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePins(
    env: Env,
    _cls: jclass,
    h: jlong,
) -> jbyteArray {
    with(h, |e| new_bytes(env, e.pins().join("\n").as_bytes())).unwrap_or(null_mut())
}

/// `nativeKey(nameUtf8, down)` — Sondertaste (Kotlin ruft down + up).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeKey(
    env: Env,
    _cls: jclass,
    h: jlong,
    name: jbyteArray,
    down: jboolean,
) {
    let key = text(env, name);
    with(h, |e| e.send(ClientMsg::Key { key, down }));
}

/// `nativeAndroidKey(keyCode, metaState)` → true = behandelt (drücken + loslassen).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeAndroidKey(
    _env: Env,
    _cls: jclass,
    h: jlong,
    code: jint,
    _meta: jint,
) -> jboolean {
    match android_key(code) {
        Some(k) => with(h, |e| e.key(k)).is_some(),
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
    with(h, |e| send_char(e, c)).unwrap_or(false)
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
    with(h, |e| send_text(e, &s));
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
    // SAFETY: wie `with`.
    unsafe { crate::ffi::lbw_mouse(h as *const LbwEngine, x, y) };
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
    // SAFETY: wie `with`.
    unsafe { crate::ffi::lbw_button(h as *const LbwEngine, button, down) };
}

/// `nativeWheel(dy)` — Scrollen als PageUp/PageDown (v3 hat kein Rad).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeWheel(
    _env: Env,
    _cls: jclass,
    h: jlong,
    dy: jint,
) {
    with(h, |e| e.wheel(dy));
}
