//! `04_jni` — JNI-Exporte für `de.lbw.client.CoreBridge` (Kotlin).
//!
//! Handle = `Box<Mutex<Engine>>` als `long`. Zeichenketten reisen als
//! UTF-8-`byte[]` (JNI-`NewStringUTF` erwartet „modified UTF-8“), der
//! Canvas über einen Direct-`ByteBuffer` (kein Kopieren in den Java-Heap).
//! Alle `unsafe`-Zugriffe auf `JNIEnv` stehen in den Helfern oben.

use std::ptr::null_mut;
use std::sync::{Mutex, MutexGuard};
use std::time::Duration;

use jni_sys::{JNIEnv, jboolean, jbyte, jbyteArray, jclass, jint, jlong, jobject};
use lbw_common::Input;

use crate::engine::Engine;
use crate::keymap::{android_key, char_with_mods, meta_mods};

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

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeNew(
    env: Env,
    _: jclass,
    addr: jbyteArray,
    dead_after_s: jint,
) -> jlong {
    let secs = u64::try_from(dead_after_s).unwrap_or(90).max(5);
    let e = Engine::new(&text(env, addr), Duration::from_secs(secs));
    Box::into_raw(Box::new(Mutex::new(e))) as jlong
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeFree(_: Env, _: jclass, h: jlong) {
    if h != 0 {
        // SAFETY: `h` stammt aus `nativeNew` und wird genau einmal freigegeben.
        drop(unsafe { Box::from_raw(h as *mut Mutex<Engine>) });
    }
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePoll(
    env: Env,
    _: jclass,
    h: jlong,
    frame: jobject,
) -> jint {
    engine(h).map_or(0, |mut e| e.poll(direct(env, frame)))
}

/// `(w << 16) | h`.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeSize(
    _: Env,
    _: jclass,
    h: jlong,
) -> jint {
    engine(h).map_or(0, |e| {
        let (w, h) = e.size();
        ((w as jint) << 16) | h as jint
    })
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeTexts(
    env: Env,
    _: jclass,
    h: jlong,
) -> jbyteArray {
    engine(h).map_or(null_mut(), |mut e| new_bytes(env, &e.texts()))
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeStatus(
    env: Env,
    _: jclass,
    h: jlong,
) -> jbyteArray {
    engine(h).map_or(null_mut(), |e| new_bytes(env, e.status().as_bytes()))
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeKey(
    _: Env,
    _: jclass,
    h: jlong,
    keysym: jint,
    mods: jint,
) {
    with(h, |e| {
        e.send(Input::Key {
            keysym: keysym as u32,
            mods: mods as u8,
        });
    });
}

/// Android-Taste; `false` → nicht zugeordnet (Kotlin sendet das Zeichen).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeAndroidKey(
    _: Env,
    _: jclass,
    h: jlong,
    code: jint,
    meta: jint,
) -> jboolean {
    let Some(i) = android_key(code, meta_mods(meta)) else {
        return false;
    };
    with(h, |e| e.send(i));
    true
}

/// Zeichen mit Protokoll-Modifiern (`mods`-Bits).
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeChar(
    _: Env,
    _: jclass,
    h: jlong,
    cp: jint,
    mods: jint,
) -> jboolean {
    let Some(i) = char::from_u32(cp as u32).and_then(|c| char_with_mods(c, mods as u8)) else {
        return false;
    };
    with(h, |e| e.send(i));
    true
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativePaste(
    env: Env,
    _: jclass,
    h: jlong,
    utf8: jbyteArray,
) {
    let s = text(env, utf8);
    with(h, |e| e.paste(&s));
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeMouse(
    _: Env,
    _: jclass,
    h: jlong,
    x: jint,
    y: jint,
    force: jboolean,
) {
    with(h, |e| e.mouse(x, y, force));
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeButton(
    _: Env,
    _: jclass,
    h: jlong,
    button: jint,
    down: jboolean,
) {
    with(h, |e| {
        e.send(Input::Button {
            button: button as u8,
            down,
        });
    });
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeWheel(
    _: Env,
    _: jclass,
    h: jlong,
    dy: jint,
) {
    with(h, |e| {
        e.send(Input::Wheel {
            dy: dy.clamp(-1, 1) as i8,
        });
    });
}

/// Text im Rechteck (Server-Pixel) als UTF-8.
#[unsafe(no_mangle)]
pub extern "system" fn Java_de_lbw_client_CoreBridge_nativeSelect(
    env: Env,
    _: jclass,
    h: jlong,
    x0: jint,
    y0: jint,
    x1: jint,
    y1: jint,
) -> jbyteArray {
    engine(h).map_or(null_mut(), |e| {
        let s = e.select((x0 as f32, y0 as f32), (x1 as f32, y1 as f32));
        new_bytes(env, s.as_bytes())
    })
}
