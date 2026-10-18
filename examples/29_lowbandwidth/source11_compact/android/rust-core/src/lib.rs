//! `lbw-android-core` (A1): Engine + JNI-Brücke ohne Java-Crypto.
//!
//! Der gesamte SSH-Tunnel (`russh`) und das v4-Protokoll laufen hier;
//! Kotlin macht nur UI/Touch/Lifecycle.

#[path = "02_blob.rs"]
pub mod blob;
#[path = "01_engine.rs"]
pub mod engine;
#[path = "04_jni.rs"]
pub mod jni;
#[path = "03_keymap.rs"]
pub mod keymap;
