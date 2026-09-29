//! `lbw-core` — Rust-Kern des Android-Clients (`liblbw_core.so`).
//!
//! Wiederverwendet `lbw-client` ohne macroquad (Netz, rav1d, Szene,
//! Auswahl) und bietet Kotlin eine kleine Pull-Schnittstelle über JNI.
//! Nur Modul-Deklarationen.

#[path = "01_engine.rs"]
pub mod engine;

#[path = "02_blob.rs"]
pub mod blob;

#[path = "03_keymap.rs"]
pub mod keymap;

#[path = "04_jni.rs"]
pub mod jni;
