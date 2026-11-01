//! `lbw-mobile-core` — gemeinsamer Kern der Android- und iOS-Clients:
//! SSH-Kette (Jump-Hosts, Ed25519) + Protokoll v3 + AV1 + Szene in Rust,
//! die Apps machen nur UI/Touch/Lifecycle. Nur Modul-Deklarationen.

#[path = "01_engine.rs"]
pub mod engine;

#[path = "02_blob.rs"]
pub mod blob;

#[path = "03_keys.rs"]
pub mod keys;

#[path = "04_ffi.rs"]
pub mod ffi;

#[path = "05_jni.rs"]
pub mod jni;
