//! `lbw-client` — schlanker Client: Empfang, AV1-Dekodierung,
//! Szenen-Zusammenbau mit Unifont-Text, Eingabe-Weiterleitung.
//! Ohne Feature `desktop` (macroquad) bleibt der plattformneutrale Kern,
//! den auch der Android-Client (`android_client/rust-core`) nutzt.
//! Nur Modul-Deklarationen.

#[path = "01_config.rs"]
pub mod config;

#[path = "02_av1.rs"]
pub mod av1;

#[path = "03_net.rs"]
pub mod net;

#[path = "04_scene.rs"]
pub mod scene;

#[path = "05_input.rs"]
pub mod input;

#[cfg(feature = "desktop")]
#[path = "06_keycode.rs"]
pub mod keycode;

#[path = "07_select.rs"]
pub mod select;

#[cfg(feature = "desktop")]
#[path = "08_render.rs"]
pub mod render;

#[cfg(feature = "desktop")]
#[path = "09_app.rs"]
pub mod app;
