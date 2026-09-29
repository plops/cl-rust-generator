//! `lbw-client` — schlanker Macroquad-Client: Empfang, AV1-Dekodierung,
//! Szenen-Zusammenbau mit Unifont-Text, Eingabe-Weiterleitung.
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

#[path = "06_select.rs"]
pub mod select;

#[path = "07_render.rs"]
pub mod render;

#[path = "08_app.rs"]
pub mod app;
