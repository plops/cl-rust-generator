//! `lbw-client` — schlanker MVP-Client: Empfang, AV1-Dekodierung,
//! Szenen-Zusammenbau, Eingabe-Weiterleitung. Nur Modul-Deklarationen.

#[path = "01_config.rs"]
pub mod config;

#[path = "02_av1.rs"]
pub mod av1;

#[path = "03_net.rs"]
pub mod net;

#[path = "04_scene.rs"]
pub mod scene;

#[path = "05_app.rs"]
pub mod app;
