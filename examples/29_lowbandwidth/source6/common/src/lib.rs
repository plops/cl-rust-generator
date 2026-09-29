//! `lbw-common` — Protokoll, Framing, Ratenbegrenzung und Farbraum für
//! den Low-Bandwidth-Remote-Desktop. Nur `std`, keine Abhängigkeiten.

#[path = "01_types.rs"]
pub mod types;

#[path = "02_codec.rs"]
pub mod codec;

#[path = "03_frame.rs"]
pub mod frame;

#[path = "04_keys.rs"]
pub mod keys;

#[path = "05_yuv.rs"]
pub mod yuv;

#[path = "06_rate.rs"]
pub mod rate;

pub use types::*;
