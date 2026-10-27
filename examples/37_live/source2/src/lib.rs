//! live-silk Bibliothek: nur Modul-Deklarationen (Datenfluss-Reihenfolge).
//! 01 Config → 02 Sprachen → 03 Codec → 04 Raumbuch → 05 Gemini →
//! 06 Relay → 07 Router → 08 Status-TUI.

#[path = "03_codec.rs"]
pub mod codec;
#[path = "01_config.rs"]
pub mod config;
#[path = "05_gemini.rs"]
pub mod gemini;
#[path = "02_languages.rs"]
pub mod languages;
#[path = "04_state.rs"]
pub mod state;
