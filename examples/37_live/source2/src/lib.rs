//! live-silk Bibliothek: nur Modul-Deklarationen (Datenfluss-Reihenfolge).
//! 01 Config → 02 Sprachen → 02b Stimmen → 03 Codec → 04 Raumbuch →
//! 05 Gemini → 06 Relay (06b Opus-Pfad) → 07 Router → 08 Status-TUI.

#[path = "03_codec.rs"]
pub mod codec;
#[path = "01_config.rs"]
pub mod config;
#[path = "05_gemini.rs"]
pub mod gemini;
#[path = "02_languages.rs"]
pub mod languages;
#[path = "06_relay.rs"]
pub mod relay;
#[path = "06b_relay_opus.rs"]
pub mod relay_opus;
#[path = "07_serve.rs"]
pub mod serve;
#[path = "04_state.rs"]
pub mod state;
#[path = "08_status.rs"]
pub mod status;
#[path = "02b_voices.rs"]
pub mod voices;
