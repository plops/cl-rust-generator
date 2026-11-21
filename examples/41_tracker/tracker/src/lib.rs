//! dnb-tracker: Terminal-Tracker fuer Drum and Bass und Phonk.
//!
//! Nur Modul-Deklarationen. Reihenfolge = Datenfluss:
//! Typen → DSP → Stimme → Master-Effekte → Protokoll → Engine →
//! Audio-Ausgabe/WAV → Song-IO → Demos → UI.
//! (Leerzeilen trennen die Deklarationen, damit `cargo fmt` sie nicht alphabetisch sortiert.)

#[path = "01_types.rs"]
pub mod types;

#[path = "02_dsp.rs"]
pub mod dsp;

#[path = "03_voice.rs"]
pub mod voice;

#[path = "04_master_fx.rs"]
pub mod master_fx;

#[path = "05_protocol.rs"]
pub mod protocol;

#[path = "06_engine.rs"]
pub mod engine;

#[path = "07_audio_out.rs"]
pub mod audio_out;

#[path = "08_wav.rs"]
pub mod wav;

#[path = "09_song_io.rs"]
pub mod song_io;

#[path = "10_demo_songs.rs"]
pub mod demo_songs;

#[path = "11_ui_state.rs"]
pub mod ui_state;

#[path = "12_ui_view.rs"]
pub mod ui_view;

#[path = "13_ui_runner.rs"]
pub mod ui_runner;
