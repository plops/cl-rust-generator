//! dnb-tracker: Terminal-Tracker fuer Drum and Bass und Phonk.
//!
//! Nur Modul-Deklarationen. Reihenfolge = Datenfluss:
//! Typen → DSP → Stimme → Protokoll → Engine → Audio-Ausgabe/WAV → Song-IO → Demos → UI.

#[path = "06_audio_out.rs"]
pub mod audio_out;
#[path = "09_demo_songs.rs"]
pub mod demo_songs;
#[path = "02_dsp.rs"]
pub mod dsp;
#[path = "05_engine.rs"]
pub mod engine;
#[path = "04_protocol.rs"]
pub mod protocol;
#[path = "08_song_io.rs"]
pub mod song_io;
#[path = "01_types.rs"]
pub mod types;
#[path = "12_ui_runner.rs"]
pub mod ui_runner;
#[path = "10_ui_state.rs"]
pub mod ui_state;
#[path = "11_ui_view.rs"]
pub mod ui_view;
#[path = "03_voice.rs"]
pub mod voice;
#[path = "07_wav.rs"]
pub mod wav;
