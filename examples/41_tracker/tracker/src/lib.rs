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
#[path = "01_types.rs"]
pub mod types;
#[path = "03_voice.rs"]
pub mod voice;
#[path = "07_wav.rs"]
pub mod wav;
