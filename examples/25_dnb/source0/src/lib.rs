//! dark_dnb_synth — 174 BPM Drum-and-Bass-Engine (ALSA via CPAL, DSP via fundsp).
//! `lib.rs` enthaelt nur Modul-Deklarationen + Re-Exporte (Dateiregel s. Prompt).

#[path = "04_arrange.rs"]
mod arrange;
#[path = "06_backend.rs"]
mod backend;
#[path = "02_bass.rs"]
mod bass;
#[path = "01_clock.rs"]
mod clock;
#[path = "03_drums.rs"]
mod drums;
#[path = "05_mix.rs"]
mod mixdown;
#[path = "00_preset.rs"]
mod preset;

pub use arrange::{drum_active, section_at, section_gain, Section, ARR_VER};
pub use backend::{
    device_matches, find_output_device, list_devices, play_live, render_bars, write_wav,
};
pub use bass::{build_reese, reese_sample, sub_808_sample};
pub use clock::{bar_index, bar_secs, frames_per_step, sixteenth_hz, step_index};
pub use drums::{
    decay, hat_sample, kick_sample, pattern_for, siren_sample, snare_sample, Noise, PATTERN,
};
pub use mixdown::{gain_linear, mix, soft_clip, Master, MAX_GAIN_DB};
pub use preset::{Preset, PRESET_VER};
