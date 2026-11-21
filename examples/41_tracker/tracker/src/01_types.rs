//! Song data model shared by UI and audio thread.
//!
//! Everything a song can ever hold is allocated up front by [`Song::new`],
//! so edits arriving in the audio thread never allocate.

use std::sync::Arc;

pub const CHANNELS: usize = 8;
pub const ROWS: usize = 64;
pub const MAX_PATTERNS: usize = 32;
pub const MAX_ORDER: usize = 64;
pub const MAX_INSTR: usize = 16;
/// Rows per beat (a "line per beat" of 4 = 16th notes).
pub const LPB: u32 = 4;

/// MIDI-like note number: 0 = empty, 1..=120 = C-0..B-9, 255 = note off.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Note(pub u8);

impl Note {
    pub const NONE: Note = Note(0);
    pub const OFF: Note = Note(255);

    pub fn new(octave: u8, semitone: u8) -> Note {
        Note((octave * 12 + semitone + 1).min(120))
    }
    pub fn is_note(self) -> bool {
        (1..=120).contains(&self.0)
    }
    /// Frequency in Hz, A-4 = 440 Hz (note 58).
    pub fn freq(self) -> f32 {
        midi_freq(self.0 as f32 - 1.0)
    }
}

/// Frequency of a (fractional) semitone index where 0 = C-0.
pub fn midi_freq(semis: f32) -> f32 {
    440.0 * ((semis - 57.0) / 12.0).exp2()
}

const NAMES: [&str; 12] = [
    "C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-", "G#", "A-", "A#", "B-",
];

pub fn note_name(n: Note) -> String {
    match n {
        Note::NONE => "---".into(),
        Note::OFF => "===".into(),
        Note(v) => {
            let i = (v - 1) as usize;
            format!("{}{}", NAMES[i % 12], i / 12)
        }
    }
}

pub fn parse_note(s: &str) -> Option<Note> {
    match s {
        "---" => return Some(Note::NONE),
        "===" => return Some(Note::OFF),
        _ => {}
    }
    let (name, oct) = s.split_at_checked(2)?;
    let semi = NAMES.iter().position(|&x| x == name)? as u8;
    let oct: u8 = oct.parse().ok().filter(|&o| o <= 9)?;
    Some(Note::new(oct, semi))
}

/// One tracker cell. `0xFF` in `instr`/`vol` means "not set"; `fx == 0` means no effect.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cell {
    pub note: Note,
    pub instr: u8,
    pub vol: u8,
    pub fx: u8,
    pub arg: u8,
}

impl Cell {
    pub const EMPTY: Cell = Cell {
        note: Note::NONE,
        instr: 0xFF,
        vol: 0xFF,
        fx: 0,
        arg: 0,
    };
    pub const MAX_VOL: u8 = 0x40;

    pub fn note(note: Note, instr: u8) -> Cell {
        Cell {
            note,
            instr,
            ..Cell::EMPTY
        }
    }
    pub fn vol(mut self, v: u8) -> Cell {
        self.vol = v.min(Self::MAX_VOL);
        self
    }
    pub fn fx(mut self, fx: u8, arg: u8) -> Cell {
        self.fx = fx;
        self.arg = arg;
        self
    }
}

impl Default for Cell {
    fn default() -> Self {
        Cell::EMPTY
    }
}

pub type Pattern = [[Cell; CHANNELS]; ROWS];

/// Synthesis model of an instrument.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Kick,
    Snare,
    Hat,
    Reese,
    Sub,
    Bell,
    Lead,
    Sample,
}

impl Kind {
    pub const ALL: [Kind; 8] = [
        Kind::Kick,
        Kind::Snare,
        Kind::Hat,
        Kind::Reese,
        Kind::Sub,
        Kind::Bell,
        Kind::Lead,
        Kind::Sample,
    ];
    pub fn name(self) -> &'static str {
        match self {
            Kind::Kick => "kick",
            Kind::Snare => "snare",
            Kind::Hat => "hat",
            Kind::Reese => "reese",
            Kind::Sub => "sub",
            Kind::Bell => "bell",
            Kind::Lead => "lead",
            Kind::Sample => "sample",
        }
    }
    pub fn parse(s: &str) -> Option<Kind> {
        Kind::ALL.into_iter().find(|k| k.name() == s)
    }
}

/// Instrument parameters. Each [`Kind`] uses the subset that makes sense for it.
#[derive(Clone, Debug, PartialEq)]
pub struct Patch {
    pub name: String,
    pub kind: Kind,
    /// Seconds.
    pub attack: f32,
    pub decay: f32,
    /// 0..1
    pub sustain: f32,
    pub release: f32,
    /// Filter cutoff in Hz and resonance (Q).
    pub cutoff: f32,
    pub reso: f32,
    /// 0 = clean .. 1 = heavily saturated.
    pub drive: f32,
    /// Pitch envelope depth in semitones and its decay time in seconds.
    pub pitch_env: f32,
    pub pitch_decay: f32,
    /// Detune of the second oscillator in cents.
    pub detune: f32,
    pub volume: f32,
    /// Reverb send 0..1.
    pub reverb: f32,
    /// Path of the WAV file for [`Kind::Sample`] (kept for saving).
    pub sample_path: String,
    /// Mono sample data; shared so cloning a song stays cheap.
    pub sample: Arc<[f32]>,
    pub sample_rate: f32,
}

impl Patch {
    pub fn new(name: &str, kind: Kind) -> Patch {
        let mut p = Patch {
            name: name.into(),
            kind,
            attack: 0.001,
            decay: 0.3,
            sustain: 0.0,
            release: 0.05,
            cutoff: 18000.0,
            reso: 0.707,
            drive: 0.0,
            pitch_env: 0.0,
            pitch_decay: 0.05,
            detune: 0.0,
            volume: 0.8,
            reverb: 0.0,
            sample_path: String::new(),
            sample: Arc::from(Vec::new()),
            sample_rate: 48000.0,
        };
        match kind {
            Kind::Kick => {
                p.decay = 0.45;
                p.pitch_env = 36.0;
                p.pitch_decay = 0.035;
                p.drive = 0.3;
            }
            Kind::Snare => {
                p.decay = 0.18;
                p.cutoff = 1800.0;
                p.pitch_env = 5.0;
                p.pitch_decay = 0.03;
                p.reverb = 0.25;
            }
            Kind::Hat => {
                p.decay = 0.045;
                p.cutoff = 8000.0;
                p.volume = 0.45;
                p.reverb = 0.12;
            }
            Kind::Reese => {
                p.attack = 0.005;
                p.sustain = 1.0;
                p.release = 0.08;
                p.cutoff = 900.0;
                p.reso = 1.2;
                p.detune = 18.0;
                p.drive = 0.35;
                p.volume = 0.55;
            }
            Kind::Sub => {
                p.decay = 1.2;
                p.sustain = 0.0;
                p.release = 0.1;
                p.pitch_env = 7.0;
                p.pitch_decay = 0.02;
                p.drive = 0.6;
            }
            Kind::Bell => {
                p.decay = 0.35;
                p.cutoff = 2500.0;
                p.reso = 1.0;
                p.reverb = 0.3;
                p.volume = 0.5;
            }
            Kind::Lead => {
                p.attack = 0.01;
                p.decay = 0.25;
                p.sustain = 0.5;
                p.release = 0.2;
                p.cutoff = 2500.0;
                p.detune = 7.0;
                p.volume = 0.4;
                p.reverb = 0.35;
            }
            Kind::Sample => {
                p.decay = 0.0;
                p.sustain = 1.0;
                p.release = 0.02;
            }
        }
        p
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Song {
    pub bpm: u16,
    /// Ticks per row (effects like retrigger work in ticks).
    pub speed: u8,
    pub order: [u8; MAX_ORDER],
    pub order_len: usize,
    pub patterns: Vec<Box<Pattern>>,
    pub instruments: Vec<Patch>,
    /// Per-channel pan, -1 (left) .. 1 (right).
    pub pan: [f32; CHANNELS],
}

impl Song {
    pub fn new() -> Song {
        Song {
            bpm: 174,
            speed: 6,
            order: [0; MAX_ORDER],
            order_len: 1,
            patterns: (0..MAX_PATTERNS)
                .map(|_| Box::new([[Cell::EMPTY; CHANNELS]; ROWS]))
                .collect(),
            instruments: (0..MAX_INSTR)
                .map(|i| Patch::new(&format!("inst{i:02X}"), Kind::Lead))
                .collect(),
            pan: [0.0; CHANNELS],
        }
    }

    pub fn cell(&self, pat: usize, row: usize, ch: usize) -> Cell {
        self.patterns[pat][row][ch]
    }
    pub fn set_cell(&mut self, pat: usize, row: usize, ch: usize, c: Cell) {
        self.patterns[pat][row][ch] = c;
    }
}

impl Default for Song {
    fn default() -> Self {
        Song::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn note_names_roundtrip() {
        for v in 1..=120u8 {
            let n = Note(v);
            assert_eq!(parse_note(&note_name(n)), Some(n), "{}", note_name(n));
        }
        assert_eq!(parse_note("---"), Some(Note::NONE));
        assert_eq!(parse_note("==="), Some(Note::OFF));
        assert_eq!(parse_note("H-4"), None);
    }

    #[test]
    fn a4_is_440() {
        let a4 = parse_note("A-4").unwrap();
        assert!((a4.freq() - 440.0).abs() < 1e-3);
        assert!((parse_note("A-5").unwrap().freq() - 880.0).abs() < 1e-2);
    }

    #[test]
    fn kinds_roundtrip() {
        for k in Kind::ALL {
            assert_eq!(Kind::parse(k.name()), Some(k));
        }
    }
}
