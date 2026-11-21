//! Built-in demo songs, generated from compact rhythm/melody tables.
//!
//! Two families, told apart by [`Song::sources`]:
//! * `synth_*`: fully computed, built into the binary.
//! * `mixed_*`: play recorded WAVs (Amen break, phonk kit) next to synth parts.
//!   Their sample paths are relative to `songs/` (see `examples/write_songs.rs`).
//!
//! Channel layout of the synth demos:
//! 0 kick · 1 snare · 2 hat · 3 reese · 4 sub/808 · 5 bell · 6 lead · 7 open hat

use crate::engine::fx;
use crate::types::{Cell, Kind, Note, Patch, ROWS, Song, parse_note};

fn n(s: &str) -> Note {
    parse_note(s).unwrap_or_else(|| panic!("bad note {s}"))
}

const C4: &str = "C-4";

fn kit(song: &mut Song) {
    let ins = &mut song.instruments;
    ins[0] = Patch::new("Kick", Kind::Kick);
    ins[1] = Patch::new("Snare", Kind::Snare);
    ins[2] = Patch::new("Hat", Kind::Hat);
    ins[3] = Patch::new("Reese", Kind::Reese);
    ins[4] = Patch::new("808", Kind::Sub);
    ins[5] = Patch::new("Cowbell", Kind::Bell);
    ins[6] = Patch::new("Lead", Kind::Lead);
    ins[7] = Patch::new("OpenHat", Kind::Hat);
    ins[7].decay = 0.25;
    ins[7].cutoff = 6500.0;
    ins[7].volume = 0.3;
    song.pan = [0.0, 0.0, 0.35, 0.0, 0.0, -0.3, 0.25, -0.35];
}

/// Put `note` with instrument `ins` on every row of `rows` (relative to bar starts).
fn hits(song: &mut Song, pat: usize, ch: usize, ins: u8, rows: &[usize], vol: u8) {
    for bar in (0..ROWS).step_by(16) {
        for &r in rows {
            song.set_cell(pat, bar + r, ch, Cell::note(n(C4), ins).vol(vol));
        }
    }
}

/// Write a melody/bass line: `(row, note, fx, arg)`; note `"==="` = off.
fn line(song: &mut Song, pat: usize, ch: usize, ins: u8, events: &[(usize, &str, u8, u8)]) {
    for &(row, note, f, arg) in events {
        let i = if note == "===" { 0xFF } else { ins };
        song.set_cell(pat, row, ch, Cell::note(n(note), i).fx(f, arg));
    }
}

fn hats(song: &mut Song, pat: usize, ch: usize, ins: u8, every: usize) {
    for r in (0..ROWS).step_by(every) {
        let vol = if r % 4 == 2 { 0x38 } else { 0x20 };
        song.set_cell(pat, r, ch, Cell::note(n(C4), ins).vol(vol));
    }
}

/// Drum and Bass at 174 BPM: two-step beat, detuned reese bass with filter sweeps.
pub fn demo_dnb() -> Song {
    let mut s = Song::new();
    s.title = "Two-Step Reese".into();
    s.bpm = 174;
    kit(&mut s);
    s.instruments[6].cutoff = 3200.0;
    for pat in 0..3 {
        hits(&mut s, pat, 0, 0, &[0, 10], 0x40);
        hits(&mut s, pat, 1, 1, &[4, 12], 0x40);
        hats(&mut s, pat, 2, 2, 2);
        hits(&mut s, pat, 7, 7, &[14], 0x30);
    }
    // Ghost snares and a roll at the end of the variation.
    for pat in 1..3 {
        hits(&mut s, pat, 1, 1, &[7, 15], 0x14);
    }
    s.set_cell(2, 60, 1, Cell::note(n(C4), 1).vol(0x30).fx(fx::RETRIG, 2));
    s.set_cell(2, 62, 1, Cell::note(n(C4), 1).vol(0x38).fx(fx::RETRIG, 1));
    // Reese bassline in D minor, the cutoff opens and closes per bar.
    let roots = ["D-2", "D-2", "A#1", "C-2"];
    for pat in 1..3 {
        for (bar, root) in roots.iter().enumerate() {
            let r = bar * 16;
            line(
                &mut s,
                pat,
                3,
                3,
                &[
                    (r, root, fx::CUTOFF, 0x60),
                    (r + 10, root, fx::CUTOFF, 0x90),
                ],
            );
            for (k, cut) in [(4, 0x78), (8, 0x88), (14, 0x50)] {
                s.set_cell(pat, r + k, 3, Cell::EMPTY.fx(fx::CUTOFF, cut));
            }
        }
    }
    // Sparse lead in the variation.
    line(
        &mut s,
        2,
        6,
        6,
        &[
            (0, "A-4", 0, 0),
            (6, "F-4", 0, 0),
            (12, "E-4", 0, 0),
            (16, "D-4", 0, 0),
            (24, "===", 0, 0),
            (32, "A-4", 0, 0),
            (38, "C-5", 0, 0),
            (44, "A#4", 0, 0),
            (48, "A-4", 0, 0),
            (56, "===", 0, 0),
        ],
    );
    s.order[..4].copy_from_slice(&[0, 1, 1, 2]);
    s.order_len = 4;
    s
}

/// Phonk at 140 BPM: cowbell riff, distorted 808 with slides, rolling hats.
pub fn demo_phonk() -> Song {
    let mut s = Song::new();
    s.title = "Cowbell Drift".into();
    s.bpm = 140;
    kit(&mut s);
    let ins = &mut s.instruments;
    ins[0].decay = 0.3;
    ins[0].drive = 0.6;
    ins[1].decay = 0.22;
    ins[1].cutoff = 1800.0;
    ins[4].decay = 1.6;
    ins[4].drive = 0.85;
    ins[4].volume = 0.7;
    let bell_a = [
        (0, "F-5"),
        (3, "F-5"),
        (6, "G#5"),
        (8, "F-5"),
        (10, "C-6"),
        (12, "A#5"),
        (14, "G#5"),
    ];
    let bell_b = [
        (0, "F-5"),
        (3, "F-5"),
        (6, "G#5"),
        (8, "D#5"),
        (10, "C#5"),
        (12, "C-5"),
        (14, "C#5"),
    ];
    for pat in 0..2 {
        hits(&mut s, pat, 0, 0, &[0, 10], 0x40);
        hits(&mut s, pat, 1, 1, &[8], 0x40);
        hats(&mut s, pat, 2, 2, 2);
        for bar in 0..4 {
            let riff = if bar % 2 == 0 { &bell_a } else { &bell_b };
            for &(r, note) in riff {
                s.set_cell(pat, bar * 16 + r, 5, Cell::note(n(note), 5));
            }
        }
    }
    // Triplet-ish hat rolls before each snare.
    for bar in (0..ROWS).step_by(16) {
        s.set_cell(
            1,
            bar + 6,
            2,
            Cell::note(n(C4), 2).vol(0x28).fx(fx::RETRIG, 2),
        );
        s.set_cell(
            1,
            bar + 14,
            2,
            Cell::note(n(C4), 2).vol(0x30).fx(fx::RETRIG, 3),
        );
    }
    // 808 follows the kick and slides up into the next bar.
    let bass = [
        ("F-2", "G#2"),
        ("C#2", "D#2"),
        ("F-2", "C-3"),
        ("A#1", "C-2"),
    ];
    for (bar, (root, slide)) in bass.iter().enumerate() {
        let r = bar * 16;
        line(
            &mut s,
            1,
            4,
            4,
            &[
                (r, root, 0, 0),
                (r + 10, root, 0, 0),
                (r + 14, slide, fx::GLIDE, 0x10),
            ],
        );
    }
    s.order[..4].copy_from_slice(&[0, 1, 1, 1]);
    s.order_len = 4;
    s
}

pub fn by_name(name: &str) -> Option<Song> {
    match name {
        "synth-dnb" => Some(demo_dnb()),
        "synth-phonk" => Some(demo_phonk()),
        "empty" => {
            let mut s = Song::new();
            kit(&mut s);
            Some(s)
        }
        _ => None,
    }
}

/// Names accepted by `--demo` (all fully synthesized, so they need no files).
pub const SYNTH_DEMOS: [&str; 3] = ["synth-dnb", "synth-phonk", "empty"];

/// Sample folder as seen from the `songs/` directory.
const SAMPLES: &str = "../samples";
const PHONK: &str = "phonk_src/repo";

fn sample(name: &str, path: &str, root: &str) -> Patch {
    let mut p = Patch::new(name, Kind::Sample);
    p.sample_path = format!("{SAMPLES}/{path}");
    p.root = n(root);
    p
}

/// Jungle at 175 BPM: the Amen break (rhythm-lab.com, `cw_amen01_175.wav`, exactly
/// two bars) chopped with `Oxx` into 32 slices = 16ths, plus synth reese and lead.
pub fn mixed_amen_jungle() -> Song {
    let mut s = Song::new();
    s.title = "Amen Jungle".into();
    s.bpm = 175;
    kit(&mut s);
    s.instruments[8] = sample("Amen", "amen/cw_amen01_175.wav", C4);
    s.instruments[8].volume = 0.9;
    s.instruments[3].cutoff = 700.0;
    s.instruments[3].volume = 0.38;
    s.instruments[6].attack = 0.3;
    s.instruments[6].release = 0.8;
    s.instruments[6].cutoff = 1800.0;
    s.instruments[6].reverb = 0.6;
    // One effect column per cell: a roll is `Rxx` on the row *after* the slice
    // (no note there, so the engine retriggers the slice including its offset).
    let amen = |s: &mut Song, pat: usize, row: usize, slice: u8, roll: u8| {
        s.set_cell(pat, row, 0, Cell::note(n(C4), 8).fx(fx::OFFSET, slice));
        if roll > 0 {
            s.set_cell(pat, row + 1, 0, Cell::EMPTY.fx(fx::RETRIG, roll));
        }
    };
    // Pattern 0: the loop as recorded (one trigger per two bars).
    amen(&mut s, 0, 0, 0, 0);
    amen(&mut s, 0, 32, 0, 0);
    // Pattern 1: retriggered every 8th; the second half re-orders slices and rolls.
    let straight: Vec<u8> = (0..32).step_by(2).collect();
    let chopped = [0, 2, 4, 4, 8, 10, 12, 12, 16, 18, 4, 22, 24, 12, 28, 30];
    for (i, (&a, &b)) in straight.iter().zip(&chopped).enumerate() {
        amen(&mut s, 1, i * 2, a, 0);
        let roll = match i {
            14 => 3,
            15 => 2,
            _ => 0,
        };
        amen(&mut s, 1, 32 + i * 2, b, roll);
    }
    for pat in 0..2 {
        // Synth kick under the break for weight on small speakers.
        hits(&mut s, pat, 1, 0, &[0, 10], 0x28);
        let roots = ["A-1", "A-1", "F-1", "G-1"];
        for (bar, root) in roots.iter().enumerate() {
            let r = bar * 16;
            line(&mut s, pat, 3, 3, &[(r, root, fx::CUTOFF, 0x58)]);
            s.set_cell(pat, r + 8, 3, Cell::EMPTY.fx(fx::CUTOFF, 0x80));
            s.set_cell(pat, r + 14, 3, Cell::EMPTY.fx(fx::CUTOFF, 0x48));
        }
    }
    line(
        &mut s,
        1,
        6,
        6,
        &[
            (0, "E-4", 0, 0),
            (16, "C-4", 0, 0),
            (32, "E-4", 0, 0),
            (48, "D-4", 0, 0),
        ],
    );
    s.order[..4].copy_from_slice(&[0, 1, 0, 1]);
    s.order_len = 4;
    s
}

/// Drift phonk at 125 BPM from the free Phonk Producer Toolkit (GareBear99):
/// sampled kick/clap/hat, 808 (root F-1) and cowbell (root F-4); riffs follow the
/// toolkit's `melodies.json`/`grooves.json`. Vinyl texture, riser and a synth pad.
pub fn mixed_drift_phonk() -> Song {
    let mut s = Song::new();
    s.title = "Drift Phonk Kit".into();
    s.bpm = 125;
    kit(&mut s);
    let ins = &mut s.instruments;
    ins[8] = sample(
        "Kick",
        &format!("{PHONK}/Free_Phonk_Producer_Toolkit/Drums/Kicks/Kick_1.wav"),
        C4,
    );
    ins[9] = sample(
        "Clap",
        &format!("{PHONK}/Drift_Phonk_Pack_V3/Drums/Claps/Clap_1.wav"),
        C4,
    );
    ins[9].reverb = 0.2;
    ins[10] = sample(
        "Hat",
        &format!("{PHONK}/Free_Phonk_Producer_Toolkit/Drums/Hats/Hat_1.wav"),
        C4,
    );
    ins[10].volume = 0.5;
    ins[11] = sample(
        "808 F",
        &format!("{PHONK}/Drift_Phonk_Pack_V3/808s/Drift808_F.wav"),
        "F-1",
    );
    ins[11].volume = 0.5;
    ins[12] = sample(
        "Cowbell F",
        &format!("{PHONK}/Free_Phonk_Producer_Toolkit/Cowbells/Cowbell_F.wav"),
        "F-4",
    );
    ins[12].volume = 0.6;
    ins[12].reverb = 0.25;
    ins[13] = sample(
        "Vinyl",
        &format!("{PHONK}/Free_Phonk_Producer_Toolkit/Textures/VinylNoise.wav"),
        C4,
    );
    ins[13].volume = 0.35;
    ins[14] = sample(
        "Riser",
        &format!("{PHONK}/Free_Phonk_Producer_Toolkit/FX/PhonkRiser.wav"),
        C4,
    );
    ins[14].volume = 0.5;
    ins[6].attack = 0.4;
    ins[6].release = 1.0;
    ins[6].cutoff = 1200.0;
    ins[6].volume = 0.3;
    ins[6].reverb = 0.6;
    // Cowbell phrases from melodies.json (F minor), 3-3-2 rhythm + echo of the turn.
    let melodies = [
        ["F-4", "F-4", "G-4", "G#4"],
        ["F-4", "G-4", "G#4", "C-5"],
        ["F-4", "G#4", "G-4", "F-4"],
        ["F-4", "F-4", "G-4", "G#4"],
    ];
    // 808 grooves from grooves.json, one octave down (root F-1).
    let grooves = [["F-1", "F-1", "F-1", "C-2"], ["F-1", "G-1", "G#1", "F-1"]];
    for pat in 0..3 {
        for (bar, m) in melodies.iter().enumerate() {
            let r = bar * 16;
            let notes = [m[0], m[1], m[2], m[3], m[2], m[1]];
            for (k, row) in [0, 3, 6, 8, 10, 12].into_iter().enumerate() {
                s.set_cell(pat, r + row, 5, Cell::note(n(notes[k]), 12));
            }
        }
    }
    s.set_cell(0, 0, 7, Cell::note(n(C4), 13));
    s.set_cell(0, 48, 6, Cell::note(n(C4), 14));
    for pat in 1..3 {
        hits(&mut s, pat, 0, 8, &[0, 10], 0x40);
        hits(&mut s, pat, 1, 9, &[4, 12], 0x38);
        hats(&mut s, pat, 2, 10, 2);
        for bar in 0..4 {
            let g = grooves[bar % 2];
            let r = bar * 16;
            line(
                &mut s,
                pat,
                4,
                11,
                &[
                    (r, g[0], 0, 0),
                    (r + 6, g[1], 0, 0),
                    (r + 10, g[2], 0, 0),
                    (r + 14, g[3], fx::GLIDE, 0x0C),
                ],
            );
        }
        line(
            &mut s,
            pat,
            6,
            6,
            &[(0, "F-3", 0, 0), (32, "C#3", 0, 0), (48, "D#3", 0, 0)],
        );
    }
    for bar in (0..ROWS).step_by(16) {
        s.set_cell(
            2,
            bar + 7,
            2,
            Cell::note(n(C4), 10).vol(0x28).fx(fx::RETRIG, 2),
        );
        s.set_cell(
            2,
            bar + 15,
            2,
            Cell::note(n(C4), 10).vol(0x30).fx(fx::RETRIG, 1),
        );
    }
    s.order[..4].copy_from_slice(&[0, 1, 2, 1]);
    s.order_len = 4;
    s
}

/// Song files written by `examples/write_songs.rs` into `songs/`.
pub fn mixed_songs() -> [(&'static str, Song); 2] {
    [
        ("mixed_amen_jungle.txt", mixed_amen_jungle()),
        ("mixed_drift_phonk.txt", mixed_drift_phonk()),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Sources;

    #[test]
    fn badges_tell_synth_from_sampled_demos() {
        for name in ["synth-dnb", "synth-phonk"] {
            assert_eq!(by_name(name).unwrap().sources(), Sources::Synth, "{name}");
        }
        for (file, song) in mixed_songs() {
            assert_eq!(song.sources(), Sources::Mixed, "{file}");
        }
    }
}
