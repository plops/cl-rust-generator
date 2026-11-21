//! Built-in demo songs, generated from compact rhythm/melody tables.
//!
//! Channel layout of both demos:
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
        "dnb" => Some(demo_dnb()),
        "phonk" => Some(demo_phonk()),
        "empty" => {
            let mut s = Song::new();
            kit(&mut s);
            Some(s)
        }
        _ => None,
    }
}
