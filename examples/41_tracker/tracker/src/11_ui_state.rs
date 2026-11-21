//! Editor state and key handling. No terminal I/O here, so all of it is unit-testable.
//!
//! Every edit is applied to the UI's song copy *and* returned as a [`Command`],
//! which keeps the audio thread's copy in sync.

use crate::engine::fx;
use crate::protocol::{Command, Status};
use crate::types::{CHANNELS, Cell, MAX_INSTR, MAX_ORDER, MAX_PATTERNS, Note, ROWS, Song};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use std::path::PathBuf;

/// Cursor columns inside one channel: note, instr hi/lo, vol hi/lo, fx, arg hi/lo.
pub const COLS: usize = 8;
pub const COL_NOTE: usize = 0;
pub const COL_FX: usize = 5;

/// Lower and upper piano rows (FastTracker layout), semitone offset per key.
const PIANO: [(char, u8); 29] = [
    ('z', 0),
    ('s', 1),
    ('x', 2),
    ('d', 3),
    ('c', 4),
    ('v', 5),
    ('g', 6),
    ('b', 7),
    ('h', 8),
    ('n', 9),
    ('j', 10),
    ('m', 11),
    ('q', 12),
    ('2', 13),
    ('w', 14),
    ('3', 15),
    ('e', 16),
    ('r', 17),
    ('5', 18),
    ('t', 19),
    ('6', 20),
    ('y', 21),
    ('7', 22),
    ('u', 23),
    ('i', 24),
    ('9', 25),
    ('o', 26),
    ('0', 27),
    ('p', 28),
];

pub fn piano_offset(c: char) -> Option<u8> {
    PIANO.iter().find(|(k, _)| *k == c).map(|(_, o)| *o)
}

pub struct App {
    pub song: Song,
    pub path: PathBuf,
    pub pattern: usize,
    pub order_pos: usize,
    pub row: usize,
    pub ch: usize,
    pub col: usize,
    pub edit: bool,
    pub octave: u8,
    pub instr: u8,
    pub step: usize,
    pub mute: [bool; CHANNELS],
    pub status: Status,
    pub message: String,
    pub save_requested: bool,
    pub quit: bool,
}

impl App {
    pub fn new(song: Song, path: PathBuf) -> App {
        let pattern = song.order[0] as usize;
        App {
            song,
            path,
            pattern,
            order_pos: 0,
            row: 0,
            ch: 0,
            col: 0,
            edit: false,
            octave: 4,
            instr: 0,
            step: 1,
            mute: [false; CHANNELS],
            status: Status::default(),
            message: "Esc: edit mode · Space: play/stop · ?: help".into(),
            save_requested: false,
            quit: false,
        }
    }

    /// Feed the latest audio status; the cursor follows playback outside edit mode.
    pub fn on_status(&mut self, st: Status) {
        self.status = st;
        if st.playing && !self.edit {
            self.row = st.row as usize;
            self.pattern = st.pattern as usize;
            self.order_pos = st.order_pos as usize;
        }
    }

    pub fn cell(&self) -> Cell {
        self.song.cell(self.pattern, self.row, self.ch)
    }

    fn put(&mut self, cell: Cell, out: &mut Vec<Command>) {
        self.song.set_cell(self.pattern, self.row, self.ch, cell);
        out.push(Command::SetCell {
            pat: self.pattern as u8,
            row: self.row as u8,
            ch: self.ch as u8,
            cell,
        });
    }

    fn move_row(&mut self, d: isize) {
        self.row = (self.row as isize + d).rem_euclid(ROWS as isize) as usize;
    }

    fn move_col(&mut self, d: isize) {
        let total = (CHANNELS * COLS) as isize;
        let pos = (self.ch * COLS + self.col) as isize + d;
        let pos = pos.rem_euclid(total) as usize;
        self.ch = pos / COLS;
        self.col = pos % COLS;
    }

    fn order_cmd(&self) -> Command {
        Command::SetOrder {
            len: self.song.order_len as u8,
            order: self.song.order,
        }
    }

    pub fn handle_key(&mut self, k: KeyEvent) -> Vec<Command> {
        let mut out = Vec::new();
        let ctrl = k.modifiers.contains(KeyModifiers::CONTROL);
        match k.code {
            KeyCode::Char('q' | 'c') if ctrl => self.quit = true,
            KeyCode::Char('s') if ctrl => self.save_requested = true,
            KeyCode::Left if ctrl => self.set_order_pos(self.order_pos.saturating_sub(1)),
            KeyCode::Right if ctrl => self.set_order_pos(self.order_pos + 1),
            KeyCode::Esc => {
                self.edit = !self.edit;
                self.message = if self.edit { "EDIT on" } else { "EDIT off" }.into();
            }
            KeyCode::Char(' ') => out.push(if self.status.playing {
                Command::Stop
            } else {
                Command::Play {
                    order_pos: self.order_pos as u8,
                    loop_pattern: None,
                }
            }),
            KeyCode::Enter => out.push(Command::Play {
                order_pos: self.order_pos as u8,
                loop_pattern: Some(self.pattern as u8),
            }),
            KeyCode::Up => self.move_row(-1),
            KeyCode::Down => self.move_row(1),
            KeyCode::PageUp => self.move_row(-16),
            KeyCode::PageDown => self.move_row(16),
            KeyCode::Home => self.row = 0,
            KeyCode::End => self.row = ROWS - 1,
            KeyCode::Left => self.move_col(-1),
            KeyCode::Right => self.move_col(1),
            KeyCode::Tab => self.move_col(COLS as isize - self.col as isize),
            KeyCode::BackTab => self.move_col(-(COLS as isize) - self.col as isize),
            KeyCode::F(1) => self.octave = self.octave.saturating_sub(1),
            KeyCode::F(2) => self.octave = (self.octave + 1).min(8),
            KeyCode::F(3) => self.instr = (self.instr + MAX_INSTR as u8 - 1) % MAX_INSTR as u8,
            KeyCode::F(4) => self.instr = (self.instr + 1) % MAX_INSTR as u8,
            KeyCode::F(5) => self.pattern = (self.pattern + MAX_PATTERNS - 1) % MAX_PATTERNS,
            KeyCode::F(6) => self.pattern = (self.pattern + 1) % MAX_PATTERNS,
            KeyCode::F(7 | 8) => {
                let d: i32 = if k.code == KeyCode::F(7) { -1 } else { 1 };
                self.song.bpm = (self.song.bpm as i32 + d).clamp(30, 400) as u16;
                out.push(Command::SetBpm(self.song.bpm));
            }
            KeyCode::F(9) => self.step = self.step.saturating_sub(1),
            KeyCode::F(10) => self.step = (self.step + 1).min(16),
            KeyCode::F(12) => {
                self.mute[self.ch] = !self.mute[self.ch];
                out.push(Command::SetMute {
                    ch: self.ch as u8,
                    mute: self.mute[self.ch],
                });
            }
            KeyCode::Char('=') if !ctrl => {
                if self.song.order_len < MAX_ORDER {
                    self.song.order[self.song.order_len] = self.pattern as u8;
                    self.song.order_len += 1;
                    out.push(self.order_cmd());
                }
            }
            KeyCode::Char('-') if !ctrl => {
                if self.song.order_len > 1 {
                    self.song.order_len -= 1;
                    self.order_pos = self.order_pos.min(self.song.order_len - 1);
                    out.push(self.order_cmd());
                }
            }
            KeyCode::Char('?') => {
                self.message = "z..m/q..p notes · 1 note-off · 0-9a-f hex · fx: C D G R K T · \
                                F1/2 oct F3/4 ins F5/6 pat F7/8 bpm F9/10 step F12 mute · \
                                =/- order · Ctrl+←/→ order pos · Ctrl+S save"
                    .into()
            }
            KeyCode::Delete | KeyCode::Backspace if self.edit => self.clear_field(&mut out),
            KeyCode::Char(c) if !ctrl => self.on_char(c.to_ascii_lowercase(), &mut out),
            _ => {}
        }
        out
    }

    fn set_order_pos(&mut self, pos: usize) {
        self.order_pos = pos.min(self.song.order_len - 1);
        self.pattern = self.song.order[self.order_pos] as usize;
    }

    fn clear_field(&mut self, out: &mut Vec<Command>) {
        let mut c = self.cell();
        match self.col {
            COL_NOTE => {
                c.note = Note::NONE;
                c.instr = 0xFF;
                c.vol = 0xFF;
            }
            1 | 2 => c.instr = 0xFF,
            3 | 4 => c.vol = 0xFF,
            _ => {
                c.fx = 0;
                c.arg = 0;
            }
        }
        self.put(c, out);
        self.move_row(self.step as isize);
    }

    fn on_char(&mut self, c: char, out: &mut Vec<Command>) {
        if self.col == COL_NOTE || !self.edit {
            if self.edit && self.col == COL_NOTE && c == '1' {
                let cell = Cell::note(Note::OFF, 0xFF);
                self.put(cell, out);
                self.move_row(self.step as isize);
                return;
            }
            let Some(off) = piano_offset(c) else { return };
            let note = Note::new(self.octave, 0).0.saturating_add(off).min(120);
            let note = Note(note);
            out.push(Command::Preview {
                instr: self.instr,
                note,
            });
            if self.edit && self.col == COL_NOTE {
                let old = self.cell();
                let cell = Cell {
                    note,
                    instr: self.instr,
                    ..old
                };
                self.put(cell, out);
                self.move_row(self.step as isize);
            }
            return;
        }
        let mut cell = self.cell();
        if self.col == COL_FX {
            let up = c.to_ascii_uppercase() as u8;
            if fx::ALL.contains(&up) {
                cell.fx = up;
                self.put(cell, out);
                self.move_col(1);
            }
            return;
        }
        let Some(d) = c.to_digit(16).map(|d| d as u8) else {
            return;
        };
        let high = matches!(self.col, 1 | 3 | 6);
        let set = |v: u8| {
            let v = if v == 0xFF { 0 } else { v };
            if high {
                (d << 4) | (v & 0x0F)
            } else {
                (v & 0xF0) | d
            }
        };
        match self.col {
            1 | 2 => cell.instr = set(cell.instr).min(MAX_INSTR as u8 - 1),
            3 | 4 => cell.vol = set(cell.vol).min(Cell::MAX_VOL),
            _ => cell.arg = set(if cell.fx == 0 { 0 } else { cell.arg }),
        }
        self.put(cell, out);
        if high {
            self.move_col(1);
        } else {
            self.move_col(-1);
            self.move_row(self.step as isize);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::parse_note;

    fn key(c: KeyCode) -> KeyEvent {
        KeyEvent::new(c, KeyModifiers::NONE)
    }
    fn ch(c: char) -> KeyEvent {
        key(KeyCode::Char(c))
    }
    fn app() -> App {
        App::new(Song::new(), PathBuf::from("song.txt"))
    }

    #[test]
    fn jam_mode_only_previews() {
        let mut a = app();
        let cmds = a.handle_key(ch('q'));
        assert!(
            matches!(cmds[..], [Command::Preview { note, .. }] if note == parse_note("C-5").unwrap())
        );
        assert_eq!(a.cell(), Cell::EMPTY);
    }

    #[test]
    fn edit_mode_writes_note_and_advances() {
        let mut a = app();
        a.instr = 3;
        a.handle_key(key(KeyCode::Esc));
        let cmds = a.handle_key(ch('z'));
        assert_eq!(cmds.len(), 2, "preview + set cell");
        let c = a.song.cell(0, 0, 0);
        assert_eq!(c.note, parse_note("C-4").unwrap());
        assert_eq!(c.instr, 3);
        assert_eq!(a.row, 1);
        match &cmds[1] {
            Command::SetCell {
                row: 0,
                ch: 0,
                cell,
                ..
            } => assert_eq!(*cell, c),
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn hex_entry_volume_and_fx() {
        let mut a = app();
        a.handle_key(key(KeyCode::Esc));
        a.col = 3;
        a.handle_key(ch('3'));
        a.handle_key(ch('f'));
        assert_eq!(a.song.cell(0, 0, 0).vol, 0x3F);
        assert_eq!((a.row, a.col), (1, 3));
        a.col = COL_FX;
        a.handle_key(ch('r'));
        a.handle_key(ch('0'));
        a.handle_key(ch('3'));
        let c = a.song.cell(0, 1, 0);
        assert_eq!((c.fx, c.arg), (b'R', 0x03));
        a.col = COL_FX;
        a.handle_key(ch('x'));
        assert_eq!(a.song.cell(0, 2, 0).fx, 0, "x is no effect");
    }

    #[test]
    fn volume_is_clamped() {
        let mut a = app();
        a.handle_key(key(KeyCode::Esc));
        a.col = 3;
        a.handle_key(ch('f'));
        assert_eq!(a.song.cell(0, 0, 0).vol, Cell::MAX_VOL);
    }

    #[test]
    fn cursor_wraps_across_channels() {
        let mut a = app();
        a.handle_key(key(KeyCode::Left));
        assert_eq!((a.ch, a.col), (CHANNELS - 1, COLS - 1));
        a.handle_key(key(KeyCode::Tab));
        assert_eq!((a.ch, a.col), (0, 0));
        a.handle_key(key(KeyCode::Up));
        assert_eq!(a.row, ROWS - 1);
    }

    #[test]
    fn note_off_and_delete() {
        let mut a = app();
        a.handle_key(key(KeyCode::Esc));
        a.handle_key(ch('z'));
        a.handle_key(ch('1'));
        assert_eq!(a.song.cell(0, 1, 0).note, Note::OFF);
        a.row = 0;
        a.handle_key(key(KeyCode::Delete));
        assert_eq!(a.song.cell(0, 0, 0), Cell::EMPTY);
    }

    #[test]
    fn transport_and_order() {
        let mut a = app();
        assert!(matches!(
            a.handle_key(ch(' '))[..],
            [Command::Play {
                loop_pattern: None,
                ..
            }]
        ));
        a.status.playing = true;
        assert!(matches!(a.handle_key(ch(' '))[..], [Command::Stop]));
        a.pattern = 5;
        a.handle_key(ch('='));
        assert_eq!(&a.song.order[..a.song.order_len], &[0, 5]);
        a.handle_key(KeyEvent::new(KeyCode::Right, KeyModifiers::CONTROL));
        assert_eq!((a.order_pos, a.pattern), (1, 5));
        a.handle_key(ch('-'));
        assert_eq!(a.song.order_len, 1);
        assert_eq!(a.order_pos, 0);
    }
}
