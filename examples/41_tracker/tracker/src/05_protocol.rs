//! Messages between UI thread and audio thread over lock-free rings.
//!
//! Three single-producer/single-consumer rings:
//! UI → audio: [`Command`]; audio → UI: [`Status`]; audio → UI: old songs
//! to drop, so the audio thread never frees memory.

use crate::types::{Cell, Note, Song};
use rtrb::{Consumer, Producer, RingBuffer};

#[derive(Debug)]
pub enum Command {
    SetCell {
        pat: u8,
        row: u8,
        ch: u8,
        cell: Cell,
    },
    SetBpm(u16),
    /// Replace the whole order list (length + entries).
    SetOrder {
        len: u8,
        order: [u8; crate::types::MAX_ORDER],
    },
    Play {
        order_pos: u8,
        /// Loop the pattern at `order_pos` instead of following the order list.
        loop_pattern: Option<u8>,
    },
    Stop,
    Preview {
        instr: u8,
        note: Note,
    },
    SetMute {
        ch: u8,
        mute: bool,
    },
    LoadSong(Box<Song>),
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Status {
    pub playing: bool,
    pub order_pos: u8,
    pub pattern: u8,
    pub row: u8,
    pub bpm: u16,
    pub peak_l: f32,
    pub peak_r: f32,
    /// Per-channel envelope peak since the last status.
    pub levels: [f32; crate::types::CHANNELS],
}

pub struct UiLink {
    pub cmd: Producer<Command>,
    pub status: Consumer<Status>,
    pub garbage: Consumer<Box<Song>>,
}

pub struct AudioLink {
    pub cmd: Consumer<Command>,
    pub status: Producer<Status>,
    pub garbage: Producer<Box<Song>>,
}

pub fn link() -> (UiLink, AudioLink) {
    let (cp, cc) = RingBuffer::new(4096);
    let (sp, sc) = RingBuffer::new(64);
    let (gp, gc) = RingBuffer::new(8);
    (
        UiLink {
            cmd: cp,
            status: sc,
            garbage: gc,
        },
        AudioLink {
            cmd: cc,
            status: sp,
            garbage: gp,
        },
    )
}

impl UiLink {
    /// Send a command; returns false when the ring is full (command dropped).
    pub fn send(&mut self, c: Command) -> bool {
        self.cmd.push(c).is_ok()
    }
    /// Latest status (drains older ones) and drops returned songs.
    pub fn poll(&mut self) -> Option<Status> {
        while self.garbage.pop().is_ok() {}
        let mut last = None;
        while let Ok(s) = self.status.pop() {
            last = Some(s);
        }
        last
    }
}
