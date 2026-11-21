//! Sequencer + mixer. Pure function of song, commands and sample rate, so the
//! same code drives the sound card, the WAV export and the tests.
//!
//! Real-time rules: no allocation, no locks, no frees inside [`Engine::process`].

use crate::dsp::pan_gains;
use crate::protocol::{AudioLink, Command, Status};
use crate::types::{CHANNELS, Cell, LPB, Note, ROWS, Song};
use crate::voice::Voice;

/// Effect command letters (stored as ASCII in [`Cell::fx`]).
pub mod fx {
    pub const CUTOFF: u8 = b'C';
    pub const DRIVE: u8 = b'D';
    pub const GLIDE: u8 = b'G';
    pub const RETRIG: u8 = b'R';
    pub const CUT: u8 = b'K';
    pub const TEMPO: u8 = b'T';
    pub const ALL: [u8; 6] = [CUTOFF, DRIVE, GLIDE, RETRIG, CUT, TEMPO];
}

/// `Cxx` argument → cutoff in Hz (exponential 40 Hz .. 18 kHz).
pub fn cutoff_hz(arg: u8) -> f32 {
    40.0 * (18000.0f32 / 40.0).powf(arg as f32 / 255.0)
}

const MASTER: f32 = 0.7;
/// Frames between two status messages (~10 ms at 48 kHz).
const STATUS_EVERY: u32 = 512;

#[derive(Clone, Copy, Default)]
struct ChanState {
    note: Note,
    instr: u8,
    vel: f32,
    retrig: u8,
    cut: u8,
}

pub struct Engine {
    song: Box<Song>,
    sr: f32,
    voices: [Voice; CHANNELS],
    preview: Voice,
    chan: [ChanState; CHANNELS],
    gains: [(f32, f32); CHANNELS],
    mute: [bool; CHANNELS],
    playing: bool,
    order_pos: usize,
    loop_pattern: Option<usize>,
    /// Next row to process and the row that is currently sounding.
    next_row: usize,
    cur_row: usize,
    cur_pattern: usize,
    tick: u8,
    tick_wait: f64,
    peak: (f32, f32),
    levels: [f32; CHANNELS],
    since_status: u32,
}

impl Engine {
    pub fn new(song: Box<Song>, sample_rate: f32) -> Engine {
        let mut e = Engine {
            song,
            sr: sample_rate,
            voices: Default::default(),
            preview: Voice::default(),
            chan: [ChanState::default(); CHANNELS],
            gains: [(0.0, 0.0); CHANNELS],
            mute: [false; CHANNELS],
            playing: false,
            order_pos: 0,
            loop_pattern: None,
            next_row: 0,
            cur_row: 0,
            cur_pattern: 0,
            tick: 0,
            tick_wait: 0.0,
            peak: (0.0, 0.0),
            levels: [0.0; CHANNELS],
            since_status: 0,
        };
        e.update_gains();
        e
    }

    pub fn song(&self) -> &Song {
        &self.song
    }

    fn update_gains(&mut self) {
        for (g, &p) in self.gains.iter_mut().zip(&self.song.pan) {
            *g = pan_gains(p);
        }
    }

    pub fn status(&self) -> Status {
        Status {
            playing: self.playing,
            order_pos: self.order_pos as u8,
            pattern: self.cur_pattern as u8,
            row: self.cur_row as u8,
            bpm: self.song.bpm,
            peak_l: self.peak.0,
            peak_r: self.peak.1,
            levels: self.levels,
        }
    }

    pub fn play(&mut self, order_pos: usize, loop_pattern: Option<usize>) {
        self.order_pos = order_pos.min(self.song.order_len.saturating_sub(1));
        self.loop_pattern = loop_pattern;
        self.next_row = 0;
        self.tick = 0;
        self.tick_wait = 0.0;
        self.playing = true;
    }

    pub fn stop(&mut self) {
        self.playing = false;
        self.voices.iter_mut().for_each(Voice::release);
    }

    /// Apply one command. Returns a song that must be dropped elsewhere.
    pub fn apply(&mut self, cmd: Command) -> Option<Box<Song>> {
        match cmd {
            Command::SetCell { pat, row, ch, cell } => {
                let (p, r, c) = (pat as usize, row as usize, ch as usize);
                if p < self.song.patterns.len() && r < ROWS && c < CHANNELS {
                    self.song.set_cell(p, r, c, cell);
                }
            }
            Command::SetBpm(b) => self.song.bpm = b.clamp(30, 400),
            Command::SetOrder { len, order } => {
                self.song.order = order;
                self.song.order_len = (len as usize).clamp(1, order.len());
                self.order_pos = self.order_pos.min(self.song.order_len - 1);
            }
            Command::Play {
                order_pos,
                loop_pattern,
            } => self.play(order_pos as usize, loop_pattern.map(usize::from)),
            Command::Stop => self.stop(),
            Command::Preview { instr, note } => {
                if note == Note::OFF {
                    self.preview.release();
                } else if let Some(p) = self.song.instruments.get(instr as usize) {
                    self.preview.trigger(instr as usize, p, note, 1.0, self.sr);
                }
            }
            Command::SetMute { ch, mute } => {
                if let Some(m) = self.mute.get_mut(ch as usize) {
                    *m = mute;
                }
            }
            Command::LoadSong(song) => {
                let old = std::mem::replace(&mut self.song, song);
                self.update_gains();
                self.order_pos = self.order_pos.min(self.song.order_len - 1);
                return Some(old);
            }
        }
        None
    }

    fn samples_per_tick(&self) -> f64 {
        self.sr as f64 * 60.0 / (self.song.bpm as f64 * LPB as f64 * self.song.speed.max(1) as f64)
    }

    fn trigger(&mut self, ch: usize) {
        let st = self.chan[ch];
        if let Some(p) = self.song.instruments.get(st.instr as usize) {
            self.voices[ch].trigger(st.instr as usize, p, st.note, st.vel, self.sr);
        }
    }

    fn process_row(&mut self) {
        let pat = match self.loop_pattern {
            Some(p) => p,
            None => self.song.order[self.order_pos] as usize,
        }
        .min(self.song.patterns.len() - 1);
        self.cur_pattern = pat;
        self.cur_row = self.next_row;
        for ch in 0..CHANNELS {
            let c: Cell = self.song.patterns[pat][self.cur_row][ch];
            let st = &mut self.chan[ch];
            st.retrig = 0;
            st.cut = 0;
            if c.fx == fx::TEMPO && c.arg >= 30 {
                self.song.bpm = c.arg as u16;
            }
            if c.note.is_note() {
                if c.instr != 0xFF {
                    st.instr = c.instr;
                }
                st.vel = if c.vol == 0xFF {
                    1.0
                } else {
                    c.vol as f32 / 64.0
                };
                if c.fx == fx::GLIDE && self.voices[ch].is_active() {
                    st.note = c.note;
                    self.voices[ch].glide_to(c.note, c.arg as f32 * 0.005, self.sr);
                } else {
                    st.note = c.note;
                    self.trigger(ch);
                }
            } else if c.note == Note::OFF {
                self.voices[ch].release();
            }
            let st = &mut self.chan[ch];
            match c.fx {
                fx::CUTOFF => self.voices[ch].set_cutoff(cutoff_hz(c.arg), self.sr),
                fx::DRIVE => self.voices[ch].set_drive(c.arg as f32 / 255.0),
                fx::RETRIG => st.retrig = c.arg & 0x0F,
                fx::CUT => st.cut = c.arg,
                _ => {}
            }
        }
    }

    fn advance_row(&mut self) {
        self.next_row += 1;
        if self.next_row >= ROWS {
            self.next_row = 0;
            if self.loop_pattern.is_none() {
                self.order_pos = (self.order_pos + 1) % self.song.order_len.max(1);
            }
        }
    }

    fn do_tick(&mut self) {
        if self.tick == 0 {
            self.process_row();
        } else {
            for ch in 0..CHANNELS {
                let st = self.chan[ch];
                if st.retrig > 0 && self.tick.is_multiple_of(st.retrig) && st.note.is_note() {
                    self.trigger(ch);
                }
                if st.cut > 0 && self.tick == st.cut {
                    self.voices[ch].cut(self.sr);
                }
            }
        }
        self.tick += 1;
        if self.tick >= self.song.speed.max(1) {
            self.tick = 0;
            self.advance_row();
        }
    }

    /// Render interleaved stereo frames into `out` (length must be even).
    pub fn render(&mut self, out: &mut [f32]) {
        let sr = self.sr;
        for frame in out.as_chunks_mut::<2>().0 {
            if self.playing {
                while self.tick_wait <= 0.0 {
                    self.do_tick();
                    self.tick_wait += self.samples_per_tick();
                }
                self.tick_wait -= 1.0;
            }
            let (mut l, mut r) = (0.0, 0.0);
            for ch in 0..CHANNELS {
                let s = self.voices[ch].next(&self.song.instruments, sr);
                self.levels[ch] = self.levels[ch].max(s.abs());
                if !self.mute[ch] {
                    l += s * self.gains[ch].0;
                    r += s * self.gains[ch].1;
                }
            }
            let p = self.preview.next(&self.song.instruments, sr) * std::f32::consts::FRAC_1_SQRT_2;
            l = ((l + p) * MASTER).tanh();
            r = ((r + p) * MASTER).tanh();
            self.peak = (self.peak.0.max(l.abs()), self.peak.1.max(r.abs()));
            frame[0] = l;
            frame[1] = r;
        }
    }

    /// One audio callback: apply pending commands, render, report status.
    pub fn process(&mut self, out: &mut [f32], link: &mut AudioLink) {
        while let Ok(cmd) = link.cmd.pop() {
            if let Some(old) = self.apply(cmd) {
                // If the UI stopped draining, freeing here is the lesser evil.
                let _ = link.garbage.push(old);
            }
        }
        self.render(out);
        self.since_status += (out.len() / 2) as u32;
        if self.since_status >= STATUS_EVERY && link.status.push(self.status()).is_ok() {
            self.since_status = 0;
            self.peak = (0.0, 0.0);
            self.levels = [0.0; CHANNELS];
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Kind, Patch, parse_note};

    const SR: f32 = 48000.0;

    fn song_with(kind: Kind) -> Box<Song> {
        let mut s: Box<Song> = Box::default();
        s.bpm = 120;
        s.speed = 6;
        s.instruments[0] = Patch::new("x", kind);
        s
    }

    fn render(e: &mut Engine, frames: usize) -> Vec<f32> {
        let mut buf = vec![0.0; frames * 2];
        e.render(&mut buf);
        buf
    }

    fn c4() -> Note {
        parse_note("C-4").unwrap()
    }

    #[test]
    fn row_timing_is_sample_exact() {
        // 120 BPM, 4 rows/beat → 8 rows/s → 6000 samples per row.
        let mut e = Engine::new(song_with(Kind::Kick), SR);
        e.play(0, Some(0));
        render(&mut e, 6000);
        assert_eq!(e.status().row, 0);
        render(&mut e, 1);
        assert_eq!(e.status().row, 1);
        render(&mut e, 6000 * 62);
        assert_eq!(e.status().row, 63);
        render(&mut e, 6000);
        assert_eq!(e.status().row, 0, "pattern loops");
    }

    #[test]
    fn empty_song_is_silent_and_notes_are_not() {
        let mut e = Engine::new(song_with(Kind::Kick), SR);
        e.play(0, None);
        assert!(render(&mut e, 4800).iter().all(|&x| x == 0.0));

        let mut s = song_with(Kind::Kick);
        s.set_cell(0, 0, 0, Cell::note(c4(), 0));
        let mut e = Engine::new(s, SR);
        e.play(0, None);
        let out = render(&mut e, 4800);
        let peak = out.iter().fold(0.0f32, |m, x| m.max(x.abs()));
        assert!(peak > 0.2 && peak <= 1.0, "{peak}");
    }

    #[test]
    fn render_is_deterministic() {
        let mk = || {
            let mut s = song_with(Kind::Snare);
            for r in (0..ROWS).step_by(4) {
                s.set_cell(0, r, 0, Cell::note(c4(), 0));
            }
            let mut e = Engine::new(s, SR);
            e.play(0, None);
            render(&mut e, 48000)
        };
        assert_eq!(mk(), mk());
    }

    /// Count rising edges of the channel envelope by watching the voice restart.
    fn onsets(out: &[f32], win: usize) -> usize {
        let env: Vec<f32> = out
            .chunks(win * 2)
            .map(|c| c.iter().fold(0.0f32, |m, x| m.max(x.abs())))
            .collect();
        env.windows(2)
            .filter(|w| w[1] > w[0] * 1.8 && w[1] > 0.05)
            .count()
    }

    #[test]
    fn retrigger_produces_rolls() {
        let mut s = song_with(Kind::Hat);
        s.instruments[0].decay = 0.01;
        s.set_cell(0, 0, 0, Cell::note(c4(), 0).fx(fx::RETRIG, 2));
        let mut e = Engine::new(s, SR);
        e.play(0, Some(0));
        // one row = 6 ticks; retrig every 2 ticks → onsets at ticks 0, 2, 4.
        let mut out = vec![0.0; 200];
        out.extend(render(&mut e, 6000));
        assert_eq!(onsets(&out, 100), 3);
    }

    #[test]
    fn note_off_and_cut_silence() {
        let mut s = song_with(Kind::Reese);
        s.set_cell(0, 0, 0, Cell::note(c4(), 0));
        s.set_cell(0, 1, 0, Cell::note(Note::OFF, 0xFF));
        s.set_cell(0, 4, 1, Cell::note(c4(), 0).fx(fx::CUT, 1));
        let mut e = Engine::new(s, SR);
        e.play(0, Some(0));
        render(&mut e, 6000 * 3);
        let tail = render(&mut e, 600);
        assert!(tail.iter().all(|x| x.abs() < 1e-3), "released");
        render(&mut e, 6000 + 2000);
        let tail = render(&mut e, 600);
        assert!(tail.iter().all(|x| x.abs() < 1e-3), "cut after 1 tick");
    }

    #[test]
    fn tempo_effect_and_song_swap() {
        let mut s = song_with(Kind::Kick);
        s.set_cell(0, 0, 0, Cell::EMPTY.fx(fx::TEMPO, 0xAE));
        let mut e = Engine::new(s, SR);
        e.play(0, None);
        render(&mut e, 10);
        assert_eq!(e.status().bpm, 174);
        let old = e.apply(Command::LoadSong(Box::default()));
        assert!(old.is_some());
        assert_eq!(e.song().bpm, 174);
    }

    #[test]
    fn process_reports_status_over_ring() {
        let (mut ui, mut audio) = crate::protocol::link();
        let mut e = Engine::new(song_with(Kind::Kick), SR);
        ui.send(Command::Play {
            order_pos: 0,
            loop_pattern: None,
        });
        let mut buf = [0.0; 1024];
        e.process(&mut buf, &mut audio);
        let st = ui.poll().expect("status");
        assert!(st.playing);
    }
}
