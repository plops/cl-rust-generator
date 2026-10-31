//! 07 — Opus-Walk: ein Paket → Frames → SILK/CELT/Redundanz, wie der Decoder.
//!
//! Spiegel der Modus-Logik von `opus_decode_frame` (libopus) bzw.
//! `OpusDecoder::decode` (`opus-pure`): wann welcher Zustand
//! zurueckgesetzt wird, wo die Redundanz (5-ms-CELT-Frame zum
//! Moduswechsel) steckt und wie der Hybrid-Coder nach SILK an CELT
//! weitergereicht wird. Jeder Opus-Frame endet mit `rng` — dem
//! Range-Coder-Zustand, den der Decoder als `final_range` meldet
//! (XOR Redundanz-`rng`). Stimmt er, hat der Walk **jedes Bit** in der
//! richtigen Reihenfolge gelesen.
//!
//! Regeln (alle aus der Referenz, Zeilen-Hinweise im Walkthrough):
//! - SILK-Zustand: Reset nur bei Rueckkehr aus CELT-only.
//! - CELT-Zustand: Reset beim Wechsel nach CELT/Hybrid, ausser ein
//!   SILK→CELT-Redundanz-Frame hat ihn schon „vorgewaermt".
//! - SILK-only: Redundanz implizit (≥ 17 Rest-Bits), Laenge = Rest-Bytes.
//! - Hybrid: Redundanz-Flag (logp 12), Laenge `uint(256)+2`, der
//!   Haupt-Coder wird um diese Bytes gekuerzt (Roh-Bits vom neuen Ende).

use crate::celt::{
    CeltFrame, CeltState, endband_of, lm_of_ms, walk_celt_frame, walk_celt_frame_rc,
};
use crate::range::RangeDec;
use crate::silk::Bandwidth as SilkBw;
use crate::silk_dec::{SilkDec, SilkOpusFrame};
use crate::toc::TocInfo;
use crate::types::{Bandwidth, Error, Mode};

/// CELT-Startband im Hybrid-Modus (`HYBRID_START_BAND`).
pub const HYBRID_START: usize = 17;

/// 5-ms-CELT-Redundanz-Frame am Ende eines SILK-/Hybrid-Frames.
#[derive(Debug, Clone)]
pub struct Redundancy {
    /// `true`: CELT→SILK (Ausblenden), `false`: SILK→CELT (Vorwaermen).
    pub celt_to_silk: bool,
    /// Bytes des Redundanz-Frames (vom Payload-Ende).
    pub bytes: usize,
    /// Gewalkter Redundanz-Frame.
    pub celt: CeltFrame,
}

/// Ein Opus-Frame (Teil eines Pakets) nach dem Walk.
#[derive(Debug, Clone)]
pub struct FrameWalk {
    /// Payload-Bytes dieses Frames.
    pub bytes: usize,
    /// SILK-Schicht (SILK-only, Hybrid).
    pub silk: Option<SilkOpusFrame>,
    /// CELT-Schicht (CELT-only, Hybrid-Oberband).
    pub celt: Option<CeltFrame>,
    /// Redundanz-Frame (Moduswechsel).
    pub redundancy: Option<Redundancy>,
    /// Hybrid-Redundanz-Header (Flag + Laenge) in 1/8 Bit.
    pub red_header_bits8: i32,
    /// Verbrauch des Haupt-Coders am Frame-Ende in 1/8 Bit.
    pub used_bits8: i32,
    /// Final Range dieses Frames (Haupt-`rng` XOR Redundanz-`rng`).
    pub rng: u32,
    /// Payload leer (≤ 1 Byte): Decoder verschleiert, kein Orakel.
    pub plc: bool,
    /// `tell_frac` direkt nach der Coder-Initialisierung (1/8 Bit).
    pub init_bits8: i32,
}

/// Bit-Budget eines Frames: wohin jedes 1/8-Bit des Payloads ging.
///
/// Summe aller Felder == `bytes * 64` (exakt, Test `budget_is_exact`).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Budget {
    /// Range-Coder-Initialisierung (1 Bit, `tell()==1`).
    pub init: i32,
    /// SILK: VAD/LBRR-Flags.
    pub silk_flags: i32,
    /// SILK: LBRR-Kopien (FEC fuer das Vorgaenger-Paket).
    pub silk_lbrr: i32,
    /// SILK: Stereo-Praediktion + Mid-only.
    pub silk_stereo: i32,
    /// SILK: Seiteninfo (Typ, Gains, NLSF, LTP, Seed).
    pub silk_side: i32,
    /// SILK: Excitation (Pulse).
    pub silk_pulses: i32,
    /// Redundanz-Header (Hybrid-Flag/Laenge, SILK-Richtungsbit).
    pub red_header: i32,
    /// CELT: Header (Flags, TF/Spread, Allokation).
    pub celt_header: i32,
    /// CELT: Energien (Coarse + Fine + Finalise).
    pub celt_energy: i32,
    /// CELT: Banddaten (Theta, PVQ, Vorzeichen).
    pub celt_pvq: i32,
    /// Redundanz-Frame (ganze Bytes).
    pub redundancy: i32,
    /// Ungenutzt (Coder-Rest); negativ = implizite Null-Bytes am Ende.
    pub unused: i32,
}

impl Budget {
    /// Summe aller Posten in 1/8 Bit.
    #[must_use]
    pub fn total(&self) -> i32 {
        self.init
            + self.silk_flags
            + self.silk_lbrr
            + self.silk_stereo
            + self.silk_side
            + self.silk_pulses
            + self.red_header
            + self.celt_header
            + self.celt_energy
            + self.celt_pvq
            + self.redundancy
            + self.unused
    }
}

impl FrameWalk {
    /// Bit-Budget des Frames (alle Posten in 1/8 Bit).
    #[must_use]
    pub fn budget(&self) -> Budget {
        if self.plc {
            return Budget {
                unused: self.bytes as i32 * 64,
                ..Budget::default()
            };
        }
        let mut b = Budget {
            init: self.init_bits8,
            red_header: self.red_header_bits8,
            redundancy: self.redundancy.as_ref().map_or(0, |r| r.bytes as i32 * 64),
            unused: self.unused_bits8(),
            ..Budget::default()
        };
        if let Some(s) = &self.silk {
            let fb = s.frame_bits();
            b.silk_flags = s.flag_bits8;
            b.silk_lbrr = s.lbrr_bits8();
            b.silk_stereo = s.stereo_bits8;
            b.silk_side = fb.total() - fb.pulses;
            b.silk_pulses = fb.pulses;
        }
        if let Some(c) = &self.celt {
            b.celt_header = c.bits.flags + c.bits.tf_spread + c.bits.alloc;
            b.celt_energy = c.bits.coarse + c.bits.fine + c.bits.finalise;
            b.celt_pvq = c.bits.pvq;
        }
        b
    }

    fn plc(bytes: usize) -> Self {
        Self {
            bytes,
            silk: None,
            celt: None,
            redundancy: None,
            red_header_bits8: 0,
            used_bits8: 0,
            rng: 0,
            plc: true,
            init_bits8: 0,
        }
    }

    /// Ungenutzte 1/8-Bits (Payload minus Haupt-Coder minus Redundanz).
    ///
    /// Negativ ist erlaubt: Der Range-Coder darf ueber das Payload-Ende
    /// hinaus „lesen" (implizite Null-Bytes) — der Encoder spart so die
    /// letzten Bytes, deren Inhalt ohnehin feststeht.
    #[must_use]
    pub fn unused_bits8(&self) -> i32 {
        let red = self.redundancy.as_ref().map_or(0, |r| r.bytes as i32 * 64);
        self.bytes as i32 * 64 - red - self.used_bits8
    }
}

/// Ein Paket nach dem Walk.
#[derive(Debug, Clone)]
pub struct PacketWalk {
    pub toc: TocInfo,
    pub frames: Vec<FrameWalk>,
}

impl PacketWalk {
    /// Final Range des Pakets (letzter Frame), `None` bei PLC-Frames.
    #[must_use]
    pub fn final_range(&self) -> Option<u32> {
        self.frames.last().filter(|f| !f.plc).map(|f| f.rng)
    }
}

/// Decoder-Zustand des Walks ueber alle Pakete einer Datei.
#[derive(Default)]
pub struct Walker {
    celt: CeltState,
    silk: SilkDec,
    prev_mode: Option<Mode>,
    prev_redundancy: bool,
}

/// SILK-Bandbreite aus TOC (NB/MB direkt, Rest = 16 kHz intern).
fn silk_bw(bw: Bandwidth) -> SilkBw {
    match bw {
        Bandwidth::Narrow => SilkBw::Narrow,
        Bandwidth::Medium => SilkBw::Medium,
        _ => SilkBw::Wide,
    }
}

impl Walker {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Ein komplettes Paket walken (TOC, Split, alle Frames).
    pub fn packet(&mut self, data: &[u8]) -> Result<PacketWalk, Error> {
        let toc = crate::toc::parse(data)?;
        let split = crate::split::split(data)?;
        let frames = match toc.mode {
            Mode::Silk => self.silk_only(&toc, &split.frames),
            Mode::Celt => self.celt_only(&toc, &split.frames)?,
            Mode::Hybrid => self.hybrid(&toc, &split.frames)?,
        };
        Ok(PacketWalk { toc, frames })
    }

    /// Redundanz-Frame walken (eigener Coder, 5 ms, Start 0).
    fn redundant(&mut self, tail: &[u8], toc: &TocInfo, reset: bool) -> (CeltFrame, u32) {
        if reset {
            self.celt.reset();
        }
        let end = endband_of(toc.bandwidth);
        walk_celt_frame(tail, 1, toc.channels as usize, end, &mut self.celt)
            .unwrap_or_else(|_| unreachable!("lm 1 ist gueltig"))
    }

    fn silk_only(&mut self, toc: &TocInfo, payloads: &[&[u8]]) -> Vec<FrameWalk> {
        if self.prev_mode == Some(Mode::Celt) {
            self.silk.reset();
        }
        let ch = toc.channels as usize;
        let mut prev_frame_mode = self.prev_mode;
        let mut out = Vec::with_capacity(payloads.len());
        for payload in payloads {
            if payload.len() <= 1 {
                out.push(FrameWalk::plc(payload.len()));
                continue;
            }
            let mut rc = RangeDec::new(payload);
            let init_bits8 = rc.tell_frac();
            let silk = self
                .silk
                .opus_frame(&mut rc, silk_bw(toc.bandwidth), toc.frame_ms, ch);
            let plen = payload.len();
            let mut red: Option<(bool, usize)> = None;
            let t_red = rc.tell_frac();
            if rc.tell() + 17 <= plen as i32 * 8 {
                let c2s = rc.decode_bit_logp(1);
                let rb = plen - ((rc.tell() + 7) >> 3) as usize;
                if rb >= 2 && rb < plen {
                    red = Some((c2s, rb));
                }
            }
            let used_bits8 = rc.tell_frac();
            let red_header_bits8 = used_bits8 - t_red;
            let mut redundancy = None;
            let mut red_rng = 0;
            if let Some((true, rb)) = red {
                let (f, r) = self.redundant(&payload[plen - rb..], toc, false);
                red_rng = r;
                redundancy = Some(Redundancy {
                    celt_to_silk: true,
                    bytes: rb,
                    celt: f,
                });
            }
            // Hybrid→SILK: CELT blendet mit einem 2-Byte-Stille-Frame aus.
            if prev_frame_mode == Some(Mode::Hybrid)
                && !(matches!(red, Some((true, _))) && self.prev_redundancy)
            {
                let _ = walk_celt_frame(
                    &[0xff, 0xff],
                    0,
                    ch,
                    endband_of(toc.bandwidth),
                    &mut self.celt,
                );
            }
            if let Some((false, rb)) = red {
                let (f, r) = self.redundant(&payload[plen - rb..], toc, true);
                red_rng = r;
                redundancy = Some(Redundancy {
                    celt_to_silk: false,
                    bytes: rb,
                    celt: f,
                });
            }
            self.prev_redundancy = matches!(red, Some((false, _)));
            prev_frame_mode = Some(Mode::Silk);
            out.push(FrameWalk {
                bytes: plen,
                silk: Some(silk),
                celt: None,
                redundancy,
                red_header_bits8,
                used_bits8,
                rng: rc.rng ^ red_rng,
                plc: false,
                init_bits8,
            });
        }
        self.prev_mode = Some(Mode::Silk);
        out
    }

    fn celt_only(&mut self, toc: &TocInfo, payloads: &[&[u8]]) -> Result<Vec<FrameWalk>, Error> {
        if self.prev_mode.is_some_and(|m| m != Mode::Celt) && !self.prev_redundancy {
            self.celt.reset();
        }
        self.prev_redundancy = false;
        let (lm, end, ch) = (
            lm_of_ms(toc.frame_ms),
            endband_of(toc.bandwidth),
            toc.channels as usize,
        );
        let mut out = Vec::with_capacity(payloads.len());
        for payload in payloads {
            if payload.len() <= 1 {
                out.push(FrameWalk::plc(payload.len()));
                continue;
            }
            let mut rc = RangeDec::new(payload);
            let init_bits8 = rc.tell_frac();
            let celt = walk_celt_frame_rc(
                &mut rc,
                payload.len() as i32 * 8,
                lm,
                ch,
                0,
                end,
                &mut self.celt,
            )
            .map_err(Error::BadPacket)?;
            out.push(FrameWalk {
                bytes: payload.len(),
                silk: None,
                celt: Some(celt),
                redundancy: None,
                red_header_bits8: 0,
                used_bits8: rc.tell_frac(),
                rng: rc.rng,
                plc: false,
                init_bits8,
            });
        }
        self.prev_mode = Some(Mode::Celt);
        Ok(out)
    }

    fn hybrid(&mut self, toc: &TocInfo, payloads: &[&[u8]]) -> Result<Vec<FrameWalk>, Error> {
        if self.prev_mode == Some(Mode::Celt) {
            self.silk.reset();
        }
        let (lm, end, ch) = (
            lm_of_ms(toc.frame_ms),
            endband_of(toc.bandwidth),
            toc.channels as usize,
        );
        let mut out = Vec::with_capacity(payloads.len());
        for (fi, payload) in payloads.iter().enumerate() {
            if payload.len() <= 1 {
                out.push(FrameWalk::plc(payload.len()));
                continue;
            }
            let mut rc = RangeDec::new(payload);
            let init_bits8 = rc.tell_frac();
            let silk = self
                .silk
                .opus_frame(&mut rc, silk_bw(toc.bandwidth), toc.frame_ms, ch);
            let plen = payload.len();
            let mut effective = plen;
            let mut red: Option<(bool, usize)> = None;
            let t_red = rc.tell_frac();
            if rc.tell() + 37 <= plen as i32 * 8 && rc.decode_bit_logp(12) {
                let c2s = rc.decode_bit_logp(1);
                let rb = rc.dec_uint(256) as usize + 2;
                if rb <= effective && (effective - rb) as i32 * 8 >= rc.tell() {
                    effective -= rb;
                    rc.storage -= rb as u32;
                    red = Some((c2s, rb));
                }
            }
            let red_header_bits8 = rc.tell_frac() - t_red;
            let mut redundancy = None;
            let mut red_rng = 0;
            if let Some((true, rb)) = red {
                let (f, r) = self.redundant(&payload[plen - rb..], toc, false);
                red_rng = r;
                redundancy = Some(Redundancy {
                    celt_to_silk: true,
                    bytes: rb,
                    celt: f,
                });
            }
            if fi == 0 && self.prev_mode.is_some_and(|m| m != Mode::Hybrid) && !self.prev_redundancy
            {
                self.celt.reset();
            }
            let celt = walk_celt_frame_rc(
                &mut rc,
                effective as i32 * 8,
                lm,
                ch,
                HYBRID_START,
                end,
                &mut self.celt,
            )
            .map_err(Error::BadPacket)?;
            let used_bits8 = rc.tell_frac();
            if let Some((false, rb)) = red {
                let (f, r) = self.redundant(&payload[plen - rb..], toc, true);
                red_rng = r;
                redundancy = Some(Redundancy {
                    celt_to_silk: false,
                    bytes: rb,
                    celt: f,
                });
            }
            self.prev_redundancy = matches!(red, Some((false, _)));
            out.push(FrameWalk {
                bytes: plen,
                silk: Some(silk),
                celt: Some(celt),
                redundancy,
                red_header_bits8,
                used_bits8,
                rng: rc.rng ^ red_rng,
                plc: false,
                init_bits8,
            });
        }
        self.prev_mode = Some(Mode::Hybrid);
        Ok(out)
    }
}

/// Alle Pakete einer Datei der Reihe nach walken (ein Zustand fuer alle).
pub fn walk_packets(ps: &crate::analyze::PacketSet) -> Result<Vec<PacketWalk>, Error> {
    let mut w = Walker::new();
    ps.packets.iter().map(|p| w.packet(&p.data)).collect()
}
