//! 06e — SILK-Decoder-Rahmen: Flags, LBRR, Stereo, Frame-Schleife.
//!
//! Spiegel von `silk_Decode` (libopus `dec_API.c`, `opus-pure`
//! `silk/dec_api.rs`) ohne Synthese. Ein Aufruf von [`SilkDec::opus_frame`]
//! walkt **einen Opus-Frame** (10–60 ms SILK) auf dessen eigenem
//! Range-Coder:
//!
//! 1. je Kanal VAD-Flags aller SILK-Frames + LBRR-Flag,
//! 2. je Kanal ggf. LBRR-Muster (40/60 ms),
//! 3. alle LBRR-Frames (FEC-Kopien des *vorigen* Pakets),
//! 4. je SILK-Frame: Stereo-Praediktion + Mid-only-Flag, Mid-Kanal,
//!    Seitenkanal (falls codiert).
//!
//! `source1` las die Flags nur aus dem ersten Payload, kannte weder LBRR
//! noch Stereo und codierte Folge-Frames eines Pakets falsch bedingt —
//! jede dieser Luecken verschiebt alle folgenden Bits.

use crate::range::RangeDec;
use crate::silk::{Bandwidth, SilkBits, SilkCtx, SilkFrame, SilkState, walk_silk_frame};
use crate::silk_tab::*;

/// Stereo-Seiteninfo eines SILK-Frames.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct StereoInfo {
    /// Praediktor-Indizes `[kanal][grob, fein]` (Draht-Format).
    pub pred_ix: [[u8; 2]; 2],
    /// Nur Mid codiert (Seitenkanal still).
    pub mid_only: bool,
    /// Gelesene 1/8-Bits (Praediktor + Flag).
    pub bits8: i32,
}

/// Walk-Ergebnis eines Opus-Frames im SILK-Teil.
#[derive(Debug, Clone, Default)]
pub struct SilkOpusFrame {
    /// Regulaere Frames in Lesereihenfolge (Mid, ggf. Side, je SILK-Frame).
    pub frames: Vec<SilkFrame>,
    /// Kanal je Eintrag in `frames` (0 = Mid/Mono, 1 = Side).
    pub channel: Vec<u8>,
    /// LBRR-Frames (In-Band-FEC des Vorgaenger-Pakets).
    pub lbrr: Vec<SilkFrame>,
    /// Stereo-Info je SILK-Frame (leer bei Mono).
    pub stereo: Vec<StereoInfo>,
    /// VAD-Flags `[kanal][frame]`.
    pub vad: [[bool; 3]; 2],
    /// Gelesene 1/8-Bits fuer Flags (VAD, LBRR, LBRR-Muster).
    pub flag_bits8: i32,
    /// Gelesene 1/8-Bits fuer Stereo-Praediktion + Mid-only (inkl. LBRR).
    pub stereo_bits8: i32,
    /// Gelesene 1/8-Bits gesamt (Flags + LBRR + Stereo + Frames).
    pub total_bits8: i32,
}

impl SilkOpusFrame {
    /// Bits nach Posten aufsummiert (regulaere Frames).
    #[must_use]
    pub fn frame_bits(&self) -> SilkBits {
        let mut b = SilkBits::default();
        for f in &self.frames {
            b.signal += f.bits.signal;
            b.gains += f.bits.gains;
            b.nlsf += f.bits.nlsf;
            b.ltp += f.bits.ltp;
            b.seed += f.bits.seed;
            b.pulses += f.bits.pulses;
        }
        b
    }

    /// Bits der LBRR-Kopien in 1/8 Bit.
    #[must_use]
    pub fn lbrr_bits8(&self) -> i32 {
        self.lbrr.iter().map(|f| f.bits.total()).sum()
    }
}

/// SILK-Decoder-Zustand ueber Pakete: zwei Kanaele plus Stereo-Gedaechtnis.
#[derive(Debug, Clone)]
pub struct SilkDec {
    ch: [SilkState; 2],
    internal_channels: usize,
    prev_mid_only: bool,
}

impl Default for SilkDec {
    fn default() -> Self {
        Self::new()
    }
}

impl SilkDec {
    #[must_use]
    pub fn new() -> Self {
        Self {
            ch: [SilkState::new(), SilkState::new()],
            internal_channels: 1,
            prev_mid_only: false,
        }
    }

    /// Kompletter Reset (Rueckkehr aus CELT-only, wie `silk_init_decoder`).
    pub fn reset(&mut self) {
        self.ch[0].reset();
        self.ch[1].reset();
        self.prev_mid_only = false;
    }

    /// Einen Opus-Frame walken. `frame_ms` = Opus-Frame-Dauer (10/20/40/60),
    /// `channels` = Kanaele laut TOC.
    pub fn opus_frame(
        &mut self,
        rc: &mut RangeDec<'_>,
        bw: Bandwidth,
        frame_ms: f64,
        channels: usize,
    ) -> SilkOpusFrame {
        let channels = channels.clamp(1, 2);
        if channels > self.internal_channels {
            // Mono → Stereo: Seitenkanal startet frisch (`setup_silk_channels`).
            self.ch[1].reset();
        }
        self.internal_channels = channels;
        let (n_frames, nb_subfr) = match frame_ms as u32 {
            0..=10 => (1, 2),
            11..=20 => (1, 4),
            21..=40 => (2, 4),
            _ => (3, 4),
        };
        let t_start = rc.tell_frac();
        let mut out = SilkOpusFrame::default();

        // 1. VAD + LBRR-Flag je Kanal.
        let mut lbrr_flag = [false; 2];
        for (n, flag) in lbrr_flag.iter_mut().enumerate().take(channels) {
            for i in 0..n_frames {
                out.vad[n][i] = rc.decode_bit_logp(1);
            }
            *flag = rc.decode_bit_logp(1);
        }
        // 2. LBRR-Muster je Kanal.
        let mut lbrr = [[false; 3]; 2];
        for n in 0..channels {
            if !lbrr_flag[n] {
                continue;
            }
            if n_frames == 1 {
                lbrr[n][0] = true;
            } else {
                let icdf: &[u8] = if n_frames == 2 {
                    &SILK_LBRR_FLAGS_2_ICDF
                } else {
                    &SILK_LBRR_FLAGS_3_ICDF
                };
                let sym = rc.decode_icdf(icdf, 8) + 1;
                for (i, l) in lbrr[n].iter_mut().enumerate().take(n_frames) {
                    *l = (sym >> i) & 1 != 0;
                }
            }
        }
        out.flag_bits8 = rc.tell_frac() - t_start;

        // 3. LBRR-Frames (Indizes + Pulse; aendern nur ec_prev_*).
        for i in 0..n_frames {
            for n in 0..channels {
                if !lbrr[n][i] {
                    continue;
                }
                if channels == 2 && n == 0 {
                    let t = rc.tell_frac();
                    let _ = stereo_pred(rc);
                    if !lbrr[1][i] {
                        let _ = rc.decode_icdf(&SILK_STEREO_ONLY_CODE_MID_ICDF, 8);
                    }
                    out.stereo_bits8 += rc.tell_frac() - t;
                }
                let cond = if i > 0 && lbrr[n][i - 1] {
                    CODE_CONDITIONALLY
                } else {
                    CODE_INDEPENDENTLY
                };
                let ctx = SilkCtx {
                    bw,
                    nb_subfr,
                    vad: 1,
                    cond,
                    lbrr: true,
                };
                out.lbrr.push(walk_silk_frame(rc, &ctx, &mut self.ch[n]));
            }
        }

        // 4. Regulaere Frames.
        for i in 0..n_frames {
            let mut mid_only = false;
            if channels == 2 {
                let t = rc.tell_frac();
                let pred_ix = stereo_pred(rc);
                if !out.vad[1][i] {
                    mid_only = rc.decode_icdf(&SILK_STEREO_ONLY_CODE_MID_ICDF, 8) != 0;
                }
                let bits8 = rc.tell_frac() - t;
                out.stereo_bits8 += bits8;
                out.stereo.push(StereoInfo {
                    pred_ix,
                    mid_only,
                    bits8,
                });
                if !mid_only && self.prev_mid_only {
                    // Seitenkanal war still: Gedaechtnis wie nach Reset.
                    self.ch[1].last_gain = 10;
                    self.ch[1].first = true;
                }
            }
            let cond = if i == 0 {
                CODE_INDEPENDENTLY
            } else {
                CODE_CONDITIONALLY
            };
            let ctx = SilkCtx {
                bw,
                nb_subfr,
                vad: i32::from(out.vad[0][i]),
                cond,
                lbrr: false,
            };
            out.frames.push(walk_silk_frame(rc, &ctx, &mut self.ch[0]));
            out.channel.push(0);
            if channels == 2 && !mid_only {
                let cond1 = if i == 0 {
                    CODE_INDEPENDENTLY
                } else if self.prev_mid_only {
                    CODE_INDEPENDENTLY_NO_LTP_SCALING
                } else {
                    CODE_CONDITIONALLY
                };
                let ctx = SilkCtx {
                    bw,
                    nb_subfr,
                    vad: i32::from(out.vad[1][i]),
                    cond: cond1,
                    lbrr: false,
                };
                out.frames.push(walk_silk_frame(rc, &ctx, &mut self.ch[1]));
                out.channel.push(1);
            }
            if channels == 2 {
                self.prev_mid_only = mid_only;
            }
        }
        out.total_bits8 = rc.tell_frac() - t_start;
        out
    }
}

/// Stereo-Praediktor-Indizes lesen (`silk_stereo_decode_pred`).
fn stereo_pred(rc: &mut RangeDec<'_>) -> [[u8; 2]; 2] {
    let n = rc.decode_icdf(&SILK_STEREO_PRED_JOINT_ICDF, 8);
    let mut ix = [[0u8; 2]; 2];
    let coarse = [n / 5, n % 5];
    for (j, c) in coarse.iter().enumerate() {
        let a = rc.decode_icdf(&SILK_UNIFORM3_ICDF, 8);
        let b = rc.decode_icdf(&SILK_UNIFORM5_ICDF, 8);
        ix[j] = [(a + 3 * c) as u8, b as u8];
    }
    ix
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mono_20ms_reads_one_frame_and_counts_bits() {
        let data = [0x3cu8; 50];
        let mut dec = SilkDec::new();
        let mut rc = RangeDec::new(&data);
        let t0 = rc.tell_frac();
        let f = dec.opus_frame(&mut rc, Bandwidth::Wide, 20.0, 1);
        assert_eq!(f.frames.len() + f.lbrr.len(), 1 + f.lbrr.len());
        assert!(f.stereo.is_empty());
        assert_eq!(f.total_bits8, rc.tell_frac() - t0);
        let sum = f.flag_bits8 + f.lbrr_bits8() + f.frame_bits().total() + f.stereo_bits8;
        assert_eq!(sum, f.total_bits8, "Posten summieren nicht");
    }

    #[test]
    fn stereo_60ms_reads_three_mid_frames_and_stereo_info() {
        let data = [0xa7u8; 200];
        let mut dec = SilkDec::new();
        let mut rc = RangeDec::new(&data);
        let f = dec.opus_frame(&mut rc, Bandwidth::Narrow, 60.0, 2);
        assert_eq!(f.stereo.len(), 3);
        let mids = f.channel.iter().filter(|c| **c == 0).count();
        assert_eq!(mids, 3);
        let sum = f.flag_bits8 + f.lbrr_bits8() + f.frame_bits().total() + f.stereo_bits8;
        assert_eq!(sum, f.total_bits8);
    }
}
