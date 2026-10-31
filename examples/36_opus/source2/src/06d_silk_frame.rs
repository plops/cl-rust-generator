//! 06d — SILK-Frame-Walk: Seiteninfo + Excitation eines Kanals.
//!
//! Port von `silk_decode_indices` + `silk_decode_pulses` (Lesen) und der
//! Anzeige-Mathematik aus `silk_decode_parameters` (Gains, Pitch, NLSF).
//! Ein Aufruf = ein SILK-Frame (10/20 ms) eines Kanals. Flags, LBRR,
//! Stereo-Praediktion und die Frame-Schleife stehen eine Ebene hoeher in
//! [`crate::silk_dec`] — genau wie `silk_Decode` in libopus ueber
//! `silk_decode_frame` sitzt.
//!
//! Neu gegenueber `source1`: Bedingte Kodierung kennt alle drei Stufen
//! (unabhaengig / unabhaengig ohne LTP-Skalierung / bedingt) und
//! LBRR-Frames (`lbrr = true`: Signaltyp immer „aktiv", kein Eingriff in
//! Gain- und NLSF-Gedaechtnis — wie die Referenz, die fuer LBRR nur
//! Indizes und Pulse liest). Bits zaehlen in 1/8 Bit (`tell_frac`).

use crate::range::RangeDec;
use crate::silk_tab::*;

use Bandwidth::*;

// --- Festkomma-Helfer (Port aus `macros.rs`, nur Bedarf) ---

pub fn limit32(a: i32, lo: i32, hi: i32) -> i32 {
    a.clamp(lo, hi)
}

pub fn smulbb(a: i32, b: i32) -> i32 {
    (a as i16 as i32) * (b as i16 as i32)
}

pub fn smulwb(a: i32, b: i32) -> i32 {
    (((a as i64) * (b as i16 as i64)) >> 16) as i32
}

pub fn smlawb(a: i32, b: i32, c: i32) -> i32 {
    a.wrapping_add(smulwb(b, c))
}

pub fn mul(a: i32, b: i32) -> i32 {
    a.wrapping_mul(b)
}

pub fn lshift(a: i32, s: i32) -> i32 {
    a << s
}

pub fn rshift_round(a: i32, s: u32) -> i32 {
    (a >> s) + (a & 1)
}

pub fn log2lin(log_gain_q7: i32) -> i32 {
    if log_gain_q7 < 0 {
        return 0;
    }
    if log_gain_q7 >= 3967 {
        return i32::MAX;
    }
    let out = lshift(1, log_gain_q7 >> 7);
    let frac_q7 = log_gain_q7 & 0x7F;
    let val = smlawb(frac_q7, smulbb(frac_q7, 128 - frac_q7), -174);
    if log_gain_q7 < 2048 {
        out.wrapping_add(mul(out, val) >> 7)
    } else {
        out + mul(out >> 7, val)
    }
}

// --- Bandbreite / Setup ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bandwidth {
    Narrow,
    Medium,
    Wide,
}

impl Bandwidth {
    /// Interne SILK-Rate in kHz (Referenz: `decoder.rs`, Hybrid = 16).
    fn fs_khz(self) -> i32 {
        match self {
            Narrow => 8,
            Medium => 12,
            Wide => 16,
        }
    }

    /// LPC-Ordnung (NB/MB 10, WB 16 — `init_decoder.rs`).
    fn lpc_order(self) -> usize {
        if self == Wide {
            MAX_LPC_ORDER
        } else {
            MIN_LPC_ORDER
        }
    }

    /// NLSF-Codebook (`init_decoder.rs`).
    fn nlsf_cb(self) -> &'static NlsfCodebook {
        if self == Wide {
            &SILK_NLSF_CB_WB
        } else {
            &SILK_NLSF_CB_NB_MB
        }
    }
}

// --- Zustand, Typen ---

/// Kanal-Zustand ueber Frames (konditionelle Kodierung + Dequant).
#[derive(Debug, Clone)]
pub struct SilkState {
    pub(crate) ec_prev_signal: i8,
    pub(crate) ec_prev_lag: i16,
    pub(crate) last_gain: i8,
    pub(crate) prev_nlsf: [i16; MAX_LPC_ORDER],
    /// Wie `first_frame_after_reset`: erzwingt Interp = 4 (kein Vor-Nlsf).
    pub(crate) first: bool,
    pub(crate) fs_khz: i32,
}

impl SilkState {
    pub fn new() -> Self {
        Self {
            ec_prev_signal: 0,
            ec_prev_lag: 0,
            last_gain: 10,
            prev_nlsf: [0; MAX_LPC_ORDER],
            first: true,
            fs_khz: 0,
        }
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }
}

impl Default for SilkState {
    fn default() -> Self {
        Self::new()
    }
}

/// Bit-Anteile eines SILK-Frames in 1/8 Bit (`tell_frac`-Deltas, exakt).
#[derive(Debug, Clone, Copy, Default)]
pub struct SilkBits {
    pub signal: i32,
    pub gains: i32,
    pub nlsf: i32,
    pub ltp: i32,
    pub seed: i32,
    pub pulses: i32,
}

impl SilkBits {
    /// Summe aller Posten in 1/8 Bit.
    #[must_use]
    pub fn total(&self) -> i32 {
        self.signal + self.gains + self.nlsf + self.ltp + self.seed + self.pulses
    }
}

/// Alles Darstellbare eines SILK-Frames (nur `nb_subfr` Eintraege belegt).
#[derive(Debug, Clone)]
pub struct SilkFrame {
    pub signal_type: i8,
    pub quant_offset: i8,
    /// Roh-Gain-Indizes (Draht-Format, fuer Sync-Tests).
    pub gains_idx: [i8; MAX_NB_SUBFR],
    /// Subframe-Gains in dB (aus Q16 linear).
    pub gains_db: [f32; MAX_NB_SUBFR],
    /// Pitch je Subframe in Hz (0 = stimmlos/inaktiv).
    pub pitch_hz: [f32; MAX_NB_SUBFR],
    /// Pitch-Lag je Subframe in Samples (@fs_khz).
    pub pitch_lag: [i32; MAX_NB_SUBFR],
    pub per_index: i8,
    pub ltp_index: [i8; MAX_NB_SUBFR],
    pub ltp_scale: i8,
    /// NLSF-Vektor Q15 (nur `lpc_order` Eintraege belegt).
    pub nlsf_q15: [i16; MAX_LPC_ORDER],
    pub interp_q2: i8,
    pub seed: i8,
    pub bits: SilkBits,
    pub nb_subfr: usize,
    pub fs_khz: i32,
    /// LPC-Ordnung (10 NB/MB, 16 WB — bestimmt NLSF-Laenge).
    pub lpc_order: usize,
    /// `true`: LBRR-Kopie (FEC) — nur Indizes/Pulse, kein Zustand.
    pub lbrr: bool,
}

/// Paket-gemittelte SILK-Kennzahlen (Hops, JSON, Report).
#[derive(Debug, Clone, Copy, Default)]
pub struct SilkSummary {
    /// Stimmhaftestes Frame (0 inaktiv, 1 stimmlos, 2 stimmhaft).
    pub signal: i8,
    /// Leistungs-Mittel der Subframe-Gains in dB.
    pub gains_db: f32,
    /// Mittel ueber stimmhafte Subframes in Hz (0 ohne).
    pub pitch_hz: f32,
    /// Seiteninfo-Bits (Typ, Gains, NLSF, LTP, Seed), ganze Bits.
    pub header_bits: i32,
    /// Excitation-Bits, ganze Bits.
    pub pulse_bits: i32,
    /// Gewalkte SILK-Frames im Paket.
    pub n_frames: usize,
    /// Interpolierter NLSF-Vektor Q15 (nur `order` Eintraege belegt).
    pub nlsf_q15: [i16; MAX_LPC_ORDER],
    /// LPC-Ordnung (10 NB/MB, 16 WB).
    pub order: usize,
    /// Interne Rate in kHz (Nyquist-Grenze der Huelle).
    pub fs_khz: i32,
}

// --- Indices-Walk (Port `silk_decode_indices`, Werte fallen weg wo ungenutzt) ---

struct Indices {
    signal: i8,
    quant: i8,
    gains: [i8; MAX_NB_SUBFR],
    nlsf: [i8; 17],
    interp: i8,
    lag: i16,
    contour: i8,
    per: i8,
    ltp: [i8; MAX_NB_SUBFR],
    ltp_scale: i8,
    seed: i8,
}

fn walk_indices(
    rc: &mut RangeDec<'_>,
    bw: Bandwidth,
    nb_subfr: usize,
    vad: i32,
    cond: i32,
    state: &mut SilkState,
    bits: &mut SilkBits,
) -> Indices {
    let fs = bw.fs_khz();
    let t0 = rc.tell_frac();
    let ix = if vad != 0 {
        rc.decode_icdf(&SILK_TYPE_OFFSET_VAD_ICDF, 8) + 2
    } else {
        rc.decode_icdf(&SILK_TYPE_OFFSET_NO_VAD_ICDF, 8)
    };
    let (signal, quant) = ((ix >> 1) as i8, (ix & 1) as i8);
    bits.signal = rc.tell_frac() - t0;

    let t0 = rc.tell_frac();
    let mut gains = [0i8; MAX_NB_SUBFR];
    if cond == CODE_CONDITIONALLY {
        gains[0] = rc.decode_icdf(&SILK_DELTA_GAIN_ICDF, 8) as i8;
    } else {
        gains[0] = ((rc.decode_icdf(&SILK_GAIN_ICDF[signal as usize], 8) << 3) as i8)
            .wrapping_add(rc.decode_icdf(&SILK_UNIFORM8_ICDF, 8) as i8);
    }
    for g in gains.iter_mut().take(nb_subfr).skip(1) {
        *g = rc.decode_icdf(&SILK_DELTA_GAIN_ICDF, 8) as i8;
    }
    bits.gains = rc.tell_frac() - t0;

    let t0 = rc.tell_frac();
    let cb = bw.nlsf_cb();
    let mut nlsf = [0i8; 17];
    nlsf[0] = rc.decode_icdf(
        &cb.cb1_icdf[((signal >> 1) as usize) * (cb.n_vectors as usize)..],
        8,
    ) as i8;
    // Stufe 2 ueber entpackte Praediktions-Auswahl (`nlsf_unpack`-Port).
    let order = cb.order as usize;
    let sel = &cb.ec_sel[(nlsf[0] as usize) * order / 2..];
    let mut ec_ix = [0i16; MAX_LPC_ORDER];
    for i in (0..order).step_by(2) {
        let entry = sel[i / 2];
        ec_ix[i] = smulbb(((entry >> 1) & 7) as i32, 2 * NLSF_QUANT_MAX_AMPLITUDE + 1) as i16;
        ec_ix[i + 1] = smulbb(((entry >> 5) & 7) as i32, 2 * NLSF_QUANT_MAX_AMPLITUDE + 1) as i16;
    }
    for i in 0..order {
        let mut v = rc.decode_icdf(&cb.ec_icdf[ec_ix[i] as usize..], 8);
        if v == 0 {
            v -= rc.decode_icdf(&SILK_NLSF_EXT_ICDF, 8);
        } else if v == 2 * NLSF_QUANT_MAX_AMPLITUDE {
            v += rc.decode_icdf(&SILK_NLSF_EXT_ICDF, 8);
        }
        nlsf[i + 1] = (v - NLSF_QUANT_MAX_AMPLITUDE) as i8;
    }
    let interp = if nb_subfr == MAX_NB_SUBFR {
        rc.decode_icdf(&SILK_NLSF_INTERPOLATION_FACTOR_ICDF, 8) as i8
    } else {
        4
    };
    bits.nlsf = rc.tell_frac() - t0;

    let t0 = rc.tell_frac();
    let (mut lag, mut contour, mut per, mut ltp, mut scale) = (0i16, 0i8, 0i8, [0i8; 4], 0i8);
    if i32::from(signal) == TYPE_VOICED {
        let mut absolute = true;
        if cond == CODE_CONDITIONALLY && i32::from(state.ec_prev_signal) == TYPE_VOICED {
            let d = rc.decode_icdf(&SILK_PITCH_DELTA_ICDF, 8) as i16;
            if d > 0 {
                lag = state.ec_prev_lag + d - 9;
                absolute = false;
            }
        }
        if absolute {
            lag = (rc.decode_icdf(&SILK_PITCH_LAG_ICDF, 8) * (fs >> 1)) as i16
                + rc.decode_icdf(low_bits_icdf(fs), 8) as i16;
        }
        state.ec_prev_lag = lag;
        contour = rc.decode_icdf(contour_icdf(fs, nb_subfr), 8) as i8;
        per = rc.decode_icdf(&SILK_LTP_PER_INDEX_ICDF, 8) as i8;
        for l in ltp.iter_mut().take(nb_subfr) {
            *l = rc.decode_icdf(SILK_LTP_GAIN_ICDF_PTRS[per as usize], 8) as i8;
        }
        if cond == CODE_INDEPENDENTLY {
            scale = rc.decode_icdf(&SILK_LTPSCALE_ICDF, 8) as i8;
        }
    }
    state.ec_prev_signal = signal;
    bits.ltp = rc.tell_frac() - t0;

    let t0 = rc.tell_frac();
    let seed = rc.decode_icdf(&SILK_UNIFORM4_ICDF, 8) as i8;
    bits.seed = rc.tell_frac() - t0;

    Indices {
        signal,
        quant,
        gains,
        nlsf,
        interp,
        lag,
        contour,
        per,
        ltp,
        ltp_scale: scale,
        seed,
    }
}

// --- Paket-Einstieg (Aufrufer: `05b_bitstream`, Tests) ---

use crate::silk_dq::{decode_pitch, dequant_gains, dequant_nlsf};
use crate::silk_pulse::walk_pulses;

/// Walk-Kontext eines SILK-Frames (Position bestimmt [`crate::silk_dec`]).
pub struct SilkCtx {
    pub bw: Bandwidth,
    pub nb_subfr: usize,
    /// VAD-Flag des Frames (LBRR: immer aktiv).
    pub vad: i32,
    /// `CODE_INDEPENDENTLY` / `..._NO_LTP_SCALING` / `CODE_CONDITIONALLY`.
    pub cond: i32,
    /// LBRR-Kopie: Indizes + Pulse lesen, Gain-/NLSF-Gedaechtnis lassen.
    pub lbrr: bool,
}

/// Ein SILK-Frame walken: Indices + Pulse lesen, Anzeige-Werte rechnen.
/// Der Aufrufer besitzt `rc` — Hybrid reicht denselben Coder an CELT weiter.
pub fn walk_silk_frame(rc: &mut RangeDec<'_>, ctx: &SilkCtx, state: &mut SilkState) -> SilkFrame {
    let fs = ctx.bw.fs_khz();
    if state.fs_khz != fs {
        // `silk_decoder_set_fs`: Ratenwechsel setzt Gain-/NLSF-Gedaechtnis.
        state.fs_khz = fs;
        state.first = true;
        state.last_gain = 10;
    }
    let mut bits = SilkBits::default();
    let vad = if ctx.lbrr { 1 } else { ctx.vad };
    let idx = walk_indices(rc, ctx.bw, ctx.nb_subfr, vad, ctx.cond, state, &mut bits);
    let frame_length = SUB_FRAME_LENGTH_MS * fs as usize * ctx.nb_subfr;
    bits.pulses = walk_pulses(rc, idx.signal as i32, idx.quant as i32, frame_length);

    // Anzeige-Mathematik; LBRR rechnet auf einer Kopie des Gedaechtnisses.
    let mut last_gain = state.last_gain;
    let cond_q = i32::from(ctx.cond == CODE_CONDITIONALLY);
    let gains_q16 = dequant_gains(&idx.gains, &mut last_gain, cond_q, ctx.nb_subfr);
    let mut gains_db = [0.0f32; MAX_NB_SUBFR];
    for (d, q) in gains_db.iter_mut().zip(gains_q16.iter()).take(ctx.nb_subfr) {
        *d = 20.0 * ((*q).max(1) as f32 / 65536.0).log10();
    }
    let (mut pitch_hz, mut pitch_lag) = ([0.0f32; MAX_NB_SUBFR], [0i32; MAX_NB_SUBFR]);
    if i32::from(idx.signal) == TYPE_VOICED {
        let lags = decode_pitch(idx.lag, idx.contour, fs, ctx.nb_subfr);
        for ((h, l), lag) in pitch_hz
            .iter_mut()
            .zip(pitch_lag.iter_mut())
            .zip(lags.iter())
            .take(ctx.nb_subfr)
        {
            *l = *lag;
            *h = fs as f32 * 1000.0 / (*lag).max(1) as f32;
        }
    }
    let fresh = dequant_nlsf(&idx.nlsf, ctx.bw.nlsf_cb());
    let interp = if state.first { 4 } else { idx.interp };
    // NLSF-Interpolation wortwoertlich nach `decode_parameters`.
    let order = ctx.bw.lpc_order();
    let mut nlsf_q15 = [0i16; MAX_LPC_ORDER];
    for i in 0..order {
        let (p, c) = (i32::from(state.prev_nlsf[i]), i32::from(fresh[i]));
        nlsf_q15[i] = if interp < 4 {
            (p + ((i32::from(interp) * (c - p)) >> 2)) as i16
        } else {
            fresh[i]
        };
    }
    if !ctx.lbrr {
        state.last_gain = last_gain;
        state.prev_nlsf = fresh;
        state.first = false;
    }

    SilkFrame {
        signal_type: idx.signal,
        quant_offset: idx.quant,
        gains_idx: idx.gains,
        gains_db,
        pitch_hz,
        pitch_lag,
        per_index: idx.per,
        ltp_index: idx.ltp,
        ltp_scale: idx.ltp_scale,
        nlsf_q15,
        interp_q2: interp,
        seed: idx.seed,
        bits,
        nb_subfr: ctx.nb_subfr,
        fs_khz: fs,
        lpc_order: order,
        lbrr: ctx.lbrr,
    }
}

/// Pitch-Low-Bits-Tabelle je interner Rate (`init_decoder.rs`).
fn low_bits_icdf(fs_khz: i32) -> &'static [u8] {
    match fs_khz {
        8 => &SILK_UNIFORM4_ICDF,
        12 => &SILK_UNIFORM6_ICDF,
        _ => &SILK_UNIFORM8_ICDF,
    }
}

/// Pitch-Kontur-Tabelle je Rate und Rahmendauer (`init_decoder.rs`).
fn contour_icdf(fs_khz: i32, nb_subfr: usize) -> &'static [u8] {
    if fs_khz == 8 {
        if nb_subfr == MAX_NB_SUBFR {
            &SILK_PITCH_CONTOUR_NB_ICDF
        } else {
            &SILK_PITCH_CONTOUR_10_MS_NB_ICDF
        }
    } else if nb_subfr == MAX_NB_SUBFR {
        &SILK_PITCH_CONTOUR_ICDF
    } else {
        &SILK_PITCH_CONTOUR_10_MS_ICDF
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zustand_setzt_bei_reset_zurueck() {
        let mut state = SilkState::new();
        state.last_gain = 40;
        state.first = false;
        state.reset();
        assert_eq!((state.last_gain, state.first), (10, true));
    }

    #[test]
    fn lbrr_laesst_gedaechtnis_unberuehrt() {
        let data = [0x5au8; 60];
        let mut st = SilkState::new();
        let mut rc = RangeDec::new(&data);
        let t0 = rc.tell_frac();
        let ctx = SilkCtx {
            bw: Bandwidth::Wide,
            nb_subfr: 4,
            vad: 1,
            cond: CODE_INDEPENDENTLY,
            lbrr: true,
        };
        let f = walk_silk_frame(&mut rc, &ctx, &mut st);
        assert!(f.lbrr);
        assert!(st.first && st.last_gain == 10 && st.prev_nlsf == [0; MAX_LPC_ORDER]);
        assert_eq!(f.bits.total(), rc.tell_frac() - t0);
    }
}
