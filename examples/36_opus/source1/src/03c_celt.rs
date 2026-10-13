//! 03c — CELT bitstream walk: band energies + bit allocation from packet bytes.
//!
//! Minimal port of the `opus-pure` CELT header path
//! (`decode_impl_from_rc` up to `unquant_fine_energy` + `clt_compute_allocation`).
//! Reads only: silence/prefilter/transient/intra flags, coarse energies, TF,
//! spread, dynalloc, trim, intensity/dual stereo, and the allocation. The PVQ
//! band data itself is skipped — per-band coarse bits come straight out of the
//! allocation (`pulses[]` holds 1/8-bit units, not pulse counts).
//!
//! Deliberate simplifications (documented, bounded):
//! - No loss concealment: files have no packet loss, so the `loss_duration`
//!   guard and the `old_band_e2/e3` history are omitted.
//! - No `unquant_energy_finalise`: its bits sit behind the (skipped) band data
//!   and adjust energies by <0.06 log2 units; state drift stays bounded.
//! - Redundancy-primed CELT state (`prev_redundancy`) is not detected; the
//!   caller resets [`CeltState`] on every mode change, which is exact for
//!   streams without in-band FEC (opusenc default, test vectors).
//!
//! Uses [`crate::range`] (entropy reads) and [`crate::alloc`] (dynalloc,
//! trim, bit allocation).

use crate::alloc::{NB, compute_allocation, dynalloc_section};
use crate::range::{BITRES, RangeDec};
use crate::types::Bandwidth;

/// Log-energy means per band (`E_MEANS`): amplitude = energy + mean.
pub const E_MEANS: [f32; 25] = [
    6.4375, 6.25, 5.75, 5.3125, 5.0625, 4.8125, 4.5, 4.375, 4.875, 4.6875, 4.5625, 4.4375, 4.875,
    4.625, 4.3125, 4.5, 4.375, 4.625, 4.75, 4.4375, 3.75, 3.75, 3.75, 3.75, 3.75,
];
/// Inter-frame prediction coefficients per `lm`.
pub const PRED_COEF: [f32; 4] = [0.8984375, 0.796875, 0.6484375, 0.5];
/// Inter-band prediction coefficients per `lm`.
pub const BETA_COEF: [f32; 4] = [0.91998, 0.67993, 0.36999, 0.2];
/// Inter-band prediction for intra frames.
pub const BETA_INTRA: f32 = 0.15;
/// Laplace parameters for coarse energy, indexed `[lm][intra]`.
pub const E_PROB_MODEL: [[[u8; 42]; 2]; 4] = [
    [
        [
            72, 127, 65, 129, 66, 128, 65, 128, 64, 128, 62, 128, 64, 128, 64, 128, 92, 78, 92, 79,
            92, 78, 90, 79, 116, 41, 115, 40, 114, 40, 132, 26, 132, 26, 145, 17, 161, 12, 176, 10,
            177, 11,
        ],
        [
            24, 179, 48, 138, 54, 135, 54, 132, 53, 134, 56, 133, 55, 132, 55, 132, 61, 114, 70,
            96, 74, 88, 75, 88, 87, 74, 89, 66, 91, 67, 100, 59, 108, 50, 120, 40, 122, 37, 97, 43,
            78, 50,
        ],
    ],
    [
        [
            83, 78, 84, 81, 88, 75, 86, 74, 87, 71, 90, 73, 93, 74, 93, 74, 109, 40, 114, 36, 117,
            34, 117, 34, 143, 17, 145, 18, 146, 19, 162, 12, 165, 10, 178, 7, 189, 6, 190, 8, 177,
            9,
        ],
        [
            23, 178, 54, 115, 63, 102, 66, 98, 69, 99, 74, 89, 71, 91, 73, 91, 78, 89, 86, 80, 92,
            66, 93, 64, 102, 59, 103, 60, 104, 60, 117, 52, 123, 44, 138, 35, 133, 31, 97, 38, 77,
            45,
        ],
    ],
    [
        [
            61, 90, 93, 60, 105, 42, 107, 41, 110, 45, 116, 38, 113, 38, 112, 38, 124, 26, 132, 27,
            136, 19, 140, 20, 155, 14, 159, 16, 158, 18, 170, 13, 177, 10, 187, 8, 192, 6, 175, 9,
            159, 10,
        ],
        [
            21, 178, 59, 110, 71, 86, 75, 85, 84, 83, 91, 66, 88, 73, 87, 72, 92, 75, 98, 72, 105,
            58, 107, 54, 115, 52, 114, 55, 112, 56, 129, 51, 132, 40, 150, 33, 140, 29, 98, 35, 77,
            42,
        ],
    ],
    [
        [
            42, 121, 96, 66, 108, 43, 111, 40, 117, 44, 123, 32, 120, 36, 119, 33, 127, 33, 134,
            34, 139, 21, 147, 23, 152, 20, 158, 25, 154, 26, 166, 21, 173, 16, 184, 13, 184, 10,
            150, 13, 139, 15,
        ],
        [
            22, 178, 63, 114, 74, 82, 84, 83, 92, 82, 103, 62, 96, 72, 96, 67, 101, 73, 107, 72,
            113, 55, 118, 52, 125, 52, 118, 52, 117, 55, 135, 49, 137, 39, 157, 32, 145, 29, 97,
            33, 77, 40,
        ],
    ],
];
pub const SMALL_ENERGY_ICDF: [u8; 3] = [2, 1, 0];
pub const SPREAD_ICDF: [u8; 4] = [25, 23, 2, 0];
pub const TAPSET_ICDF: [u8; 3] = [2, 1, 0];
pub const TF_SELECT_TABLE: [[i8; 8]; 4] = [
    [0, -1, 0, -1, 0, -1, 0, -1],
    [0, -1, 0, -2, 1, 0, 1, -1],
    [0, -2, 0, -3, 2, 0, 1, -1],
    [0, -2, 0, -3, 3, 0, 1, -1],
];
pub const SPREAD_NORMAL: i32 = 2;
pub const COMBFILTER_MINPERIOD: usize = 15;
/// Silence floor of the log-energy history.
pub const E_FLOOR: f32 = -28.0;

// --- Energy decode ---

fn unquant_coarse(
    start: usize,
    end: usize,
    e: &mut [f32],
    intra: bool,
    rc: &mut RangeDec,
    channels: usize,
    lm: usize,
) {
    let model = &E_PROB_MODEL[lm][usize::from(intra)];
    let coef = if intra { 0.0 } else { PRED_COEF[lm] };
    let beta = if intra { BETA_INTRA } else { BETA_COEF[lm] };
    let mut prev = [0.0f32; 2];
    let budget = (rc.storage * 8) as i32;
    for i in start..end {
        for (c, pc) in prev.iter_mut().enumerate().take(channels) {
            let tell = rc.tell();
            let qi = if budget - tell >= 15 {
                let p = 2 * i.min(20);
                rc.laplace_decode(u32::from(model[p]) << 7, i32::from(model[p + 1]) << 6)
            } else if budget - tell >= 2 {
                let s = rc.decode_icdf(&SMALL_ENERGY_ICDF, 2);
                (s >> 1) ^ -(s & 1)
            } else if budget - tell >= 1 {
                if rc.decode_bit_logp(1) { -1 } else { 0 }
            } else {
                -1
            };
            let k = c * NB + i;
            e[k] = e[k].max(-9.0);
            let tmp = coef * e[k] + *pc + qi as f32;
            e[k] = tmp;
            *pc += qi as f32 - beta * qi as f32;
        }
    }
}

fn unquant_fine(
    start: usize,
    end: usize,
    e: &mut [f32],
    ebits: &[i32],
    rc: &mut RangeDec,
    channels: usize,
) {
    for i in start..end {
        for c in 0..channels {
            let bits = ebits[i];
            if bits <= 0 {
                continue;
            }
            let q = rc.dec_bits(bits as u32);
            e[c * NB + i] += (q as f32 + 0.5) / (1 << bits) as f32 - 0.5;
        }
    }
}

fn tf_decode(
    start: usize,
    end: usize,
    is_transient: bool,
    tf_res: &mut [i32],
    lm: i32,
    rc: &mut RangeDec,
) {
    let mut curr = 0;
    let mut changed = 0;
    let mut logp = if is_transient { 2 } else { 4 };
    let tell0 = rc.tell();
    let budget0 = rc.storage as i32 * 8;
    let rsv = i32::from(lm > 0 && tell0 + logp < budget0);
    let budget = budget0 - rsv;
    let mut tell = tell0;
    for t in tf_res[start..end].iter_mut() {
        if tell + logp <= budget {
            curr ^= i32::from(rc.decode_bit_logp(logp as u32));
            tell = rc.tell();
            changed |= curr;
        }
        *t = curr;
        logp = if is_transient { 4 } else { 5 };
    }
    let mut select = 0;
    if rsv > 0
        && TF_SELECT_TABLE[lm as usize][4 * usize::from(is_transient) + changed as usize]
            != TF_SELECT_TABLE[lm as usize][4 * usize::from(is_transient) + 2 + changed as usize]
    {
        select = i32::from(rc.decode_bit_logp(1));
    }
    for t in tf_res[start..end].iter_mut() {
        *t = TF_SELECT_TABLE[lm as usize]
            [4 * usize::from(is_transient) + 2 * select as usize + *t as usize] as i32;
    }
}

// --- Public API ---

/// Log-energy history across frames of one CELT run (2 channels × 21 bands).
/// The caller resets on every Opus mode change (mirrors `celt_dec.reset()`).
#[derive(Clone)]
pub struct CeltState {
    e: [f32; 42],
}

impl CeltState {
    pub fn new() -> Self {
        Self { e: [0.0; 42] }
    }

    pub fn reset(&mut self) {
        self.e.fill(0.0);
    }
}

impl Default for CeltState {
    fn default() -> Self {
        Self::new()
    }
}

/// Everything the header walk learned about one CELT frame.
pub struct CeltFrame {
    /// Coarse + fine log2 energies per channel and band (means NOT added).
    pub log_e: [[f32; NB]; 2],
    /// Coarse allocation per band in 1/8 bit (exact, from the allocation).
    pub coarse_bits: [i32; NB],
    /// Fine energy bits per band and channel (exact).
    pub ebits: [i32; NB],
    pub transient: bool,
    pub intra: bool,
    pub trim: i32,
    pub spread: i32,
    pub intensity: i32,
    pub dual: i32,
    pub coded_bands: i32,
    pub silence: bool,
    /// Allocation input in 1/8 bit (for the sum check).
    pub alloc_bits: i32,
    /// Leftover 1/8 bits after the last band (for the sum check).
    pub balance: i32,
    /// Reserves consumed by header reads (skip/intensity/dual), in 1/8 bit.
    /// Exact identity: bands + `balance` + `overhead` == `alloc_bits`.
    pub overhead: i32,
    /// Budget after reserve add-backs, in 1/8 bit (sum-check identity).
    pub total_rsv: i32,
}

/// Log2 energy (+ band mean) converted to dB.
pub fn energy_db(log_e: f32, band: usize) -> f32 {
    6.0206 * (log_e + E_MEANS[band.min(20)])
}

/// Frame-size exponent from the TOC frame duration (2.5 ms → 0 … 20 ms → 3).
pub fn lm_of_ms(frame_ms: f64) -> usize {
    match frame_ms {
        x if x < 3.0 => 0,
        x if x < 7.5 => 1,
        x if x < 15.0 => 2,
        _ => 3,
    }
}

/// Last coded CELT band from the packet bandwidth (RFC 6716 §3.1).
pub fn endband_of(bandwidth: Bandwidth) -> usize {
    match bandwidth {
        Bandwidth::Narrow => 13,
        Bandwidth::Medium | Bandwidth::Wide => 17,
        Bandwidth::SuperWide => 19,
        Bandwidth::Full => 21,
    }
}

/// Walk one CELT frame's header. `data` is the frame payload WITHOUT the TOC
/// byte, `lm` the frame-size exponent (0 = 2.5 ms … 3 = 20 ms), `channels`
/// the coded channel count (1–2), `[start, end)` the coded band range
/// (CELT-only: 0; hybrid: 17; `end` from the bandwidth).
pub fn parse_celt_frame(
    data: &[u8],
    lm: usize,
    channels: usize,
    start: usize,
    end: usize,
    state: &mut CeltState,
) -> Result<CeltFrame, &'static str> {
    if data.is_empty() {
        return Err("bad celt frame parameters");
    }
    let mut rc = RangeDec::new(data);
    parse_celt_frame_rc(
        &mut rc,
        data.len() as i32 * 8,
        lm,
        channels,
        start,
        end,
        state,
    )
}

/// Walk one CELT frame's header on an already positioned range coder
/// (Hybrid: derselbe Coder nach dem SILK-Praefix; `total_bits` ist die
/// effektive CELT-Laenge nach Redundanz-Abzug, Rueckwaerts-Bits lesen
/// vom Payload-Ende wie in der Referenz ohne Storage-Kuerzung).
pub fn parse_celt_frame_rc(
    rc: &mut RangeDec<'_>,
    total_bits: i32,
    lm: usize,
    channels: usize,
    start: usize,
    end: usize,
    state: &mut CeltState,
) -> Result<CeltFrame, &'static str> {
    let channels = channels.clamp(1, 2);
    let end = end.min(NB).max(start);
    if lm > 3 || total_bits < 0 {
        return Err("bad celt frame parameters");
    }

    // Mono packet in stereo state: predict from the hotter channel.
    if channels == 1 {
        for i in 0..NB {
            state.e[i] = state.e[i].max(state.e[NB + i]);
        }
    }

    let tell = rc.tell();
    let mut silence = false;
    if tell >= total_bits {
        silence = true;
    } else if tell == 1 {
        silence = rc.decode_bit_logp(15);
    }
    if silence {
        rc.nbits_total += total_bits - rc.tell();
    }

    // Prefilter: consume, values unused (synthesis-only).
    if start == 0 && !silence && rc.tell() + 16 <= total_bits && rc.decode_bit_logp(1) {
        let octave = rc.dec_uint(6);
        let _ = rc.dec_bits(4 + octave);
        let _ = rc.dec_bits(3);
        if rc.tell() + 2 <= total_bits {
            let _ = rc.decode_icdf(&TAPSET_ICDF, 2);
        }
    }

    let transient = lm > 0 && rc.tell() + 3 <= total_bits && rc.decode_bit_logp(3);
    let intra = if rc.tell() + 3 <= total_bits {
        rc.decode_bit_logp(3)
    } else {
        false
    };

    unquant_coarse(start, end, &mut state.e, intra, &mut *rc, channels, lm);
    let mut tf_res = [0i32; NB];
    tf_decode(start, end, transient, &mut tf_res, lm as i32, &mut *rc);
    let spread = if rc.tell() + 4 <= total_bits {
        rc.decode_icdf(&SPREAD_ICDF, 5)
    } else {
        SPREAD_NORMAL
    };

    let mid = dynalloc_section(&mut *rc, total_bits, lm, channels, start, end, transient);
    let (offsets, cap, trim, anti_collapse_rsv) =
        (mid.offsets, mid.cap, mid.trim, mid.anti_collapse_rsv);

    let mut intensity = 0;
    let mut dual = i32::from(channels == 2);
    let mut pulses = [0i32; NB];
    let mut ebits = [0i32; NB];
    let mut fine_priority = [0i32; NB];
    let mut balance = 0;
    let alloc_bits = (total_bits << BITRES) - rc.tell_frac() - 1 - anti_collapse_rsv;
    let mut overhead = 0;
    let mut total_rsv = 0;
    let coded = compute_allocation(
        start,
        end,
        &offsets,
        &cap,
        trim,
        &mut intensity,
        &mut dual,
        alloc_bits,
        &mut balance,
        &mut pulses,
        &mut ebits,
        &mut fine_priority,
        channels as i32,
        lm as i32,
        &mut *rc,
        &mut overhead,
        &mut total_rsv,
    );
    // Guard against degenerate budgets (division by zero in the spread).
    if coded < 0 {
        return Err("allocation failed");
    }
    unquant_fine(start, end, &mut state.e, &ebits, &mut *rc, channels);

    // Frame-end bookkeeping (mirrors decode_impl_from_rc).
    if silence {
        for v in state.e.iter_mut().take(channels * NB) {
            *v = E_FLOOR;
        }
    }
    let mut log_e = [[0.0f32; NB]; 2];
    log_e[0].copy_from_slice(&state.e[..NB]);
    if channels == 1 {
        let (ch0, ch1) = state.e.split_at_mut(NB);
        ch1[..NB].copy_from_slice(&ch0[..NB]);
        log_e[1] = log_e[0];
    } else {
        log_e[1].copy_from_slice(&state.e[NB..2 * NB]);
    }
    for c in 0..2 {
        for i in 0..start {
            state.e[c * NB + i] = 0.0;
        }
        for i in end..NB {
            state.e[c * NB + i] = 0.0;
        }
    }

    Ok(CeltFrame {
        log_e,
        coarse_bits: pulses,
        ebits,
        transient,
        intra,
        trim,
        spread,
        intensity,
        dual,
        coded_bands: coded,
        silence,
        alloc_bits,
        balance,
        overhead,
        total_rsv,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_empty_frame() {
        let mut st = CeltState::new();
        assert!(parse_celt_frame(&[], 3, 2, 0, 21, &mut st).is_err());
    }
}
