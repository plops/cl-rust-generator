//! 03f — SILK-Dequant: reine Mathematik ohne Bit-Reads.
//!
//! Port aus `opus-pure` (`silk/gain_quant.rs`, `silk/nlsf_decode.rs`,
//! `silk/nlsf_unpack.rs`, `silk/nlsf.rs`-Stabilisierung,
//! `decode_parameters.rs`-Pitch): Gains Q16, NLSF-Vektor, Pitch-Lags.
//! Keine Range-Coder-Reads — Eingaben sind gewalkte Indizes.

use crate::silk::{limit32, log2lin, rshift_round, smulbb, smulwb};
use crate::silk_tab::*;

// --- Reine Dequant-Mathematik (keine Bits; Port `decode_parameters`) ---

/// Subframe-Gains Q16 aus den Indizes (`silk_gains_dequant`-Port).
pub(crate) fn dequant_gains(
    ind: &[i8; MAX_NB_SUBFR],
    last: &mut i8,
    cond: i32,
    nb: usize,
) -> [i32; 4] {
    let mut out = [0i32; 4];
    for (k, o) in out.iter_mut().enumerate().take(nb) {
        if k == 0 && cond == 0 {
            *last = (*last as i32 - 16).max(ind[k] as i32) as i8;
        } else {
            let tmp = ind[k] as i32 + MIN_DELTA_GAIN_QUANT;
            let thr = 2 * MAX_DELTA_GAIN_QUANT - N_LEVELS_QGAIN + *last as i32;
            *last = if tmp > thr {
                *last as i32 + (tmp << 1) - thr
            } else {
                *last as i32 + tmp
            } as i8;
        }
        *last = limit32(*last as i32, 0, N_LEVELS_QGAIN - 1) as i8;
        // Begrenzung 3967 gilt dem Q7-Eingang (Referenz), nicht dem Ausgang.
        let q7 = (smulwb(INV_SCALE_Q16, *last as i32) + GAIN_OFFSET).min(3967);
        *o = log2lin(q7);
    }
    out
}

/// NLSF-Vektor Q15 aus den Indizes (`silk_nlsf_decode`-Port).
pub(crate) fn dequant_nlsf(nlsf_idx: &[i8; 17], cb: &NlsfCodebook) -> [i16; MAX_LPC_ORDER] {
    const ADJ: i32 = 102; // NLSF_QUANT_LEVEL_ADJ
    let order = cb.order as usize;
    // Stufe-2-Auswahl entpacken (wie im Walk).
    let sel = &cb.ec_sel[(nlsf_idx[0] as usize) * order / 2..];
    let mut pred_q8 = [0u8; MAX_LPC_ORDER];
    let mut ec_ix = [0i16; MAX_LPC_ORDER];
    for i in (0..order).step_by(2) {
        let entry = sel[i / 2];
        ec_ix[i] = smulbb(((entry >> 1) & 7) as i32, 9) as i16;
        pred_q8[i] = cb.pred_q8[i + ((entry & 1) as usize) * (order - 1)];
        ec_ix[i + 1] = smulbb(((entry >> 5) & 7) as i32, 9) as i16;
        pred_q8[i + 1] = cb.pred_q8[i + (((entry >> 4) & 1) as usize) * (order - 1) + 1];
    }
    // Residuen dequantisieren (rueckwaerts-Praediktion).
    let mut res_q10 = [0i16; MAX_LPC_ORDER];
    let mut acc_q10 = 0i32;
    for i in (0..order).rev() {
        let pred = smulbb(acc_q10, pred_q8[i] as i32) >> 8;
        let mut cur = (nlsf_idx[i + 1] as i32) << 10;
        if cur > 0 {
            cur -= ADJ;
        } else if cur < 0 {
            cur += ADJ;
        }
        acc_q10 = pred.wrapping_add(smulwb(cur, cb.quant_step_size_q16));
        res_q10[i] = acc_q10 as i16;
    }
    // Gegen Stufe 1 aufloesen + stabilisieren.
    let mut out = [0i16; MAX_LPC_ORDER];
    let o = order;
    let el = &cb.cb1_nlsf_q8[(nlsf_idx[0] as usize) * o..];
    let wg = &cb.cb1_wght_q9[(nlsf_idx[0] as usize) * o..];
    for i in 0..o {
        let v = (res_q10[i] as i32) << 14;
        out[i] = limit32(v / wg[i] as i32 + (el[i] as i32) * 128, 0, 32767) as i16;
    }
    stabilize(&mut out[..o], &cb.delta_min_q15[..=o]);
    out
}

/// Mindestabstaende der NLSF erzwingen (`silk_nlsf_stabilize`-Port).
pub(crate) fn stabilize(nlsf: &mut [i16], dmin: &[i16]) {
    for _ in 0..20 {
        let mut min_diff = nlsf[0] as i32 - dmin[0] as i32;
        let mut at = 0usize;
        for i in 1..nlsf.len() {
            let d = nlsf[i] as i32 - (nlsf[i - 1] as i32 + dmin[i] as i32);
            if d < min_diff {
                min_diff = d;
                at = i;
            }
        }
        let n = nlsf.len();
        let end = (1 << 15) - (nlsf[n - 1] as i32 + dmin[n] as i32);
        if end < min_diff {
            min_diff = end;
            at = n;
        }
        if min_diff >= 0 {
            return;
        }
        if at == 0 {
            nlsf[0] = dmin[0];
        } else if at == n {
            nlsf[n - 1] = ((1i32 << 15) - dmin[n] as i32) as i16;
        } else {
            let mut lo: i32 = dmin[..at].iter().map(|&x| x as i32).sum();
            lo += (dmin[at] as i32) >> 1;
            let mut hi = 1 << 15;
            for d in dmin[at + 1..=n].iter().rev() {
                hi -= *d as i32;
            }
            hi -= (dmin[at] as i32) >> 1;
            let c = limit32(
                rshift_round(nlsf[at - 1] as i32 + nlsf[at] as i32, 1),
                lo,
                hi,
            ) as i16;
            nlsf[at - 1] = c - ((dmin[at] as i32) >> 1) as i16;
            nlsf[at] = nlsf[at - 1] + dmin[at];
        }
    }
    let n = nlsf.len();
    nlsf[..n].sort();
    nlsf[0] = nlsf[0].max(dmin[0]);
    for i in 1..n {
        nlsf[i] = nlsf[i].max(nlsf[i - 1].saturating_add(dmin[i]));
    }
    nlsf[n - 1] = nlsf[n - 1].min(((1i32 << 15) - dmin[n] as i32) as i16);
    for i in (0..n - 1).rev() {
        nlsf[i] = nlsf[i].min(nlsf[i + 1].wrapping_sub(dmin[i + 1]));
    }
}

/// Pitch-Lags je Subframe in Samples (`silk_decode_pitch`-Port).
pub(crate) fn decode_pitch(lag_index: i16, contour: i8, fs_khz: i32, nb_subfr: usize) -> [i32; 4] {
    let min_lag = PITCH_EST_MIN_LAG_MS * fs_khz;
    let max_lag = PITCH_EST_MAX_LAG_MS * fs_khz;
    let lag = min_lag + lag_index as i32;
    let mut out = [0i32; 4];
    for (k, o) in out.iter_mut().enumerate().take(nb_subfr) {
        let ci = contour as usize;
        let d = if fs_khz == 8 {
            if nb_subfr == MAX_NB_SUBFR {
                SILK_CB_LAGS_STAGE2[k % 4][ci % PE_NB_CBKS_STAGE2_EXT] as i32
            } else {
                SILK_CB_LAGS_STAGE2_10_MS[k % 2][ci % PE_NB_CBKS_STAGE2_10MS] as i32
            }
        } else if nb_subfr == MAX_NB_SUBFR {
            SILK_CB_LAGS_STAGE3[k % 4][ci % PE_NB_CBKS_STAGE3_MAX] as i32
        } else {
            SILK_CB_LAGS_STAGE3_10_MS[k % 2][ci % PE_NB_CBKS_STAGE3_10MS] as i32
        };
        *o = (lag + d).clamp(min_lag, max_lag);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gains_abs_und_delta_rechnen_sich_fort() {
        // Absolut: last uebernimmt Index 20, Deltas 4 = keine Aenderung.
        let mut last = 10i8;
        let g = dequant_gains(&[20, 4, 4, 4], &mut last, 0, 4);
        assert_eq!(last, 20);
        assert_eq!(g, [1925120, 1925120, 1925120, 1925120]);
        // Konditionell: Index 0 heisst tmp = -4 (leiser).
        let h = dequant_gains(&[0, 4, 4, 4], &mut last, 1, 4);
        assert!(h[0] < g[0], "leiserer Index, kleinerer Gain");
        assert_eq!(h[1], h[0]);
    }
}
