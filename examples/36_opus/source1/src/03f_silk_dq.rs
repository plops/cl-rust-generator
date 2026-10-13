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

/// Signaltyp-Kurzname (Hover, JSON, Report).
pub fn signal_name(signal: i8) -> &'static str {
    match signal {
        2 => "STIMMHAFT",
        1 => "STIMMLOS",
        _ => "INAKTIV",
    }
}

/// Paket-Mittel aus SILK-Frames (`None` bei leerem Paket).
pub fn summarize(frames: &[crate::silk::SilkFrame]) -> Option<crate::silk::SilkSummary> {
    if frames.is_empty() {
        return None;
    }
    let mut signal = 0i8;
    let mut gain_pow = 0.0f64;
    let mut gain_n = 0u32;
    let mut pitch_sum = 0.0f64;
    let mut pitch_n = 0u32;
    let mut header = frames.len() as i32 + 1; // VAD je Frame + LBRR.
    let mut pulses = 0i32;
    for f in frames {
        signal = signal.max(f.signal_type);
        for (g, h) in f.gains_db.iter().zip(f.pitch_hz.iter()).take(f.nb_subfr) {
            gain_pow += 10.0f64.powf(f64::from(*g) / 10.0);
            gain_n += 1;
            if *h > 0.0 {
                pitch_sum += f64::from(*h);
                pitch_n += 1;
            }
        }
        header += f.bits.signal + f.bits.gains + f.bits.nlsf + f.bits.ltp + f.bits.seed;
        pulses += f.bits.pulses;
    }
    let last = &frames[frames.len() - 1];
    let order = if last.fs_khz == 16 {
        MAX_LPC_ORDER
    } else {
        MIN_LPC_ORDER
    };
    let mut nlsf_q15 = [0i16; MAX_LPC_ORDER];
    nlsf_q15[..order].copy_from_slice(&last.nlsf_q15[..order]);
    Some(crate::silk::SilkSummary {
        signal,
        gains_db: (10.0 * (gain_pow / gain_n.max(1) as f64 + 1e-12).log10()) as f32,
        pitch_hz: if pitch_n > 0 {
            (pitch_sum / pitch_n as f64) as f32
        } else {
            0.0
        },
        header_bits: header,
        pulse_bits: pulses,
        n_frames: frames.len(),
        nlsf_q15,
        order,
        fs_khz: last.fs_khz,
    })
}

/// LPC-Huellkurve in dB aus NLSF (Peak = 0 dB) auf den 256 Log-Bins.
///
/// Eigene Float-Rechnung (Lehrbuch-Formel LSF→LPC→Frequenzgang, kein
/// Festkomma-Port): LSF-Radianten, P/Q-Polynome aus Biquad-Kaskaden,
/// A=(P+Q)/2, |H|=1/|A|. Ueber Nyquist der SILK-Rate steht Boden
/// (kein Modell, kein Signal). Orakel in `tests/silk.rs`.
pub fn lpc_envelope_db(
    nlsf_q15: &[i16],
    order: usize,
    fs_khz: i32,
) -> [f32; crate::spectrum::BINS] {
    use crate::spectrum::{BINS, DB_FLOOR, FMAX, FMIN};
    use std::f64::consts::PI;
    let d = order.min(nlsf_q15.len()).min(MAX_LPC_ORDER) & !1;
    if d < 2 {
        return [0.0; BINS];
    }
    let mut w = [0.0f64; MAX_LPC_ORDER];
    for (o, n) in w.iter_mut().zip(nlsf_q15.iter()).take(d) {
        *o = f64::from(*n) / 32768.0 * PI;
    }
    // P aus LSF 1,3,5.. (0-indexiert gerade), Q aus 2,4,6...
    let mut p = [0.0f64; MAX_LPC_ORDER + 1];
    let mut q = [0.0f64; MAX_LPC_ORDER + 1];
    p[0] = 1.0;
    q[0] = 1.0;
    let (mut np, mut nq) = (0usize, 0usize);
    for (i, wi) in w.iter().enumerate().take(d) {
        let (poly, n) = if i % 2 == 0 {
            (&mut p, &mut np)
        } else {
            (&mut q, &mut nq)
        };
        let c = -2.0 * wi.cos();
        let mut nxt = [0.0f64; MAX_LPC_ORDER + 1];
        for k in 0..=*n {
            nxt[k] += poly[k];
            nxt[k + 1] += c * poly[k];
            nxt[k + 2] += poly[k];
        }
        let len = poly.len();
        poly.copy_from_slice(&nxt[..len]);
        *n += 2;
    }
    let mut a = [0.0f64; MAX_LPC_ORDER + 1];
    for (k, v) in a.iter_mut().enumerate().take(d + 1) {
        *v = 0.5 * (p[k] + q[k]);
    }
    // Anzeige-Stabilisierung (Decoder-Analogon `silk_bwexpander`): enge
    // LSF-Paare legen Pole fast auf den Einheitskreis (unendlich schmal,
    // Bin-Raster trifft sie per Lotterie). gamma=0.99 zieht sie nach innen
    // (Mindest-Bandbreite ~50 Hz) — Formant-LAGEN bleiben erhalten.
    for (k, v) in a.iter_mut().enumerate().take(d + 1) {
        *v *= 0.99f64.powi(k as i32);
    }
    let nyq = f64::from(fs_khz) * 500.0;
    let mut out = [DB_FLOOR; BINS];
    let mut peak = f64::NEG_INFINITY;
    let mut mags = [f64::NEG_INFINITY; BINS];
    for (b, m) in mags.iter_mut().enumerate() {
        let f = f64::from(FMIN) * f64::from(FMAX / FMIN).powf((b as f64 + 0.5) / BINS as f64);
        if f > nyq {
            continue;
        }
        // Filter lebt auf der SILK-Rate (nicht 48 kHz!): sonst wandert
        // der Peak um den Faktor 48/fs_khz (hier 3x bei WB).
        let om = f / (f64::from(fs_khz) * 1000.0) * 2.0 * PI;
        let (mut re, mut im) = (0.0f64, 0.0f64);
        for (k, ak) in a.iter().enumerate().take(d + 1) {
            re += ak * (k as f64 * om).cos();
            im -= ak * (k as f64 * om).sin();
        }
        *m = -10.0 * (re * re + im * im + 1e-30).log10();
        peak = peak.max(*m);
    }
    for (b, o) in out.iter_mut().enumerate() {
        if mags[b] > f64::NEG_INFINITY {
            *o = (mags[b] - peak) as f32;
        }
    }
    out
}

#[cfg(test)]
mod envelope_tests {
    use super::*;
    use crate::spectrum::{BINS, FMAX, FMIN};

    fn peak_hz(env: &[f32; BINS]) -> f32 {
        let (mut bi, mut bv) = (0, f32::NEG_INFINITY);
        for (b, v) in env.iter().enumerate() {
            if *v > bv {
                (bi, bv) = (b, *v);
            }
        }
        FMIN * (FMAX / FMIN).powf((bi as f32 + 0.5) / BINS as f32)
    }

    #[test]
    fn formant_bei_1khz_trifft_1khz() {
        // NLSF-Paar um 1000 Hz (32768 = Nyquist 8 kHz): Huellen-Peak muss dort liegen.
        let d = 100i16;
        let mut nlsf = [0i16; MAX_LPC_ORDER];
        nlsf[0] = 4096 - d;
        nlsf[1] = 4096 + d;
        let env = lpc_envelope_db(&nlsf, 2, 16);
        let peak = peak_hz(&env);
        eprintln!("peak={peak:.0}Hz");
        assert!((800.0..1250.0).contains(&peak), "peak {peak}");
    }

    #[test]
    fn zwei_formanten_treffen_beide() {
        // Paare bei 500 Hz (nlsf 2048) und 2500 Hz (nlsf 10240), Ordnung 4:
        // die zwei groessten lokalen Maxima muessen dort liegen (±20 %).
        let nlsf = [2048 - 100, 2048 + 100, 10240 - 100, 10240 + 100];
        let env = lpc_envelope_db(&nlsf, 4, 16);
        let mut peaks = Vec::new();
        for b in 1..BINS - 1 {
            if env[b] > env[b - 1] && env[b] >= env[b + 1] {
                peaks.push((
                    FMIN * (FMAX / FMIN).powf((b as f32 + 0.5) / BINS as f32),
                    env[b],
                ));
            }
        }
        peaks.sort_by(|a, b| b.1.total_cmp(&a.1));
        eprintln!("formanten={peaks:.0?}");
        assert!(peaks.len() >= 2);
        for ziel in [500.0, 2500.0] {
            assert!(
                peaks
                    .iter()
                    .take(2)
                    .any(|(f, _)| (f / ziel - 1.0).abs() < 0.2),
                "formant {ziel} fehlt in {peaks:.0?}"
            );
        }
    }
}
