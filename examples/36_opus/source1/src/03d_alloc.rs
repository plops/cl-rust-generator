//! 03d — Bit-Allokation: Baender-Grid, Dynalloc, Trim, `clt_compute_allocation`.
//!
//! Port von `opus-pure` celt/rate.rs (Decode-Seite): verteilt das
//! Frame-Budget auf Coarse-Bits (`pulses`) und Fine-Energie-Bits (`ebits`)
//! je Band. Exakte Identitaet: Baender + Rest + Overhead = Input.

use crate::range::{BITRES, RangeDec};

/// Coded frequency bands (bark-scale), index 0..21.
pub(crate) const NB: usize = 21;

/// Critical-band edges in short-MDCT bins (`EBAND_5MS`).
pub const EBANDS: [i32; 22] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 34, 40, 48, 60, 78, 100,
];
const TRIM_ICDF: [u8; 11] = [126, 124, 119, 109, 87, 41, 19, 9, 4, 2, 0];

/// Allocation prototype vectors, 11 rows of 21 (`BAND_ALLOCATION`).
const ALLOC_VECTORS: [u8; 231] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90, 80, 75, 69, 63, 56, 49, 40,
    34, 29, 20, 18, 10, 0, 0, 0, 0, 0, 0, 0, 0, 110, 100, 90, 84, 78, 71, 65, 58, 51, 45, 39, 32,
    26, 20, 12, 0, 0, 0, 0, 0, 0, 118, 110, 103, 93, 86, 80, 75, 70, 65, 59, 53, 47, 40, 31, 23,
    15, 4, 0, 0, 0, 0, 126, 119, 112, 104, 95, 89, 83, 78, 72, 66, 60, 54, 47, 39, 32, 25, 17, 12,
    1, 0, 0, 134, 127, 120, 114, 103, 97, 91, 85, 78, 72, 66, 60, 54, 47, 41, 35, 29, 23, 16, 10,
    1, 144, 137, 130, 124, 113, 107, 101, 95, 88, 82, 76, 70, 64, 57, 51, 45, 39, 33, 26, 15, 1,
    152, 145, 138, 132, 123, 117, 111, 105, 98, 92, 86, 80, 74, 67, 61, 55, 49, 43, 36, 20, 1, 162,
    155, 148, 142, 133, 127, 121, 115, 108, 102, 96, 90, 84, 77, 71, 65, 59, 53, 46, 30, 1, 172,
    165, 158, 152, 143, 137, 131, 125, 118, 112, 106, 100, 94, 87, 81, 75, 69, 63, 56, 45, 20, 200,
    200, 200, 200, 200, 200, 200, 200, 198, 193, 188, 183, 178, 173, 168, 163, 158, 153, 148, 129,
    104,
];
/// Dynalloc caps per `(lm, channels)` pair, 8 rows of 21 (`CACHE_CAPS50`).
const CAPS: [u8; 168] = [
    224, 224, 224, 224, 224, 224, 224, 224, 160, 160, 160, 160, 185, 185, 185, 178, 178, 168, 134,
    61, 37, 224, 224, 224, 224, 224, 224, 224, 224, 240, 240, 240, 240, 207, 207, 207, 198, 198,
    183, 144, 66, 40, 160, 160, 160, 160, 160, 160, 160, 160, 185, 185, 185, 185, 193, 193, 193,
    183, 183, 172, 138, 64, 38, 240, 240, 240, 240, 240, 240, 240, 240, 207, 207, 207, 207, 204,
    204, 204, 193, 193, 180, 143, 66, 40, 185, 185, 185, 185, 185, 185, 185, 185, 193, 193, 193,
    193, 193, 193, 193, 183, 183, 172, 138, 65, 39, 207, 207, 207, 207, 207, 207, 207, 207, 204,
    204, 204, 204, 201, 201, 201, 188, 188, 176, 141, 66, 40, 193, 193, 193, 193, 193, 193, 193,
    193, 193, 193, 193, 193, 194, 194, 194, 184, 184, 173, 139, 65, 39, 204, 204, 204, 204, 204,
    204, 204, 204, 201, 201, 201, 201, 198, 198, 198, 187, 187, 175, 140, 66, 40,
];
/// Log band widths (`LOG_N_400`).
const LOG_N: [i16; 21] = [
    0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 16, 16, 16, 21, 21, 24, 29, 34, 36,
];
/// Fractional log2 table for the intensity-stereo reservation.
const LOG2_FRAC_TABLE: [u8; 24] = [
    0, 8, 13, 16, 19, 21, 23, 24, 26, 27, 28, 29, 30, 31, 32, 32, 33, 34, 34, 35, 36, 36, 37, 37,
];
const FINE_OFFSET: i32 = 21;
const MAX_FINE_BITS: i32 = 8;

#[allow(clippy::too_many_arguments)]
fn interp_bits2pulses(
    start: usize,
    end: usize,
    skip_start: usize,
    bits1: &[i32],
    bits2: &[i32],
    thresh: &[i32],
    cap: &[i32],
    total: i32,
    balance_out: &mut i32,
    skip_rsv: i32,
    intensity: &mut i32,
    mut intensity_rsv: i32,
    dual: &mut i32,
    dual_rsv: i32,
    pulses: &mut [i32],
    ebits: &mut [i32],
    fine_priority: &mut [i32],
    c: i32,
    lm: i32,
    rc: &mut RangeDec,
    overhead_out: &mut i32,
    total_rsv_out: &mut i32,
) -> i32 {
    let mut lo = 0;
    let mut hi = 1 << 6;
    let alloc_floor = c << BITRES;
    let stereo = i32::from(c > 1);
    let log_m = lm << BITRES;
    let mut bits = [0i32; NB];
    for _ in 0..6 {
        let mid = (lo + hi) >> 1;
        let mut psum = 0;
        let mut done = false;
        for j in (start..end).rev() {
            let tmp = bits1[j] + ((mid * bits2[j]) >> 6);
            if tmp >= thresh[j] || done {
                done = true;
                psum += tmp.min(cap[j]);
            } else if tmp >= alloc_floor {
                psum += alloc_floor;
            }
        }
        if psum > total {
            hi = mid;
        } else {
            lo = mid;
        }
    }
    let mut psum = 0;
    let mut done = false;
    for j in (start..end).rev() {
        let mut tmp = bits1[j] + ((lo * bits2[j]) >> 6);
        if tmp < thresh[j] && !done {
            tmp = if tmp >= alloc_floor { alloc_floor } else { 0 };
        } else {
            done = true;
        }
        tmp = tmp.min(cap[j]);
        bits[j] = tmp;
        psum += tmp;
    }

    let mut coded = end;
    let mut total_rsv = total;
    let mut skip_reads = 0;
    let mut break_bit = false;
    loop {
        if coded <= start {
            break;
        }
        let j = coded - 1;
        if j <= skip_start {
            total_rsv += skip_rsv;
            break;
        }
        let left = total_rsv - psum;
        let nb_samples = EBANDS[coded] - EBANDS[start];
        let percoeff = if nb_samples > 0 { left / nb_samples } else { 0 };
        let rem = (left - nb_samples * percoeff - (EBANDS[j] - EBANDS[start])).max(0);
        let band_width = EBANDS[coded] - EBANDS[j];
        let mut band_bits = bits[j] + percoeff * band_width + rem;
        if band_bits >= thresh[j].max(alloc_floor + (1 << BITRES)) {
            if rc.decode_bit_logp(1) {
                break_bit = true;
                break;
            }
            psum += 1 << BITRES;
            skip_reads += 1;
            band_bits -= 1 << BITRES;
        }
        psum -= bits[j] + intensity_rsv;
        if intensity_rsv > 0 {
            intensity_rsv = LOG2_FRAC_TABLE[j - start] as i32;
        }
        psum += intensity_rsv;
        if band_bits >= alloc_floor {
            psum += alloc_floor;
            bits[j] = alloc_floor;
        } else {
            bits[j] = 0;
        }
        coded -= 1;
    }

    if intensity_rsv > 0 {
        *intensity = start as i32 + rc.dec_uint((coded + 1 - start) as u32) as i32;
    } else {
        *intensity = 0;
    }
    let mut dual_rsv = dual_rsv;
    if *intensity <= start as i32 {
        total_rsv += dual_rsv;
        dual_rsv = 0;
    }
    let dual_consumed;
    if dual_rsv > 0 {
        dual_consumed = 1 << BITRES;
        *dual = i32::from(rc.decode_bit_logp(1));
    } else {
        dual_consumed = 0;
        *dual = 0;
    }
    // Reserves consumed by header reads (vs the allocation input).
    *overhead_out = skip_reads * (1 << BITRES)
        + i32::from(break_bit) * (1 << BITRES)
        + intensity_rsv
        + dual_consumed;
    *total_rsv_out = total_rsv;

    // Spread the remainder over the coded bands.
    let mut left = total_rsv - psum;
    let nb_samples = EBANDS[coded] - EBANDS[start];
    let percoeff = if nb_samples > 0 { left / nb_samples } else { 0 };
    left -= nb_samples * percoeff;
    for j in start..coded {
        bits[j] += percoeff * (EBANDS[j + 1] - EBANDS[j]);
    }
    for j in start..coded {
        let tmp = left.min(EBANDS[j + 1] - EBANDS[j]);
        bits[j] += tmp;
        left -= tmp;
    }

    // Split each band into coarse bits (`pulses`) and fine energy bits.
    let mut balance = 0;
    for j in start..coded {
        let n0 = EBANDS[j + 1] - EBANDS[j];
        let n = n0 << lm;
        let bit = bits[j] + balance;
        let excess;
        if n > 1 {
            excess = (bit - cap[j]).max(0);
            bits[j] = bit - excess;
            let den = c * n + i32::from(c == 2 && n > 2 && *dual == 0 && (j as i32) < *intensity);
            let nc_log_n = den * (i32::from(LOG_N[j]) + log_m);
            let mut offset = (nc_log_n >> 1) - den * FINE_OFFSET;
            if n == 2 {
                offset += den << BITRES >> 2;
            }
            if bits[j] + offset < (den * 2) << BITRES {
                offset += nc_log_n >> 2;
            } else if bits[j] + offset < (den * 3) << BITRES {
                offset += nc_log_n >> 3;
            }
            let num = (bits[j] + offset + (den << (BITRES - 1))).max(0);
            ebits[j] = ((num as u32 / den as u32) >> BITRES) as i32;
            if c * ebits[j] > (bits[j] >> BITRES) {
                ebits[j] = bits[j] >> stereo >> BITRES;
            }
            ebits[j] = ebits[j].min(MAX_FINE_BITS);
            fine_priority[j] = i32::from(ebits[j] * (den << BITRES) >= bits[j] + offset);
            bits[j] -= (c * ebits[j]) << BITRES;
        } else {
            excess = (bit - (c << BITRES)).max(0);
            bits[j] = bit - excess;
            ebits[j] = 0;
            fine_priority[j] = 1;
        }
        if excess > 0 {
            let extra_fine = (excess >> (stereo + BITRES)).min(MAX_FINE_BITS - ebits[j]);
            ebits[j] += extra_fine;
            let extra_bits = (extra_fine * c) << BITRES;
            // `balance` here is still the previous band's (libopus order).
            fine_priority[j] = i32::from(extra_bits >= excess - balance);
            balance = excess - extra_bits;
        } else {
            balance = 0;
        }
        pulses[j] = bits[j];
    }
    *balance_out = balance;
    for j in coded..end {
        ebits[j] = bits[j] >> stereo >> BITRES;
        bits[j] = 0;
        fine_priority[j] = i32::from(ebits[j] < 1);
        pulses[j] = 0;
    }
    coded as i32
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn compute_allocation(
    start: usize,
    end: usize,
    offsets: &[i32],
    cap: &[i32],
    trim: i32,
    intensity: &mut i32,
    dual: &mut i32,
    mut total: i32,
    balance: &mut i32,
    pulses: &mut [i32],
    ebits: &mut [i32],
    fine_priority: &mut [i32],
    c: i32,
    lm: i32,
    rc: &mut RangeDec,
    overhead_out: &mut i32,
    total_rsv_out: &mut i32,
) -> i32 {
    total = total.max(0);
    let mut skip_start = start;
    let skip_rsv = if total >= (1 << BITRES) {
        1 << BITRES
    } else {
        0
    };
    total -= skip_rsv;
    let mut intensity_rsv = 0;
    let mut dual_rsv = 0;
    if c == 2 {
        intensity_rsv = LOG2_FRAC_TABLE[end - start] as i32;
        if intensity_rsv > total {
            intensity_rsv = 0;
        } else {
            total -= intensity_rsv;
            dual_rsv = if total >= (1 << BITRES) {
                1 << BITRES
            } else {
                0
            };
            total -= dual_rsv;
        }
    }
    let mut thresh = [0i32; NB];
    let mut trim_offset = [0i32; NB];
    for j in start..end {
        let w = EBANDS[j + 1] - EBANDS[j];
        thresh[j] = (c << BITRES).max(((3 * w) << (lm + BITRES)) >> 4);
        trim_offset[j] =
            (c * w * (trim - 5 - lm) * (end - j - 1) as i32 * (1 << (lm + BITRES))) >> 6;
        if w << lm == 1 {
            trim_offset[j] -= c << BITRES;
        }
    }
    // Binary search over the 11 prototype vectors.
    let mut lo = 1;
    let mut hi = 10;
    while lo <= hi {
        let mut done = false;
        let mut psum = 0;
        let mid = (lo + hi) >> 1;
        for j in (start..end).rev() {
            let n = EBANDS[j + 1] - EBANDS[j];
            let raw = ALLOC_VECTORS[mid as usize * NB + j] as i32;
            let mut bitsj = (c * n * raw) << lm >> 2;
            if bitsj > 0 {
                bitsj = (bitsj + trim_offset[j]).max(0);
            }
            bitsj += offsets[j];
            if bitsj >= thresh[j] || done {
                done = true;
                psum += bitsj.min(cap[j]);
            } else if bitsj >= (c << BITRES) {
                psum += c << BITRES;
            }
        }
        if psum > total {
            hi = mid - 1;
        } else {
            lo = mid + 1;
        }
    }
    let (lo_f, hi_f) = ((lo - 1) as usize, lo as usize);
    let mut bits1 = [0i32; NB];
    let mut bits2 = [0i32; NB];
    for j in start..end {
        let n = EBANDS[j + 1] - EBANDS[j];
        let mut b1 = (c * n * ALLOC_VECTORS[lo_f * NB + j] as i32) << lm >> 2;
        let mut b2 = if hi_f >= 11 {
            cap[j]
        } else {
            (c * n * ALLOC_VECTORS[hi_f * NB + j] as i32) << lm >> 2
        };
        if b1 > 0 {
            b1 = (b1 + trim_offset[j]).max(0);
        }
        if b2 > 0 {
            b2 = (b2 + trim_offset[j]).max(0);
        }
        if lo_f > 0 {
            b1 += offsets[j];
        }
        b2 += offsets[j];
        if offsets[j] > 0 {
            skip_start = j;
        }
        bits1[j] = b1;
        bits2[j] = (b2 - b1).max(0);
    }
    interp_bits2pulses(
        start,
        end,
        skip_start,
        &bits1,
        &bits2,
        &thresh,
        cap,
        total,
        balance,
        skip_rsv,
        intensity,
        intensity_rsv,
        dual,
        dual_rsv,
        pulses,
        ebits,
        fine_priority,
        c,
        lm,
        rc,
        overhead_out,
        total_rsv_out,
    )
}

/// Ideal-Abfluss des Bilanz-Reservoirs auf codierte Baender (1/8-Bit).
/// Regel aus `quant_all_bands`: Band i erhaelt `Rest / min(3, Restbaender)`
/// als Extra-Budget ueber `pulses[i]` hinaus; das letzte Band nimmt den Rest.
/// Summe = `balance` exakt (Trunkierung schluckt das letzte Band).
pub(crate) fn drain_shares(balance: i32, coded: usize, start: usize) -> [i32; NB] {
    let mut shares = [0i32; NB];
    let mut rest = balance.max(0);
    for (i, sh) in shares
        .iter_mut()
        .enumerate()
        .take(coded.min(NB))
        .skip(start)
    {
        let div = 3.min(coded - i) as i32;
        let take = rest / div;
        *sh = take;
        rest -= take;
    }
    shares
}

/// Ergebnis der Mid-Section: Caps, Dynalloc-Boosts, Trim, Collapse-Reserve.
pub(crate) struct MidSection {
    pub(crate) offsets: [i32; NB],
    pub(crate) cap: [i32; NB],
    pub(crate) trim: i32,
    pub(crate) anti_collapse_rsv: i32,
}

/// Caps + Dynalloc-Schleife + Trim-Read + Anti-Collapse-Reserve lesen.
pub(crate) fn dynalloc_section(
    rc: &mut RangeDec,
    total_bits: i32,
    lm: usize,
    channels: usize,
    start: usize,
    end: usize,
    transient: bool,
) -> MidSection {
    let mut cap = [0i32; NB];
    for (i, cap_i) in cap.iter_mut().enumerate() {
        let n = (EBANDS[i + 1] - EBANDS[i]) << lm;
        *cap_i =
            ((i32::from(CAPS[NB * (2 * lm + channels - 1) + i]) + 64) * channels as i32 * n) >> 2;
    }
    let mut offsets = [0i32; NB];
    let mut dynalloc_logp = 6i32;
    let mut total_dyn = total_bits << BITRES;
    let mut tell_frac = rc.tell_frac();
    for i in start..end {
        let width = channels as i32 * (EBANDS[i + 1] - EBANDS[i]) * (1 << lm);
        let quanta = (width << BITRES).min((6 << BITRES).max(width));
        let mut logp = dynalloc_logp;
        let mut boost = 0;
        while tell_frac + (logp << BITRES) < total_dyn && boost < cap[i] {
            if !rc.decode_bit_logp(logp as u32) {
                break;
            }
            tell_frac = rc.tell_frac();
            boost += quanta;
            total_dyn -= quanta;
            logp = 1;
        }
        offsets[i] = boost;
        if boost > 0 {
            dynalloc_logp = dynalloc_logp.max(2) - 1;
            dynalloc_logp = dynalloc_logp.max(2);
        }
    }
    let trim = if rc.tell_frac() + (6 << BITRES) <= total_dyn {
        rc.decode_icdf(&TRIM_ICDF, 7)
    } else {
        5
    };
    let anti_collapse_rsv = if transient && lm >= 2 {
        let remaining = (total_bits << BITRES) - rc.tell_frac() - 1;
        if remaining >= ((lm as i32 + 2) << BITRES) {
            1 << BITRES
        } else {
            0
        }
    } else {
        0
    };
    MidSection {
        offsets,
        cap,
        trim,
        anti_collapse_rsv,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tables_have_expected_sizes() {
        assert_eq!(ALLOC_VECTORS.len(), 11 * NB);
        assert_eq!(CAPS.len(), 8 * NB);
        assert_eq!(EBANDS.len(), NB + 1);
    }

    #[test]
    fn caps_scale_with_frame_size() {
        // Formel: ((CAPS + 64) * C * (Breite << lm)) >> 2.
        // Der `<< lm` fehlte einmal (Caps 8x zu klein bei lm=3, alles
        // kaskadierte in die Bilanz); diese Werte pinnen ihn fest.
        let mut rc = RangeDec::new(&[]);
        let mid = dynalloc_section(&mut rc, 0, 3, 2, 0, 21, false);
        assert_eq!(mid.cap[0], 1072); // ((204 + 64) * 2 * 8) >> 2
        assert_eq!(mid.cap[20], 9152); // ((40 + 64) * 2 * 176) >> 2
        let mut rc = RangeDec::new(&[]);
        let mid = dynalloc_section(&mut rc, 0, 0, 2, 0, 21, false);
        assert_eq!(mid.cap[0], 144); // ((224 + 64) * 2 * 1) >> 2, CAPS-Zeile 1
    }

    #[test]
    fn drain_splits_balance_front_loaded() {
        let s = drain_shares(900, 5, 0);
        assert_eq!(s[..5], [300, 200, 133, 133, 134]);
        assert_eq!(s.iter().sum::<i32>(), 900);
        // Leere/faulige Faelle: nichts, kein Panik.
        assert_eq!(drain_shares(0, 21, 0).iter().sum::<i32>(), 0);
        assert_eq!(drain_shares(-50, 21, 0).iter().sum::<i32>(), 0);
        assert_eq!(drain_shares(100, 0, 0).iter().sum::<i32>(), 0);
    }

    /// Allocation conservation over synthetic budgets: bands + rest +
    /// header overhead must equal the budget exactly (LCG fuzz, no deps).
    #[test]
    fn allocation_conserves_bits() {
        let mut rng = 0x1234_5678u32;
        let mut next = move || {
            rng = rng.wrapping_mul(1664525).wrapping_add(1013904223);
            rng
        };
        for iter in 0..2000 {
            let ch = if next() % 2 == 0 { 1 } else { 2 };
            let lm = (next() % 4) as usize;
            let (start, end) = if next() % 3 == 0 { (17, 21) } else { (0, 21) };
            let total = (next() % 30000) as i32;
            let trim = (next() % 11) as i32;
            let mut cap = [0i32; NB];
            for (i, c) in cap.iter_mut().enumerate() {
                let n = (EBANDS[i + 1] - EBANDS[i]) << lm;
                *c = ((i32::from(CAPS[NB * (2 * lm + ch - 1) + i]) + 64) * ch as i32 * n) >> 2;
            }
            let mut offsets = [0i32; NB];
            for o in offsets.iter_mut().take(end).skip(start) {
                *o = if next() % 4 == 0 {
                    (next() % 200) as i32
                } else {
                    0
                };
            }
            let mut bytes = [0u8; 64];
            for b in bytes.iter_mut() {
                *b = (next() >> (iter % 25)) as u8;
            }
            let mut rc = RangeDec::new(&bytes);
            let mut intensity = 0;
            let mut dual = 0;
            let mut pulses = [0i32; NB];
            let mut ebits = [0i32; NB];
            let mut fine_priority = [0i32; NB];
            let mut balance = 0;
            let mut overhead = 0;
            let mut total_rsv = 0;
            compute_allocation(
                start,
                end,
                &offsets,
                &cap,
                trim,
                &mut intensity,
                &mut dual,
                total,
                &mut balance,
                &mut pulses,
                &mut ebits,
                &mut fine_priority,
                ch as i32,
                lm as i32,
                &mut rc,
                &mut overhead,
                &mut total_rsv,
            );
            let mut sum = balance + overhead;
            for b in start..end {
                sum += pulses[b] + ch as i32 * ebits[b] * 8;
            }
            assert_eq!(
                sum,
                total.max(0),
                "iter {iter}: ch {ch} lm {lm} total {total} trim {trim}"
            );
        }
    }
}
