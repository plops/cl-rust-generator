//! 03f — SILK-Excitation-Walk: Pulse lesen, Werte verwerfen.
//!
//! Port aus `opus-pure` (`silk/decode_pulses.rs`, Shell-Baum aus
//! `silk/shell_coder.rs`-Logik): Raten-Stufe, Puls-Summen, Shell-Baum,
//! LSBs, Vorzeichen. Nur die Lesereihenfolge zaehlt — Werte fallen weg
//! (Spiegel des PVQ-Skips in `03c_celt.rs`).

use crate::range::RangeDec;
use crate::silk_tab::*;

// --- Excitation-Walk (Port `silk_decode_pulses`, Werte fallen weg) ---

/// Shell-Split: ein Puls-Total in zwei Haelften (liest genau wie Referenz).
pub(crate) fn shell_split(rc: &mut RangeDec<'_>, p: i32, table: &[u8]) -> (i16, i16) {
    if p > 0 {
        let idx = SILK_SHELL_CODE_TABLE_OFFSETS[p as usize] as usize;
        let c1 = rc.decode_icdf(&table[idx..], 8) as i16;
        (c1, (p - c1 as i32) as i16)
    } else {
        (0, 0)
    }
}

/// Shell-Baum ueber 16 Pulse (Lesereihenfolge = Referenz).
pub(crate) fn shell_block(rc: &mut RangeDec<'_>, pulses: &mut [i16], total: i32) {
    let (p3_0, p3_1) = shell_split(rc, total, &SILK_SHELL_CODE_TABLE3);
    let (p2_0, p2_1) = shell_split(rc, p3_0 as i32, &SILK_SHELL_CODE_TABLE2);
    let (p1_0, p1_1) = shell_split(rc, p2_0 as i32, &SILK_SHELL_CODE_TABLE1);
    let (p0_0, p0_1) = shell_split(rc, p1_0 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[0] = p0_0;
    pulses[1] = p0_1;
    let (p0_2, p0_3) = shell_split(rc, p1_1 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[2] = p0_2;
    pulses[3] = p0_3;
    let (p1_2, p1_3) = shell_split(rc, p2_1 as i32, &SILK_SHELL_CODE_TABLE1);
    let (p0_4, p0_5) = shell_split(rc, p1_2 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[4] = p0_4;
    pulses[5] = p0_5;
    let (p0_6, p0_7) = shell_split(rc, p1_3 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[6] = p0_6;
    pulses[7] = p0_7;
    let (p2_2, p2_3) = shell_split(rc, p3_1 as i32, &SILK_SHELL_CODE_TABLE2);
    let (p1_4, p1_5) = shell_split(rc, p2_2 as i32, &SILK_SHELL_CODE_TABLE1);
    let (p0_8, p0_9) = shell_split(rc, p1_4 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[8] = p0_8;
    pulses[9] = p0_9;
    let (p0_10, p0_11) = shell_split(rc, p1_5 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[10] = p0_10;
    pulses[11] = p0_11;
    let (p1_6, p1_7) = shell_split(rc, p2_3 as i32, &SILK_SHELL_CODE_TABLE1);
    let (p0_12, p0_13) = shell_split(rc, p1_6 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[12] = p0_12;
    pulses[13] = p0_13;
    let (p0_14, p0_15) = shell_split(rc, p1_7 as i32, &SILK_SHELL_CODE_TABLE0);
    pulses[14] = p0_14;
    pulses[15] = p0_15;
}

/// Puls-Vorzeichen je Block (Referenz: `silk_decode_signs`).
pub(crate) fn signs_block(
    rc: &mut RangeDec<'_>,
    pulses: &mut [i16],
    signal: i32,
    quant: i32,
    sum: i32,
) {
    if sum <= 0 {
        return;
    }
    let idx = ((sum & 0x1F).min(6)) as usize;
    let icdf = [
        SILK_SIGN_ICDF[7 * (quant + (signal << 1)) as usize + idx],
        0,
    ];
    for p in pulses.iter_mut().take(SHELL_CODEC_FRAME_LENGTH) {
        if *p > 0 && rc.decode_icdf(&icdf, 8) == 0 {
            *p = -*p;
        }
    }
}

/// Excitation lesen, Pulse verwerfen; Rueckgabe = gelesene Bits.
pub(crate) fn walk_pulses(
    rc: &mut RangeDec<'_>,
    signal: i32,
    quant: i32,
    frame_length: usize,
) -> i32 {
    let t0 = rc.tell();
    let rate = rc.decode_icdf(&SILK_RATE_LEVELS_ICDF[(signal >> 1) as usize], 8) as usize;
    let mut iter = frame_length.div_ceil(SHELL_CODEC_FRAME_LENGTH);
    let mut sums = [0i32; 20];
    let mut shifts = [0i32; 20];
    for i in 0..iter {
        let mut s = rc.decode_icdf(&SILK_PULSES_PER_BLOCK_ICDF[rate], 8);
        while s == SILK_MAX_PULSES as i32 + 1 {
            shifts[i] += 1;
            s = if shifts[i] == 10 {
                rc.decode_icdf(&SILK_PULSES_PER_BLOCK_ICDF[N_RATE_LEVELS - 1][1..], 8)
            } else {
                rc.decode_icdf(&SILK_PULSES_PER_BLOCK_ICDF[N_RATE_LEVELS - 1], 8)
            };
        }
        sums[i] = s;
    }
    let mut pulses = [0i16; 320];
    iter = iter.min(20);
    for i in 0..iter {
        let start = i * SHELL_CODEC_FRAME_LENGTH;
        if sums[i] > 0 {
            shell_block(
                rc,
                &mut pulses[start..start + SHELL_CODEC_FRAME_LENGTH],
                sums[i],
            );
        }
        if shifts[i] > 0 {
            for p in pulses
                .iter_mut()
                .take(start + SHELL_CODEC_FRAME_LENGTH)
                .skip(start)
            {
                let mut q = *p as i32;
                for _ in 0..shifts[i] {
                    q = (q << 1) + rc.decode_icdf(&SILK_LSB_ICDF, 8);
                }
                *p = q as i16;
            }
            sums[i] |= shifts[i] << 5;
        }
        signs_block(
            rc,
            &mut pulses[start..start + SHELL_CODEC_FRAME_LENGTH],
            signal,
            quant,
            sums[i],
        );
    }
    rc.tell() - t0
}
