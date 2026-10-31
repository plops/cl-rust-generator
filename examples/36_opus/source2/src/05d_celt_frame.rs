//! 05d — CELT-Frame-Walk: jeder Bit eines CELT-Frames, ohne Synthese.
//!
//! Port der Lesereihenfolge von `opus-pure` `decode_impl_from_rc`:
//! Silence → Prefilter → Transient → Intra → Coarse-Energie → TF → Spread
//! → Dynalloc/Trim → Allokation → Fine-Energie → **Banddaten (PVQ,
//! [`crate::bands`])** → Anti-Collapse → Energie-Finalise. Anders als in
//! `source1` endet der Walk damit exakt dort, wo der Decoder endet: Der
//! Range-Coder-Zustand `rng` ist danach gleich `OpusDecoder::final_range`
//! (Orakel in `tests/oracle.rs`).
//!
//! Buchhaltung: Jede Lese-Gruppe wird per `tell_frac`-Delta in 1/8 Bit
//! gezaehlt ([`CeltBits`]); je Band zusaetzlich Coarse, Fine, PVQ und
//! Finalise ([`CeltFrame::band_bits8`]). Die Summe aller Posten ist der
//! Gesamtverbrauch des Frames — keine Schaetzung, keine Drain-Regel.
//!
//! Bewusst weggelassen: Loss-Concealment (Dateien haben keine Verluste)
//! und alles Synthese-Seitige (MDCT, Folding, Postfilter-Werte).

use crate::alloc::{NB, compute_allocation, dynalloc_section};
use crate::bands::{BandInput, walk_bands};
use crate::range::{BITRES, RangeDec};
use crate::types::Bandwidth;

use crate::celt_tab::*;

// --- Energy decode ---

#[allow(clippy::too_many_arguments, clippy::needless_range_loop)]
fn unquant_coarse(
    start: usize,
    end: usize,
    e: &mut [f32],
    intra: bool,
    rc: &mut RangeDec,
    channels: usize,
    lm: usize,
    bits8: &mut [i32; NB],
) {
    let model = &E_PROB_MODEL[lm][usize::from(intra)];
    let coef = if intra { 0.0 } else { PRED_COEF[lm] };
    let beta = if intra { BETA_INTRA } else { BETA_COEF[lm] };
    let mut prev = [0.0f32; 2];
    let budget = (rc.storage * 8) as i32;
    for i in start..end {
        let t0 = rc.tell_frac();
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
        bits8[i] += rc.tell_frac() - t0;
    }
}

fn unquant_fine(
    start: usize,
    end: usize,
    e: &mut [f32],
    ebits: &[i32],
    rc: &mut RangeDec,
    channels: usize,
    bits8: &mut [i32; NB],
) {
    for i in start..end {
        for c in 0..channels {
            let bits = ebits[i];
            if bits <= 0 {
                continue;
            }
            let q = rc.dec_bits(bits as u32);
            e[c * NB + i] += (q as f32 + 0.5) / (1 << bits) as f32 - 0.5;
            bits8[i] += bits << BITRES;
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

// --- Finalise ---

/// `unquant_energy_finalise`: Rest-Bits als 1-Bit-Feinkorrektur (Prio 0, 1).
#[allow(clippy::too_many_arguments)]
fn unquant_finalise(
    start: usize,
    end: usize,
    e: &mut [f32],
    ebits: &[i32; NB],
    prio: &[i32; NB],
    mut bits_left: i32,
    rc: &mut RangeDec,
    channels: usize,
    bits8: &mut [i32; NB],
) {
    for p in 0..2 {
        let mut i = start;
        while i < end && bits_left >= channels as i32 {
            if ebits[i] >= 8 || prio[i] != p {
                i += 1;
                continue;
            }
            for c in 0..channels {
                let q2 = rc.dec_bits(1) as f32;
                e[c * NB + i] += (q2 - 0.5) * (1i32 << (14 - ebits[i] - 1)) as f32 / 16384.0;
                bits_left -= 1;
            }
            bits8[i] += (channels as i32) << BITRES;
            i += 1;
        }
    }
}

// --- Public API ---

/// Log-Energie-Gedaechtnis ueber Frames (2 Kanaele × 21 Baender).
/// Reset-Regeln bestimmt der Opus-Walk ([`crate::walk`]), wie im Decoder.
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

/// Bitverbrauch eines CELT-Frames nach Lese-Gruppe (1/8 Bit, exakt).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CeltBits {
    /// Silence + Prefilter + Transient + Intra.
    pub flags: i32,
    /// Coarse-Energie (Laplace) aller Baender.
    pub coarse: i32,
    /// TF-Aufloesung + Spread.
    pub tf_spread: i32,
    /// Dynalloc + Trim + Allokations-Reads (Skip/Intensitaet/Dual).
    pub alloc: i32,
    /// Fine-Energie (Roh-Bits).
    pub fine: i32,
    /// Banddaten: Theta, PVQ-Indizes, Vorzeichen.
    pub pvq: i32,
    /// Anti-Collapse-Bit + Finalise-Bits.
    pub finalise: i32,
}

impl CeltBits {
    /// Summe aller Posten in 1/8 Bit.
    #[must_use]
    pub fn total(&self) -> i32 {
        self.flags
            + self.coarse
            + self.tf_spread
            + self.alloc
            + self.fine
            + self.pvq
            + self.finalise
    }
}

/// Alles, was der Walk ueber einen CELT-Frame weiss.
#[derive(Debug, Clone)]
pub struct CeltFrame {
    /// Log2-Energien je Kanal und Band nach Finalise (ohne Band-Mittel).
    pub log_e: [[f32; NB]; 2],
    /// Allokation je Band in 1/8 Bit (Budget fuer PVQ, `pulses[]`).
    pub alloc8: [i32; NB],
    /// Fine-Energie-Bits je Band und Kanal.
    pub ebits: [i32; NB],
    /// Tatsaechlich gelesene 1/8-Bits je Band (Coarse+Fine+PVQ+Finalise).
    pub band_bits8: [i32; NB],
    /// PVQ-Pulse je Band (0 = Band ohne Feinstruktur → Rauschfuellung).
    pub pulses: [u32; NB],
    /// TF-Aenderung je Band (negativ = feinere Zeit-, positiv = Frequenzaufloesung).
    pub tf_res: [i32; NB],
    /// Verbrauch nach Lese-Gruppe.
    pub bits: CeltBits,
    /// Prefilter (Pitch-Kammfilter): Periode in Samples, Gain 0..1.
    pub prefilter: Option<(u32, f32)>,
    pub transient: bool,
    pub intra: bool,
    pub silence: bool,
    pub anti_collapse: bool,
    pub trim: i32,
    pub spread: i32,
    pub intensity: i32,
    pub dual: i32,
    pub coded_bands: i32,
    /// Allokations-Eingang in 1/8 Bit.
    pub alloc_bits: i32,
    /// Allokations-Rest (Bilanz) in 1/8 Bit.
    pub balance: i32,
    /// Header-Reserven (Skip/Intensitaet/Dual) in 1/8 Bit.
    pub overhead: i32,
    /// Frame-Budget in ganzen Bits.
    pub total_bits: i32,
    /// Bandbereich `[start, end)`.
    pub start: usize,
    pub end: usize,
    pub channels: usize,
    pub lm: usize,
}

/// Log2-Energie (+ Band-Mittel) in dB.
pub fn energy_db(log_e: f32, band: usize) -> f32 {
    6.0206 * (log_e + E_MEANS[band.min(20)])
}

/// Frame-Groessen-Exponent aus der Dauer (2,5 ms → 0 … 20 ms → 3).
pub fn lm_of_ms(frame_ms: f64) -> usize {
    match frame_ms {
        x if x < 3.0 => 0,
        x if x < 7.5 => 1,
        x if x < 15.0 => 2,
        _ => 3,
    }
}

/// Letztes CELT-Band aus der Paket-Bandbreite (RFC 6716 §3.1).
pub fn endband_of(bandwidth: Bandwidth) -> usize {
    match bandwidth {
        Bandwidth::Narrow => 13,
        Bandwidth::Medium | Bandwidth::Wide => 17,
        Bandwidth::SuperWide => 19,
        Bandwidth::Full => 21,
    }
}

/// CELT-Frame auf eigenem Coder walken (CELT-only, Redundanz-Frames).
/// Rueckgabe zusaetzlich der End-`rng` (Final-Range-Orakel).
pub fn walk_celt_frame(
    data: &[u8],
    lm: usize,
    channels: usize,
    end: usize,
    state: &mut CeltState,
) -> Result<(CeltFrame, u32), &'static str> {
    let mut rc = RangeDec::new(data);
    let f = walk_celt_frame_rc(&mut rc, data.len() as i32 * 8, lm, channels, 0, end, state)?;
    Ok((f, rc.rng))
}

/// CELT-Frame auf bereits positioniertem Coder walken (Hybrid: nach SILK).
/// `total_bits` ist das effektive CELT-Budget (nach Redundanz-Abzug).
pub fn walk_celt_frame_rc(
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
    // Mono-Paket im Stereo-Zustand: Vorhersage aus dem lauteren Kanal.
    if channels == 1 {
        for i in 0..NB {
            state.e[i] = state.e[i].max(state.e[NB + i]);
        }
    }
    let mut bits = CeltBits::default();
    let mut band_bits8 = [0i32; NB];
    let t0 = rc.tell_frac();

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
    let mut prefilter = None;
    if start == 0 && !silence && rc.tell() + 16 <= total_bits && rc.decode_bit_logp(1) {
        let octave = rc.dec_uint(6);
        let period = (16 << octave) + rc.dec_bits(4 + octave) - 1;
        let qg = rc.dec_bits(3);
        if rc.tell() + 2 <= total_bits {
            let _tapset = rc.decode_icdf(&TAPSET_ICDF, 2);
        }
        prefilter = Some((period, 0.09375 * (qg + 1) as f32));
    }
    let transient = lm > 0 && rc.tell() + 3 <= total_bits && rc.decode_bit_logp(3);
    let intra = rc.tell() + 3 <= total_bits && rc.decode_bit_logp(3);
    bits.flags = rc.tell_frac() - t0;

    let t = rc.tell_frac();
    unquant_coarse(
        start,
        end,
        &mut state.e,
        intra,
        &mut *rc,
        channels,
        lm,
        &mut band_bits8,
    );
    bits.coarse = rc.tell_frac() - t;

    let t = rc.tell_frac();
    let mut tf_res = [0i32; NB];
    tf_decode(start, end, transient, &mut tf_res, lm as i32, &mut *rc);
    let spread = if rc.tell() + 4 <= total_bits {
        rc.decode_icdf(&SPREAD_ICDF, 5)
    } else {
        SPREAD_NORMAL
    };
    bits.tf_spread = rc.tell_frac() - t;

    let t = rc.tell_frac();
    let mid = dynalloc_section(&mut *rc, total_bits, lm, channels, start, end, transient);
    let (offsets, cap, trim, anti_collapse_rsv) =
        (mid.offsets, mid.cap, mid.trim, mid.anti_collapse_rsv);
    let mut intensity = 0;
    let mut dual = i32::from(channels == 2);
    let mut alloc8 = [0i32; NB];
    let mut ebits = [0i32; NB];
    let mut fine_priority = [0i32; NB];
    let mut balance = 0;
    let alloc_bits = (total_bits << BITRES) - rc.tell_frac() - 1 - anti_collapse_rsv;
    let (mut overhead, mut total_rsv) = (0, 0);
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
        &mut alloc8,
        &mut ebits,
        &mut fine_priority,
        channels as i32,
        lm as i32,
        &mut *rc,
        &mut overhead,
        &mut total_rsv,
    );
    if coded < 0 {
        return Err("allocation failed");
    }
    bits.alloc = rc.tell_frac() - t;

    let t = rc.tell_frac();
    unquant_fine(
        start,
        end,
        &mut state.e,
        &ebits,
        &mut *rc,
        channels,
        &mut band_bits8,
    );
    bits.fine = rc.tell_frac() - t;

    let t = rc.tell_frac();
    let walk = walk_bands(
        &mut *rc,
        &BandInput {
            start,
            end,
            lm: lm as i32,
            channels,
            short_blocks: transient,
            alloc: &alloc8,
            tf_res: &tf_res,
            intensity: intensity.max(0) as usize,
            dual: dual != 0,
            total8: (total_bits << BITRES) - anti_collapse_rsv,
            balance,
            coded_bands: coded,
        },
    );
    bits.pvq = rc.tell_frac() - t;
    for (b, w) in band_bits8.iter_mut().zip(walk.bits8.iter()) {
        *b += *w;
    }

    let t = rc.tell_frac();
    let anti_collapse = anti_collapse_rsv > 0 && rc.dec_bits(1) != 0;
    let bits_left = total_bits - rc.tell();
    unquant_finalise(
        start,
        end,
        &mut state.e,
        &ebits,
        &fine_priority,
        bits_left,
        &mut *rc,
        channels,
        &mut band_bits8,
    );
    bits.finalise = rc.tell_frac() - t;

    // Frame-Ende-Buchhaltung wie `decode_impl_from_rc`.
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
        for i in (0..start).chain(end..NB) {
            state.e[c * NB + i] = 0.0;
        }
    }

    Ok(CeltFrame {
        log_e,
        alloc8,
        ebits,
        band_bits8,
        pulses: walk.pulses,
        tf_res,
        bits,
        prefilter,
        transient,
        intra,
        silence,
        anti_collapse,
        trim,
        spread,
        intensity,
        dual,
        coded_bands: coded,
        alloc_bits,
        balance,
        overhead,
        total_bits,
        start,
        end,
        channels,
        lm,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_frame_is_silence_and_consumes_budget() {
        let mut st = CeltState::new();
        let (f, _) = walk_celt_frame(&[], 3, 1, 21, &mut st).unwrap();
        assert!(f.silence);
        assert_eq!(f.pulses, [0; NB]);
    }

    #[test]
    fn bit_groups_sum_to_tell() {
        // Beliebige Bytes: Der Walk liest, was er liest — die Posten
        // muessen trotzdem exakt den Gesamtverbrauch ergeben.
        let mut seed = 0x9e37_79b9u32;
        for len in [8usize, 40, 120, 400] {
            let data: Vec<u8> = (0..len)
                .map(|_| {
                    seed = seed.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
                    (seed >> 24) as u8
                })
                .collect();
            let mut st = CeltState::new();
            let mut rc = RangeDec::new(&data);
            let t0 = rc.tell_frac();
            let f = walk_celt_frame_rc(&mut rc, len as i32 * 8, 3, 2, 0, 21, &mut st).unwrap();
            assert_eq!(f.bits.total(), rc.tell_frac() - t0, "len {len}");
            assert!(rc.tell() <= len as i32 * 8 + 1, "ueberlesen: {}", rc.tell());
        }
    }
}
