//! 05c — CELT-Band-Walk: `quant_all_bands` (Decode-Seite) ohne Synthese.
//!
//! Port der Bit-Fluss-Logik aus `opus-pure` `celt/bands.rs`
//! (`quant_all_bands` → `quant_band(_stereo)` → `quant_partition` →
//! `compute_theta` / `alg_unquant`). Gelesen wird alles, was der Decoder
//! liest — Theta-Winkel, Stereo-Vorzeichen, PVQ-Indizes —, aber kein
//! Spektrum rekonstruiert: Der Bitverbrauch haengt nur von Allokation,
//! Theta und Pulszahl ab, nie von Signalwerten (Lowband-Folding,
//! Collapse-Masken und Rausch-Seeds sind reine Synthese).
//!
//! Ergebnis je Band: verbrauchte 1/8-Bits (exakt per `tell_frac`-Delta)
//! und die Summe der PVQ-Pulse K. Damit endet der Walk am selben
//! Range-Coder-Zustand wie der Decoder — das `final_range`-Orakel prueft
//! genau das.

use crate::alloc::{EBANDS, LOG_N, NB};
use crate::range::{BITRES, RangeDec};

/// Pulse-Cache-Index je `(lm+1, band)` (`CACHE_INDEX50`).
const CACHE_INDEX: [i16; 105] = [
    -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 41, 41, 41, 82, 82, 123, 164, 200, 222, 0, 0, 0, 0,
    0, 0, 0, 0, 41, 41, 41, 41, 123, 123, 123, 164, 164, 240, 266, 283, 295, 41, 41, 41, 41, 41,
    41, 41, 41, 123, 123, 123, 123, 240, 240, 240, 266, 266, 305, 318, 328, 336, 123, 123, 123,
    123, 123, 123, 123, 123, 240, 240, 240, 240, 305, 305, 305, 318, 318, 343, 351, 358, 364, 240,
    240, 240, 240, 240, 240, 240, 240, 305, 305, 305, 305, 343, 343, 343, 351, 351, 370, 376, 382,
    387,
];

/// Pulse-Cache: Bits je Pseudo-Pulszahl (`CACHE_BITS50`).
const CACHE_BITS: [u8; 392] = [
    40, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 40, 15, 23, 28, 31, 34, 36, 38, 39, 41, 42, 43, 44, 45, 46, 47,
    47, 49, 50, 51, 52, 53, 54, 55, 55, 57, 58, 59, 60, 61, 62, 63, 63, 65, 66, 67, 68, 69, 70, 71,
    71, 40, 20, 33, 41, 48, 53, 57, 61, 64, 66, 69, 71, 73, 75, 76, 78, 80, 82, 85, 87, 89, 91, 92,
    94, 96, 98, 101, 103, 105, 107, 108, 110, 112, 114, 117, 119, 121, 123, 124, 126, 128, 40, 23,
    39, 51, 60, 67, 73, 79, 83, 87, 91, 94, 97, 100, 102, 105, 107, 111, 115, 118, 121, 124, 126,
    129, 131, 135, 139, 142, 145, 148, 150, 153, 155, 159, 163, 166, 169, 172, 174, 177, 179, 35,
    28, 49, 65, 78, 89, 99, 107, 114, 120, 126, 132, 136, 141, 145, 149, 153, 159, 165, 171, 176,
    180, 185, 189, 192, 199, 205, 211, 216, 220, 225, 229, 232, 239, 245, 251, 21, 33, 58, 79, 97,
    112, 125, 137, 148, 157, 166, 174, 182, 189, 195, 201, 207, 217, 227, 235, 243, 251, 17, 35,
    63, 86, 106, 123, 139, 152, 165, 177, 187, 197, 206, 214, 222, 230, 237, 250, 25, 31, 55, 75,
    91, 105, 117, 128, 138, 146, 154, 161, 168, 174, 180, 185, 190, 200, 208, 215, 222, 229, 235,
    240, 245, 255, 16, 36, 65, 89, 110, 128, 144, 159, 173, 185, 196, 207, 217, 226, 234, 242, 250,
    11, 41, 74, 103, 128, 151, 172, 191, 209, 225, 241, 255, 9, 43, 79, 110, 138, 163, 186, 207,
    227, 246, 12, 39, 71, 99, 123, 144, 164, 182, 198, 214, 228, 241, 253, 9, 44, 81, 113, 142,
    168, 192, 214, 235, 255, 7, 49, 90, 127, 160, 191, 220, 247, 6, 51, 95, 134, 170, 203, 234, 7,
    47, 87, 123, 155, 184, 212, 237, 6, 52, 97, 137, 174, 208, 240, 5, 57, 106, 151, 192, 231, 5,
    59, 111, 158, 202, 243, 5, 55, 103, 147, 187, 224, 5, 60, 113, 161, 206, 248, 4, 65, 122, 175,
    224, 4, 67, 127, 182, 234,
];

/// Groesste PVQ-Dimension / Pulszahl, die der Walk tabelliert.
const MAX_N: usize = 352;
const MAX_K: usize = 130;

/// `V(N,K)`: Zahl der PVQ-Codewoerter (N Dimensionen, K Pulse).
///
/// Rekursion `V(N,K) = V(N-1,K) + V(N,K-1) + V(N-1,K-1)` in `u32` mit
/// Wrap: modulo 2^32 linear, also fuer alle gueltigen (< 2^32) Paare
/// exakt — dieselben Werte wie libopus' `CELT_PVQ_U`-Tabelle.
pub fn pvq_v(n: usize, k: usize) -> u32 {
    use std::sync::OnceLock;
    static TABLE: OnceLock<Vec<u32>> = OnceLock::new();
    let t = TABLE.get_or_init(|| {
        let mut v = vec![0u32; (MAX_N + 1) * MAX_K];
        for nn in 0..=MAX_N {
            v[nn * MAX_K] = 1;
        }
        for nn in 1..=MAX_N {
            for kk in 1..MAX_K {
                let a = v[(nn - 1) * MAX_K + kk];
                let b = v[nn * MAX_K + kk - 1];
                let c = v[(nn - 1) * MAX_K + kk - 1];
                v[nn * MAX_K + kk] = a.wrapping_add(b).wrapping_add(c);
            }
        }
        v
    });
    t[n.min(MAX_N) * MAX_K + k.min(MAX_K - 1)]
}

/// Pseudo-Pulszahl → echte Pulszahl K (`get_pulses`).
fn get_pulses(q: i32) -> i32 {
    if q < 8 {
        q
    } else {
        (8 + (q & 7)) << ((q >> 3) - 1)
    }
}

/// Cache-Zeile eines Bands bei `lm` (`None` = Band ohne PVQ).
fn cache_row(band: usize, lm: i32) -> Option<&'static [u8]> {
    let idx = CACHE_INDEX[(lm + 1) as usize * NB + band];
    (idx >= 0).then(|| &CACHE_BITS[idx as usize..])
}

/// Bits (1/8) → Pseudo-Pulszahl (`bits2pulses`, Binaersuche).
fn bits2pulses(band: usize, lm: i32, bits: i32) -> i32 {
    let Some(cache) = cache_row(band, lm) else {
        return 0;
    };
    let (mut lo, mut hi) = (0i32, i32::from(cache[0]));
    let bits = bits - 1;
    for _ in 0..6 {
        let mid = (lo + hi + 1) >> 1;
        if i32::from(cache[mid as usize]) >= bits {
            hi = mid;
        } else {
            lo = mid;
        }
    }
    let lo_val = if lo == 0 {
        -1
    } else {
        i32::from(cache[lo as usize])
    };
    if bits - lo_val <= i32::from(cache[hi as usize]) - bits {
        lo
    } else {
        hi
    }
}

/// Pseudo-Pulszahl → Bits (1/8) (`pulses2bits`).
fn pulses2bits(band: usize, lm: i32, q: i32) -> i32 {
    match (q, cache_row(band, lm)) {
        (0, _) | (_, None) => 0,
        (q, Some(cache)) => i32::from(cache[q as usize]) + 1,
    }
}

/// Bit-exakter Kosinus (Q15, `bitexact_cos`).
fn bitexact_cos(x: i32) -> i32 {
    let frac_mul16 = |a: i32, b: i32| (16384 + (a as i16 as i32) * (b as i16 as i32)) >> 15;
    let x2 = ((4096 + x * x) >> 13) as i16 as i32;
    let x2 = (32767 - x2 + frac_mul16(x2, -7651 + frac_mul16(x2, 8277 + frac_mul16(-626, x2))))
        as i16 as i32;
    1 + x2
}

/// Bit-exakter log2(tan) (Q11, `bitexact_log2tan`).
fn bitexact_log2tan(isin: i32, icos: i32) -> i32 {
    let ilog = |x: i32| 32 - (x.max(0) as u32).leading_zeros() as i32;
    let (lc, ls) = (ilog(icos), ilog(isin));
    let icos = icos << (15 - lc);
    let isin = isin << (15 - ls);
    let fm = |a: i32, b: i32| (a * b + 16384) >> 15;
    (ls - lc) * (1 << 11) + fm(isin, fm(isin, -2597) + 7932) - fm(icos, fm(icos, -2597) + 7932)
}

/// Ganzzahlige Wurzel (`isqrt32`).
fn isqrt32(mut val: u32) -> u32 {
    let mut g = 0u32;
    let mut bshift = (32 - val.leading_zeros() as i32 - 1) >> 1;
    let mut b = 1u32 << bshift.max(0);
    while bshift >= 0 {
        let t = (u64::from((g << 1) + b)) << bshift;
        if t <= u64::from(val) {
            g += b;
            val -= t as u32;
        }
        b >>= 1;
        bshift -= 1;
    }
    g
}

/// Quantisierungsstufen fuer Theta (`compute_qn`).
fn compute_qn(n: usize, b: i32, offset: i32, pulse_cap: i32, stereo: bool) -> i32 {
    const EXP2: [i32; 8] = [16384, 17866, 19483, 21247, 23170, 25267, 27554, 30048];
    let mut n2 = 2 * n as i32 - 1;
    if stereo && n == 2 {
        n2 -= 1;
    }
    let qb = ((b + n2 * offset) / n2)
        .min(b - pulse_cap - (4 << BITRES))
        .min(8 << BITRES);
    if qb < (1 << BITRES >> 1) {
        return 1;
    }
    let shift = 14 - (qb >> BITRES);
    let raw = if (0..32).contains(&shift) {
        EXP2[(qb & 7) as usize] >> shift
    } else {
        0
    };
    (((raw + 1) >> 1) << 1).min(256)
}

/// Laufzeit-Kontext eines Bands (Decoder-`BandCtx` ohne Signalpuffer).
struct Ctx<'r, 'b> {
    rc: &'r mut RangeDec<'b>,
    band: usize,
    remaining: i32,
    tf_change: i32,
    intensity: usize,
    pulses: u32,
}

/// Theta-Ergebnis (`SplitCtx`): Winkel Q14, Bit-Delta, verbrauchte 1/8-Bits.
struct Theta {
    itheta: i32,
    delta: i32,
    qalloc: i32,
}

impl Ctx<'_, '_> {
    /// `compute_theta` (Decode): Winkel lesen, `b` um Theta-Kosten kuerzen.
    #[allow(clippy::too_many_arguments)]
    fn theta(&mut self, n: usize, b: &mut i32, b0: i32, lm: i32, stereo: bool) -> Theta {
        let pulse_cap = i32::from(LOG_N[self.band]) + (lm << BITRES);
        let offset = (pulse_cap >> 1) - if stereo && n == 2 { 16 } else { 4 };
        let mut qn = compute_qn(n, *b, offset, pulse_cap, stereo);
        if stereo && self.band >= self.intensity {
            qn = 1;
        }
        let tell0 = self.rc.tell_frac();
        let mut itheta = 0;
        if qn != 1 {
            if stereo && n > 2 {
                // Stufen-PDF: Mitte (x0) dreifach gewichtet.
                let (p0, x0) = (3, qn / 2);
                let ft = (p0 * (x0 + 1) + x0) as u32;
                let fs = self.rc.decode(ft) as i32;
                itheta = if fs < (x0 + 1) * p0 {
                    fs / p0
                } else {
                    x0 + 1 + (fs - (x0 + 1) * p0)
                };
                let (fl, fh) = if itheta <= x0 {
                    (p0 * itheta, p0 * (itheta + 1))
                } else {
                    (itheta - 1 - x0 + (x0 + 1) * p0, itheta - x0 + (x0 + 1) * p0)
                };
                self.rc.update(fl as u32, fh as u32, ft);
            } else if b0 > 1 || stereo {
                itheta = self.rc.dec_uint((qn + 1) as u32) as i32;
            } else {
                // Dreiecks-PDF ueber [0, qn].
                let ft = ((qn >> 1) + 1) * ((qn >> 1) + 1);
                let fm = self.rc.decode(ft as u32) as i32;
                let (fl, fs);
                if fm < (((qn >> 1) * ((qn >> 1) + 1)) >> 1) {
                    itheta = (isqrt32((8 * fm + 1) as u32) as i32 - 1) >> 1;
                    fl = (itheta * (itheta + 1)) >> 1;
                    fs = itheta + 1;
                } else {
                    itheta = (2 * (qn + 1) - isqrt32((8 * (ft - fm - 1) + 1) as u32) as i32) >> 1;
                    fs = qn + 1 - itheta;
                    fl = ft - (((qn + 1 - itheta) * (qn + 2 - itheta)) >> 1);
                }
                self.rc.update(fl as u32, (fl + fs) as u32, ft as u32);
            }
            itheta = (itheta as u32 * 16384 / qn as u32) as i32;
        } else if stereo && *b > (2 << BITRES) && self.remaining > (2 << BITRES) {
            // Intensitaets-Stereo: nur das Inversions-Bit.
            let _inv = self.rc.decode_bit_logp(2);
        }
        let qalloc = self.rc.tell_frac() - tell0;
        *b -= qalloc;
        let delta = match itheta {
            0 => -16384,
            16384 => 16384,
            t => {
                let (imid, iside) = (bitexact_cos(t), bitexact_cos(16384 - t));
                (((n as i32 - 1) << 7) * bitexact_log2tan(iside, imid) + 16384) >> 15
            }
        };
        Theta {
            itheta,
            delta,
            qalloc,
        }
    }

    /// `quant_partition` (Decode): rekursiv spalten oder PVQ lesen.
    fn partition(&mut self, n: usize, b: i32, blocks: i32, lm: i32) {
        let split = lm >= 0
            && n > 2
            && cache_row(self.band, lm).is_some_and(|c| b > i32::from(c[c[0] as usize]) + 12);
        if !split {
            let mut q = bits2pulses(self.band, lm, b);
            let mut curr = pulses2bits(self.band, lm, q);
            self.remaining -= curr;
            while self.remaining < 0 && q > 0 {
                self.remaining += curr;
                q -= 1;
                curr = pulses2bits(self.band, lm, q);
                self.remaining -= curr;
            }
            if q != 0 {
                let k = get_pulses(q) as usize;
                let ft = pvq_v(n, k);
                let _index = self.rc.dec_uint(ft);
                self.pulses += k as u32;
            }
            return;
        }
        let (mid, lm1, b0) = (n / 2, lm - 1, blocks);
        let blocks = (blocks + 1) >> 1;
        let mut b = b;
        let th = self.theta(mid, &mut b, b0, lm1, false);
        self.remaining -= th.qalloc;
        let mut delta = th.delta;
        if b0 > 1 && th.itheta & 0x3fff != 0 {
            if th.itheta > 8192 {
                delta -= delta >> (4 - lm1);
            } else {
                delta = 0.min(delta + (((mid as i32) << BITRES) >> (5 - lm1)));
            }
        }
        let mut mbits = ((b - delta) / 2).max(0).min(b);
        let mut sbits = b - mbits;
        let rebalance = self.remaining;
        if mbits >= sbits {
            self.partition(mid, mbits, blocks, lm1);
            let r = mbits - (rebalance - self.remaining);
            if r > (3 << 3) && th.itheta != 0 {
                sbits += r - (3 << 3);
            }
            self.partition(mid, sbits, blocks, lm1);
        } else {
            self.partition(mid, sbits, blocks, lm1);
            let r = sbits - (rebalance - self.remaining);
            if r > (3 << 3) && th.itheta != 16384 {
                mbits += r - (3 << 3);
            }
            self.partition(mid, mbits, blocks, lm1);
        }
    }

    /// N=1-Band: nur Vorzeichen-Bits (roh vom Paketende).
    fn band_n1(&mut self, stereo: bool) {
        for _ in 0..1 + usize::from(stereo) {
            if self.remaining >= 1 << BITRES {
                let _sign = self.rc.dec_bits(1);
                self.remaining -= 1 << BITRES;
            }
        }
    }

    /// `quant_band` (Decode): TF-Umordnung nur als Blockzahl, dann Partition.
    fn band(&mut self, n: usize, b: i32, blocks: i32, lm: i32) {
        if n == 1 {
            self.band_n1(false);
            return;
        }
        let mut tf = self.tf_change;
        let recombine = tf.max(0);
        let mut n_b = (n / blocks as usize) << recombine;
        let mut blocks = blocks >> recombine;
        while n_b & 1 == 0 && tf < 0 {
            blocks <<= 1;
            n_b >>= 1;
            tf += 1;
        }
        self.partition(n, b, blocks, lm);
    }

    /// `quant_band_stereo` (Decode): Mid/Side-Theta, dann zwei Baender.
    fn band_stereo(&mut self, n: usize, b: i32, blocks: i32, lm: i32) {
        if n == 1 {
            self.band_n1(true);
            return;
        }
        let mut b = b;
        let th = self.theta(n, &mut b, blocks, lm, true);
        if n == 2 {
            let sbits = if th.itheta != 0 && th.itheta != 16384 {
                1 << BITRES
            } else {
                0
            };
            self.remaining -= th.qalloc + sbits;
            if sbits != 0 {
                let _sign = self.rc.dec_bits(1);
            }
            self.band(n, b - sbits, blocks, lm);
            return;
        }
        self.remaining -= th.qalloc;
        let mut mbits = ((b - th.delta) / 2).max(0).min(b);
        let mut sbits = b - mbits;
        let rebalance = self.remaining;
        if mbits >= sbits {
            self.band(n, mbits, blocks, lm);
            let r = mbits - (rebalance - self.remaining);
            if r > (3 << 3) && th.itheta != 0 {
                sbits += r - (3 << 3);
            }
            self.band(n, sbits, blocks, lm);
        } else {
            self.band(n, sbits, blocks, lm);
            let r = sbits - (rebalance - self.remaining);
            if r > (3 << 3) && th.itheta != 16384 {
                mbits += r - (3 << 3);
            }
            self.band(n, mbits, blocks, lm);
        }
    }
}

/// Ergebnis des Band-Walks je Band.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BandWalk {
    /// Verbrauchte 1/8-Bits je Band (Theta + PVQ + Vorzeichen, exakt).
    pub bits8: [i32; NB],
    /// PVQ-Pulse je Band (Summe ueber Kanaele und Partitionen).
    pub pulses: [u32; NB],
}

/// Eingaben des Band-Walks (alles aus Header + Allokation).
pub struct BandInput<'a> {
    pub start: usize,
    pub end: usize,
    pub lm: i32,
    pub channels: usize,
    pub short_blocks: bool,
    /// Allokation je Band in 1/8 Bit (`pulses[]` der Allokation).
    pub alloc: &'a [i32; NB],
    pub tf_res: &'a [i32; NB],
    pub intensity: usize,
    pub dual: bool,
    /// Budget in 1/8 Bit (`(total_bits << 3) - anti_collapse_rsv`).
    pub total8: i32,
    /// Start-Bilanz aus der Allokation.
    pub balance: i32,
    pub coded_bands: i32,
}

/// `quant_all_bands` (Decode) als reiner Bit-Walk.
pub fn walk_bands(rc: &mut RangeDec<'_>, inp: &BandInput<'_>) -> BandWalk {
    let blocks = if inp.short_blocks { 1 << inp.lm } else { 1 };
    let m = 1usize << inp.lm;
    let mut balance = inp.balance;
    let mut dual = inp.dual;
    let mut out = BandWalk {
        bits8: [0; NB],
        pulses: [0; NB],
    };
    for i in inp.start..inp.end {
        let n = m * (EBANDS[i + 1] - EBANDS[i]) as usize;
        let tell = rc.tell_frac();
        if i != inp.start {
            balance -= tell;
        }
        let remaining = inp.total8 - tell - 1;
        let mut b = 0;
        if (i as i32) < inp.coded_bands {
            let curr = balance / 3.min(inp.coded_bands - i as i32);
            b = (remaining + 1).min(inp.alloc[i] + curr).clamp(0, 16383);
        }
        if dual && i == inp.intensity {
            dual = false;
        }
        let mut ctx = Ctx {
            rc: &mut *rc,
            band: i,
            remaining,
            tf_change: inp.tf_res[i],
            intensity: inp.intensity,
            pulses: 0,
        };
        if dual {
            ctx.band(n, b / 2, blocks, inp.lm);
            ctx.band(n, b / 2, blocks, inp.lm);
        } else if inp.channels == 2 {
            ctx.band_stereo(n, b, blocks, inp.lm);
        } else {
            ctx.band(n, b, blocks, inp.lm);
        }
        out.pulses[i] = ctx.pulses;
        out.bits8[i] = rc.tell_frac() - tell;
        balance += inp.alloc[i] + tell;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pvq_v_matches_known_values() {
        // V(N,K) aus RFC 6716 §4.3.4.4 / libopus-Tabelle.
        assert_eq!(pvq_v(1, 1), 2);
        assert_eq!(pvq_v(2, 1), 4);
        assert_eq!(pvq_v(2, 2), 8);
        assert_eq!(pvq_v(3, 2), 18);
        assert_eq!(pvq_v(4, 4), 192);
        assert_eq!(pvq_v(5, 0), 1);
        // Geschlossene Form (RFC 6716 §4.3.4.4, Fischer):
        // V(N,K) = sum_j 2^j * C(N,j) * C(K-1,j-1), unabhaengig von der Rekursion.
        fn binom(n: u64, k: u64) -> u64 {
            if k > n {
                return 0;
            }
            (0..k).fold(1u64, |acc, i| acc * (n - i) / (i + 1))
        }
        for (n, k) in [(8u64, 10u64), (16, 7), (24, 4), (176, 3), (3, 40), (2, 128)] {
            let want: u64 = (1..=n.min(k))
                .map(|j| (1u64 << j) * binom(n, j) * binom(k - 1, j - 1))
                .sum();
            assert!(want < 1 << 32, "Testpaar zu gross");
            assert_eq!(u64::from(pvq_v(n as usize, k as usize)), want, "V({n},{k})");
        }
    }

    #[test]
    fn pulses_and_bits_roundtrip_through_cache() {
        assert_eq!(get_pulses(7), 7);
        assert_eq!(get_pulses(8), 8);
        assert_eq!(get_pulses(15), 15);
        assert_eq!(get_pulses(16), 16);
        assert_eq!(get_pulses(17), 18);
        // Mehr Bits → nie weniger Pulse (monoton).
        let mut last = 0;
        for bits in (0..2000).step_by(8) {
            let q = bits2pulses(10, 2, bits);
            assert!(q >= last, "bits {bits}: {q} < {last}");
            last = q;
        }
        assert_eq!(pulses2bits(10, 2, 0), 0);
        assert!(pulses2bits(10, 2, 1) > 0);
        // Baender 0–7 bei lm=-1 haben keine Cache-Zeile → 0 Pulse.
        assert_eq!(bits2pulses(0, -1, 500), 0);
    }

    #[test]
    fn bitexact_helpers_hit_reference_points() {
        // cos(pi/4) in Q15 (itheta 0/16384 behandelt der Aufrufer separat).
        assert!(
            (bitexact_cos(8192) - 23170).abs() <= 2,
            "{}",
            bitexact_cos(8192)
        );
        assert!(bitexact_cos(16383) < 10, "{}", bitexact_cos(16383)); // cos(pi/2) ~ 0
        assert_eq!(bitexact_log2tan(23170, 23170), 0); // tan(pi/4) = 1
        assert_eq!(isqrt32(1), 1);
        assert_eq!(isqrt32(81), 9);
        assert_eq!(isqrt32(80), 8);
        assert_eq!(compute_qn(4, 0, 0, 0, false), 1);
    }
}
