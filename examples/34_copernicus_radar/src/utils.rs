//! Shared constants, the sequential bit reader, reconstruction helpers and
//! `%.3g`-compatible float formatting.
//!
//! Ports `utils.h` (`sequential_bit_t`, `get_sequential_bit`,
//! `get_threshold_index`, `MAX_NUMBER_QUADS`), `consume_padding_bits` and the
//! reconstruction-law fragments from `copernicus_04_decode_packet.cpp`.

use crate::error::{Error, Result};

/// Maximum number of complex quads per packet (protocol document, page 55).
pub const MAX_NUMBER_QUADS: usize = 52378;
/// Length of the space-packet header copies kept in memory: 62 + 6 bytes.
pub const HEADER_LEN: usize = 68;
/// Expected packet sync marker (bytes 12..=15, big-endian).
pub const SYNC_MARKER: u32 = 0x352E_F853;
/// Sampling reference frequency in MHz.
pub const FREF: f64 = 37.53472;
/// Number of range samples decoded per BAQ block.
pub const SYMBOLS_PER_BLOCK: usize = 128;

/// MSB-first bit reader over the mapped file.
///
/// Mirrors `sequential_bit_t`: bits are consumed from bit 7 down to bit 0 of
/// each byte, then the reader advances to the next byte. `pos` is the
/// absolute byte offset into the file so [`BitReader::consume_padding_bits`]
/// can reproduce the even/odd alignment rule, which is relative to the start
/// of the mapped region.
#[derive(Debug, Clone)]
pub struct BitReader<'a> {
    bytes: &'a [u8],
    pos: usize,
    bit: u8,
}

impl<'a> BitReader<'a> {
    /// Create a reader over `bytes` starting at absolute offset `start`.
    pub fn new(bytes: &'a [u8], start: usize) -> Self {
        BitReader {
            bytes,
            pos: start,
            bit: 0,
        }
    }

    /// Absolute byte offset of the byte currently being read.
    pub fn byte_offset(&self) -> usize {
        self.pos
    }

    /// Number of bits already consumed from the current byte (0..=7).
    pub fn bit_count(&self) -> u8 {
        self.bit
    }

    /// Read one bit, MSB first. Returns an error past end of file.
    pub fn bit(&mut self) -> Result<bool> {
        let byte = *self.bytes.get(self.pos).ok_or(Error::Truncated {
            offset: self.pos,
            filesize: self.bytes.len(),
        })?;
        let res = ((byte >> (7 - self.bit)) & 1) != 0;
        self.bit += 1;
        if self.bit > 7 {
            self.bit = 0;
            self.pos += 1;
        }
        Ok(res)
    }

    /// Read `n` bits (1..=32), MSB first, as an unsigned integer.
    pub fn bits(&mut self, n: u32) -> Result<u32> {
        debug_assert!((1..=32).contains(&n));
        let mut v = 0u32;
        for _ in 0..n {
            v = (v << 1) | u32::from(self.bit()?);
        }
        Ok(v)
    }

    /// Read the 8-bit threshold index (`get_threshold_index`).
    pub fn threshold_index(&mut self) -> Result<u8> {
        Ok(self.bits(8)? as u8)
    }

    /// Skip to the first bit of the next even byte (`consume_padding_bits`):
    /// in an even byte with no bits consumed there is nothing to do; in an
    /// even byte mid-way the reader jumps two bytes; in an odd byte it jumps
    /// one byte.
    pub fn consume_padding_bits(&mut self) {
        if self.pos.is_multiple_of(2) {
            if self.bit != 0 {
                self.pos += 2;
                self.bit = 0;
            }
        } else {
            self.pos += 1;
            self.bit = 0;
        }
    }
}

/// Parameters selecting the reconstruction law for one BAQ block.
#[derive(Debug, Clone, Copy)]
pub struct ReconParams {
    /// Threshold-index limit: `thidx <= simple_limit` selects the simple
    /// reconstruction law, larger values the normal law.
    pub simple_limit: u8,
    /// Largest magnitude code handled by the simple law; anything larger is
    /// a decode error.
    pub max_mcode: u32,
    /// Simple reconstruction table (B for FDBAQ, A for type C).
    pub simple: &'static [f32],
    /// Normalized reconstruction levels (NRL / NRLA).
    pub nrl: &'static [f32],
}

/// Apply one reconstruction law (`decode qe/qo/ie/io p.74/75` fragments):
/// simple law for `thidx <= simple_limit`, normal law otherwise.
///
/// * simple: `mcode < max_mcode` yields `sign * mcode`,
///   `mcode == max_mcode` yields `sign * simple[thidx]`;
/// * normal: `sign * nrl[mcode] * sf[thidx]`.
pub fn reconstruct(
    params: ReconParams,
    sf: &[f32],
    thidx: u8,
    mcode: u32,
    symbol_sign: f32,
) -> Result<f32> {
    if thidx <= params.simple_limit {
        if mcode < params.max_mcode {
            Ok(symbol_sign * mcode as f32)
        } else if mcode == params.max_mcode {
            let b = params
                .simple
                .get(usize::from(thidx))
                .ok_or(Error::TableIndex {
                    table: "simple",
                    index: usize::from(thidx),
                })?;
            Ok(symbol_sign * b)
        } else {
            Err(Error::McodeTooLarge {
                mcode,
                brc: params.simple_limit,
            })
        }
    } else {
        let nrl = params.nrl.get(mcode as usize).ok_or(Error::TableIndex {
            table: "nrl",
            index: mcode as usize,
        })?;
        let s = sf.get(usize::from(thidx)).ok_or(Error::TableIndex {
            table: "sf",
            index: usize::from(thidx),
        })?;
        Ok(symbol_sign * nrl * s)
    }
}

/// Format a float like C `%.3g` (3 significant digits).
///
/// The C++ program sets `std::setprecision(3)` once and keeps the default
/// float field, so every float in the CSV reports uses this format.
pub fn fmt_g3(v: f64) -> String {
    if v == 0.0 {
        return "0".to_string();
    }
    if !v.is_finite() {
        return format!("{v}");
    }
    let neg = v.is_sign_negative();
    let a = v.abs();
    // Round to 3 significant digits first; the rounding may carry into the
    // exponent (e.g. 999.9 -> 1000 -> 1e+03), which then selects the form.
    let mut exp = a.log10().floor() as i32;
    let mut mant = (a / 10f64.powi(exp) * 100.0).round() / 100.0;
    if mant >= 10.0 {
        mant /= 10.0;
        exp += 1;
    }
    // %.3g uses exponential form when exp < -4 or exp >= precision (3).
    if !(-4..3).contains(&exp) {
        let digits = format!("{mant:.2}");
        let digits = digits.trim_end_matches('0').trim_end_matches('.');
        format!("{}{}e{exp:+03}", if neg { "-" } else { "" }, digits)
    } else {
        // Fixed form with (2 - exp) decimals after the point.
        let decimals = (2 - exp).max(0) as usize;
        let s = format!("{:.decimals$}", mant * 10f64.powi(exp));
        let s = if s.contains('.') {
            s.trim_end_matches('0').trim_end_matches('.').to_string()
        } else {
            s
        };
        format!("{}{}", if neg { "-" } else { "" }, s)
    }
}

/// Format an `f32` like C `%.3g`.
pub fn fmt_g3_f32(v: f32) -> String {
    fmt_g3(f64::from(v))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_order_is_msb_first() {
        let bytes = [0b1011_0001u8, 0b0100_0000];
        let mut r = BitReader::new(&bytes, 0);
        let bits: Vec<bool> = (0..10).map(|_| r.bit().unwrap()).collect();
        assert_eq!(
            bits,
            [true, false, true, true, false, false, false, true, false, true]
        );
        assert_eq!(r.byte_offset(), 1);
        assert_eq!(r.bit_count(), 2);
    }

    #[test]
    fn multi_bit_reads_are_msb_first() {
        let bytes = [0b1101_0110u8];
        let mut r = BitReader::new(&bytes, 0);
        assert_eq!(r.bits(3).unwrap(), 0b110);
        assert_eq!(r.bits(5).unwrap(), 0b10110);
    }

    #[test]
    fn threshold_index_reads_a_full_byte() {
        let bytes = [0xA5u8];
        let mut r = BitReader::new(&bytes, 0);
        assert_eq!(r.threshold_index().unwrap(), 0xA5);
    }

    #[test]
    fn padding_skips_to_next_even_byte() {
        // Even byte, mid-way: jump two bytes.
        let bytes = [0u8; 8];
        let mut r = BitReader::new(&bytes, 0);
        r.bit().unwrap();
        r.consume_padding_bits();
        assert_eq!((r.byte_offset(), r.bit_count()), (2, 0));
        // Even byte, untouched: nothing to do.
        r.consume_padding_bits();
        assert_eq!((r.byte_offset(), r.bit_count()), (2, 0));
        // Odd byte: jump one byte.
        let mut r = BitReader::new(&bytes, 3);
        r.bit().unwrap();
        r.consume_padding_bits();
        assert_eq!((r.byte_offset(), r.bit_count()), (4, 0));
    }

    #[test]
    fn read_past_end_is_truncated() {
        let bytes = [0xFFu8];
        let mut r = BitReader::new(&bytes, 0);
        for _ in 0..8 {
            r.bit().unwrap();
        }
        assert!(matches!(
            r.bit(),
            Err(Error::Truncated {
                offset: 1,
                filesize: 1
            })
        ));
    }

    #[test]
    fn g3_matches_oracle() {
        let cases = [
            (0.0, "0"),
            (1.0, "1"),
            (-1.0, "-1"),
            (37.53472, "37.5"),
            (1234.5, "1.23e+03"),
            (-1234.0, "-1.23e+03"),
            (0.001234, "0.00123"),
            (6_380_000.0, "6.38e+06"),
            (100.0, "100"),
            (999.9, "1e+03"),
            (0.3637, "0.364"),
            (1.52587890625e-05, "1.53e-05"),
            (320.0, "320"),
            (1e-05, "1e-05"),
            (123456.0, "1.23e+05"),
            (0.1, "0.1"),
            (12.0, "12"),
            (3.16, "3.16"),
            (255.99, "256"),
        ];
        for (v, expected) in cases {
            assert_eq!(fmt_g3(v), expected, "v={v}");
        }
    }
}
