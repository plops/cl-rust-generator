//! Type-C packet decoder (`copernicus_07_decode_type_c_packet.cpp`).
//!
//! Fixed-rate BAQ modes with 3, 4 or 5 bits per sample. Each code holds a
//! sign bit plus a magnitude; blocks carry a threshold index read alongside
//! the QE channel and reused for QO and for the deferred IE/IO
//! reconstruction. Reconstruction uses tables A / NRLA instead of B / NRL.
//!
//! The binary dispatches signal packets here when `baq_mode` is 3, 4 or 5
//! (fixed-rate noise or echo packets); modes 12/13/14 go to the FDBAQ
//! decoder. The C++ `main` routes every signal packet through FDBAQ and
//! fails on these modes instead.

use crate::decode_packet::{number_of_baq_blocks, DecodedPacket};
use crate::error::Result;
use crate::tables;
use crate::utils::{reconstruct, BitReader, ReconParams, SYMBOLS_PER_BLOCK};

/// Fixed-rate BAQ width in bits per sample.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaqBits {
    /// 3 bits: 1 sign + 2 magnitude.
    B3,
    /// 4 bits: 1 sign + 3 magnitude.
    B4,
    /// 5 bits: 1 sign + 4 magnitude.
    B5,
}

impl BaqBits {
    fn width(self) -> u32 {
        match self {
            BaqBits::B3 => 3,
            BaqBits::B4 => 4,
            BaqBits::B5 => 5,
        }
    }

    fn magnitude_mask(self) -> u32 {
        match self {
            BaqBits::B3 => 0x3,
            BaqBits::B4 => 0x7,
            BaqBits::B5 => 0xF,
        }
    }

    /// Reconstruction parameters: simple limits 3/5/10, magnitude maxima
    /// 3/7/15, tables A and NRLA.
    fn params(self) -> ReconParams {
        match self {
            BaqBits::B3 => ReconParams {
                simple_limit: 3,
                max_mcode: 3,
                simple: &tables::A3,
                nrl: &tables::NRLA3,
            },
            BaqBits::B4 => ReconParams {
                simple_limit: 5,
                max_mcode: 7,
                simple: &tables::A4,
                nrl: &tables::NRLA4,
            },
            BaqBits::B5 => ReconParams {
                simple_limit: 10,
                max_mcode: 15,
                simple: &tables::A5,
                nrl: &tables::NRLA5,
            },
        }
    }
}

/// Read one BAQ code (`get_baq3/4/5_code`).
pub fn baq_code(reader: &mut BitReader, bits: BaqBits) -> Result<u32> {
    reader.bits(bits.width())
}

fn split_code(smcode: u32, bits: BaqBits) -> (f32, u32) {
    let sign_bit = (smcode >> (bits.width() - 1)) & 1;
    let sign = if sign_bit == 0 { 1.0 } else { -1.0 };
    (sign, smcode & bits.magnitude_mask())
}

/// Decode one type-C packet with the given BAQ width
/// (`init_decode_type_c_packet_baq3/4/5`).
pub fn decode_type_c(
    reader: &mut BitReader,
    number_of_quads: usize,
    bits: BaqBits,
) -> Result<DecodedPacket> {
    let params = bits.params();
    let number_of_blocks = number_of_baq_blocks(number_of_quads);
    let mut thidxs = Vec::with_capacity(number_of_blocks);

    // IE / IO: deferred reconstruction (no threshold index yet).
    let mut ie = Vec::with_capacity(number_of_quads);
    while ie.len() < number_of_quads {
        for _ in 0..SYMBOLS_PER_BLOCK {
            if ie.len() >= number_of_quads {
                break;
            }
            let (sign, mcode) = split_code(baq_code(reader, bits)?, bits);
            ie.push(sign * mcode as f32);
        }
    }
    reader.consume_padding_bits();
    let mut io = Vec::with_capacity(number_of_quads);
    while io.len() < number_of_quads {
        for _ in 0..SYMBOLS_PER_BLOCK {
            if io.len() >= number_of_quads {
                break;
            }
            let (sign, mcode) = split_code(baq_code(reader, bits)?, bits);
            io.push(sign * mcode as f32);
        }
    }
    reader.consume_padding_bits();

    // QE: read and record one threshold index per block.
    let mut qe = Vec::with_capacity(number_of_quads);
    while qe.len() < number_of_quads {
        let thidx = reader.threshold_index()?;
        thidxs.push(thidx);
        for _ in 0..SYMBOLS_PER_BLOCK {
            if qe.len() >= number_of_quads {
                break;
            }
            let (sign, mcode) = split_code(baq_code(reader, bits)?, bits);
            qe.push(reconstruct(params, &tables::SF, thidx, mcode, sign)?);
        }
    }
    reader.consume_padding_bits();

    // QO: reuse the recorded threshold indices.
    let mut qo = Vec::with_capacity(number_of_quads);
    let mut block = 0;
    while qo.len() < number_of_quads {
        let thidx = thidxs[block];
        for _ in 0..SYMBOLS_PER_BLOCK {
            if qo.len() >= number_of_quads {
                break;
            }
            let (sign, mcode) = split_code(baq_code(reader, bits)?, bits);
            qo.push(reconstruct(params, &tables::SF, thidx, mcode, sign)?);
        }
        block += 1;
    }
    reader.consume_padding_bits();

    // Reconstruct the deferred IE / IO channels block by block.
    for symbols in [&mut ie, &mut io] {
        // Indexing (rather than iterating) so a short table panics instead
        // of silently decoding fewer blocks than the C++ loop would touch.
        #[allow(clippy::needless_range_loop)]
        for block in 0..number_of_blocks {
            let thidx = thidxs[block];
            for i in 0..SYMBOLS_PER_BLOCK {
                let pos = i + SYMBOLS_PER_BLOCK * block;
                if pos >= symbols.len() {
                    break;
                }
                let scode = symbols[pos];
                let mcode = scode.abs() as u32;
                let sign = 1f32.copysign(scode);
                symbols[pos] = reconstruct(params, &tables::SF, thidx, mcode, sign)?;
            }
        }
    }

    debug_assert_eq!(ie.len(), io.len());
    debug_assert_eq!(ie.len(), qe.len());
    debug_assert_eq!(qo.len(), qe.len());
    Ok(DecodedPacket {
        ie,
        io,
        qe,
        qo,
        brcs: Vec::new(),
        thidxs,
    })
}

/// Decode one BAQ-mode-3 type-C packet.
pub fn decode_baq3(reader: &mut BitReader, number_of_quads: usize) -> Result<DecodedPacket> {
    decode_type_c(reader, number_of_quads, BaqBits::B3)
}

/// Decode one BAQ-mode-4 type-C packet.
pub fn decode_baq4(reader: &mut BitReader, number_of_quads: usize) -> Result<DecodedPacket> {
    decode_type_c(reader, number_of_quads, BaqBits::B4)
}

/// Decode one BAQ-mode-5 type-C packet.
pub fn decode_baq5(reader: &mut BitReader, number_of_quads: usize) -> Result<DecodedPacket> {
    decode_type_c(reader, number_of_quads, BaqBits::B5)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a one-quad BAQ3 payload: IE/IO/QO code `code`, QE threshold
    /// `thidx` and code `qe_code`. Channels start at even payload bytes.
    fn baq3_payload(code: u32, thidx: u8, qe_code: u32) -> Vec<u8> {
        let mut payload = vec![0u8; 8];
        let mut put = |byte: usize, bit: usize, v: u32, n: u32| {
            for i in 0..n {
                if (v >> (n - 1 - i)) & 1 == 1 {
                    let pos = byte * 8 + bit + i as usize;
                    payload[pos / 8] |= 1 << (7 - (pos % 8));
                }
            }
        };
        put(0, 0, code, 3); // IE
        put(2, 0, code, 3); // IO
        put(4, 0, thidx as u32, 8); // QE thidx
        put(5, 0, qe_code, 3); // QE code
        put(6, 0, code, 3); // QO
        payload
    }

    #[test]
    fn baq3_normal_law() {
        // smcode 0b010 -> sign +, mcode 2; thidx 10 -> normal law.
        let mut file = vec![0u8; 68];
        file.extend_from_slice(&baq3_payload(0b010, 10, 0b010));
        let mut r = BitReader::new(&file, 68);
        let p = decode_baq3(&mut r, 1).unwrap();
        assert_eq!(p.thidxs, [10]);
        let expected = tables::NRLA3[2] * tables::SF[10];
        for ch in [&p.ie, &p.io, &p.qe, &p.qo] {
            assert!((ch[0] - expected).abs() < 1e-4, "{}", ch[0]);
        }
        assert_eq!(p.sample_count(), 2);
    }

    #[test]
    fn baq3_simple_law() {
        // mcode 3 with thidx 1 -> simple law -> A3[1] = 3.
        let mut file = vec![0u8; 68];
        file.extend_from_slice(&baq3_payload(0b011, 1, 0b011));
        let mut r = BitReader::new(&file, 68);
        let p = decode_baq3(&mut r, 1).unwrap();
        for ch in [&p.ie, &p.io, &p.qe, &p.qo] {
            assert!((ch[0] - tables::A3[1]).abs() < 1e-6, "{}", ch[0]);
        }
    }

    #[test]
    fn baq3_sign_handling() {
        // smcode 0b110 -> sign -, mcode 2; thidx 0 -> simple, mcode < 3.
        let mut file = vec![0u8; 68];
        file.extend_from_slice(&baq3_payload(0b110, 0, 0b110));
        let mut r = BitReader::new(&file, 68);
        let p = decode_baq3(&mut r, 1).unwrap();
        for ch in [&p.ie, &p.io, &p.qe, &p.qo] {
            assert_eq!(ch[0], -2.0);
        }
    }
}
