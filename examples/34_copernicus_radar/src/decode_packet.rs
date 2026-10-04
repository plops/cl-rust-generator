//! FDBAQ range-data decoder (`copernicus_04_decode_packet.cpp`).
//!
//! Decodes the four sample channels (IE, IO, QE, QO) of a signal packet:
//! IE carries fresh bit-rate codes per 128-symbol block, IO reuses them,
//! QE reads a threshold index per block, and QO reuses both. IE/IO symbols
//! are stored unscaled and reconstructed afterwards once the threshold
//! indices are known.

use num_complex::Complex32;

use crate::error::{Error, Result};
use crate::tables;
use crate::utils::{reconstruct, BitReader, ReconParams, SYMBOLS_PER_BLOCK};

/// Number of BAQ blocks for a packet: `ceil(2 * quads / 256)`.
pub fn number_of_baq_blocks(number_of_quads: usize) -> usize {
    (2 * number_of_quads).div_ceil(256)
}

/// The four decoded channels plus the per-block codes.
#[derive(Debug, Default)]
pub struct DecodedPacket {
    /// In-phase even channel.
    pub ie: Vec<f32>,
    /// In-phase odd channel.
    pub io: Vec<f32>,
    /// Quadrature even channel.
    pub qe: Vec<f32>,
    /// Quadrature odd channel.
    pub qo: Vec<f32>,
    /// Bit-rate code per block (FDBAQ only).
    pub brcs: Vec<u8>,
    /// Threshold index per block (FDBAQ and type C only).
    pub thidxs: Vec<u8>,
}

impl DecodedPacket {
    /// Number of complex samples: `ie + io` (the C++ return value `n`).
    pub fn sample_count(&self) -> usize {
        self.ie.len() + self.io.len()
    }

    /// Interleave the channels into complex range samples: even samples
    /// from IE/QE, odd samples from IO/QO.
    ///
    /// The C++ decoders take an output pointer but never write through it,
    /// so the `.cf` files contain uninitialized heap memory. This port fills
    /// the image with the decoded samples instead; see the crate README.
    pub fn to_complex(&self) -> Vec<Complex32> {
        let mut out = Vec::with_capacity(self.sample_count());
        for k in 0..self.ie.len() {
            out.push(Complex32::new(self.ie[k], self.qe[k]));
            out.push(Complex32::new(self.io[k], self.qo[k]));
        }
        out
    }
}

/// Read a 3-bit bit-rate code, rejecting values above 4 (`get_bit_rate_code`).
pub fn bit_rate_code(reader: &mut BitReader) -> Result<u8> {
    let brc = reader.bits(3)?;
    if brc > 4 {
        return Err(Error::BadBrc {
            brc,
            offset: reader.byte_offset(),
        });
    }
    Ok(brc as u8)
}

/// Huffman-decode one magnitude code for the given BRC
/// (`decode_huffman_brc0..4`). The nested tests mirror the C++ trees exactly.
pub fn decode_huffman(brc: u8, s: &mut BitReader) -> Result<u32> {
    match brc {
        0 => {
            if s.bit()? {
                if s.bit()? {
                    if s.bit()? {
                        Ok(3)
                    } else {
                        Ok(2)
                    }
                } else {
                    Ok(1)
                }
            } else {
                Ok(0)
            }
        }
        1 => {
            if s.bit()? {
                if s.bit()? {
                    if s.bit()? {
                        if s.bit()? {
                            Ok(4)
                        } else {
                            Ok(3)
                        }
                    } else {
                        Ok(2)
                    }
                } else {
                    Ok(1)
                }
            } else {
                Ok(0)
            }
        }
        2 => {
            if s.bit()? {
                if s.bit()? {
                    if s.bit()? {
                        if s.bit()? {
                            if s.bit()? {
                                if s.bit()? {
                                    Ok(6)
                                } else {
                                    Ok(5)
                                }
                            } else {
                                Ok(4)
                            }
                        } else {
                            Ok(3)
                        }
                    } else {
                        Ok(2)
                    }
                } else {
                    Ok(1)
                }
            } else {
                Ok(0)
            }
        }
        3 => {
            if s.bit()? {
                if s.bit()? {
                    if s.bit()? {
                        if s.bit()? {
                            if s.bit()? {
                                if s.bit()? {
                                    if s.bit()? {
                                        if s.bit()? {
                                            Ok(9)
                                        } else {
                                            Ok(8)
                                        }
                                    } else {
                                        Ok(7)
                                    }
                                } else {
                                    Ok(6)
                                }
                            } else {
                                Ok(5)
                            }
                        } else {
                            Ok(4)
                        }
                    } else {
                        Ok(3)
                    }
                } else {
                    Ok(2)
                }
            } else if s.bit()? {
                Ok(1)
            } else {
                Ok(0)
            }
        }
        4 => {
            if s.bit()? {
                if s.bit()? {
                    if s.bit()? {
                        if s.bit()? {
                            if s.bit()? {
                                if s.bit()? {
                                    if s.bit()? {
                                        if s.bit()? {
                                            if s.bit()? {
                                                Ok(15)
                                            } else {
                                                Ok(14)
                                            }
                                        } else if s.bit()? {
                                            Ok(13)
                                        } else {
                                            Ok(12)
                                        }
                                    } else if s.bit()? {
                                        Ok(11)
                                    } else {
                                        Ok(10)
                                    }
                                } else {
                                    Ok(9)
                                }
                            } else {
                                Ok(8)
                            }
                        } else {
                            Ok(7)
                        }
                    } else if s.bit()? {
                        Ok(6)
                    } else {
                        Ok(5)
                    }
                } else if s.bit()? {
                    Ok(4)
                } else {
                    Ok(3)
                }
            } else if s.bit()? {
                if s.bit()? {
                    Ok(2)
                } else {
                    Ok(1)
                }
            } else {
                Ok(0)
            }
        }
        _ => Err(Error::BadBrc {
            brc: u32::from(brc),
            offset: s.byte_offset(),
        }),
    }
}

/// Reconstruction parameters per BRC (simple limits 3/3/5/6/8 and magnitude
/// maxima 3/4/6/9/15, tables B and NRL).
pub fn fdbaq_params(brc: u8) -> Result<ReconParams> {
    match brc {
        0 => Ok(ReconParams {
            simple_limit: 3,
            max_mcode: 3,
            simple: &tables::B0,
            nrl: &tables::NRL0,
        }),
        1 => Ok(ReconParams {
            simple_limit: 3,
            max_mcode: 4,
            simple: &tables::B1,
            nrl: &tables::NRL1,
        }),
        2 => Ok(ReconParams {
            simple_limit: 5,
            max_mcode: 6,
            simple: &tables::B2,
            nrl: &tables::NRL2,
        }),
        3 => Ok(ReconParams {
            simple_limit: 6,
            max_mcode: 9,
            simple: &tables::B3,
            nrl: &tables::NRL3,
        }),
        4 => Ok(ReconParams {
            simple_limit: 8,
            max_mcode: 15,
            simple: &tables::B4,
            nrl: &tables::NRL4,
        }),
        _ => Err(Error::BadBrc {
            brc: u32::from(brc),
            offset: 0,
        }),
    }
}

fn sign_of(sign_bit: bool) -> f32 {
    if sign_bit {
        -1.0
    } else {
        1.0
    }
}

/// Parse one in-phase channel (IE reads fresh BRCs, IO reuses `brcs`).
fn parse_in_phase(
    reader: &mut BitReader,
    number_of_quads: usize,
    brcs: &mut Vec<u8>,
    read_brc: bool,
) -> Result<Vec<f32>> {
    let mut symbols = Vec::with_capacity(number_of_quads);
    let mut block = 0;
    while symbols.len() < number_of_quads {
        let brc = if read_brc {
            let brc = bit_rate_code(reader)?;
            brcs.push(brc);
            brc
        } else {
            brcs[block]
        };
        for _ in 0..SYMBOLS_PER_BLOCK {
            if symbols.len() >= number_of_quads {
                break;
            }
            let symbol_sign = sign_of(reader.bit()?);
            let mcode = decode_huffman(brc, reader)?;
            // thidx is unknown here; reconstruction happens later.
            symbols.push(symbol_sign * mcode as f32);
        }
        block += 1;
    }
    Ok(symbols)
}

/// Parse one quadrature channel. QE reads and records a threshold index per
/// block; QO reuses the recorded indices.
fn parse_quadrature(
    reader: &mut BitReader,
    number_of_quads: usize,
    brcs: &[u8],
    thidxs: &mut Vec<u8>,
    read_thidx: bool,
) -> Result<Vec<f32>> {
    let mut symbols = Vec::with_capacity(number_of_quads);
    let mut block = 0;
    while symbols.len() < number_of_quads {
        let brc = brcs[block];
        let thidx = if read_thidx {
            let thidx = reader.threshold_index()?;
            thidxs.push(thidx);
            thidx
        } else {
            thidxs[block]
        };
        let params = fdbaq_params(brc)?;
        for _ in 0..SYMBOLS_PER_BLOCK {
            if symbols.len() >= number_of_quads {
                break;
            }
            let symbol_sign = sign_of(reader.bit()?);
            let mcode = decode_huffman(brc, reader)?;
            symbols.push(reconstruct(params, &tables::SF, thidx, mcode, symbol_sign)?);
        }
        block += 1;
    }
    Ok(symbols)
}

/// Reconstruct a deferred in-phase channel block by block (`decode ie/io
/// p.74`): split each stored value back into sign and magnitude and apply
/// the block's reconstruction law.
fn rescale_in_phase(
    symbols: &mut [f32],
    brcs: &[u8],
    thidxs: &[u8],
    number_of_baq_blocks: usize,
) -> Result<()> {
    for block in 0..number_of_baq_blocks {
        let params = fdbaq_params(brcs[block])?;
        let thidx = thidxs[block];
        for i in 0..SYMBOLS_PER_BLOCK {
            let pos = i + SYMBOLS_PER_BLOCK * block;
            if pos >= symbols.len() {
                break;
            }
            let scode = symbols[pos];
            let mcode = scode.abs() as u32;
            let symbol_sign = 1f32.copysign(scode);
            symbols[pos] = reconstruct(params, &tables::SF, thidx, mcode, symbol_sign)?;
        }
    }
    Ok(())
}

/// Decode one FDBAQ signal packet (`init_decode_packet`).
///
/// The reader must be positioned at the first payload byte (packet offset +
/// 68). Returns the four reconstructed channels; the C++ return value `n`
/// is [`DecodedPacket::sample_count`].
pub fn decode_fdbaq(reader: &mut BitReader, number_of_quads: usize) -> Result<DecodedPacket> {
    let number_of_baq_blocks = number_of_baq_blocks(number_of_quads);
    let mut brcs = Vec::with_capacity(number_of_baq_blocks);
    let mut thidxs = Vec::with_capacity(number_of_baq_blocks);

    let mut ie = parse_in_phase(reader, number_of_quads, &mut brcs, true)?;
    reader.consume_padding_bits();
    let mut io = parse_in_phase(reader, number_of_quads, &mut brcs, false)?;
    reader.consume_padding_bits();
    let qe = parse_quadrature(reader, number_of_quads, &brcs, &mut thidxs, true)?;
    reader.consume_padding_bits();
    let qo = parse_quadrature(reader, number_of_quads, &brcs, &mut thidxs, false)?;
    reader.consume_padding_bits();

    rescale_in_phase(&mut ie, &brcs, &thidxs, number_of_baq_blocks)?;
    rescale_in_phase(&mut io, &brcs, &thidxs, number_of_baq_blocks)?;

    debug_assert_eq!(ie.len(), io.len());
    debug_assert_eq!(ie.len(), qe.len());
    debug_assert_eq!(qo.len(), qe.len());
    Ok(DecodedPacket {
        ie,
        io,
        qe,
        qo,
        brcs,
        thidxs,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Feed `bits` (MSB first, e.g. "01011") to a Huffman decoder.
    fn huffman_bits(brc: u8, bits: &str) -> u32 {
        let mut bytes = vec![0u8; bits.len().div_ceil(8) + 1];
        for (i, c) in bits.chars().enumerate() {
            if c == '1' {
                bytes[i / 8] |= 1 << (7 - (i % 8));
            }
        }
        let mut r = BitReader::new(&bytes, 0);
        decode_huffman(brc, &mut r).unwrap()
    }

    #[test]
    fn huffman_trees_match_cpp() {
        assert_eq!(huffman_bits(0, "0"), 0);
        assert_eq!(huffman_bits(0, "10"), 1);
        assert_eq!(huffman_bits(0, "110"), 2);
        assert_eq!(huffman_bits(0, "111"), 3);
        assert_eq!(huffman_bits(1, "1111"), 4);
        assert_eq!(huffman_bits(1, "1110"), 3);
        assert_eq!(huffman_bits(2, "111111"), 6);
        assert_eq!(huffman_bits(2, "111110"), 5);
        assert_eq!(huffman_bits(3, "00"), 0);
        assert_eq!(huffman_bits(3, "01"), 1);
        assert_eq!(huffman_bits(3, "10"), 2);
        assert_eq!(huffman_bits(3, "11111111"), 9);
        assert_eq!(huffman_bits(3, "11111110"), 8);
        assert_eq!(huffman_bits(4, "00"), 0);
        assert_eq!(huffman_bits(4, "010"), 1);
        assert_eq!(huffman_bits(4, "011"), 2);
        assert_eq!(huffman_bits(4, "100"), 3);
        assert_eq!(huffman_bits(4, "101"), 4);
        assert_eq!(huffman_bits(4, "1100"), 5);
        assert_eq!(huffman_bits(4, "1101"), 6);
        assert_eq!(huffman_bits(4, "1110"), 7);
        assert_eq!(huffman_bits(4, "11110"), 8);
        assert_eq!(huffman_bits(4, "111110"), 9);
        assert_eq!(huffman_bits(4, "11111100"), 10);
        assert_eq!(huffman_bits(4, "11111101"), 11);
        assert_eq!(huffman_bits(4, "111111100"), 12);
        assert_eq!(huffman_bits(4, "111111101"), 13);
        assert_eq!(huffman_bits(4, "111111110"), 14);
        assert_eq!(huffman_bits(4, "111111111"), 15);
    }

    #[test]
    fn brc_above_4_is_rejected() {
        let bytes = [0b1110_0000u8]; // brc = 7
        let mut r = BitReader::new(&bytes, 0);
        assert!(matches!(
            bit_rate_code(&mut r),
            Err(Error::BadBrc { brc: 7, .. })
        ));
    }

    #[test]
    fn baq_block_count_matches_formula() {
        assert_eq!(number_of_baq_blocks(1), 1);
        assert_eq!(number_of_baq_blocks(128), 1);
        assert_eq!(number_of_baq_blocks(129), 2);
        assert_eq!(number_of_baq_blocks(2561), 21);
    }

    struct TestWriter {
        payload: Vec<u8>,
        bitpos: usize,
    }

    impl TestWriter {
        fn put(&mut self, bits: u32, n: u32) {
            for i in (0..n).rev() {
                if (bits >> i) & 1 == 1 {
                    self.payload[self.bitpos / 8] |= 1 << (7 - (self.bitpos % 8));
                }
                self.bitpos += 1;
            }
        }

        fn pad(&mut self) {
            let byte = self.bitpos / 8;
            let used = self.bitpos % 8;
            if byte.is_multiple_of(2) {
                if used != 0 {
                    self.bitpos = (byte + 2) * 8;
                }
            } else {
                self.bitpos = (byte + 1) * 8;
            }
        }

        fn symbol(&mut self, m: u32) {
            let (code, n) = match m {
                0 => (0b0, 1),
                1 => (0b10, 2),
                2 => (0b110, 3),
                _ => (0b111, 3),
            };
            self.put(0, 1); // sign 0
            self.put(code, n);
        }
    }

    /// Encode `quads` symbols per channel with BRC 0 and threshold index 10
    /// (normal law), all magnitude codes as given.
    fn fdbaq_payload(quads: usize, mcodes: &[u32]) -> Vec<u8> {
        let mut w = TestWriter {
            payload: vec![0u8; 64],
            bitpos: 0,
        };
        w.put(0, 3); // IE bit-rate code
        for &m in &mcodes[..quads] {
            w.symbol(m);
        }
        w.pad();
        for &m in &mcodes[..quads] {
            w.symbol(m);
        }
        w.pad();
        w.put(10, 8); // QE threshold index
        for &m in &mcodes[..quads] {
            w.symbol(m);
        }
        w.pad();
        for &m in &mcodes[..quads] {
            w.symbol(m);
        }
        w.payload.truncate(w.bitpos.div_ceil(8));
        w.payload
    }

    #[test]
    fn fdbaq_roundtrip_normal_law() {
        // Data starts at an even absolute offset like a real packet (68).
        let mut file = vec![0u8; 68];
        file.extend_from_slice(&fdbaq_payload(2, &[1, 2]));
        let mut r = BitReader::new(&file, 68);
        let p = decode_fdbaq(&mut r, 2).unwrap();
        assert_eq!(p.brcs, [0]);
        assert_eq!(p.thidxs, [10]);
        assert_eq!(p.sample_count(), 4);
        // Normal law: sign * NRL0[mcode] * SF[10]; SF[10] = 6.27.
        let expect = |m: usize| tables::NRL0[m] * tables::SF[10];
        assert!((p.ie[0] - expect(1)).abs() < 1e-4);
        assert!((p.ie[1] - expect(2)).abs() < 1e-4);
        assert!((p.io[0] - expect(1)).abs() < 1e-4);
        assert!((p.qe[0] - expect(1)).abs() < 1e-4);
        assert!((p.qo[1] - expect(2)).abs() < 1e-4);
        let c = p.to_complex();
        assert_eq!(c.len(), 4);
        assert_eq!((c[0].re, c[0].im), (p.ie[0], p.qe[0]));
        assert_eq!((c[1].re, c[1].im), (p.io[0], p.qo[0]));
    }

    #[test]
    fn fdbaq_simple_law_uses_table_b() {
        // Same layout but thidx = 2 (simple law) and mcode 3 -> B0[2].
        let mut file = vec![0u8; 68];
        let mut payload = fdbaq_payload(1, &[3]);
        // Patch thidx byte: IE used 3 + 4 = 7 bits -> pad to byte 2, IO used
        // 4 bits -> pad to byte 4, so thidx starts at payload byte 4.
        payload[4] = 2;
        file.extend_from_slice(&payload);
        let mut r = BitReader::new(&file, 68);
        let p = decode_fdbaq(&mut r, 1).unwrap();
        assert_eq!(p.thidxs, [2]);
        assert!((p.qe[0] - tables::B0[2]).abs() < 1e-6);
        assert!((p.ie[0] - tables::B0[2]).abs() < 1e-6);
    }
}
