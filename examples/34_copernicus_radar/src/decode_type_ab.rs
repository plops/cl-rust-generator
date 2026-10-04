//! Bypass (type A/B) packet decoder
//! (`copernicus_05_decode_type_ab_packet.cpp`).
//!
//! Calibration packets use BAQ mode 0: each sample is a raw 10-bit
//! sign-magnitude code (1 sign bit + 9 magnitude bits) on each of the four
//! channels, with 16-bit alignment padding between channels.

use crate::decode_packet::DecodedPacket;
use crate::error::Result;
use crate::utils::BitReader;

/// Read one 10-bit sign-magnitude code (`get_data_type_a_or_b`).
pub fn data_type_a_or_b(reader: &mut BitReader) -> Result<u32> {
    reader.bits(10)
}

/// Split a 10-bit code into sign and floating-point value.
pub fn sign_magnitude(smcode: u32) -> f32 {
    let sign_bit = (smcode >> 9) & 1;
    let mcode = smcode & 0x1FF;
    let sign = if sign_bit == 0 { 1.0 } else { -1.0 };
    sign * mcode as f32
}

fn parse_channel(reader: &mut BitReader, number_of_quads: usize) -> Result<Vec<f32>> {
    let mut symbols = Vec::with_capacity(number_of_quads);
    for _ in 0..number_of_quads {
        symbols.push(sign_magnitude(data_type_a_or_b(reader)?));
    }
    reader.consume_padding_bits();
    Ok(symbols)
}

/// Decode one bypass calibration packet
/// (`init_decode_packet_type_a_or_b`).
///
/// The reader must be positioned at the first payload byte. The C++ return
/// value `n` is [`DecodedPacket::sample_count`].
pub fn decode_type_a_or_b(reader: &mut BitReader, number_of_quads: usize) -> Result<DecodedPacket> {
    let ie = parse_channel(reader, number_of_quads)?;
    let io = parse_channel(reader, number_of_quads)?;
    let qe = parse_channel(reader, number_of_quads)?;
    let qo = parse_channel(reader, number_of_quads)?;
    debug_assert_eq!(ie.len(), io.len());
    debug_assert_eq!(ie.len(), qe.len());
    debug_assert_eq!(qo.len(), qe.len());
    Ok(DecodedPacket {
        ie,
        io,
        qe,
        qo,
        brcs: Vec::new(),
        thidxs: Vec::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sign_magnitude_split() {
        assert_eq!(sign_magnitude(0b0_000000001), 1.0);
        assert_eq!(sign_magnitude(0b1_000000001), -1.0);
        assert_eq!(sign_magnitude(0b0_111111111), 511.0);
        assert_eq!(sign_magnitude(0), 0.0);
    }

    #[test]
    fn decodes_four_channels_with_padding() {
        // One quad per channel: IE=+5, IO=-7, QE=+511, QO=0.
        // Each channel is 10 bits, padded to an even byte afterwards.
        let codes = [5u32, 0x200 | 7, 511, 0];
        let mut payload = vec![0u8; 8];
        for (ch, code) in codes.iter().enumerate() {
            // Channel ch starts at payload byte 2*ch (even), 10 bits.
            let base = ch * 16;
            for i in 0..10 {
                if (code >> (9 - i)) & 1 == 1 {
                    let pos = base + i;
                    payload[pos / 8] |= 1 << (7 - (pos % 8));
                }
            }
        }
        let mut file = vec![0u8; 68];
        file.extend_from_slice(&payload);
        let mut r = BitReader::new(&file, 68);
        let p = decode_type_a_or_b(&mut r, 1).unwrap();
        assert_eq!(p.ie, [5.0]);
        assert_eq!(p.io, [-7.0]);
        assert_eq!(p.qe, [511.0]);
        assert_eq!(p.qo, [0.0]);
        assert_eq!(p.sample_count(), 2);
        let c = p.to_complex();
        assert_eq!(c.len(), 2);
    }
}
