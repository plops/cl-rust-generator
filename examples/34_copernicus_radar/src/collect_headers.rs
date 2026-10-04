//! Packet-header collection (`copernicus_02_collect_packet_headers.cpp`).
//!
//! Walks the mapped file packet by packet: each packet is `6 + 1 +
//! data_length` bytes long, where `data_length` is the big-endian 16-bit
//! value at header bytes 4..=5. The first 68 bytes of every packet are
//! copied into the header table together with the packet file offset.

use crate::error::{Error, Result};
use crate::utils::HEADER_LEN;

/// Offsets and 68-byte header copies of all space packets in the file.
#[derive(Debug, Default)]
pub struct PacketHeaders {
    /// File offset of each packet (`state._header_offset`).
    pub offsets: Vec<usize>,
    /// First 68 bytes of each packet (`state._header_data`).
    pub headers: Vec<[u8; HEADER_LEN]>,
}

impl PacketHeaders {
    /// Number of packets found.
    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    /// True if the file contained no packets.
    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }
}

/// Collect all packet headers from the mapped file
/// (`init_collect_packet_headers`).
pub fn collect_packet_headers(data: &[u8]) -> Result<PacketHeaders> {
    let mut out = PacketHeaders::default();
    let mut offset = 0usize;
    while offset < data.len() {
        if offset + HEADER_LEN > data.len() {
            return Err(Error::HeaderTooShort { offset });
        }
        let data_length = (u32::from(data[offset + 4]) << 8 | u32::from(data[offset + 5])) as usize;
        let mut chunk = [0u8; HEADER_LEN];
        chunk.copy_from_slice(&data[offset..offset + HEADER_LEN]);
        out.offsets.push(offset);
        out.headers.push(chunk);
        offset += 6 + 1 + data_length;
    }
    if out.is_empty() {
        return Err(Error::NoPackets("file contains no space packets"));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn packet(data_length: u16) -> Vec<u8> {
        let mut v = vec![0u8; 6 + 1 + data_length as usize];
        v[4] = (data_length >> 8) as u8;
        v[5] = data_length as u8;
        v
    }

    #[test]
    fn walks_packets_by_data_length() {
        // Two packets: 68-byte header + payload each.
        let mut file = packet(61 + 8);
        file.extend_from_slice(&packet(61 + 16));
        let headers = collect_packet_headers(&file).unwrap();
        assert_eq!(headers.len(), 2);
        assert_eq!(headers.offsets, [0, 68 + 8]);
        assert_eq!(headers.headers[1][5], ((61 + 16) & 0xFF) as u8);
    }

    #[test]
    fn truncated_header_is_an_error() {
        let file = vec![0u8; 10];
        assert!(matches!(
            collect_packet_headers(&file),
            Err(Error::HeaderTooShort { offset: 0 })
        ));
    }

    #[test]
    fn empty_file_is_an_error() {
        assert!(matches!(
            collect_packet_headers(&[]),
            Err(Error::NoPackets(_))
        ));
    }
}
