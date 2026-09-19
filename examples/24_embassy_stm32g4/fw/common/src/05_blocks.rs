//! Snapshot block-transfer helpers (spec §5: no live streaming).
//!
//! Large acquisitions are split into `Block` chunks that fit the 128 B
//! COBS frame budget; the host reassembles them and checks `BlockEnd.crc`.

/// Max payload bytes per `DeviceResp::Block` (keeps the COBS frame ≤128 B).
pub const BLOCK_DATA_MAX: usize = 96;

/// Number of blocks needed for `len` payload bytes.
pub fn block_count(len: usize) -> usize {
    len.div_ceil(BLOCK_DATA_MAX)
}

/// Byte range of block `seq` (0-based) within a `len`-byte snapshot.
pub fn block_range(len: usize, seq: usize) -> Option<(usize, usize)> {
    let total = block_count(len);
    if seq >= total {
        return None;
    }
    let start = seq * BLOCK_DATA_MAX;
    Some((start, (start + BLOCK_DATA_MAX).min(len)))
}

/// CRC-16/CCITT (poly 0x1021, init 0xFFFF) over the raw snapshot bytes.
pub fn crc16(data: &[u8]) -> u16 {
    crc16_update(0xFFFF, data)
}

/// Continue a CRC-16 over the next chunk (streaming, same parameters).
pub fn crc16_update(mut crc: u16, data: &[u8]) -> u16 {
    for &b in data {
        crc ^= (b as u16) << 8;
        for _ in 0..8 {
            if crc & 0x8000 != 0 {
                crc = (crc << 1) ^ 0x1021;
            } else {
                crc <<= 1;
            }
        }
    }
    crc
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_split_cover() {
        assert_eq!(block_count(0), 0);
        assert_eq!(block_count(96), 1);
        assert_eq!(block_count(97), 2);
        // Ranges tile the snapshot without gaps or overlap.
        let len = 250;
        let mut pos = 0;
        for seq in 0..block_count(len) {
            let (s, e) = block_range(len, seq).unwrap();
            assert_eq!(s, pos);
            pos = e;
        }
        assert_eq!(pos, len);
        assert_eq!(block_range(len, block_count(len)), None);
    }

    #[test]
    fn crc16_reference() {
        // Standard check vector for CRC-16/CCITT-FALSE.
        assert_eq!(crc16(b"123456789"), 0x29B1);
        assert_eq!(crc16(b""), 0xFFFF);
    }

    #[test]
    fn crc16_streaming_matches_oneshot() {
        let data = b"123456789";
        let mut crc = 0xFFFF;
        for chunk in data.chunks(3) {
            crc = crc16_update(crc, chunk);
        }
        assert_eq!(crc, crc16(data));
    }
}
