//! Binary-mode helpers: postcard + COBS encode, `CobsAccumulator` decode.

use crate::{DeviceResp, HostCmd, MAX_FRAME};
use postcard::accumulator::{CobsAccumulator, FeedResult};

/// Encode a host command into `buf`, COBS-framed (`0x00` terminated).
pub fn encode_cmd(cmd: &HostCmd, buf: &mut [u8]) -> Option<usize> {
    postcard::to_slice_cobs(cmd, buf).ok().map(|s| s.len())
}

/// Encode a device response into `buf`, COBS-framed.
pub fn encode_resp(resp: &DeviceResp, buf: &mut [u8]) -> Option<usize> {
    postcard::to_slice_cobs(resp, buf).ok().map(|s| s.len())
}

/// Decode result for one fed chunk.
#[derive(Debug, PartialEq)]
pub enum DecodeEvent {
    None,
    Cmd(HostCmd),
    OverFull,
    DeserError,
}

/// Stateful COBS receiver for `HostCmd` (device side).
pub struct CmdReceiver {
    acc: CobsAccumulator<MAX_FRAME>,
}

impl CmdReceiver {
    pub fn new() -> Self {
        Self {
            acc: CobsAccumulator::new(),
        }
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }

    pub fn feed(&mut self, chunk: &[u8]) -> DecodeEvent {
        match self.acc.feed::<HostCmd>(chunk) {
            FeedResult::Consumed => DecodeEvent::None,
            FeedResult::Success { data, .. } => DecodeEvent::Cmd(data),
            FeedResult::OverFull(_) => {
                self.reset();
                DecodeEvent::OverFull
            }
            FeedResult::DeserError(_) => {
                self.reset();
                DecodeEvent::DeserError
            }
        }
    }
}

impl Default for CmdReceiver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::err;

    fn roundtrip(cmd: HostCmd) {
        let mut tx = [0u8; 160];
        let n = encode_cmd(&cmd, &mut tx).expect("encode");
        // COBS invariant: 0x00 only as the final delimiter.
        assert_eq!(tx[n - 1], 0x00);
        assert!(!tx[..n - 1].contains(&0x00));
        let mut rx = CmdReceiver::new();
        // Feed byte-by-byte (worst-case chunking).
        let mut got = None;
        for b in &tx[..n] {
            match rx.feed(&[*b]) {
                DecodeEvent::Cmd(c) => {
                    got = Some(c);
                    break;
                }
                DecodeEvent::None => {}
                e => panic!("unexpected {:?}", e),
            }
        }
        assert_eq!(got, Some(cmd));
    }

    #[test]
    fn roundtrip_all_variants() {
        roundtrip(HostCmd::Ping);
        roundtrip(HostCmd::GetVer);
        let mut v: heapless::Vec<u8, 64> = heapless::Vec::new();
        v.extend_from_slice(&[1, 2, 3, 4]).unwrap();
        roundtrip(HostCmd::Echo(v));
        let mut big: heapless::Vec<u8, 64> = heapless::Vec::new();
        big.extend_from_slice(&[0xAA; 64]).unwrap();
        roundtrip(HostCmd::Echo(big));
    }

    #[test]
    fn resp_roundtrip_host_side() {
        let resp = DeviceResp::Ver {
            proto: 1,
            fw: heapless::String::from("0.1.0"),
        };
        let mut tx = [0u8; 160];
        let n = encode_resp(&resp, &mut tx).expect("encode");
        assert_eq!(tx[n - 1], 0x00);
        let mut acc: CobsAccumulator<160> = CobsAccumulator::new();
        let mut got = None;
        for b in &tx[..n] {
            match acc.feed::<DeviceResp>(&[*b]) {
                FeedResult::Success { data, .. } => {
                    got = Some(data);
                    break;
                }
                FeedResult::Consumed => {}
                _ => panic!("unexpected feed result"),
            }
        }
        assert_eq!(got, Some(resp));
    }

    #[test]
    fn corrupt_frame_resets_and_recovers() {
        let mut rx = CmdReceiver::new();
        // Garbage that fails deserialization once terminated.
        assert_eq!(rx.feed(&[0xFF, 0xFF, 0xFF]), DecodeEvent::None);
        assert_eq!(rx.feed(&[0x00]), DecodeEvent::DeserError);
        // Receiver must work again after reset.
        let mut tx = [0u8; 64];
        let n = encode_cmd(&HostCmd::Ping, &mut tx).unwrap();
        let mut got = None;
        for b in &tx[..n] {
            if let DecodeEvent::Cmd(c) = rx.feed(&[*b]) {
                got = Some(c);
                break;
            }
        }
        assert_eq!(got, Some(HostCmd::Ping));
        let _ = (err::UNKNOWN_CMD, err::BAD_FRAME, err::OVERFLOW);
    }

    #[test]
    fn multi_byte_chunks() {
        let mut tx = [0u8; 160];
        let n = encode_cmd(&HostCmd::GetVer, &mut tx).unwrap();
        let mut rx = CmdReceiver::new();
        // Split in the middle: first half consumed, second half completes.
        assert_eq!(rx.feed(&tx[..n / 2]), DecodeEvent::None);
        assert_eq!(rx.feed(&tx[n / 2..n]), DecodeEvent::Cmd(HostCmd::GetVer));
    }
}
