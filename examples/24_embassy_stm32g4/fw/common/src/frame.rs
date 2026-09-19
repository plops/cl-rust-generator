//! Binary-mode helpers: postcard + COBS encode, `CobsAccumulator` decode.

use crate::{DeviceResp, HostCmd, MAX_FRAME};
use postcard::accumulator::{CobsAccumulator, FeedResult};

/// Encode a host command into `buf`: leading `0x00` marker, then a
/// COBS frame (`0x00` terminated).
///
/// The leading marker puts the shared byte-stream router into binary mode,
/// so `0x0A`/`0x0D` bytes *inside* the COBS payload are never mistaken for
/// text-line terminators (COBS only excludes `0x00`, e.g. `AwgStart(1000)`
/// contains a `0x0A`). See `router::Router`.
pub fn encode_cmd(cmd: &HostCmd, buf: &mut [u8]) -> Option<usize> {
    let (head, tail) = buf.split_first_mut()?;
    *head = 0x00;
    postcard::to_slice_cobs(cmd, tail).ok().map(|s| s.len() + 1)
}

/// Encode a device response into `buf` (same framing as [`encode_cmd`]).
pub fn encode_resp(resp: &DeviceResp, buf: &mut [u8]) -> Option<usize> {
    let (head, tail) = buf.split_first_mut()?;
    *head = 0x00;
    postcard::to_slice_cobs(resp, tail)
        .ok()
        .map(|s| s.len() + 1)
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
///
/// Leading `0x00` framing markers are skipped, so whole encoded frames
/// (with marker) and bare COBS payloads both decode.
pub struct CmdReceiver {
    acc: CobsAccumulator<MAX_FRAME>,
    started: bool,
}

impl CmdReceiver {
    pub fn new() -> Self {
        Self {
            acc: CobsAccumulator::new(),
            started: false,
        }
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }

    pub fn feed(&mut self, chunk: &[u8]) -> DecodeEvent {
        let mut chunk = chunk;
        if !self.started {
            match chunk.iter().position(|&b| b != 0x00) {
                None => return DecodeEvent::None,
                Some(i) => {
                    chunk = &chunk[i..];
                    self.started = true;
                }
            }
        }
        match self.acc.feed::<HostCmd>(chunk) {
            FeedResult::Consumed => DecodeEvent::None,
            FeedResult::Success { data, .. } => {
                self.started = false;
                DecodeEvent::Cmd(data)
            }
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
        // Framing invariant: leading 0x00 marker, then COBS (0x00 only last).
        assert_eq!(tx[0], 0x00);
        assert_eq!(tx[n - 1], 0x00);
        assert!(!tx[1..n - 1].contains(&0x00));
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
        assert_eq!(tx[0], 0x00);
        assert_eq!(tx[n - 1], 0x00);
        let mut acc: CobsAccumulator<160> = CobsAccumulator::new();
        let mut got = None;
        let mut started = false;
        for b in &tx[..n] {
            // Host-side decoders skip the leading framing marker.
            if !started {
                if *b == 0x00 {
                    continue;
                }
                started = true;
            }
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
    fn roundtrip_v2_mode_variants() {
        use crate::modes_04::{AwgConfig, CapConfig, FreqConfig, ScopeConfig, VnaConfig};
        let cfg = FreqConfig {
            level_mv: 1650,
            hyst: 1,
            filter: 0,
            gate_ms: 1000,
        };
        for cmd in [
            HostCmd::ModeStop,
            HostCmd::SelfTest,
            HostCmd::FreqStart(cfg),
            HostCmd::FreqRead,
            HostCmd::ScopeStart(ScopeConfig {
                interleaved: 1,
                level_mv: 100,
            }),
            HostCmd::ScopeRead { off: 0, len: 96 },
            HostCmd::AwgStart(AwgConfig { freq_hz: 1000 }),
            HostCmd::CapStart(CapConfig { pin: 0 }),
            HostCmd::CapRead,
            HostCmd::VnaStart(VnaConfig {
                f0_hz: 1000,
                f1_hz: 2000,
                points: 11,
            }),
            HostCmd::BlockAck { seq: 3 },
        ] {
            roundtrip(cmd);
        }
    }

    #[test]
    fn roundtrip_cap_resp() {
        for resp in [
            DeviceResp::Cap {
                pin: 1,
                time_us: 1234,
                timeout: false,
            },
            DeviceResp::Cap {
                pin: 2,
                time_us: 200_000,
                timeout: true,
            },
        ] {
            let mut tx = [0u8; 160];
            let n = encode_resp(&resp, &mut tx).expect("encode");
            assert_eq!(tx[0], 0x00);
            assert_eq!(tx[n - 1], 0x00);
            let mut acc: CobsAccumulator<160> = CobsAccumulator::new();
            let mut got = None;
            let mut started = false;
            for b in &tx[..n] {
                if !started {
                    if *b == 0x00 {
                        continue;
                    }
                    started = true;
                }
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
    }

    #[test]
    fn block_resp_fits_frame_budget() {
        use crate::MAX_FRAME;
        let mut data: heapless::Vec<u8, 96> = heapless::Vec::new();
        data.extend_from_slice(&[0xAA; 96]).unwrap();
        let resp = DeviceResp::Block {
            seq: 41,
            total: 42,
            data,
        };
        let mut tx = [0u8; MAX_FRAME + 32];
        let n = encode_resp(&resp, &mut tx).expect("encode");
        assert!(n <= MAX_FRAME, "block frame {} > {}", n, MAX_FRAME);
        assert_eq!(tx[n - 1], 0x00);
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
