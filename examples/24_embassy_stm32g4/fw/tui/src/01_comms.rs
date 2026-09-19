//! Blocking serial transactions (T1): text lines and binary frames.
//!
//! Mirrors the host-smoke codec (leading-`0x00` framing marker, 15 s read
//! budget for long gates/sweeps). Unifies with host-smoke as cleanup.

use g474_common::frame::encode_cmd;
use g474_common::{DeviceResp, HostCmd, PROTO_VER};
use postcard::accumulator::{CobsAccumulator, FeedResult};
use std::io::{Read, Write};
use std::time::Duration;

pub struct Comms {
    port: Box<dyn serialport::SerialPort>,
}

impl Comms {
    pub fn open(path: &str) -> Result<Self, String> {
        let port = serialport::new(path, 115_200)
            .timeout(Duration::from_millis(15000))
            .open()
            .map_err(|e| format!("open {}: {}", path, e))?;
        Ok(Self { port })
    }

    /// One binary round-trip; returns the first decoded response frame.
    pub fn roundtrip(&mut self, cmd: &HostCmd) -> Result<DeviceResp, String> {
        let mut tx = [0u8; 192];
        let n = encode_cmd(cmd, &mut tx).ok_or("encode failed")?;
        self.port
            .write_all(&tx[..n])
            .map_err(|e| format!("write: {}", e))?;
        self.read_frame()
    }

    /// Download a scope snapshot; returns raw LE sample bytes.
    pub fn scope_read(&mut self, off: u32, len: u16) -> Result<Vec<u8>, String> {
        let mut tx = [0u8; 192];
        let cmd = HostCmd::ScopeRead { off, len };
        let n = encode_cmd(&cmd, &mut tx).ok_or("encode failed")?;
        self.port
            .write_all(&tx[..n])
            .map_err(|e| format!("write: {}", e))?;
        let mut bytes = Vec::new();
        let mut total = usize::MAX;
        let mut next = 0u16;
        loop {
            match self.read_frame()? {
                DeviceResp::Block {
                    seq,
                    total: t,
                    data,
                } => {
                    if seq != next || (total != usize::MAX && t as usize != total) {
                        return Err(format!("block order seq={} total={}", seq, t));
                    }
                    total = t as usize;
                    next += 1;
                    bytes.extend_from_slice(&data);
                }
                DeviceResp::BlockEnd { total: t, crc } => {
                    if t as usize != total || next as usize != total {
                        return Err("block end mismatch".to_string());
                    }
                    if g474_common::blocks_05::crc16(&bytes) != crc {
                        return Err("crc mismatch".to_string());
                    }
                    return Ok(bytes);
                }
                other => return Err(format!("unexpected {:?}", other)),
            }
        }
    }

    /// Download VNA points; returns `(freq_hz, mvpp)` pairs.
    pub fn vna_read(&mut self, off: u32, len: u16) -> Result<Vec<(u32, u16)>, String> {
        let raw = self.vna_bytes(off, len)?;
        if raw.len() != len as usize * 6 {
            return Err(format!("byte count {}", raw.len()));
        }
        let mut pts = Vec::new();
        for c in raw.as_chunks::<6>().0 {
            pts.push((
                u32::from_le_bytes(c[0..4].try_into().unwrap()),
                u16::from_le_bytes(c[4..6].try_into().unwrap()),
            ));
        }
        Ok(pts)
    }

    fn vna_bytes(&mut self, off: u32, len: u16) -> Result<Vec<u8>, String> {
        let mut tx = [0u8; 192];
        let cmd = HostCmd::VnaRead { off, len };
        let n = encode_cmd(&cmd, &mut tx).ok_or("encode failed")?;
        self.port
            .write_all(&tx[..n])
            .map_err(|e| format!("write: {}", e))?;
        let mut bytes = Vec::new();
        let mut total = usize::MAX;
        let mut next = 0u16;
        loop {
            match self.read_frame()? {
                DeviceResp::Block {
                    seq,
                    total: t,
                    data,
                } => {
                    if seq != next || (total != usize::MAX && t as usize != total) {
                        return Err(format!("block order seq={} total={}", seq, t));
                    }
                    total = t as usize;
                    next += 1;
                    bytes.extend_from_slice(&data);
                }
                DeviceResp::BlockEnd { total: t, crc } => {
                    if t as usize != total || next as usize != total {
                        return Err("block end mismatch".to_string());
                    }
                    if g474_common::blocks_05::crc16(&bytes) != crc {
                        return Err("crc mismatch".to_string());
                    }
                    return Ok(bytes);
                }
                other => return Err(format!("unexpected {:?}", other)),
            }
        }
    }

    fn read_frame(&mut self) -> Result<DeviceResp, String> {
        let mut acc: CobsAccumulator<256> = CobsAccumulator::new();
        let mut one = [0u8; 1];
        let mut started = false;
        for _ in 0..2048 {
            self.port
                .read_exact(&mut one)
                .map_err(|e| format!("read: {}", e))?;
            if !started {
                if one[0] == 0x00 {
                    continue;
                }
                started = true;
            }
            match acc.feed::<DeviceResp>(&one) {
                FeedResult::Success { data, .. } => return Ok(data),
                FeedResult::Consumed => {}
                _ => return Err("decode error".to_string()),
            }
        }
        Err("frame timeout".to_string())
    }
}

/// Verify the firmware speaks our protocol version (plan §6.3: show an
/// update hint instead of failing obscurely on mismatch).
pub fn check_proto(comms: &mut Comms) -> Result<String, String> {
    match comms.roundtrip(&HostCmd::GetVer)? {
        DeviceResp::Ver { proto, fw } => {
            if proto != PROTO_VER {
                return Err(format!(
                    "firmware proto={} != tui proto={}: update one side",
                    proto, PROTO_VER
                ));
            }
            Ok(format!("proto={} fw={}", proto, fw.as_str()))
        }
        other => Err(format!("unexpected {:?}", other)),
    }
}
