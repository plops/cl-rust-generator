//! First-byte dispatch router: one byte stream in, text lines and binary
//! commands out. Terminator decides the mode: `\n`/`\r` → text,
//! `0x00` → postcard+COBS frame. Host- and firmware-side shared so the
//! exact dispatch logic is unit-testable on the host.

use crate::frame::{CmdReceiver, DecodeEvent};
use crate::{HostCmd, MAX_FRAME, MAX_LINE};

/// One completed inbound item.
#[derive(Debug, PartialEq)]
pub enum InEvent {
    /// A text line without terminator (non-empty).
    Text(heapless::String<MAX_LINE>),
    /// A decoded binary command.
    Cmd(HostCmd),
    /// A text line exceeded [`MAX_LINE`]; resynced at the terminator.
    TextTooLong,
    /// A binary frame was corrupt or oversized; receiver reset.
    BinError,
}

/// Byte-stream router shared by firmware and tests.
pub struct Router {
    staging: heapless::Vec<u8, MAX_FRAME>,
    overflow: bool,
    cobs: CmdReceiver,
}

impl Router {
    pub fn new() -> Self {
        Self {
            staging: heapless::Vec::new(),
            overflow: false,
            cobs: CmdReceiver::new(),
        }
    }

    fn clear(&mut self) {
        self.staging.clear();
        self.overflow = false;
    }

    /// Feed one byte. Returns an event once a terminator completes an item;
    /// `None` while items are incomplete (or for skipped empty lines).
    pub fn feed(&mut self, b: u8) -> Option<InEvent> {
        if b == 0x00 {
            if self.overflow {
                self.clear();
                self.cobs.reset();
                return Some(InEvent::BinError);
            }
            let mut event = DecodeEvent::None;
            if !self.staging.is_empty() {
                // Staging is drained; clone to satisfy the borrow checker.
                event = self.cobs.feed(&self.staging.clone());
            }
            if event == DecodeEvent::None {
                event = self.cobs.feed(&[0x00]);
            }
            self.clear();
            return Some(match event {
                DecodeEvent::Cmd(cmd) => InEvent::Cmd(cmd),
                DecodeEvent::None => InEvent::BinError, // lone 0x00: empty frame
                _ => InEvent::BinError,
            });
        }
        if b == b'\n' || b == b'\r' {
            if self.overflow {
                self.clear();
                self.cobs.reset();
                return Some(InEvent::TextTooLong);
            }
            if self.staging.is_empty() {
                return None; // skip empty lines (lone \r of \r\n)
            }
            let line = core::str::from_utf8(&self.staging).ok()?;
            let mut out: heapless::String<MAX_LINE> = heapless::String::new();
            out.push_str(line).ok()?;
            self.clear();
            return Some(InEvent::Text(out));
        }
        if !self.overflow && self.staging.push(b).is_err() {
            self.overflow = true;
        }
        None
    }
}

impl Default for Router {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frame::encode_cmd;

    fn feed_all(r: &mut Router, bytes: &[u8]) -> Vec<InEvent> {
        bytes.iter().filter_map(|b| r.feed(*b)).collect()
    }

    #[test]
    fn text_line_with_crlf() {
        let mut r = Router::new();
        let ev = feed_all(&mut r, b"PING\r\n");
        assert_eq!(ev.len(), 1);
        assert!(matches!(&ev[0], InEvent::Text(l) if l.as_str() == "PING"));
    }

    #[test]
    fn binary_ping_split_across_packets() {
        let mut tx = [0u8; 64];
        let n = encode_cmd(&HostCmd::Ping, &mut tx).expect("encode");
        // Split like two 64 B USB packets would (frame is tiny: 1 + rest).
        let mut r = Router::new();
        let mut ev = feed_all(&mut r, &tx[..1]);
        assert!(ev.is_empty());
        ev = feed_all(&mut r, &tx[1..n]);
        assert_eq!(ev, vec![InEvent::Cmd(HostCmd::Ping)]);
    }

    #[test]
    fn mixed_text_then_binary_then_text() {
        let mut tx = [0u8; 64];
        let n = encode_cmd(&HostCmd::GetVer, &mut tx).expect("encode");
        let mut stream = Vec::new();
        stream.extend_from_slice(b"PING\n");
        stream.extend_from_slice(&tx[..n]);
        stream.extend_from_slice(b"HELP\n");
        let mut r = Router::new();
        let ev = feed_all(&mut r, &stream);
        assert_eq!(ev.len(), 3);
        assert!(matches!(&ev[0], InEvent::Text(l) if l.as_str() == "PING"));
        assert_eq!(ev[1], InEvent::Cmd(HostCmd::GetVer));
        assert!(matches!(&ev[2], InEvent::Text(l) if l.as_str() == "HELP"));
    }

    #[test]
    fn corrupt_frame_recovers_and_text_still_works() {
        let mut r = Router::new();
        let ev = feed_all(&mut r, &[0xFF, 0xFF, 0xFF, 0x00]);
        assert_eq!(ev, vec![InEvent::BinError]);
        let ev = feed_all(&mut r, b"PING\n");
        assert!(matches!(&ev[0], InEvent::Text(ref l) if l.as_str() == "PING"));
    }

    #[test]
    fn long_text_reports_toolong_then_resyncs() {
        let mut r = Router::new();
        let mut long = vec![b'A'; MAX_FRAME + 10];
        long.push(b'\n');
        let ev = feed_all(&mut r, &long);
        assert_eq!(ev, vec![InEvent::TextTooLong]);
        let ev = feed_all(&mut r, b"PING\n");
        assert!(matches!(&ev[0], InEvent::Text(ref l) if l.as_str() == "PING"));
    }

    #[test]
    fn lone_nul_is_bin_error_not_hang() {
        let mut r = Router::new();
        assert_eq!(r.feed(0x00), Some(InEvent::BinError));
        let ev = feed_all(&mut r, b"PING\n");
        assert!(matches!(&ev[0], InEvent::Text(ref l) if l.as_str() == "PING"));
    }
}
