//! Byte-stream router: one byte stream in, text lines and binary commands out.
//!
//! Framing rule: a `0x00` byte on an empty line opens **binary mode** — bytes
//! accumulate (including `0x0A`/`0x0D`, which COBS does not exclude) until the
//! closing `0x00`, then decode as one postcard+COBS frame. Any other first
//! byte opens **text mode**: bytes accumulate until `\n`/`\r` and decode as a
//! text line; a `0x00` inside text is a protocol violation (`BinError`).
//! Host- and firmware-side shared so the exact dispatch logic is
//! unit-testable on the host.

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
    /// A binary frame was corrupt or oversized, or a `0x00` appeared in text
    /// mode; receiver reset.
    BinError,
}

/// Byte-stream router shared by firmware and tests.
pub struct Router {
    staging: heapless::Vec<u8, MAX_FRAME>,
    overflow: bool,
    binary: bool,
    cobs: CmdReceiver,
}

impl Router {
    pub fn new() -> Self {
        Self {
            staging: heapless::Vec::new(),
            overflow: false,
            binary: false,
            cobs: CmdReceiver::new(),
        }
    }

    fn clear(&mut self) {
        self.staging.clear();
        self.overflow = false;
        self.binary = false;
    }

    /// Feed one byte. Returns an event once a terminator completes an item;
    /// `None` while items are incomplete (or for skipped empty lines and the
    /// binary-mode opening marker).
    pub fn feed(&mut self, b: u8) -> Option<InEvent> {
        if self.binary {
            return self.feed_binary(b);
        }
        if b == 0x00 {
            // Leading marker opens binary mode only on an empty line; text
            // never contains NUL, so bytes before it are a violation.
            if !self.staging.is_empty() || self.overflow {
                self.clear();
                self.cobs.reset();
                return Some(InEvent::BinError);
            }
            self.binary = true;
            self.cobs.reset();
            return None;
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

    fn feed_binary(&mut self, b: u8) -> Option<InEvent> {
        if b == 0x00 {
            // Closing marker: decode the staged COBS payload.
            let event = if self.overflow || self.staging.is_empty() {
                DecodeEvent::DeserError
            } else {
                // Staging is drained; clone to satisfy the borrow checker.
                // The trailing marker completes the COBS frame for the feed.
                let mut frame = self.staging.clone();
                if frame.push(0x00).is_ok() {
                    self.cobs.feed(&frame)
                } else {
                    DecodeEvent::OverFull
                }
            };
            self.clear();
            return Some(match event {
                DecodeEvent::Cmd(cmd) => InEvent::Cmd(cmd),
                _ => InEvent::BinError,
            });
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
    use crate::modes_04::AwgConfig;

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
        // Split like two 64 B USB packets would (marker + rest).
        let mut r = Router::new();
        let mut ev = feed_all(&mut r, &tx[..1]);
        assert!(ev.is_empty());
        ev = feed_all(&mut r, &tx[1..n]);
        assert_eq!(ev, vec![InEvent::Cmd(HostCmd::Ping)]);
    }

    #[test]
    fn binary_frame_with_embedded_newline() {
        // Regression: AwgStart(1000) encodes with a 0x0A payload byte, which
        // the old terminator-decides router ate as a text newline.
        let mut tx = [0u8; 64];
        let cmd = HostCmd::AwgStart(AwgConfig { freq_hz: 1000 });
        let n = encode_cmd(&cmd, &mut tx).expect("encode");
        assert!(tx[1..n - 1].contains(&0x0A));
        let mut r = Router::new();
        let ev = feed_all(&mut r, &tx[..n]);
        assert_eq!(ev, vec![InEvent::Cmd(cmd)]);
    }

    #[test]
    fn binary_frame_with_embedded_cr() {
        // 0x0D inside a frame must not split it either (freq 13 → [0x0D]).
        let mut tx = [0u8; 64];
        let cmd = HostCmd::AwgStart(AwgConfig { freq_hz: 13 });
        let n = encode_cmd(&cmd, &mut tx).expect("encode");
        assert!(tx[1..n - 1].contains(&0x0D));
        let mut r = Router::new();
        let ev = feed_all(&mut r, &tx[..n]);
        assert_eq!(ev, vec![InEvent::Cmd(cmd)]);
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
        // Garbage bytes in text mode, NUL-terminated → BinError, then clean.
        let mut r = Router::new();
        let ev = feed_all(&mut r, &[0xFF, 0xFF, 0xFF, 0x00]);
        assert_eq!(ev, vec![InEvent::BinError]);
        let ev = feed_all(&mut r, b"PING\n");
        assert!(matches!(&ev[0], InEvent::Text(ref l) if l.as_str() == "PING"));
    }

    #[test]
    fn corrupt_binary_frame_recovers() {
        // Marker opens binary mode; garbage payload still ends in BinError.
        let mut r = Router::new();
        let ev = feed_all(&mut r, &[0x00, 0xFF, 0xFF, 0xFF, 0x00]);
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
    fn lone_nul_opens_binary_without_hang() {
        // A lone marker produces no event yet; the next line still works.
        let mut r = Router::new();
        assert_eq!(r.feed(0x00), None);
        // Empty binary frame (immediate close) is a BinError, not a hang.
        assert_eq!(r.feed(0x00), Some(InEvent::BinError));
        let ev = feed_all(&mut r, b"PING\n");
        assert!(matches!(&ev[0], InEvent::Text(ref l) if l.as_str() == "PING"));
    }
}
