//! Text-mode command parser: `HELP|PING|GET VER|GET UID`, `\n`/`\r` terminated.

use crate::{FW_VER, MAX_LINE, PROTO_VER};

/// Build the reply for a received text line. `GET UID`/`G UID` is answered
/// with the caller-provided UID hex (the real chip UID on firmware);
/// everything else goes through [`handle_line`].
pub fn reply_text(line: &str, uid_hex: &str) -> heapless::String<96> {
    let trimmed = line.trim();
    if trimmed.eq_ignore_ascii_case("GET UID") || trimmed.eq_ignore_ascii_case("G UID") {
        let mut s: heapless::String<96> = heapless::String::new();
        let _ = core::fmt::write(&mut s, format_args!("OK UID {}", uid_hex));
        s
    } else {
        handle_line(line)
    }
}

/// Parse one stripped text line, return the reply line (without terminator).
pub fn handle_line(line: &str) -> heapless::String<96> {
    let mut out: heapless::String<96> = heapless::String::new();
    let cmd = line.trim();
    // Split into whitespace-separated tokens, case-insensitive on the verb.
    let mut parts = cmd.split_whitespace();
    let mut verb: heapless::String<16> = heapless::String::new();
    let _ = verb.push_str(parts.next().unwrap_or(""));
    verb.make_ascii_uppercase();
    let mut arg1: heapless::String<16> = heapless::String::new();
    let _ = arg1.push_str(parts.next().unwrap_or(""));
    arg1.make_ascii_uppercase();
    let reply: &str = match (verb.as_str(), arg1.as_str()) {
        ("H", _) | ("HELP", _) => "OK HELP PING GET-VER GET-UID",
        ("PING", _) => "PONG",
        ("G", "VER") | ("GET", "VER") => "OK VER",
        ("G", "UID") | ("GET", "UID") => "OK UID",
        _ => "ERR UNKNOWN",
    };
    // Append details for VER without growing the match table.
    if reply == "OK VER" {
        let _ = core::fmt::write(
            &mut out,
            format_args!("OK VER proto={} fw={}", PROTO_VER, FW_VER),
        );
    } else {
        let _ = out.push_str(reply);
    }
    out
}

/// Incremental line accumulator: feed raw bytes, get completed lines.
/// Overlong lines are dropped and reported as `Some(Err(Overflow))` once,
/// then resync to the next terminator.
pub struct LineAcc {
    buf: heapless::Vec<u8, { MAX_LINE + 1 }>,
    overflow: bool,
}

#[derive(Debug, PartialEq)]
pub enum LineEvent {
    Line(heapless::String<MAX_LINE>),
    TooLong,
}

impl LineAcc {
    pub fn new() -> Self {
        Self {
            buf: heapless::Vec::new(),
            overflow: false,
        }
    }

    /// Feed one byte. `\n` or `\r` completes a line (empty lines are skipped → None).
    pub fn feed(&mut self, b: u8) -> Option<LineEvent> {
        if b == b'\n' || b == b'\r' {
            if self.overflow {
                self.overflow = false;
                self.buf.clear();
                return Some(LineEvent::TooLong);
            }
            if self.buf.is_empty() {
                return None;
            }
            let s = core::str::from_utf8(&self.buf).ok()?;
            let mut line: heapless::String<MAX_LINE> = heapless::String::new();
            line.push_str(s).ok()?;
            self.buf.clear();
            return Some(LineEvent::Line(line));
        }
        if self.overflow {
            return None;
        }
        if self.buf.push(b).is_err() {
            self.overflow = true;
        }
        None
    }
}

impl Default for LineAcc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn feed_str(acc: &mut LineAcc, s: &str) -> Vec<Option<LineEvent>> {
        s.bytes().map(|b| acc.feed(b)).collect()
    }

    #[test]
    fn ping_variants() {
        assert_eq!(handle_line("PING").as_str(), "PONG");
        assert_eq!(handle_line("ping\n").as_str(), "PONG");
        assert_eq!(handle_line("  Ping  ").as_str(), "PONG");
    }

    #[test]
    fn help_aliases() {
        assert!(handle_line("H").as_str().starts_with("OK HELP"));
        assert!(handle_line("help").as_str().starts_with("OK HELP"));
    }

    #[test]
    fn get_ver_uid() {
        assert!(handle_line("GET VER").as_str().contains("proto=1"));
        assert_eq!(handle_line("G UID").as_str(), "OK UID");
    }

    #[test]
    fn reply_text_uid_branch() {
        assert_eq!(reply_text("GET UID", "AABBCC").as_str(), "OK UID AABBCC");
        assert_eq!(reply_text("g uid", "AABBCC").as_str(), "OK UID AABBCC");
        assert_eq!(reply_text("PING", "AABBCC").as_str(), "PONG");
        assert!(reply_text("GET VER", "AABBCC").as_str().contains("proto=1"));
    }

    #[test]
    fn unknown_and_empty() {
        assert_eq!(handle_line("FOO").as_str(), "ERR UNKNOWN");
        assert_eq!(handle_line("").as_str(), "ERR UNKNOWN");
        assert_eq!(handle_line("GET FOO").as_str(), "ERR UNKNOWN");
    }

    #[test]
    fn line_terminators() {
        let mut acc = LineAcc::new();
        let mut lines = vec![];
        for ev in feed_str(&mut acc, "PING\nHELP\rPING\r\n\n") {
            if let Some(LineEvent::Line(l)) = ev {
                lines.push(l);
            }
        }
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].as_str(), "PING");
    }

    #[test]
    fn too_long_resyncs() {
        let mut acc = LineAcc::new();
        let long = "A".repeat(MAX_LINE + 20);
        for b in long.bytes() {
            // Overflow is only reported at the terminator, feeding is silent.
            assert_eq!(acc.feed(b), None);
        }
        // Terminator after overflow reports TooLong once…
        assert_eq!(acc.feed(b'\n'), Some(LineEvent::TooLong));
        // …and the accumulator works again afterwards.
        let mut got = None;
        for b in b"PING\n".iter() {
            if let Some(ev) = acc.feed(*b) {
                got = Some(ev);
            }
        }
        assert!(matches!(got, Some(LineEvent::Line(_))));
    }
}
