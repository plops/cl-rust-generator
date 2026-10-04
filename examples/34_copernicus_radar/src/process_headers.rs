//! Packet-header inspection dump (`copernicus_03_process_packet_headers.cpp`).
//!
//! Prints every header field in decimal, hexadecimal and binary, followed by
//! a colored binary dump of the 68 header bytes. The C++ version loops over
//! all packets with a 16 ms delay and clears the screen between packets; the
//! delay and screen clearing are opt-in here (see `animate`).
//!
//! Note: the C++ `main` never calls this module, and the per-packet derived
//! quantities it computes (times, beam addresses, ...) are never printed, so
//! only the field dump itself is ported. Invoke with `--dump-headers`.

use std::io::Write;
use std::thread;
use std::time::Duration;

use crate::header::{PacketHeader, DUMP_NAMES, FIELD_WIDTHS};
use crate::utils::HEADER_LEN;

/// Dump one packet header: 54 fields plus a colored binary dump.
pub fn dump_packet<W: Write>(w: &mut W, header: &[u8], packet_idx: usize) -> std::io::Result<()> {
    let parsed = PacketHeader::parse(header)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;
    let values = parsed.values();
    writeln!(w, "packet_idx={packet_idx}")?;
    for (i, v) in values.iter().enumerate() {
        let mut bits = String::with_capacity(FIELD_WIDTHS[i] as usize);
        for b in (0..FIELD_WIDTHS[i]).rev() {
            bits.push(if (v >> b) & 1 == 1 { '1' } else { '0' });
        }
        writeln!(
            w,
            "{:>42}{:>12}{:>12} {}",
            format!("{} ", DUMP_NAMES[i]),
            format!("{v}"),
            format!("{v:x}"),
            bits
        )?;
    }
    // Colored binary dump, 4 bytes per line.
    for (i, byte) in header.iter().enumerate().take(HEADER_LEN) {
        let fg = 30 + ((7 + 6 + 62 - i) % (37 - 30));
        let bg = 40 + (i % (47 - 40));
        write!(w, "\x1b[{fg};{bg}m")?;
        for b in (0..8).rev() {
            write!(w, "{}", (byte >> b) & 1)?;
        }
        write!(w, "\x1b[0m ")?;
        if i % 4 == 3 {
            writeln!(w)?;
        }
    }
    writeln!(w, "\x1b[0m")?;
    w.flush()?;
    Ok(())
}

/// Dump all packet headers (`init_process_packet_headers`).
///
/// With `animate`, sleeps 16 ms and clears the screen between packets like
/// the C++ version; otherwise packets are printed back to back.
pub fn process_packet_headers<W: Write>(
    w: &mut W,
    headers: &[[u8; HEADER_LEN]],
    animate: bool,
) -> std::io::Result<()> {
    for (packet_idx, header) in headers.iter().enumerate() {
        dump_packet(w, header, packet_idx)?;
        if animate {
            thread::sleep(Duration::from_micros(16_000));
            write!(w, "\x1b[2J\x1b[1;1H")?;
            w.flush()?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dump_contains_fields_and_binary() {
        let mut header = [0u8; HEADER_LEN];
        header[12..16].copy_from_slice(&[0x35, 0x2E, 0xF8, 0x53]);
        header[65] = 0x12;
        header[66] = 0x34;
        let mut out = Vec::new();
        dump_packet(&mut out, &header, 7).unwrap();
        let text = String::from_utf8(out).unwrap();
        assert!(text.contains("packet_idx=7"));
        assert!(text.contains("sync-marker"));
        assert!(text.contains("352ef853"));
        assert!(text.contains("number-of-quads"));
        assert!(text.contains("0001001000110100")); // 0x1234, 16 bits
        assert!(text.contains("\x1b["));
    }
}
