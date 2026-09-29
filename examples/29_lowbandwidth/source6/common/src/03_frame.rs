//! `03_frame` — Längen-Framing über beliebige `Read`/`Write`-Streams.
//!
//! Frame = `[u16 LE Länge][Body]`. Der Leser puffert Teil-Frames, damit
//! Socket-Timeouts (`WouldBlock`/`TimedOut`) keine Daten verlieren — so
//! kann ein Thread regelmäßig aufwachen (Heartbeat) ohne Nebenläufigkeit.

use std::io::{self, ErrorKind, Read, Write};

use crate::types::MAX_FRAME;

/// Header-Größe eines Frames.
pub const HEADER: usize = 2;

/// Schreibt einen Frame; liefert die Anzahl Bytes auf der Leitung.
pub fn write_frame(w: &mut impl Write, body: &[u8]) -> io::Result<usize> {
    if body.len() > MAX_FRAME {
        return Err(io::Error::new(ErrorKind::InvalidInput, "Frame zu groß"));
    }
    let mut buf = Vec::with_capacity(HEADER + body.len());
    buf.extend_from_slice(&(body.len() as u16).to_le_bytes());
    buf.extend_from_slice(body);
    w.write_all(&buf)?;
    Ok(buf.len())
}

/// Ergebnis eines Leseversuchs.
#[derive(Debug, PartialEq, Eq)]
pub enum Read1 {
    /// Vollständiger Body.
    Frame(Vec<u8>),
    /// Timeout ohne vollständigen Frame (Teildaten bleiben gepuffert).
    Idle,
}

/// Puffernder Frame-Leser; zählt empfangene Bytes (für `Ack`).
#[derive(Default)]
pub struct FrameReader {
    buf: Vec<u8>,
    /// Summe aller vollständig empfangenen Frame-Bytes (inkl. Header).
    pub rx_bytes: u64,
}

impl FrameReader {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Nächster Frame aus dem Puffer, falls vollständig.
    fn pop(&mut self) -> Option<Vec<u8>> {
        if self.buf.len() < HEADER {
            return None;
        }
        let n = u16::from_le_bytes([self.buf[0], self.buf[1]]) as usize;
        if self.buf.len() < HEADER + n {
            return None;
        }
        let body = self.buf[HEADER..HEADER + n].to_vec();
        self.buf.drain(..HEADER + n);
        self.rx_bytes += (HEADER + n) as u64;
        Some(body)
    }

    /// Liest bis ein Frame komplett ist, EOF (Fehler) oder Timeout (`Idle`).
    pub fn read(&mut self, r: &mut impl Read) -> io::Result<Read1> {
        loop {
            if let Some(f) = self.pop() {
                return Ok(Read1::Frame(f));
            }
            let mut tmp = [0u8; 4096];
            match r.read(&mut tmp) {
                Ok(0) => return Err(ErrorKind::UnexpectedEof.into()),
                Ok(n) => self.buf.extend_from_slice(&tmp[..n]),
                Err(e) if matches!(e.kind(), ErrorKind::WouldBlock | ErrorKind::TimedOut) => {
                    return Ok(Read1::Idle);
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    /// Liefert je Aufruf höchstens ein Byte, danach Timeout-Fehler.
    struct Trickle(Vec<u8>, usize, bool);

    impl Read for Trickle {
        fn read(&mut self, out: &mut [u8]) -> io::Result<usize> {
            self.2 = !self.2;
            if self.2 {
                return Err(ErrorKind::WouldBlock.into());
            }
            if self.1 >= self.0.len() {
                return Ok(0);
            }
            out[0] = self.0[self.1];
            self.1 += 1;
            Ok(1)
        }
    }

    #[test]
    fn roundtrip_several_frames() {
        let mut wire = Vec::new();
        assert_eq!(write_frame(&mut wire, b"abc").unwrap(), 5);
        write_frame(&mut wire, b"").unwrap();
        write_frame(&mut wire, &[7; 1000]).unwrap();
        let mut c = Cursor::new(wire);
        let mut fr = FrameReader::new();
        assert_eq!(fr.read(&mut c).unwrap(), Read1::Frame(b"abc".to_vec()));
        assert_eq!(fr.read(&mut c).unwrap(), Read1::Frame(vec![]));
        assert_eq!(fr.read(&mut c).unwrap(), Read1::Frame(vec![7; 1000]));
        assert_eq!(fr.rx_bytes, 5 + 2 + 1002);
        assert_eq!(
            fr.read(&mut c).unwrap_err().kind(),
            ErrorKind::UnexpectedEof
        );
    }

    #[test]
    fn partial_reads_with_timeouts_lose_nothing() {
        let mut wire = Vec::new();
        write_frame(&mut wire, b"hello").unwrap();
        write_frame(&mut wire, b"world!").unwrap();
        let mut t = Trickle(wire, 0, false);
        let mut fr = FrameReader::new();
        let mut got = Vec::new();
        let mut idles = 0;
        while got.len() < 2 {
            match fr.read(&mut t).unwrap() {
                Read1::Frame(f) => got.push(f),
                Read1::Idle => idles += 1,
            }
        }
        assert_eq!(got, vec![b"hello".to_vec(), b"world!".to_vec()]);
        assert!(idles > 5);
    }

    #[test]
    fn oversized_frame_is_rejected() {
        let mut wire = Vec::new();
        assert!(write_frame(&mut wire, &vec![0; MAX_FRAME + 1]).is_err());
        assert!(wire.is_empty());
    }
}
