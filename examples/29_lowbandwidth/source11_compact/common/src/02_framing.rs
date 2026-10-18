//! `02_framing` — Nachrichtenrahmen über beliebige `Read`/`Write`-Streams.
//!
//! Frame = `[u32 LE Länge][bincode-Body]`. Der Leser puffert Teil-Frames,
//! damit Socket-Timeouts (`WouldBlock`/`TimedOut`) keine Daten verlieren.

use std::io::{self, ErrorKind, Read, Write};

use serde::Serialize;
use serde::de::DeserializeOwned;

use crate::types::MAX_MSG;

/// Header-Größe eines Frames.
pub const HEADER: usize = 4;

/// Kodiert eine Nachricht (nur Body, ohne Rahmen).
pub fn encode_msg<T: Serialize>(m: &T) -> Result<Vec<u8>, String> {
    bincode::serde::encode_to_vec(m, bincode::config::standard()).map_err(|e| e.to_string())
}

/// Dekodiert einen Body.
pub fn decode_msg<T: DeserializeOwned>(b: &[u8]) -> Result<T, String> {
    let (m, _): (T, usize) = bincode::serde::decode_from_slice(b, bincode::config::standard())
        .map_err(|e| e.to_string())?;
    Ok(m)
}

/// Schreibt eine Nachricht als Frame; liefert die Bytes auf der Leitung.
pub fn write_msg(w: &mut impl Write, m: &impl Serialize) -> io::Result<usize> {
    let body = encode_msg(m).map_err(|e| io::Error::new(ErrorKind::InvalidInput, e))?;
    if body.len() > MAX_MSG {
        return Err(io::Error::new(ErrorKind::InvalidInput, "Nachricht zu groß"));
    }
    w.write_all(&(body.len() as u32).to_le_bytes())?;
    w.write_all(&body)?;
    Ok(HEADER + body.len())
}

/// Ergebnis eines Leseversuchs.
#[derive(Debug, PartialEq, Eq)]
pub enum Read1 {
    /// Vollständiger Body.
    Frame(Vec<u8>),
    /// Timeout ohne vollständigen Frame (Teildaten bleiben gepuffert).
    Idle,
}

/// Puffernder Frame-Leser.
#[derive(Default)]
pub struct FrameReader {
    buf: Vec<u8>,
}

impl FrameReader {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Nächster Frame aus dem Puffer, falls vollständig.
    fn pop(&mut self) -> io::Result<Option<Vec<u8>>> {
        if self.buf.len() < HEADER {
            return Ok(None);
        }
        let n = u32::from_le_bytes(self.buf[..HEADER].try_into().unwrap()) as usize;
        if n > MAX_MSG {
            return Err(io::Error::new(
                ErrorKind::InvalidData,
                "Frame-Länge zu groß",
            ));
        }
        if self.buf.len() < HEADER + n {
            return Ok(None);
        }
        let body = self.buf[HEADER..HEADER + n].to_vec();
        self.buf.drain(..HEADER + n);
        Ok(Some(body))
    }

    /// Liest bis ein Frame komplett ist, EOF (Fehler) oder Timeout (`Idle`).
    pub fn read(&mut self, r: &mut impl Read) -> io::Result<Read1> {
        loop {
            if let Some(f) = self.pop()? {
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

    /// Liest genau eine Nachricht (`Idle` bei Timeout ohne Daten).
    pub fn read_msg<T: DeserializeOwned>(&mut self, r: &mut impl Read) -> io::Result<Option<T>> {
        match self.read(r)? {
            Read1::Idle => Ok(None),
            Read1::Frame(b) => decode_msg(&b)
                .map(Some)
                .map_err(|e| io::Error::new(ErrorKind::InvalidData, e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ClientMsg, ServerMsg};
    use std::io::Cursor;

    #[test]
    fn auth_challenge_survives_frame_roundtrip() {
        let m = ServerMsg::Challenge {
            version: 4,
            nonce: *b"0123456789abcdef",
        };
        let mut wire = Vec::new();
        write_msg(&mut wire, &m).unwrap();
        let mut fr = FrameReader::new();
        let back: ServerMsg = fr.read_msg(&mut Cursor::new(wire)).unwrap().unwrap();
        assert_eq!(back, m);
    }

    #[test]
    fn oversize_frame_is_rejected() {
        let mut fr = FrameReader::new();
        let mut bad = (MAX_MSG as u32 + 1).to_le_bytes().to_vec();
        bad.extend_from_slice(&[0u8; 8]);
        let r = fr.read(&mut Cursor::new(bad));
        assert!(r.is_err());
    }

    #[test]
    fn idle_on_timeout_without_data() {
        struct Timeout;
        impl std::io::Read for Timeout {
            fn read(&mut self, _b: &mut [u8]) -> std::io::Result<usize> {
                Err(std::io::Error::from(std::io::ErrorKind::TimedOut))
            }
        }
        let mut fr = FrameReader::new();
        let m: Option<ClientMsg> = fr.read_msg(&mut Timeout).unwrap();
        assert_eq!(m, None);
    }

    #[test]
    fn oversize_write_is_rejected() {
        let big = vec![0u8; MAX_MSG + 1];
        let mut wire = Vec::new();
        assert!(write_msg(&mut wire, &big).is_err());
    }

    #[test]
    fn client_hello_roundtrip() {
        let m = ClientMsg::Hello { version: 4 };
        let mut wire = Vec::new();
        write_msg(&mut wire, &m).unwrap();
        let mut fr = FrameReader::new();
        let back: ClientMsg = fr.read_msg(&mut Cursor::new(wire)).unwrap().unwrap();
        assert_eq!(back, m);
    }
}
