//! `02_framing` — Nachrichtenrahmen über beliebige `Read`/`Write`-Streams.
//!
//! Frame = `[u32 LE Länge][bincode-Body]`. Der Leser puffert Teil-Frames,
//! damit Socket-Timeouts (`WouldBlock`/`TimedOut`) keine Daten verlieren —
//! so kann ein Thread regelmäßig aufwachen ohne Nebenläufigkeit.

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
    /// Summe aller vollständig empfangenen Frame-Bytes (inkl. Header).
    pub rx_bytes: u64,
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
        self.rx_bytes += (HEADER + n) as u64;
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
    use crate::types::{ClientMsg, Rect, ServerMsg, TextItem};
    use std::io::Cursor;

    fn item(text: &str) -> TextItem {
        TextItem {
            rect: Rect::new(1, 2, 300, 16),
            fg: [0, 0, 0],
            bg: [255, 255, 250],
            text: text.into(),
        }
    }

    fn server_samples() -> Vec<ServerMsg> {
        vec![
            ServerMsg::Hello { w: 640, h: 640 },
            ServerMsg::ClearText,
            ServerMsg::AddText(item("Hallo Welt ä€𝄞")),
            ServerMsg::Tile {
                x: 0,
                y: 64,
                w: 64,
                h: 64,
                data: vec![1, 2, 3, 255],
            },
        ]
    }

    fn client_samples() -> Vec<ClientMsg> {
        vec![
            ClientMsg::Hello { version: 1 },
            ClientMsg::MouseMove { x: 639, y: 0 },
            ClientMsg::Button {
                button: 3,
                down: true,
            },
            ClientMsg::Text("zeile1\nzeile2".into()),
            ClientMsg::Key {
                key: "Enter".into(),
                down: false,
            },
        ]
    }

    #[test]
    fn roundtrip_all_variants() {
        for m in server_samples() {
            assert_eq!(
                decode_msg::<ServerMsg>(&encode_msg(&m).unwrap()).unwrap(),
                m
            );
        }
        for m in client_samples() {
            assert_eq!(
                decode_msg::<ClientMsg>(&encode_msg(&m).unwrap()).unwrap(),
                m
            );
        }
    }

    #[test]
    fn truncated_body_is_an_error() {
        for m in server_samples() {
            let b = encode_msg(&m).unwrap();
            for n in 0..b.len() {
                assert!(decode_msg::<ServerMsg>(&b[..n]).is_err(), "{m:?} bei {n}");
            }
        }
    }

    /// Liefert je zweitem Aufruf ein Byte, sonst Timeout-Fehler.
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
    fn framed_roundtrip_several_messages() {
        let mut wire = Vec::new();
        for m in server_samples() {
            write_msg(&mut wire, &m).unwrap();
        }
        let mut c = Cursor::new(wire);
        let mut fr = FrameReader::new();
        for m in server_samples() {
            let got: Option<ServerMsg> = fr.read_msg(&mut c).unwrap();
            assert_eq!(got.as_ref(), Some(&m));
        }
        assert!(fr.rx_bytes > 0);
        assert_eq!(
            fr.read(&mut c).unwrap_err().kind(),
            ErrorKind::UnexpectedEof
        );
    }

    #[test]
    fn partial_reads_with_timeouts_lose_nothing() {
        let mut wire = Vec::new();
        write_msg(&mut wire, &ServerMsg::Hello { w: 1, h: 2 }).unwrap();
        write_msg(&mut wire, &ClientMsg::Text("hi".into())).unwrap();
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
        assert!(idles > 5);
        assert_eq!(
            decode_msg::<ServerMsg>(&got[0]).unwrap(),
            ServerMsg::Hello { w: 1, h: 2 }
        );
    }

    #[test]
    fn oversized_length_is_rejected() {
        let mut fr = FrameReader::new();
        let mut c = Cursor::new((MAX_MSG as u32 + 1).to_le_bytes());
        assert_eq!(fr.read(&mut c).unwrap_err().kind(), ErrorKind::InvalidData);
    }
}
