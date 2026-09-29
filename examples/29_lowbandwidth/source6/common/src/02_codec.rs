//! `02_codec` — Binärkodierung der Nachrichten (Body = `[Typ][Payload]`).
//!
//! Little-Endian, Strings/Listen mit `u16`-Länge. Kein serde: das hält den
//! Client klein und das Format exakt kontrollierbar.

use crate::types::{ClientMsg, Input, Rect, ServerMsg, TextItem};

/// Dekodierfehler (abgeschnitten, unbekannter Typ, ungültiges UTF-8).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodecError(pub &'static str);

impl std::fmt::Display for CodecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "codec: {}", self.0)
    }
}

impl std::error::Error for CodecError {}

type R<T> = Result<T, CodecError>;

/// Schreibpuffer mit typisierten `put_*`.
#[derive(Default)]
struct W(Vec<u8>);

impl W {
    fn u8(&mut self, v: u8) -> &mut Self {
        self.0.push(v);
        self
    }
    fn u16(&mut self, v: u16) -> &mut Self {
        self.0.extend_from_slice(&v.to_le_bytes());
        self
    }
    fn u32(&mut self, v: u32) -> &mut Self {
        self.0.extend_from_slice(&v.to_le_bytes());
        self
    }
    fn u64(&mut self, v: u64) -> &mut Self {
        self.0.extend_from_slice(&v.to_le_bytes());
        self
    }
    fn bytes(&mut self, b: &[u8]) -> &mut Self {
        let n = b.len().min(u16::MAX as usize);
        self.u16(n as u16);
        self.0.extend_from_slice(&b[..n]);
        self
    }
    fn str(&mut self, s: &str) -> &mut Self {
        // Auf eine Zeichengrenze kürzen, damit UTF-8 gültig bleibt.
        let mut n = s.len().min(u16::MAX as usize);
        while !s.is_char_boundary(n) {
            n -= 1;
        }
        self.bytes(&s.as_bytes()[..n])
    }
    fn rect(&mut self, r: &Rect) -> &mut Self {
        self.u16(r.x).u16(r.y).u16(r.w).u16(r.h)
    }
    fn rgb(&mut self, c: [u8; 3]) -> &mut Self {
        self.0.extend_from_slice(&c);
        self
    }
}

/// Lesecursor mit Grenzprüfung.
struct Rd<'a>(&'a [u8]);

impl<'a> Rd<'a> {
    fn take(&mut self, n: usize) -> R<&'a [u8]> {
        if self.0.len() < n {
            return Err(CodecError("abgeschnitten"));
        }
        let (a, b) = self.0.split_at(n);
        self.0 = b;
        Ok(a)
    }
    fn u8(&mut self) -> R<u8> {
        Ok(self.take(1)?[0])
    }
    fn u16(&mut self) -> R<u16> {
        Ok(u16::from_le_bytes(self.take(2)?.try_into().unwrap()))
    }
    fn u32(&mut self) -> R<u32> {
        Ok(u32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }
    fn u64(&mut self) -> R<u64> {
        Ok(u64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }
    fn bytes(&mut self) -> R<&'a [u8]> {
        let n = self.u16()? as usize;
        self.take(n)
    }
    fn str(&mut self) -> R<String> {
        std::str::from_utf8(self.bytes()?)
            .map(str::to_owned)
            .map_err(|_| CodecError("utf8"))
    }
    fn rect(&mut self) -> R<Rect> {
        Ok(Rect::new(
            self.u16()?,
            self.u16()?,
            self.u16()?,
            self.u16()?,
        ))
    }
    fn rgb(&mut self) -> R<[u8; 3]> {
        Ok(self.take(3)?.try_into().unwrap())
    }
    fn end(&self) -> R<()> {
        if self.0.is_empty() {
            Ok(())
        } else {
            Err(CodecError("überzählige Bytes"))
        }
    }
}

fn put_item(w: &mut W, t: &TextItem) {
    w.u32(t.id).rect(&t.rect).rgb(t.fg).rgb(t.bg).str(&t.text);
}

fn get_item(r: &mut Rd) -> R<TextItem> {
    Ok(TextItem {
        id: r.u32()?,
        rect: r.rect()?,
        fg: r.rgb()?,
        bg: r.rgb()?,
        text: r.str()?,
    })
}

impl ServerMsg {
    /// Kodiert den Body (`[Typ][Payload]`).
    #[must_use]
    pub fn encode(&self) -> Vec<u8> {
        let mut w = W::default();
        match self {
            Self::Hello {
                server_id,
                w: ww,
                h,
                resumed,
            } => {
                w.u8(1)
                    .u64(*server_id)
                    .u16(*ww)
                    .u16(*h)
                    .u8(u8::from(*resumed));
            }
            Self::Text { seq, remove, add } => {
                w.u8(2).u32(*seq).u16(remove.len() as u16);
                for id in remove {
                    w.u32(*id);
                }
                w.u16(add.len() as u16);
                for t in add {
                    put_item(&mut w, t);
                }
            }
            Self::TileStart {
                tile_id,
                seq,
                rect,
                len,
            } => {
                w.u8(3).u32(*tile_id).u32(*seq).rect(rect).u32(*len);
            }
            Self::TileData {
                tile_id,
                offset,
                data,
            } => {
                w.u8(4).u32(*tile_id).u32(*offset).bytes(data);
            }
            Self::Clear => {
                w.u8(5);
            }
            Self::Ping { t } => {
                w.u8(6).u32(*t);
            }
            Self::Pong { t } => {
                w.u8(7).u32(*t);
            }
            Self::Stats {
                rate,
                backlog,
                tiles,
            } => {
                w.u8(8).u32(*rate).u32(*backlog).u32(*tiles);
            }
        }
        w.0
    }

    /// Dekodiert einen Body.
    pub fn decode(b: &[u8]) -> R<Self> {
        let mut r = Rd(b);
        let m = match r.u8()? {
            1 => Self::Hello {
                server_id: r.u64()?,
                w: r.u16()?,
                h: r.u16()?,
                resumed: r.u8()? != 0,
            },
            2 => {
                let seq = r.u32()?;
                let n = r.u16()?;
                let remove = (0..n).map(|_| r.u32()).collect::<R<_>>()?;
                let n = r.u16()?;
                let add = (0..n).map(|_| get_item(&mut r)).collect::<R<_>>()?;
                Self::Text { seq, remove, add }
            }
            3 => Self::TileStart {
                tile_id: r.u32()?,
                seq: r.u32()?,
                rect: r.rect()?,
                len: r.u32()?,
            },
            4 => Self::TileData {
                tile_id: r.u32()?,
                offset: r.u32()?,
                data: r.bytes()?.to_vec(),
            },
            5 => Self::Clear,
            6 => Self::Ping { t: r.u32()? },
            7 => Self::Pong { t: r.u32()? },
            8 => Self::Stats {
                rate: r.u32()?,
                backlog: r.u32()?,
                tiles: r.u32()?,
            },
            _ => return Err(CodecError("unbekannter Server-Typ")),
        };
        r.end()?;
        Ok(m)
    }
}

impl ClientMsg {
    /// Kodiert den Body (`[Typ][Payload]`).
    #[must_use]
    pub fn encode(&self) -> Vec<u8> {
        let mut w = W::default();
        match self {
            Self::Hello {
                version,
                server_id,
                seq,
            } => {
                w.u8(1).u16(*version).u64(*server_id).u32(*seq);
            }
            Self::Input(i) => {
                w.u8(2);
                match i {
                    Input::MouseMove { x, y } => w.u8(1).u16(*x).u16(*y),
                    Input::Button { button, down } => w.u8(2).u8(*button).u8(u8::from(*down)),
                    Input::Wheel { dy } => w.u8(3).u8(*dy as u8),
                    Input::Key { keysym, mods } => w.u8(4).u32(*keysym).u8(*mods),
                    Input::Char { ch } => w.u8(5).u32(*ch),
                    Input::Text(s) => w.u8(6).str(s),
                };
            }
            Self::Ack { rx_bytes, seq } => {
                w.u8(3).u64(*rx_bytes).u32(*seq);
            }
            Self::Ping { t } => {
                w.u8(4).u32(*t);
            }
            Self::Pong { t } => {
                w.u8(5).u32(*t);
            }
        }
        w.0
    }

    /// Dekodiert einen Body.
    pub fn decode(b: &[u8]) -> R<Self> {
        let mut r = Rd(b);
        let m = match r.u8()? {
            1 => Self::Hello {
                version: r.u16()?,
                server_id: r.u64()?,
                seq: r.u32()?,
            },
            2 => Self::Input(match r.u8()? {
                1 => Input::MouseMove {
                    x: r.u16()?,
                    y: r.u16()?,
                },
                2 => Input::Button {
                    button: r.u8()?,
                    down: r.u8()? != 0,
                },
                3 => Input::Wheel { dy: r.u8()? as i8 },
                4 => Input::Key {
                    keysym: r.u32()?,
                    mods: r.u8()?,
                },
                5 => Input::Char { ch: r.u32()? },
                6 => Input::Text(r.str()?),
                _ => return Err(CodecError("unbekannter Input-Typ")),
            }),
            3 => Self::Ack {
                rx_bytes: r.u64()?,
                seq: r.u32()?,
            },
            4 => Self::Ping { t: r.u32()? },
            5 => Self::Pong { t: r.u32()? },
            _ => return Err(CodecError("unbekannter Client-Typ")),
        };
        r.end()?;
        Ok(m)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn item(id: u32, text: &str) -> TextItem {
        TextItem {
            id,
            rect: Rect::new(1, 2, 300, 16),
            fg: [0, 0, 0],
            bg: [255, 255, 250],
            text: text.into(),
        }
    }

    fn server_samples() -> Vec<ServerMsg> {
        vec![
            ServerMsg::Hello {
                server_id: u64::MAX - 3,
                w: 640,
                h: 640,
                resumed: true,
            },
            ServerMsg::Text {
                seq: 7,
                remove: vec![1, 2, 99],
                add: vec![item(3, "Hallo Welt ä€𝄞"), item(4, "")],
            },
            ServerMsg::TileStart {
                tile_id: 5,
                seq: 8,
                rect: Rect::new(32, 64, 128, 96),
                len: 4321,
            },
            ServerMsg::TileData {
                tile_id: 5,
                offset: 512,
                data: vec![1, 2, 3, 255],
            },
            ServerMsg::Clear,
            ServerMsg::Ping { t: 12345 },
            ServerMsg::Pong { t: 1 },
            ServerMsg::Stats {
                rate: 6000,
                backlog: 1200,
                tiles: 3,
            },
        ]
    }

    fn client_samples() -> Vec<ClientMsg> {
        vec![
            ClientMsg::Hello {
                version: 1,
                server_id: 42,
                seq: 9,
            },
            ClientMsg::Input(Input::MouseMove { x: 639, y: 0 }),
            ClientMsg::Input(Input::Button {
                button: 3,
                down: true,
            }),
            ClientMsg::Input(Input::Wheel { dy: -2 }),
            ClientMsg::Input(Input::Key {
                keysym: 0xff0d,
                mods: 5,
            }),
            ClientMsg::Input(Input::Char { ch: 'ß' as u32 }),
            ClientMsg::Input(Input::Text("zeile1\nzeile2".into())),
            ClientMsg::Ack {
                rx_bytes: 1 << 40,
                seq: 3,
            },
            ClientMsg::Ping { t: 5 },
            ClientMsg::Pong { t: 6 },
        ]
    }

    #[test]
    fn server_roundtrip_all_variants() {
        for m in server_samples() {
            assert_eq!(ServerMsg::decode(&m.encode()).unwrap(), m);
        }
    }

    #[test]
    fn client_roundtrip_all_variants() {
        for m in client_samples() {
            assert_eq!(ClientMsg::decode(&m.encode()).unwrap(), m);
        }
    }

    #[test]
    fn truncated_and_trailing_bytes_are_errors() {
        for m in server_samples() {
            let b = m.encode();
            for n in 0..b.len() {
                assert!(ServerMsg::decode(&b[..n]).is_err(), "{m:?} bei {n}");
            }
            let mut long = b.clone();
            long.push(0);
            assert!(ServerMsg::decode(&long).is_err());
        }
        for m in client_samples() {
            let b = m.encode();
            for n in 0..b.len() {
                assert!(ClientMsg::decode(&b[..n]).is_err());
            }
        }
    }

    #[test]
    fn unknown_type_and_bad_utf8() {
        assert!(ServerMsg::decode(&[200]).is_err());
        assert!(ClientMsg::decode(&[2, 99]).is_err());
        assert!(ClientMsg::decode(&[2, 6, 2, 0, 0xff, 0xfe]).is_err());
    }

    #[test]
    fn long_strings_are_cut_on_char_boundary() {
        let s = "ä".repeat(40_000); // 80 000 Byte > u16::MAX
        let m = ClientMsg::Input(Input::Text(s));
        let ClientMsg::Input(Input::Text(back)) = ClientMsg::decode(&m.encode()).unwrap() else {
            panic!()
        };
        assert!(back.len() <= u16::MAX as usize && back.chars().all(|c| c == 'ä'));
    }

    #[test]
    fn text_item_is_compact() {
        // 4 id + 8 rect + 6 Farben + 2 Länge + Text: Tippen bleibt billig.
        let m = ServerMsg::Text {
            seq: 1,
            remove: vec![1],
            add: vec![item(2, "hello")],
        };
        assert_eq!(m.encode().len(), 1 + 4 + 2 + 4 + 2 + 20 + 5);
    }
}
