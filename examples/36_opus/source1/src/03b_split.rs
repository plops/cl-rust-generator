//! Paket-Anatomie: Frames, VBR/CBR, Padding (RFC 6716 §3.2).
//!
//! Zerlegt ein Opus-Paket in TOC, Laengen-Bytes, Frame-Payloads und
//! Padding. Reine Funktion ueber Bytes (kein Decoder noetig).

use crate::types::Error;

/// Zerlegtes Paket (Slices in das Original).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Split<'a> {
    /// Packing-Code 0–3 (TOC-Bits 0–1).
    pub code: u8,
    /// Variabel (Code 2, Code 3 mit VBR-Bit).
    pub vbr: bool,
    /// Frame-Payloads in Reihenfolge (ohne TOC/Laengen).
    pub frames: Vec<&'a [u8]>,
    /// Padding-Bytes (Nutzdaten-fremd, ohne Laengen-Bytes).
    pub pad_bytes: usize,
    /// Framing-Bytes (TOC + Count + Laengen + Pad-Laengen).
    pub header_bytes: usize,
}

/// Maximale Frame-Nutzdaten (RFC: 1275).
const MAX_FRAME: usize = 1275;
/// Maximale Paket-Dauer in ms (120 ms).
const MAX_MS: f64 = 120.0;

/// Laengen-Feld lesen: 1 Byte (< 252), sonst 2 (`4*b1 + b0`).
fn parse_size(data: &[u8]) -> Result<(usize, usize), Error> {
    match data {
        [] => Err(Error::BadPacket("Laenge fehlt")),
        [b0, ..] if *b0 < 252 => Ok((1, usize::from(*b0))),
        [b0, b1, ..] => Ok((2, usize::from(*b1) * 4 + usize::from(*b0))),
        [_] => Err(Error::BadPacket("Laenge abgeschnitten")),
    }
}

/// Paket zerlegen (Fehler bei jeder RFC-Verletzung).
pub fn split(packet: &[u8]) -> Result<Split<'_>, Error> {
    let toc = crate::toc::parse(packet)?; // Prueft M >= 1, Dauer-Tabelle.
    if toc.duration_ms > MAX_MS {
        return Err(Error::BadPacket("Paket laenger als 120 ms"));
    }
    let code = packet[0] & 0x03;
    let mut pos = 1usize;
    let mut frames: Vec<&[u8]> = Vec::new();
    let mut pad_bytes = 0usize;
    let mut vbr = false;
    match code {
        0 => {
            frames.push(&packet[pos..]);
        }
        1 => {
            let rest = &packet[pos..];
            if !rest.len().is_multiple_of(2) {
                return Err(Error::BadPacket("Code 1 ungerade"));
            }
            let half = rest.len() / 2;
            frames.push(&rest[..half]);
            frames.push(&rest[half..]);
        }
        2 => {
            vbr = true;
            let (n, s0) = parse_size(&packet[pos..])?;
            pos += n;
            let rest = &packet[pos..];
            if s0 > rest.len() {
                return Err(Error::BadPacket("Code 2 Groesse"));
            }
            frames.push(&rest[..s0]);
            frames.push(&rest[s0..]);
        }
        _ => {
            let count_byte = *packet.get(pos).ok_or(Error::BadPacket("Count fehlt"))?;
            pos += 1;
            let m = usize::from(count_byte & 0x3f).max(1); // 0 vorab abgewiesen.
            vbr = count_byte & 0x80 != 0;
            // Padding-Laenge (vorwaerts): 255-Ketten, Ende < 255.
            if count_byte & 0x40 != 0 {
                loop {
                    let p = *packet.get(pos).ok_or(Error::BadPacket("Pad fehlt"))?;
                    pos += 1;
                    if p == 255 {
                        pad_bytes += 254;
                    } else {
                        pad_bytes += usize::from(p);
                        break;
                    }
                }
            }
            // Nutzlast endet vor dem Padding am Paketende.
            let end = packet
                .len()
                .checked_sub(pad_bytes)
                .ok_or(Error::BadPacket("Pad groesser als Paket"))?;
            if pos > end {
                return Err(Error::BadPacket("Kopf groesser als Paket"));
            }
            let header_end;
            if vbr {
                // Alle M-1 Laengen zuerst, dann die Frames am Stueck.
                let mut sizes = Vec::with_capacity(m);
                for _ in 0..m - 1 {
                    let (n, s) = parse_size(&packet[pos..end])
                        .map_err(|_| Error::BadPacket("VBR-Laenge"))?;
                    pos += n;
                    sizes.push(s);
                }
                header_end = pos;
                let mut off = pos;
                for s in sizes {
                    let frame_end = off
                        .checked_add(s)
                        .filter(|e| *e <= end)
                        .ok_or(Error::BadPacket("VBR-Groesse"))?;
                    frames.push(&packet[off..frame_end]);
                    off = frame_end;
                }
                frames.push(&packet[off..end]);
            } else {
                header_end = pos;
                let rest = end - pos;
                if !rest.is_multiple_of(m) {
                    return Err(Error::BadPacket("Code 3 CBR ungleich"));
                }
                let each = rest / m;
                let mut off = pos;
                for _ in 0..m {
                    frames.push(&packet[off..off + each]);
                    off += each;
                }
            }
            pos = header_end;
        }
    }
    if frames.iter().any(|f| f.len() > MAX_FRAME) {
        return Err(Error::BadPacket("Frame ueber 1275 B"));
    }
    Ok(Split {
        code,
        vbr,
        frames,
        pad_bytes,
        header_bytes: pos,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// TOC Config 31 (CELT 20 ms) + Code.
    fn toc(code: u8) -> u8 {
        0xf8 | code
    }

    #[test]
    fn code0_single_frame() {
        let p = [toc(0), 1, 2, 3];
        let s = split(&p).unwrap();
        assert_eq!((s.code, s.vbr), (0, false));
        let want: &[u8] = &[1, 2, 3];
        assert_eq!(s.frames, vec![want]);
        assert_eq!((s.pad_bytes, s.header_bytes), (0, 1));
    }

    #[test]
    fn code1_even_split_and_odd_error() {
        let p = [toc(1), 1, 2, 3, 4];
        let s = split(&p).unwrap();
        assert_eq!(s.frames.len(), 2);
        assert_eq!(s.frames[0].len(), 2);
        assert!(split(&[toc(1), 1, 2, 3]).is_err());
    }

    #[test]
    fn code2_both_length_forms() {
        // 1-Byte-Laenge: Frame 0 = 2 B, Rest = Frame 1.
        let p = [toc(2), 2, 9, 9, 8, 8, 8];
        let s = split(&p).unwrap();
        assert!(s.vbr);
        assert_eq!(s.frames.len(), 2);
        assert_eq!((s.frames[0].len(), s.frames[1].len()), (2, 3));
        assert_eq!(s.header_bytes, 2);
        // 2-Byte-Laenge: 4*1+252 = 256 B.
        let mut p = vec![toc(2), 252, 1];
        p.extend(vec![7u8; 300]);
        let s = split(&p).unwrap();
        assert_eq!((s.frames[0].len(), s.frames[1].len()), (256, 44));
        // Groesse passt nicht.
        assert!(split(&[toc(2), 200, 1, 2]).is_err());
        // Abgeschnittene 2-Byte-Laenge.
        assert!(split(&[toc(2), 252]).is_err());
    }

    #[test]
    fn code3_cbr_vbr_padding() {
        // CBR, M=3: 6 B / 3 = 2 B je Frame.
        let p = [toc(3), 0x03, 1, 2, 3, 4, 5, 6];
        let s = split(&p).unwrap();
        assert_eq!((s.code, s.vbr), (3, false));
        assert_eq!(s.frames.len(), 3);
        assert!(s.frames.iter().all(|f| f.len() == 2));
        // CBR ungleich (4 B / 3) → Fehler.
        assert!(split(&[toc(3), 0x03, 1, 2, 3, 4]).is_err());
        // VBR, M=2: eine Laenge (3 B), Rest letzter Frame.
        let p = [toc(3), 0x82, 3, 1, 2, 3, 4, 5];
        let s = split(&p).unwrap();
        assert!(s.vbr);
        assert_eq!((s.frames[0].len(), s.frames[1].len()), (3, 2));
        // Padding: 2 Pad-Bytes am Ende, Laenge 2 vorneweg.
        let p = [toc(3), 0x41, 2, 0xaa, 0xbb, 0, 0];
        let s = split(&p).unwrap();
        assert_eq!(s.pad_bytes, 2);
        let want: &[u8] = &[0xaa, 0xbb];
        assert_eq!(s.frames, vec![want]);
        assert_eq!(s.header_bytes, 3); // TOC + Count + Padlen.
        // M=0 → Fehler (aus TOC-Parser).
        assert!(split(&[toc(3), 0x80]).is_err());
    }

    #[test]
    fn partition_covers_packet_exactly() {
        let cases: &[&[u8]] = &[
            &[toc(0), 1, 2, 3],
            &[toc(1), 1, 2, 3, 4],
            &[toc(2), 2, 9, 9, 8, 8, 8],
            &[toc(3), 0x03, 1, 2, 3, 4, 5, 6],
            &[toc(3), 0x82, 3, 1, 2, 3, 4, 5],
            &[toc(3), 0x41, 2, 0xaa, 0xbb, 0, 0],
        ];
        for p in cases {
            let s = split(p).unwrap();
            let audio: usize = s.frames.iter().map(|f| f.len()).sum();
            assert_eq!(s.header_bytes + audio + s.pad_bytes, p.len());
        }
    }
}
