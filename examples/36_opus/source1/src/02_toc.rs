//! TOC-Byte-Parser nach RFC 6716 §3.1, Tabelle 1.
//!
//! Das erste Byte jedes Pakets codiert Config (5 Bit: Modus, Bandbreite,
//! Frame-Dauer), Stereo-Bit und Frame-Packing-Code.

use crate::types::{Bandwidth, Error, Mode};

/// Zerlegtes TOC-Byte plus Frame-Zahl des Pakets.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TocInfo {
    /// Config 0–31 (obere 5 Bit).
    pub config: u8,
    /// Codiermodus.
    pub mode: Mode,
    /// Audio-Bandbreite.
    pub bandwidth: Bandwidth,
    /// Dauer eines Frames in ms.
    pub frame_ms: f64,
    /// Kanaele (Bit 2 gesetzt = stereo).
    pub channels: u8,
    /// Frames im Paket (aus Packing-Code + Laengen-Bytes).
    pub frame_count: usize,
    /// Paketdauer = `frame_ms` × `frame_count`.
    pub duration_ms: f64,
}

/// (Modus, Bandbreite, Frame-Dauer in ms) je Config 0–31.
const TABLE: [(Mode, Bandwidth, f64); 32] = [
    // 0–3: SILK Schmalband.
    (Mode::Silk, Bandwidth::Narrow, 10.0),
    (Mode::Silk, Bandwidth::Narrow, 20.0),
    (Mode::Silk, Bandwidth::Narrow, 40.0),
    (Mode::Silk, Bandwidth::Narrow, 60.0),
    // 4–7: SILK Mittelband.
    (Mode::Silk, Bandwidth::Medium, 10.0),
    (Mode::Silk, Bandwidth::Medium, 20.0),
    (Mode::Silk, Bandwidth::Medium, 40.0),
    (Mode::Silk, Bandwidth::Medium, 60.0),
    // 8–11: SILK Breitband.
    (Mode::Silk, Bandwidth::Wide, 10.0),
    (Mode::Silk, Bandwidth::Wide, 20.0),
    (Mode::Silk, Bandwidth::Wide, 40.0),
    (Mode::Silk, Bandwidth::Wide, 60.0),
    // 12–15: Hybrid.
    (Mode::Hybrid, Bandwidth::SuperWide, 10.0),
    (Mode::Hybrid, Bandwidth::SuperWide, 20.0),
    (Mode::Hybrid, Bandwidth::Full, 10.0),
    (Mode::Hybrid, Bandwidth::Full, 20.0),
    // 16–19: CELT Schmalband.
    (Mode::Celt, Bandwidth::Narrow, 2.5),
    (Mode::Celt, Bandwidth::Narrow, 5.0),
    (Mode::Celt, Bandwidth::Narrow, 10.0),
    (Mode::Celt, Bandwidth::Narrow, 20.0),
    // 20–23: CELT Breitband.
    (Mode::Celt, Bandwidth::Wide, 2.5),
    (Mode::Celt, Bandwidth::Wide, 5.0),
    (Mode::Celt, Bandwidth::Wide, 10.0),
    (Mode::Celt, Bandwidth::Wide, 20.0),
    // 24–27: CELT Superbreitband.
    (Mode::Celt, Bandwidth::SuperWide, 2.5),
    (Mode::Celt, Bandwidth::SuperWide, 5.0),
    (Mode::Celt, Bandwidth::SuperWide, 10.0),
    (Mode::Celt, Bandwidth::SuperWide, 20.0),
    // 28–31: CELT Vollband.
    (Mode::Celt, Bandwidth::Full, 2.5),
    (Mode::Celt, Bandwidth::Full, 5.0),
    (Mode::Celt, Bandwidth::Full, 10.0),
    (Mode::Celt, Bandwidth::Full, 20.0),
];

/// Parst das TOC-Byte plus Frame-Packing (§3.2) eines Pakets.
pub fn parse(packet: &[u8]) -> Result<TocInfo, Error> {
    let toc = *packet.first().ok_or(Error::EmptyPacket)?;
    let config = toc >> 3;
    let (mode, bandwidth, frame_ms) = TABLE[config as usize];
    let channels = if toc & 0x04 == 0 { 1 } else { 2 };
    let frame_count = match toc & 0x03 {
        0 => 1,
        1 | 2 => 2,
        _ => {
            // Code 3: zweites Byte = Zahl + VBR/Padding-Flags (§3.2.5).
            let count = *packet.get(1).ok_or(Error::EmptyPacket)?;
            usize::from(count & 0x3f)
        }
    };
    if frame_count == 0 {
        return Err(Error::BadPacket("Frame-Zahl 0"));
    }
    Ok(TocInfo {
        config,
        mode,
        bandwidth,
        frame_ms,
        channels,
        frame_count,
        duration_ms: frame_ms * frame_count as f64,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_32_configs_match_rfc6716_table1() {
        // (Modus, Bandbreite, Dauer) je Config: RFC 6716 §3.1, Tab. 1.
        use Bandwidth::{Full as FB, Medium as MB, Narrow as NB};
        use Bandwidth::{SuperWide as SWB, Wide as WB};
        use Mode::{Celt as C, Hybrid as H, Silk as S};
        let cases = [
            (S, NB, 10.0), (S, NB, 20.0), (S, NB, 40.0), (S, NB, 60.0),
            (S, MB, 10.0), (S, MB, 20.0), (S, MB, 40.0), (S, MB, 60.0),
            (S, WB, 10.0), (S, WB, 20.0), (S, WB, 40.0), (S, WB, 60.0),
            (H, SWB, 10.0), (H, SWB, 20.0), (H, FB, 10.0), (H, FB, 20.0),
            (C, NB, 2.5), (C, NB, 5.0), (C, NB, 10.0), (C, NB, 20.0),
            (C, WB, 2.5), (C, WB, 5.0), (C, WB, 10.0), (C, WB, 20.0),
            (C, SWB, 2.5), (C, SWB, 5.0), (C, SWB, 10.0), (C, SWB, 20.0),
            (C, FB, 2.5), (C, FB, 5.0), (C, FB, 10.0), (C, FB, 20.0),
        ];
        assert_eq!(cases.len(), 32);
        for (config, (mode, bw, ms)) in cases.iter().enumerate() {
            let info = parse(&[(config as u8) << 3]).unwrap();
            assert_eq!(info.config, config as u8);
            assert_eq!(info.mode, *mode, "config {config}");
            assert_eq!(info.bandwidth, *bw, "config {config}");
            assert_eq!(info.frame_ms, *ms, "config {config}");
            assert_eq!(info.channels, 1);
            assert_eq!(info.frame_count, 1);
        }
    }

    #[test]
    fn mode_boundaries_silk_hybrid_celt() {
        assert_eq!(parse(&[11 << 3]).unwrap().mode, Mode::Silk);
        assert_eq!(parse(&[12 << 3]).unwrap().mode, Mode::Hybrid);
        assert_eq!(parse(&[15 << 3]).unwrap().mode, Mode::Hybrid);
        assert_eq!(parse(&[16 << 3]).unwrap().mode, Mode::Celt);
    }

    #[test]
    fn stereo_bit_sets_two_channels() {
        let mono = parse(&[0xf8]).unwrap(); // Config 31, mono.
        let stereo = parse(&[0xfc]).unwrap(); // Config 31, stereo.
        assert_eq!((mono.channels, stereo.channels), (1, 2));
        assert_eq!(stereo.mode, Mode::Celt);
    }

    #[test]
    fn packing_codes_yield_frame_counts() {
        assert_eq!(parse(&[0xf8]).unwrap().frame_count, 1); // Code 0.
        assert_eq!(parse(&[0xf9]).unwrap().frame_count, 2); // Code 1.
        assert_eq!(parse(&[0xfa]).unwrap().frame_count, 2); // Code 2.
        // Code 3: Count-Byte 0x83 = 3 Frames (VBR-Bit ignoriert).
        let info = parse(&[0xfb, 0x83]).unwrap();
        assert_eq!(info.frame_count, 3);
        assert_eq!(info.duration_ms, 60.0); // 3 × 20 ms.
    }

    #[test]
    fn empty_packet_is_error() {
        assert!(matches!(parse(&[]), Err(Error::EmptyPacket)));
        assert!(matches!(parse(&[0xfb]), Err(Error::EmptyPacket)));
        assert!(matches!(
            parse(&[0xfb, 0xc0]),
            Err(Error::BadPacket(_))
        ));
    }
}
