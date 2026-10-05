//! Radar-Metadaten: Header-Rohwerte → physikalische Einheiten.
//!
//! Formeln aus `sentinel1decoder` (`_metadata_parser.py`, `enums.py`,
//! gegen `header.rs` kreuzgeprüft): Zeiten sind Rohzählwerte/`F_REF`,
//! `RGDEC` wählt die Range-Abtastrate, TX-Felder den Chirp.

use crate::types::{Error, F_REF_HZ, SPEED_OF_LIGHT};
use copernicus_radar::header::PacketHeader;

/// Ein abbildendes Echo: alle Fokus-relevanten Größen in SI-Einheiten.
#[derive(Clone, Copy, Debug)]
pub struct EchoMeta {
    pub packet_idx: usize,
    pub offset: usize,
    pub pri_count: u32,
    /// Pulszeit in s (Coarse + (Fine+0.5)/2¹⁶).
    pub time_s: f64,
    pub pri_s: f64,
    pub swst_s: f64,
    pub swl_s: f64,
    pub rgdec: u32,
    /// Range-Abtastrate in Hz (aus `RGDEC`).
    pub fs_hz: f64,
    pub txpsf_hz: f64,
    pub txpl_s: f64,
    pub txprr_hz_s: f64,
    pub rank: u32,
    pub data_delay: u32,
    pub nquads: u32,
    pub baq_mode: u32,
}

/// Nur FDBAQ-Echos (`signal_type == 0`, Modi 12–14, kein Kalibrierpaket)
///
/// Rauschpakete (`baq_mode 5`) und Kalibrierdaten gehören nicht ins Bild.
pub fn is_imaging_echo(h: &PacketHeader) -> bool {
    !h.is_calibration() && h.ses_ssb_signal_type == 0 && (12..=14).contains(&h.baq_mode)
}

/// Pulszeit in s (sentinel1decoder: `Coarse + (Fine+0.5)·2⁻¹⁶`).
pub fn echo_time_s(h: &PacketHeader) -> f64 {
    f64::from(h.coarse_time) + (f64::from(h.fine_time) + 0.5) / 65536.0
}

/// Range-Abtastrate in Hz: `fs = (L/M)·4·F_REF` (S1-IF-ASD-PL-0007).
pub fn range_sample_freq(rgdec: u32) -> Result<f64, Error> {
    let (l, m) = match rgdec {
        0 => (3, 4),
        1 => (2, 3),
        3 => (5, 9),
        4 => (4, 9),
        5 => (3, 8),
        6 => (1, 3),
        7 => (1, 6),
        8 => (3, 7),
        9 => (5, 16),
        10 => (3, 26),
        11 => (4, 11),
        _ => return Err(Error(format!("ungültiges RGDEC: {rgdec}"))),
    };
    Ok(f64::from(l) / f64::from(m) * 4.0 * F_REF_HZ)
}

/// TX-Rampenrate in Hz/s: `±mag·F_REF²/2²¹` (Polaritaet 1 = positiv,
/// sentinel1decoder `_txprr`; nur so komprimiert die Replika S6-Echos).
pub fn txprr_hz_s(h: &PacketHeader) -> f64 {
    let sign = if h.tx_ramp_rate_polarity == 0 {
        -1.0
    } else {
        1.0
    };
    sign * f64::from(h.tx_ramp_rate_magnitude) * F_REF_HZ * F_REF_HZ / 2_097_152.0
}

/// TX-Startfrequenz in Hz: `TXPRR/4F_REF ± mag·F_REF/2¹⁴`
/// (Polaritaet 1 = positiv, sentinel1decoder `_txpsf`).
pub fn txpsf_hz(h: &PacketHeader) -> f64 {
    let sign = if h.tx_pulse_start_frequency_polarity == 0 {
        -1.0
    } else {
        1.0
    };
    txprr_hz_s(h) / (4.0 * F_REF_HZ)
        + sign * f64::from(h.tx_pulse_start_frequency_magnitude) * F_REF_HZ / 16384.0
}

pub fn parse_echo(h: &PacketHeader, packet_idx: usize, offset: usize) -> Result<EchoMeta, Error> {
    Ok(EchoMeta {
        packet_idx,
        offset,
        pri_count: h.pri_count,
        time_s: echo_time_s(h),
        pri_s: f64::from(h.pulse_repetition_interval) / F_REF_HZ,
        swst_s: f64::from(h.sampling_window_start_time) / F_REF_HZ,
        swl_s: f64::from(h.sampling_window_length) / F_REF_HZ,
        rgdec: h.range_decimation,
        fs_hz: range_sample_freq(h.range_decimation)?,
        txpsf_hz: txpsf_hz(h),
        txpl_s: f64::from(h.tx_pulse_length) / F_REF_HZ,
        txprr_hz_s: txprr_hz_s(h),
        rank: h.rank,
        data_delay: h.data_delay(),
        nquads: h.number_of_quads,
        baq_mode: h.baq_mode,
    })
}

/// Unterdrückte Vorlaufzeit in s: `320/(8·F_REF)` (SSFocus `focus.py`).
pub fn suppressed_data_time_s() -> f64 {
    320.0 / (8.0 * F_REF_HZ)
}

/// Schrägentfernungen in m je Range-Sample (SSFocus):
/// `R[j] = (rank·PRI + SWST + suppressed + j/fs)·c/2`.
pub fn slant_range_vec(rank: u32, pri_s: f64, swst_s: f64, fs_hz: f64, n: usize) -> Vec<f64> {
    let t0 = f64::from(rank) * pri_s + swst_s + suppressed_data_time_s();
    (0..n)
        .map(|j| (t0 + j as f64 / fs_hz) * SPEED_OF_LIGHT / 2.0)
        .collect()
}

/// Chirp-Bandbreite in Hz: `|TXPRR|·TXPL`.
pub fn chirp_bandwidth_hz(txprr_hz_s: f64, txpl_s: f64) -> f64 {
    txprr_hz_s.abs() * txpl_s
}

/// Theoretische Slant-Range-Auflösung in m: `c/(2B)`.
pub fn range_resolution_m(bandwidth_hz: f64) -> f64 {
    SPEED_OF_LIGHT / (2.0 * bandwidth_hz)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Echter S6-Echo-Header (Paket 268): PRI 22564, SWST 3128, SWL 16016,
    /// RGDEC 9, Rang 10, TXPL 1918, `baq_mode` 12, Signaltyp 0, Beam 5.
    fn s6_header() -> [u8; 68] {
        let mut p = [0u8; 68];
        p[12..16].copy_from_slice(&[0x35, 0x2E, 0xF8, 0x53]);
        p[37] = 12;
        p[40] = 9;
        // TXPRR: Polarität 1 (positiv), Betrag so, dass ≈ +0,826 MHz/µs.
        // 0,826e12·2²¹/F_REF² ≈ 1229,6 → 1230.
        p[42] = 0x84;
        p[43] = 0xCE; // 0x04CE = 1230
        // TXPSF-Betrag für ≈ 21,1 MHz: (21,1e6 − TXPRR/4F_REF)·2¹⁴/F_REF.
        p[44] = 0x00;
        p[45] = 0xC0; // Platzhalter, wird unten nicht scharf geprüft
        p[46..49].copy_from_slice(&[0x00, 0x07, 0x7E]); // TXPL = 1918
        p[49] = 0x0A; // Rang 10
        p[50..53].copy_from_slice(&[0x00, 0x58, 0x24]); // PRI = 22564
        p[53..56].copy_from_slice(&[0x00, 0x0C, 0x38]); // SWST = 3128
        p[56..59].copy_from_slice(&[0x00, 0x3E, 0x90]); // SWL = 16016
        p[59] = 0x00;
        p[60] = 0x50; // Beam 5
        p[61] = 0x00;
        p[62] = 0x00;
        p[63] = 0x00; // Signaltyp 0
        p[65] = 0x26;
        p[66] = 0xF7; // 9975 Quads
        p
    }

    #[test]
    fn rgdec_tabelle() {
        // RGDEC 9: (5/16)·4·F_REF = 46,9184028 MHz.
        let fs = range_sample_freq(9).unwrap();
        assert!((fs - 46_918_402.8).abs() < 1.0, "fs = {fs}");
        assert!(range_sample_freq(2).is_err());
        assert!(range_sample_freq(12).is_err());
        // Spot-Check RGDEC 0: (3/4)·4·F_REF = 3·F_REF.
        assert!((range_sample_freq(0).unwrap() - 3.0 * F_REF_HZ).abs() < 1.0);
    }

    #[test]
    fn s6_echowerte() {
        let raw = s6_header();
        let h = PacketHeader::parse(&raw).unwrap();
        assert!(is_imaging_echo(&h));
        let m = parse_echo(&h, 268, 0).unwrap();
        // PRI = 22564/F_REF ≈ 601,150 µs → PRF ≈ 1663,5 Hz.
        assert!(
            (m.pri_s - 22_564.0 / 37_534_722.24).abs() < 1e-12,
            "pri = {}",
            m.pri_s
        );
        assert!((m.pri_s.recip() - 1663.5).abs() < 0.5);
        // SWST = 3128/F_REF ≈ 83,337 µs.
        assert!((m.swst_s - 8.3337e-5).abs() < 1e-9);
        // TXPL = 1918/F_REF ≈ 51,10 µs.
        assert!((m.txpl_s - 1918.0 / 37_534_722.24).abs() < 1e-18);
        assert!((m.txpl_s * 1e6 - 51.10).abs() < 0.01);
        // TXPRR ≈ +0,826 MHz/µs = +8,26·10¹¹ Hz/s (Up-Chirp).
        assert!(
            (m.txprr_hz_s - 8.264e11).abs() < 2e9,
            "txprr = {}",
            m.txprr_hz_s
        );
        // Bandbreite ≈ 42,2 MHz → Slant-Auflösung ≈ 3,55 m.
        let bw = chirp_bandwidth_hz(m.txprr_hz_s, m.txpl_s);
        assert!((bw - 42.2e6).abs() < 0.3e6, "bw = {bw}");
        assert!((range_resolution_m(bw) - 3.55).abs() < 0.03);
        // Slant-Near ≈ 913,8 km (Rang·PRI dominiert).
        let r = slant_range_vec(m.rank, m.pri_s, m.swst_s, m.fs_hz, 4);
        assert!((r[0] - 913_800.0).abs() < 500.0, "r0 = {}", r[0]);
        // Sample-Abstand: c/(2·fs) ≈ 3,1948 m.
        assert!((r[1] - r[0] - 3.1948).abs() < 0.001);
        // Kreuzprobe gegen header.rs (MHz-Methoden). header.rs nutzt
        // FREF = 37,53472 MHz, wir F_REF = 37,53472224 MHz (2,24 Hz mehr).
        assert!((m.txpl_s * 1e6 - h.tx_pulse_length_us()).abs() < 1e-5);
        assert!((m.txprr_hz_s / 1e12 - h.tx_ramp_rate()).abs() / h.tx_ramp_rate().abs() < 1e-6);
    }

    #[test]
    fn pulszeit() {
        let mut raw = s6_header();
        raw[6..10].copy_from_slice(&[0x57, 0xE6, 0xF3, 0x76]); // coarse = 1474753398
        raw[10] = 0x65;
        raw[11] = 0x8A; // fine = 25994
        let h = PacketHeader::parse(&raw).unwrap();
        let t = echo_time_s(&h);
        // Toleranz: f64-Epsilon bei 1,5·10⁹ s ist ≈ 2,4·10⁻⁷ s.
        assert!((t - (1_474_753_398.0 + 25994.5 / 65536.0)).abs() < 1e-6);
    }

    #[test]
    fn nur_echos() {
        let mut raw = s6_header();
        let h = PacketHeader::parse(&raw).unwrap();
        assert!(is_imaging_echo(&h));
        raw[63] = 0x10; // Signaltyp 1 (Rauschen)
        let h = PacketHeader::parse(&raw).unwrap();
        assert!(!is_imaging_echo(&h));
    }
}
