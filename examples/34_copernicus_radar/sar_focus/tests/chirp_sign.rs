//! Chirp-Vorzeichen: Header-Bits muessen einen Up-Chirp ergeben.
//!
//! Der S1-Header traegt TX-Rampe (TXPRR) und Startfrequenz (TXPSF) je als
//! Polaritaetsbit + Betrag. Polaritaet 1 = positiv (Referenz:
//! `sentinel1decoder._metadata_parser._txprr/_txpsf`, dort
//! `sign = (-1)**(1 - (vals >> 15))`). Per Echtdaten-Kompressionstest
//! verifiziert: Nur die Up-Replika komprimiert S6-Echos scharf
//! (Kurtosis 202 gegen 20, Profil-Tops 41x gegen 2,6x).
//!
//! Dieser Test simuliert ein Punktziel-Echo mit hartkodierter
//! Up-Chirp-Physik (Literale, unabhaengig von `chirp::replica`) und
//! verlangt scharfe Range-Kompression aus echten S6-Header-Bits.

mod common;

use common::fwhm;
use copernicus_radar::header::PacketHeader;
use sar_focus::chirp::ChirpParams;
use sar_focus::meta;
use sar_focus::range::RangeCompressor;
use sar_focus::types::Complex32;

/// S6-Echo-Header (Muster aus `meta`-Tests): RGDEC 9, Rang 10, TXPL 1918,
/// Beam 5, 9975 Quads — mit echten TXPRR/TXPSF-Bits (S6 VV):
/// TXPRR Polaritaet 1, Betrag 1229 (~+8,257e11 Hz/s);
/// TXPSF Polaritaet 0, Betrag 9210 (~-21,09 MHz).
fn s6_tx_header() -> [u8; 68] {
    let mut p = [0u8; 68];
    p[12..16].copy_from_slice(&[0x35, 0x2E, 0xF8, 0x53]);
    p[37] = 12;
    p[40] = 9;
    p[42] = 0x84;
    p[43] = 0xCD; // 0x04CD = 1229
    p[44] = 0x23;
    p[45] = 0xFA; // 0x23FA = 9210
    p[46..49].copy_from_slice(&[0x00, 0x07, 0x7E]); // TXPL = 1918
    p[49] = 0x0A; // Rang 10
    p[50..53].copy_from_slice(&[0x00, 0x58, 0x24]); // PRI = 22564
    p[53..56].copy_from_slice(&[0x00, 0x0C, 0x38]); // SWST = 3128
    p[56..59].copy_from_slice(&[0x00, 0x3E, 0x90]); // SWL = 16016
    p[60] = 0x50; // Beam 5
    p[65] = 0x26;
    p[66] = 0xF7; // 9975 Quads
    p
}

#[test]
fn header_bits_ergeben_up_chirp() {
    let raw = s6_tx_header();
    let h = PacketHeader::parse(&raw).unwrap();
    let m = meta::parse_echo(&h, 0, 0).unwrap();
    // sentinel1decoder-Werte (S6 VV): +8,25635554e11 Hz/s, -21,09403649e6 Hz.
    assert!(
        (m.txprr_hz_s - 8.25635554e11).abs() / 8.25635554e11 < 1e-3,
        "txprr = {}",
        m.txprr_hz_s
    );
    assert!(
        (m.txpsf_hz + 21.09403649e6).abs() < 50e3,
        "txpsf = {}",
        m.txpsf_hz
    );
}

#[test]
fn up_chirp_echo_wird_scharf_komprimiert() {
    let raw = s6_tx_header();
    let h = PacketHeader::parse(&raw).unwrap();
    let m = meta::parse_echo(&h, 0, 0).unwrap();
    // Punktziel bei Sample 4000: Up-Chirp bei Basisband (hartkodierte
    // Physik — Mittenfrequenz 0, Rate = sentinel1decoder-Wert).
    let nr = 8192;
    let r0 = 4000;
    let k = 8.25635554e11;
    let mut echo = vec![Complex32::zero(); nr];
    for (i, s) in echo.iter_mut().enumerate() {
        let t = (i as f64 - r0 as f64) / m.fs_hz;
        if t.abs() <= m.txpl_s / 2.0 {
            let ph = 2.0 * std::f64::consts::PI * (k / 2.0 * t * t);
            *s = Complex32::new(ph.cos() as f32, ph.sin() as f32);
        }
    }
    // Replika aus den Header-Bits (Produktionspfad).
    let p = ChirpParams {
        txpsf_hz: m.txpsf_hz,
        txpl_s: m.txpl_s,
        txprr_hz_s: m.txprr_hz_s,
        fs_hz: m.fs_hz,
    };
    let comp = RangeCompressor::new(&p, nr);
    comp.compress_rows(&mut echo);
    let pw: Vec<f32> = echo.iter().map(|c| c.norm_sqr()).collect();
    let pmax = pw
        .iter()
        .enumerate()
        .max_by(|a, b| a.1.total_cmp(b.1))
        .map(|(i, _)| i)
        .unwrap();
    let mean = pw.iter().sum::<f32>() / pw.len() as f32;
    // Scharf: schmaler Peak mit hohem Kontrast (Match). Matsch (Mismatch)
    // ist breit und flach — beide Schranken mit Abstand.
    assert!(
        fwhm(&pw, pmax) <= 2,
        "FWHM = {} px bei {}",
        fwhm(&pw, pmax),
        pmax
    );
    assert!(pw[pmax] / mean > 100.0, "Kontrast = {}", pw[pmax] / mean);
}
