//! Echo-Auswahl: abbildende Echos des stärksten Beams — ohne Limit.
//!
//! Die `copernicus-radar`-Main begrenzt gespeicherte Echos per Default auf
//! 512 (`stored_echoes`); dieser Pfad nutzt nur die Dekodier-Bibliothek und
//! wählt bewusst ALLE abbildenden Echos (S6 VV: 44.901). Der Test unten mit
//! 600 synthetischen Echos (> 512) schützt vor einem einsickernden Cap.

use crate::chirp::ChirpParams;
use crate::types::{Complex32, Error, Vec3d};
use copernicus_radar::collect_headers::collect_packet_headers;
use copernicus_radar::decode_packet::decode_fdbaq;
use copernicus_radar::decode_type_ab::decode_type_a_or_b;
use copernicus_radar::decode_type_c::{decode_baq3, decode_baq4, decode_baq5};
use copernicus_radar::header::PacketHeader;
use copernicus_radar::mmap::MappedFile;
use copernicus_radar::utils::{BitReader, HEADER_LEN};
use std::collections::HashMap;

/// Paket-Indizes der abbildenden Echos des quad-stärksten Beams.
///
/// Filter: `meta::is_imaging_echo` (FDBAQ, Signaltyp Echo, kein Kalibrier-
/// paket), danach Mehrheits-Beam. Keine Begrenzung der Anzahl.
pub fn select_beam_echoes(headers: &[PacketHeader]) -> Vec<usize> {
    let mut beams: HashMap<u32, u64> = HashMap::new();
    for h in headers {
        if crate::meta::is_imaging_echo(h) {
            *beams.entry(h.elevation()).or_insert(0) += u64::from(h.number_of_quads);
        }
    }
    let beam = beams.iter().max_by_key(|&(_, n)| n).map(|(&b, _)| b);
    let Some(beam) = beam else {
        return Vec::new();
    };
    headers
        .iter()
        .enumerate()
        .filter(|(_, h)| crate::meta::is_imaging_echo(h) && h.elevation() == beam)
        .map(|(i, _)| i)
        .collect()
}

/// Geladenes Echo-Fenster: dekodierte Rohmatrix plus Geometrie-Raster.
///
/// `raw` ist `naz × n0` (ausgerichtet, FFT-gepaddet), `slant` das
/// Zweiwege-Raster dazu (`c·(t_ref + j/fs)/2`), `veff` die effektive
/// Geschwindigkeit je Bin. `az0_abs` = globale Echo-Nummer von Zeile 0.
pub struct LoadedWindow {
    pub raw: Vec<Complex32>,
    pub naz: usize,
    pub n0: usize,
    pub n0raw: usize,
    pub slant: Vec<f64>,
    pub chirp: ChirpParams,
    pub bw_hz: f64,
    pub veff: Vec<f64>,
    pub pri_s: f64,
    pub fs_hz: f64,
    pub metas: Vec<crate::meta::EchoMeta>,
    pub line_pos: Vec<Vec3d>,
    pub line_vel: Vec<Vec3d>,
    pub az0_abs: usize,
    pub beam: u32,
}

/// Kleinste FFT-Größe ≥ `n` mit Primfaktoren ≤ 7 (cuFFT-sicher).
pub fn smooth_fft_len(n: usize) -> usize {
    fn smooth(mut m: usize) -> bool {
        for p in [2, 3, 5, 7] {
            while m.is_multiple_of(p) {
                m /= p;
            }
        }
        m == 1
    }
    (n..).find(|&m| smooth(m)).unwrap_or(n)
}

/// Dekodiert ein Echo in komplexe Samples (BAQ-Dispatch).
fn decode_echo(
    data: &[u8],
    offset: usize,
    baq_mode: u32,
    quads: usize,
) -> Result<Vec<Complex32>, Error> {
    let mut reader = BitReader::new(data, offset + HEADER_LEN);
    let packet = match baq_mode {
        0 => decode_type_a_or_b(&mut reader, quads),
        3 => decode_baq3(&mut reader, quads),
        4 => decode_baq4(&mut reader, quads),
        5 => decode_baq5(&mut reader, quads),
        12..=14 => decode_fdbaq(&mut reader, quads),
        m => return Err(Error(format!("BAQ-Modus {m} nicht unterstützt"))),
    }?;
    Ok(packet
        .to_complex()
        .iter()
        .map(|c| Complex32::new(c.re, c.im))
        .collect())
}

/// Lädt Echos `az0..az1` aus `.dat`: Auswahl, Orbit, Ausrichtung, Dekodierung.
pub fn load_window(input: &str, az0: usize, az1: usize) -> Result<LoadedWindow, Error> {
    let mapped = MappedFile::open(std::path::Path::new(input)).map_err(|e| Error(e.to_string()))?;
    let bytes = mapped.bytes();
    let packets = collect_packet_headers(bytes).map_err(|e| Error(e.to_string()))?;
    let mut headers = Vec::with_capacity(packets.len());
    let mut stream: Vec<(u8, u16)> = Vec::with_capacity(packets.len());
    for h in packets.headers.iter() {
        let h = PacketHeader::parse(h).map_err(|e| Error(e.to_string()))?;
        stream.push((h.sub_commutated_index as u8, h.sub_commutated_data as u16));
        headers.push(h);
    }
    let sel = select_beam_echoes(&headers);
    if sel.is_empty() {
        return Err(Error("keine Echos".to_string()));
    }
    let beam = headers[sel[0]].elevation();
    let echoes: Vec<(PacketHeader, usize)> = sel
        .iter()
        .map(|&i| (headers[i].clone(), packets.offsets[i]))
        .collect();
    let az1 = az1.min(echoes.len());
    if az0 >= az1 {
        return Err(Error(format!("leerer Azimut-Ausschnitt {az0}..{az1}")));
    }
    let echoes = &echoes[az0..az1];
    let naz = echoes.len();
    println!("Beam {beam}, Echos {naz} ({az0}..{az1})");
    let blocks = crate::ephem::collect_blocks(&stream);
    if blocks.is_empty() {
        return Err(Error("keine Ephemeridenblöcke".to_string()));
    }
    let metas: Vec<crate::meta::EchoMeta> = echoes
        .iter()
        .enumerate()
        .map(|(k, (h, off))| crate::meta::parse_echo(h, k, *off))
        .collect::<Result<_, _>>()?;
    // Echozeiten glätten: Header-Fine-Time quantisiert ±7,6 µs (±57 mm
    // Orbit-Position! ±13 rad TDBP-Phase, total inkohärent). PRI-Raster ist
    // exakt (ganzzahlige F_REF-Takte); absoluter Offset ist egal (nur
    // relative Phase zählt), daher Mitte als Anker.
    let nmid = metas.len() / 2;
    let (t_mid, pri) = (metas[nmid].time_s, metas[0].pri_s);
    let t_smooth = |k: usize| t_mid + (k as f64 - nmid as f64) * pri;
    let line_pos: Vec<Vec3d> = (0..metas.len())
        .map(|k| crate::ephem::interp_pos(&blocks, t_smooth(k)))
        .collect();
    let line_vel: Vec<Vec3d> = (0..metas.len())
        .map(|k| crate::ephem::interp_vel(&blocks, t_smooth(k)))
        .collect();
    // Orbit-Diagnose: Echozeit-Abstände (= PRI?), Block-Abdeckung, Radius.
    {
        let mut dt_max = 0.0f64;
        let mut jumps = 0u32;
        for w in metas.windows(2) {
            let dt = (w[1].time_s - w[0].time_s - pri).abs();
            dt_max = dt_max.max(dt);
            if dt > pri / 2.0 {
                jumps += 1;
            }
        }
        let q_max = metas
            .iter()
            .enumerate()
            .map(|(k, m)| (m.time_s - t_smooth(k)).abs())
            .fold(0.0f64, f64::max);
        let rmin = line_pos
            .iter()
            .map(|p| p.norm())
            .fold(f64::INFINITY, f64::min);
        let rmax = line_pos.iter().map(|p| p.norm()).fold(0.0f64, f64::max);
        println!(
            "Orbit: Δt-Abw max {:.1} µs (Sprünge {jumps}), Quant max {:.1} µs, Blöcke {:.2}–{:.2} s, Echos {:.2}–{:.2} s, |r| {:.3}–{:.3} km",
            dt_max * 1e6,
            q_max * 1e6,
            blocks.first().map(|b| b.time_s).unwrap_or(0.0),
            blocks.last().map(|b| b.time_s).unwrap_or(0.0),
            metas.first().map(|m| m.time_s).unwrap_or(0.0),
            metas.last().map(|m| m.time_s).unwrap_or(0.0),
            rmin / 1e3,
            rmax / 1e3
        );
    }
    // Ausrichten nach ECHOZEIT (Rang·PRI + SWST): data_delay allein driftet
    // über den Rahmen (16 Samples), die Zeit ist das wahre Raster.
    let m0 = &metas[0];
    let etime = |m: &crate::meta::EchoMeta| {
        f64::from(m.rank) * m.pri_s + m.swst_s + crate::meta::suppressed_data_time_s()
    };
    let t_ref = metas.iter().map(etime).fold(f64::INFINITY, f64::min);
    let bases: Vec<usize> = metas
        .iter()
        .map(|m| ((etime(m) - t_ref) * m.fs_hz).round().max(0.0) as usize)
        .collect();
    let resid_max = metas
        .iter()
        .map(|m| {
            let x = (etime(m) - t_ref) * m.fs_hz;
            (x.round() - x).abs()
        })
        .fold(0.0f64, f64::max);
    println!("Raster-Rest (Zeit→Sample): max. {resid_max:.3} Samples");
    let n0raw = metas
        .iter()
        .zip(bases.iter())
        .map(|(m, &b)| b + 2 * m.nquads as usize)
        .max()
        .unwrap_or(0);
    let n0 = smooth_fft_len(n0raw);
    println!("Range: {n0raw} + Pad → {n0}");
    let mut raw = vec![Complex32::zero(); naz * n0];
    for (k, ((h, off), &base)) in echoes.iter().zip(bases.iter()).enumerate() {
        if k % 5000 == 0 {
            println!("  dekodiere Echo {k}/{naz} …");
        }
        let s = decode_echo(bytes, *off, h.baq_mode, h.number_of_quads as usize)?;
        let take = s.len().min(n0 - base);
        raw[k * n0 + base..k * n0 + base + take].copy_from_slice(&s[..take]);
    }
    println!(
        "Rohbild: {naz} × {n0} ({:.2} GB)",
        (raw.len() * 8) as f64 / 1e9
    );
    let slant: Vec<f64> = (0..n0)
        .map(|j| crate::types::SPEED_OF_LIGHT * (t_ref + j as f64 / m0.fs_hz) / 2.0)
        .collect();
    println!(
        "Slant: {:.1}–{:.1} km, fs {:.4} MHz",
        slant[0] / 1e3,
        slant[n0 - 1] / 1e3,
        m0.fs_hz / 1e6
    );
    let chirp = ChirpParams {
        txpsf_hz: m0.txpsf_hz,
        txpl_s: m0.txpl_s,
        txprr_hz_s: m0.txprr_hz_s,
        fs_hz: m0.fs_hz,
    };
    let bw_hz = crate::meta::chirp_bandwidth_hz(m0.txprr_hz_s, m0.txpl_s);
    let veff = crate::rda::veff_mid_range(&line_pos, &line_vel, &slant);
    println!(
        "v_eff Mitte: {:.1} m/s, Bandbreite: {:.2} MHz",
        veff[n0 / 2],
        bw_hz / 1e6
    );
    Ok(LoadedWindow {
        raw,
        naz,
        n0,
        n0raw,
        slant,
        chirp,
        bw_hz,
        veff,
        pri_s: m0.pri_s,
        fs_hz: m0.fs_hz,
        metas,
        line_pos,
        line_vel,
        az0_abs: az0,
        beam,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Synthetischer Echo-Header (FDBAQ 12, Signaltyp 0, Beam wählbar).
    fn echo_header(beam: u8, signal: u8, baq: u8) -> PacketHeader {
        let mut p = [0u8; 68];
        p[37] = baq;
        p[60] = beam << 4;
        p[63] = signal << 4;
        p[65] = 0x26;
        p[66] = 0xF7; // 9975 Quads
        PacketHeader::parse(&p).unwrap()
    }

    #[test]
    fn keine_512_begrenzung() {
        // 600 Echos Beam 5 + Störer (Rauschen, Fremd-Beam, Festraten-BAQ).
        let mut headers = Vec::new();
        for _ in 0..600 {
            headers.push(echo_header(5, 0, 12));
        }
        for _ in 0..20 {
            headers.push(echo_header(5, 1, 12)); // Rauschen
        }
        for _ in 0..10 {
            headers.push(echo_header(6, 0, 12)); // Fremd-Beam
        }
        for _ in 0..10 {
            headers.push(echo_header(5, 0, 5)); // Festraten-BAQ
        }
        let sel = select_beam_echoes(&headers);
        assert_eq!(sel.len(), 600); // alle 600, kein Cap bei 512
        assert!(sel.iter().all(|&i| headers[i].elevation() == 5));
    }

    #[test]
    fn ohne_echos_leer() {
        let headers = vec![echo_header(5, 1, 12); 3];
        assert!(select_beam_echoes(&headers).is_empty());
    }
}
