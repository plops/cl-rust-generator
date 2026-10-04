//! `sar_focus`-CLI: `.dat` → fokussiertes Bild + Quicklook (wächst mit den Phasen).

use copernicus_radar::collect_headers::collect_packet_headers;
use copernicus_radar::header::PacketHeader;
use copernicus_radar::mmap::MappedFile;
use sar_focus::ephem;
use sar_focus::meta;
use std::collections::HashMap;

fn usage(program: &str) -> String {
    format!(
        "usage:\n\
         \t{program} meta <input.dat>\tHeader-/Orbit-Diagnose\n\
         \t{program} --help\t\tDiese Hilfe"
    )
}

/// Diagnose: zählt Echos, zeigt Chirp-/Orbit-Kennwerte (ohne Dekodierung).
fn cmd_meta(path: &str) -> Result<(), String> {
    let mapped = MappedFile::open(std::path::Path::new(path)).map_err(|e| e.to_string())?;
    let data = mapped.bytes();
    let packets = collect_packet_headers(data).map_err(|e| e.to_string())?;
    println!("Pakete: {}", packets.len());
    let mut beams: HashMap<u32, u64> = HashMap::new();
    let mut stream: Vec<(u8, u16)> = Vec::with_capacity(packets.len());
    let mut first_echo: Option<meta::EchoMeta> = None;
    let mut n_echo = 0u64;
    for (i, h) in packets.headers.iter().enumerate() {
        let h = PacketHeader::parse(h).map_err(|e| e.to_string())?;
        stream.push((h.sub_commutated_index as u8, h.sub_commutated_data as u16));
        if !h.is_calibration() {
            *beams.entry(h.elevation()).or_insert(0) += u64::from(h.number_of_quads);
        }
        if meta::is_imaging_echo(&h) {
            n_echo += 1;
            if first_echo.is_none() {
                first_echo =
                    Some(meta::parse_echo(&h, i, packets.offsets[i]).map_err(|e| e.to_string())?);
            }
        }
    }
    let mut beams: Vec<(u32, u64)> = beams.into_iter().collect();
    beams.sort();
    println!("Abbildende Echos (FDBAQ): {n_echo}");
    println!("Beams (Ele → Quads): {beams:?}");
    if let Some(m) = first_echo {
        let bw = meta::chirp_bandwidth_hz(m.txprr_hz_s, m.txpl_s);
        let r = meta::slant_range_vec(m.rank, m.pri_s, m.swst_s, m.fs_hz, 2 * m.nquads as usize);
        println!(
            "PRF: {:.2} Hz  PRI: {:.3} µs  Rang: {}  RGDEC: {}  fs: {:.4} MHz",
            m.pri_s.recip(),
            m.pri_s * 1e6,
            m.rank,
            m.rgdec,
            m.fs_hz / 1e6
        );
        println!(
            "Chirp: TXPL {:.3} µs  TXPSF {:.2} MHz  TXPRR {:.3} MHz/µs  B {:.2} MHz  ΔR {:.2} m",
            m.txpl_s * 1e6,
            m.txpsf_hz / 1e6,
            m.txprr_hz_s / 1e12,
            bw / 1e6,
            meta::range_resolution_m(bw)
        );
        println!(
            "Slant: nah {:.1} km  fern {:.1} km  ({} Samples)",
            r[0] / 1e3,
            r[r.len() - 1] / 1e3,
            r.len()
        );
    }
    let blocks = ephem::collect_blocks(&stream);
    println!("Ephemeridenblöcke: {}", blocks.len());
    if let Some(p) = blocks.first() {
        println!(
            "|r| = {:.1} km  |v| = {:.2} m/s  t0 = {}",
            p.pos.norm() / 1e3,
            p.vel.norm(),
            p.time_s
        );
    }
    Ok(())
}

fn main() {
    let program = std::env::args()
        .next()
        .unwrap_or_else(|| "sar_focus".to_string());
    let args: Vec<String> = std::env::args().skip(1).collect();
    let res = match args
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .as_slice()
    {
        ["meta", path] => cmd_meta(path),
        ["--help" | "-h"] | [] => {
            println!("{}", usage(&program));
            Ok(())
        }
        _ => Err(format!("unbekannte Argumente\n{}", usage(&program))),
    };
    if let Err(e) = res {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}
