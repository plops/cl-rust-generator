//! Regression tests against a real Sentinel-1 RAW `.dat` file.
//!
//! The dataset lives next to this crate (it is too large to vendor):
//! `data/vv/s1c-s6-raw-s-vv-*.dat`.
//! Override with `S1_DAT`. Tests skip silently when the file is absent so a
//! checkout without the dataset stays green.

use std::collections::BTreeMap;
use std::path::PathBuf;

use copernicus_radar::collect_headers::collect_packet_headers;
use copernicus_radar::decode_packet::decode_fdbaq;
use copernicus_radar::decode_type_c::decode_baq5;
use copernicus_radar::header::PacketHeader;
use copernicus_radar::mmap::MappedFile;
use copernicus_radar::utils::{BitReader, HEADER_LEN};

fn dat_path() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("S1_DAT") {
        let p = PathBuf::from(p);
        return p.exists().then_some(p);
    }
    let p = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("data/vv")
        .join("s1c-s6-raw-s-vv-20260929t214300-20260929t214327-009667-0133f4.dat");
    p.exists().then_some(p)
}

/// Census of the real file: 44,901 FDBAQ echoes, 16 BAQ5 noise packets and
/// 520 bypass calibration packets. Guards the header walk and the dispatch.
#[test]
fn real_data_packet_census() {
    let Some(path) = dat_path() else { return };
    let mapped = MappedFile::open(&path).unwrap();
    let data = mapped.bytes();
    let headers = collect_packet_headers(data).unwrap();
    assert_eq!(headers.len(), 45437);

    let mut census: BTreeMap<(bool, u32), usize> = BTreeMap::new();
    for off in &headers.offsets {
        let h = PacketHeader::parse(&data[*off..*off + HEADER_LEN]).unwrap();
        *census.entry((h.is_calibration(), h.baq_mode)).or_default() += 1;
    }
    assert_eq!(census.get(&(false, 12)), Some(&44901));
    assert_eq!(census.get(&(false, 5)), Some(&16));
    assert_eq!(census.get(&(true, 0)), Some(&520));
    assert_eq!(census.len(), 3);
}

fn assert_sane_samples(packet_idx: usize, samples: &[num_complex::Complex32], quads: usize) {
    assert_eq!(samples.len(), 2 * quads, "packet {packet_idx}");
    assert!(samples.iter().all(|s| s.re.is_finite() && s.im.is_finite()));
    let mean_abs: f32 = samples.iter().map(|s| s.norm()).sum::<f32>() / samples.len() as f32;
    assert!(
        mean_abs > 0.0,
        "packet {packet_idx}: decoded signal is all zero"
    );
}

#[test]
fn real_data_echo_packet_decodes() {
    let Some(path) = dat_path() else { return };
    let mapped = MappedFile::open(&path).unwrap();
    let data = mapped.bytes();
    let headers = collect_packet_headers(data).unwrap();

    // First FDBAQ echo on the dominant elevation beam (ma_ele == 5).
    let (idx, off) = headers
        .offsets
        .iter()
        .enumerate()
        .map(|(i, o)| (i, *o))
        .find(|(_, o)| {
            let h = PacketHeader::parse(&data[*o..*o + HEADER_LEN]).unwrap();
            !h.is_calibration() && h.baq_mode == 12 && h.elevation() == 5
        })
        .expect("an FDBAQ echo on beam 5 exists");
    let h = PacketHeader::parse(&data[off..off + HEADER_LEN]).unwrap();
    h.check_sync(idx).unwrap();

    let next = headers.offsets.get(idx + 1).copied().unwrap_or(data.len());
    let mut reader = BitReader::new(data, off + HEADER_LEN);
    let packet = decode_fdbaq(&mut reader, h.number_of_quads as usize)
        .unwrap_or_else(|e| panic!("echo packet {idx} failed to decode: {e:?}"));
    assert!(
        reader.byte_offset() <= next,
        "decoder ran past packet end: {} > {next}",
        reader.byte_offset()
    );
    assert_sane_samples(idx, &packet.to_complex(), h.number_of_quads as usize);
}

#[test]
fn real_data_noise_packet_decodes() {
    let Some(path) = dat_path() else { return };
    let mapped = MappedFile::open(&path).unwrap();
    let data = mapped.bytes();
    let headers = collect_packet_headers(data).unwrap();

    // First fixed-rate BAQ5 noise packet (signal type 1).
    let (idx, off) = headers
        .offsets
        .iter()
        .enumerate()
        .map(|(i, o)| (i, *o))
        .find(|(_, o)| {
            let h = PacketHeader::parse(&data[*o..*o + HEADER_LEN]).unwrap();
            !h.is_calibration() && h.baq_mode == 5
        })
        .expect("a BAQ5 noise packet exists");
    let h = PacketHeader::parse(&data[off..off + HEADER_LEN]).unwrap();
    assert_eq!(h.ses_ssb_signal_type, 1);
    h.check_sync(idx).unwrap();

    let next = headers.offsets.get(idx + 1).copied().unwrap_or(data.len());
    let mut reader = BitReader::new(data, off + HEADER_LEN);
    let packet = decode_baq5(&mut reader, h.number_of_quads as usize)
        .unwrap_or_else(|e| panic!("noise packet {idx} failed to decode: {e:?}"));
    assert!(
        reader.byte_offset() <= next,
        "decoder ran past packet end: {} > {next}",
        reader.byte_offset()
    );
    assert_sane_samples(idx, &packet.to_complex(), h.number_of_quads as usize);
}
