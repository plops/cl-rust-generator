//! Kern-Host-Tests: Analyse echter (generierter + libopus-) Dateien.

mod common;

use opus_pure::{Application, Bandwidth, Signal};
use opus_vis::analyze::analyze_reader;
use opus_vis::types::Mode;

#[test]
fn voip_forced_wideband_yields_silk() {
    let data = common::encode_mono(
        Application::Voip,
        24_000,
        Some(Bandwidth::Wideband),
        Some(Signal::Voice),
        10,
        common::speech_block,
    );
    let cfgs = common::configs(&data);
    assert_eq!(cfgs.len(), 10);
    assert!(
        cfgs.iter().all(|&c| c <= 11),
        "erwarte SILK (0-11), habe {cfgs:?}"
    );
    let a = analyze_reader(&data[..], data.len() as u64).unwrap();
    assert!(a.frames.iter().all(|f| f.mode == Mode::Silk));
}

#[test]
fn audio_music_yields_celt_near_target_bitrate() {
    let data = common::encode_mono(
        Application::Audio,
        96_000,
        None,
        None,
        10,
        |fr, off| common::sine_block(880.0, fr, off),
    );
    let a = analyze_reader(&data[..], data.len() as u64).unwrap();
    assert_eq!(a.frames.len(), 10);
    assert!(a.frames.iter().all(|f| f.mode == Mode::Celt));
    let avg = a.summary.avg_kbps;
    assert!(
        (64.0..128.0).contains(&avg),
        "avg_kbps {avg} weitab von 96"
    );
}

#[test]
fn libopus_reference_file_opens_and_decodes_meta() {
    let data = std::fs::read("tests/data/ref.opus").unwrap();
    let a = analyze_reader(&data[..], data.len() as u64).unwrap();
    // 1 s / 20 ms = 50 Pakete + 1 Flush-Paket von opusenc.
    assert!((50..=52).contains(&a.summary.frames));
    assert!((1000.0..=1060.0).contains(&a.summary.duration_ms));
    assert_eq!(a.summary.channels, 1);
    assert!((a.summary.avg_kbps - 46.5).abs() < 5.0); // opusinfo: 46.5.
    assert!(a.summary.ratio > 10.0); // Sprache+Ton stark komprimierbar.
}
