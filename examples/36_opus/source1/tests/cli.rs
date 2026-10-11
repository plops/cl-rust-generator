//! CLI-Host-Tests: JSON-Schema, Datei-Lauf, Fehlerfaelle.

mod common;

use std::ffi::OsString;

use opus_pure::{Application, OggOpusWriter, OpusHead};
use opus_vis::cli;

fn args(v: &[&str]) -> Vec<OsString> {
    v.iter().map(OsString::from).collect()
}

fn tmp(name: &str) -> std::path::PathBuf {
    std::env::temp_dir().join(format!("opusvis_{name}"))
}

#[test]
fn run_writes_png_and_json() {
    let data = common::encode_mono(Application::Audio, 64_000, None, None, 25, |fr, off| {
        common::sine_block(440.0, fr, off)
    });
    let dir = tmp("cli_ok");
    std::fs::create_dir_all(&dir).unwrap();
    let inp = dir.join("in.opus");
    let png = dir.join("out.png");
    let js = dir.join("out.json");
    std::fs::write(&inp, &data).unwrap();
    let a = args(&[
        "opus-vis",
        "-o",
        png.to_str().unwrap(),
        "--json",
        js.to_str().unwrap(),
        "--width",
        "200",
        inp.to_str().unwrap(),
    ]);
    cli::run(a.into_iter()).unwrap();
    let bytes = std::fs::read(&png).unwrap();
    assert_eq!(&bytes[..8], &opus_vis::png::MAGIC);
    let text = std::fs::read_to_string(&js).unwrap();
    assert!(text.contains("\"frames\":["));
    assert!(text.contains("\"summary\":{"));
    assert!(text.contains("\"mode\":\"CELT\""));
    assert!(text.contains("\"avg_kbps\":"));
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn stereo_fixture_has_two_channels() {
    // Stereo-Encode inline (common kennt nur mono).
    let mut enc = opus_pure::OpusEncoder::new(48_000, 2, Application::Audio).unwrap();
    enc.bitrate_bps = 96_000;
    let head = OpusHead::for_encoder(&enc, 48_000);
    let mut w = OggOpusWriter::new(Vec::new(), head).unwrap();
    let mut raw = vec![0u8; opus_pure::MAX_PACKET_BYTES];
    for f in 0..10 {
        let mut pcm = vec![0.0f32; 960 * 2];
        for (i, s) in pcm.as_chunks_mut::<2>().0.iter_mut().enumerate() {
            let t = (f * 960 + i) as f32 / 48_000.0;
            s[0] = 0.4 * (2.0 * std::f32::consts::PI * 440.0 * t).sin();
            s[1] = 0.4 * (2.0 * std::f32::consts::PI * 550.0 * t).sin();
        }
        let n = enc.encode(&pcm, 960, &mut raw).unwrap();
        w.write_packet(&raw[..n]).unwrap();
    }
    let data: Vec<u8> = w.finish().unwrap();
    let a = opus_vis::analyze::analyze_reader(&data[..], data.len() as u64).unwrap();
    assert_eq!(a.summary.channels, 2);
    let pcm = opus_vis::decode::decode_mono(&data).unwrap();
    assert!(pcm.samples.len() > 8000);
}

#[test]
fn six_channels_fail_cleanly() {
    let head = OpusHead {
        channel_count: 6,
        pre_skip: OpusHead::RECOMMENDED_PRE_SKIP,
        input_sample_rate: 48_000,
        output_gain_q8: 0,
        mapping_family: 1,
        stream_count: 4,
        coupled_count: 2,
        channel_mapping: vec![0, 4, 1, 2, 3, 5],
    };
    let data: Vec<u8> = OggOpusWriter::new(Vec::new(), head)
        .unwrap()
        .finish()
        .unwrap();
    let err = opus_vis::analyze::analyze_reader(&data[..], data.len() as u64).unwrap_err();
    assert!(matches!(
        err,
        opus_vis::types::Error::UnsupportedChannels(6)
    ));
}

#[test]
fn missing_and_garbage_inputs_error() {
    let a = args(&["opus-vis", "/tmp/opusvis_gibts_nicht.opus"]);
    assert!(matches!(
        cli::run(a.into_iter()),
        Err(opus_vis::types::Error::Io(_))
    ));
    let dir = tmp("cli_bad");
    std::fs::create_dir_all(&dir).unwrap();
    let inp = dir.join("bad.opus");
    std::fs::write(&inp, b"kein ogg container").unwrap();
    let a = args(&["opus-vis", inp.to_str().unwrap()]);
    assert!(matches!(
        cli::run(a.into_iter()),
        Err(opus_vis::types::Error::Opus(_))
    ));
    std::fs::remove_dir_all(&dir).ok();
}
