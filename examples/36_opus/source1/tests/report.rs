//! Report-Host-Tests: Abschnitte, Regeln, Gold-Snapshot.

mod common;

use opus_pure::Application;
use opus_vis::{analyze, bitstream, decode, fakten, report, spectrum};

fn fixture_report() -> String {
    let data = common::encode_mono(Application::Audio, 64_000, None, None, 25, |fr, off| {
        common::sine_block(440.0, fr, off)
    });
    let ps = analyze::read_packets(&data, data.len() as u64).unwrap();
    let a = analyze::analyze_packets(&ps).unwrap();
    let pcm = decode::decode_packets(&ps).unwrap();
    let cols = spectrum::stft(&pcm.samples);
    let bs = bitstream::analyze_packets(&ps).unwrap();
    let head = fakten::head_fakten(&ps.head);
    let tags = fakten::tag_fakten(&ps.tags);
    let enc = fakten::encoder_fakten(&a, &bs.transient);
    let pcm_f = fakten::pcm_fakten(&pcm.samples, &cols, &pcm.stats);
    let silk_f = fakten::silk_fakten(&bs);
    report::report("fixture.opus", &a, &head, &tags, &enc, &pcm_f, &silk_f)
}

#[test]
fn report_names_sections_and_fixture_facts() {
    let text = fixture_report();
    for abschnitt in [
        "opus-report: fixture.opus",
        "datei:",
        "encoder: opus-pure",
        "bitrate:",
        "kompression:",
        "modi:",
        "wechsel:",
        "bandbreite: FB 100.0 %",
        "stimme: kein SILK",
        "signal:",
        "schwerpunkt 441 hz",
        "stille:",
        "transienten:",
        "einordnung:",
        "spricht fuer musik in vollband",
    ] {
        assert!(text.contains(abschnitt), "{abschnitt} fehlt in:\n{text}");
    }
}

#[test]
fn report_matches_gold_snapshot() {
    let text = fixture_report();
    let gold = include_str!("data/report_gold.txt");
    assert_eq!(
        text, gold,
        "report abgewichen — sichten, ggf. gold neu abnehmen"
    );
}
