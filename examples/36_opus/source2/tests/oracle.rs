//! Final-Range-Orakel: Der Walk muss Bit fuer Bit dort enden, wo der
//! Decoder endet — fuer jede Datei, jeden Modus, jeden Pfad.
//!
//! Drei unabhaengige Referenzen:
//! 1. `opus-pure`-Decoder (`OpusDecoder::final_range`) — alle Fixtures.
//! 2. libopus-**Encoder** (`opusenc --save-range`, letzte Spalte je Paket).
//! 3. libopus-Decoder per `scripts/libopus_oracle.py` (ausserhalb `cargo`).
//!
//! Dazu die Budget-Identitaet: Die Posten jedes Frames summieren exakt auf
//! `Bytes × 64` (1/8 Bit).

use std::path::{Path, PathBuf};

use opus_vis::walk::Walker;

/// Ergebnis eines Datei-Laufs.
#[derive(Debug, Default)]
struct Tally {
    packets: usize,
    compared: usize,
    equal: usize,
    budget_bad: usize,
    first_bad: Option<String>,
}

fn run(path: &Path) -> Tally {
    let bytes = std::fs::read(path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let ps = opus_vis::analyze::read_packets(&bytes, bytes.len() as u64).expect("ogg");
    let dec = opus_vis::decode::decode_packets(&ps).expect("decode");
    let mut w = Walker::new();
    let mut t = Tally {
        packets: ps.packets.len(),
        ..Tally::default()
    };
    for (i, p) in ps.packets.iter().enumerate() {
        let pw = w.packet(&p.data).expect("walk");
        for f in &pw.frames {
            if f.budget().total() != f.bytes as i32 * 64 {
                t.budget_bad += 1;
            }
        }
        if let Some(r) = pw.final_range() {
            t.compared += 1;
            if r == dec.final_range[i] {
                t.equal += 1;
            } else if t.first_bad.is_none() {
                t.first_bad = Some(format!(
                    "Paket {i}: {:?} cfg {}",
                    pw.toc.mode, pw.toc.config
                ));
            }
        }
    }
    t
}

fn fixtures() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data");
    let mut out = Vec::new();
    for dir in [root.clone(), root.join("gen")] {
        for e in std::fs::read_dir(&dir).expect("tests/data") {
            let p = e.expect("eintrag").path();
            if p.extension().is_some_and(|x| x == "opus") {
                out.push(p);
            }
        }
    }
    out.sort();
    out
}

#[test]
fn final_range_matches_decoder_on_every_fixture() {
    let files = fixtures();
    assert!(
        files.len() >= 20,
        "Fixture-Matrix fehlt: scripts/mkfixtures.sh"
    );
    let mut total = 0;
    for f in &files {
        let t = run(f);
        assert!(t.compared > 0, "{}: nichts verglichen", f.display());
        assert_eq!(
            t.equal,
            t.compared,
            "{}: {}/{} gleich, erster Fehler {:?}",
            f.display(),
            t.equal,
            t.compared,
            t.first_bad
        );
        assert_eq!(
            t.budget_bad,
            0,
            "{}: Budget-Identitaet verletzt",
            f.display()
        );
        total += t.compared;
    }
    assert!(total > 10_000, "zu wenige Pakete: {total}");
}

#[test]
fn dtx_frames_are_plc_and_excluded() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data/gen/dtx.opus");
    let t = run(&root);
    assert!(t.compared < t.packets, "DTX ohne leere Pakete?");
    assert_eq!(t.equal, t.compared);
}

/// `opusenc --save-range`: Zeilen `samples, bytes, [[...]], MODE, BW, M/S, frame, rng`.
fn opusenc_ranges(path: &Path) -> Vec<u32> {
    std::fs::read_to_string(path)
        .expect("rng-datei")
        .lines()
        .filter_map(|l| {
            l.rsplit(',')
                .next()?
                .trim()
                .trim_end_matches(']')
                .parse()
                .ok()
        })
        .collect()
}

#[test]
fn final_range_matches_libopus_encoder() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data/gen");
    let want = opusenc_ranges(&root.join("opusenc_60ms.rng"));
    let bytes = std::fs::read(root.join("opusenc_60ms.opus")).expect("fixture");
    let ps = opus_vis::analyze::read_packets(&bytes, bytes.len() as u64).expect("ogg");
    assert_eq!(want.len(), ps.packets.len(), "rng-Zeilen je Paket");
    let mut w = Walker::new();
    for (i, (p, r)) in ps.packets.iter().zip(want.iter()).enumerate() {
        let got = w.packet(&p.data).expect("walk").final_range();
        assert_eq!(got, Some(*r), "Paket {i}");
    }
}

/// Grosse Real-Dateien neben `source2` (ungecommittet, je ~60 000 Pakete).
/// `cargo test --release --test oracle -- --ignored`
#[test]
#[ignore = "grosse Real-Dateien, nur Release"]
fn final_range_matches_on_large_real_files() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut checked = 0;
    for name in ["silk_silk.opus", "silk.opus"] {
        let p = base.join(name);
        if !p.exists() {
            eprintln!("skip {name} (fehlt)");
            continue;
        }
        let t = run(&p);
        assert_eq!(t.equal, t.compared, "{name}: {:?}", t.first_bad);
        assert_eq!(t.budget_bad, 0, "{name}");
        checked += t.compared;
    }
    // WebM-Quellen: Opus-Spur per ffmpeg in Ogg umpacken (ohne Neukodierung).
    for name in ["silk.webm", "rpQs3m5jtIM.webm"] {
        let src = base.join(name);
        if !src.exists() {
            continue;
        }
        let dst = std::env::temp_dir().join(format!("oracle_{name}.opus"));
        let ok = std::process::Command::new("ffmpeg")
            .args(["-v", "error", "-y", "-i"])
            .arg(&src)
            .args(["-vn", "-c:a", "copy"])
            .arg(&dst)
            .status()
            .is_ok_and(|s| s.success());
        if !ok {
            eprintln!("skip {name} (ffmpeg fehlt)");
            continue;
        }
        let t = run(&dst);
        let _ = std::fs::remove_file(&dst);
        assert_eq!(t.equal, t.compared, "{name}: {:?}", t.first_bad);
        checked += t.compared;
    }
    eprintln!("grosse Dateien: {checked} Pakete gleich");
}
