//! Regression on real data: every WAV in `$TRACKER_SAMPLES` (or `samples/` in the
//! crate) is loaded and played through a sample instrument. Skipped without data.

use dnb_tracker::engine::Engine;
use dnb_tracker::types::{Cell, Kind, Patch, Song, parse_note};
use dnb_tracker::wav;
use std::path::PathBuf;

fn sample_dir() -> Option<PathBuf> {
    let dir = std::env::var_os("TRACKER_SAMPLES")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("samples"));
    dir.is_dir().then_some(dir)
}

#[test]
fn real_samples_load_and_play() {
    let Some(dir) = sample_dir() else {
        eprintln!("skipped: no sample directory (set TRACKER_SAMPLES)");
        return;
    };
    let mut files: Vec<_> = std::fs::read_dir(&dir)
        .unwrap()
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|x| x.eq_ignore_ascii_case("wav")))
        .collect();
    files.sort();
    if files.is_empty() {
        eprintln!("skipped: {dir:?} has no .wav files");
        return;
    }
    for f in files {
        let (data, sr) = wav::load_wav(&f).unwrap_or_else(|e| panic!("{f:?}: {e:#}"));
        assert!(!data.is_empty(), "{f:?} empty");
        let mut song = Song::new();
        let mut p = Patch::new("s", Kind::Sample);
        p.sample = data.clone();
        p.sample_rate = sr;
        song.instruments[0] = p;
        song.set_cell(0, 0, 0, Cell::note(parse_note("C-4").unwrap(), 0));
        let mut e = Engine::new(Box::new(song), 48000.0);
        e.play(0, Some(0));
        let mut out = vec![0.0; 48000 * 2];
        e.render(&mut out);
        assert!(out.iter().all(|v| v.is_finite() && v.abs() <= 1.0), "{f:?}");
        let src_energy: f32 = data.iter().take(sr as usize).map(|v| v * v).sum();
        let energy: f32 = out.iter().map(|v| v * v).sum();
        assert!(src_energy < 1e-6 || energy > 0.0, "{f:?} played silent");
        eprintln!("ok {f:?}: {} frames @ {sr} Hz", data.len());
    }
}
