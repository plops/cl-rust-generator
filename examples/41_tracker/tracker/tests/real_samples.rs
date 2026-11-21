//! Regression on real data: every WAV in `$TRACKER_SAMPLES` (or `samples/` in the
//! crate, searched recursively) is loaded and played through a sample instrument.
//! Skipped without data.

use dnb_tracker::engine::Engine;
use dnb_tracker::types::{Cell, Kind, Patch, Song, Sources, parse_note};
use dnb_tracker::{demo_songs, song_io, wav};
use std::path::{Path, PathBuf};

fn sample_dir() -> Option<PathBuf> {
    let dir = std::env::var_os("TRACKER_SAMPLES")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("samples"));
    dir.is_dir().then_some(dir)
}

fn collect_wavs(dir: &Path, out: &mut Vec<PathBuf>) {
    for e in std::fs::read_dir(dir).into_iter().flatten().flatten() {
        let p = e.path();
        if p.is_dir() && !p.ends_with(".git") {
            collect_wavs(&p, out);
        } else if p.extension().is_some_and(|x| x.eq_ignore_ascii_case("wav")) {
            out.push(p);
        }
    }
}

/// The committed `songs/mixed_*.txt` must load all their samples once `samples/`
/// is populated (see README), and render with the badge they claim.
#[test]
fn mixed_songs_find_their_samples() {
    let songs = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("songs");
    if sample_dir().is_none() {
        eprintln!("skipped: no samples/ directory");
        return;
    }
    for (file, built) in demo_songs::mixed_songs() {
        let (song, warnings) = song_io::load(&songs.join(file)).unwrap();
        assert!(warnings.is_empty(), "{file}: {warnings:?}");
        assert_eq!(song.title, built.title);
        assert_eq!(song.sources(), Sources::Mixed);
        let out = wav::render(song, 4.0, 48000);
        assert!(
            out.iter().all(|v| v.is_finite() && v.abs() <= 1.0),
            "{file}"
        );
        let rms = (out.iter().map(|v| v * v).sum::<f32>() / out.len() as f32).sqrt();
        assert!(rms > 0.02, "{file}: rms {rms}");
    }
}

#[test]
fn real_samples_load_and_play() {
    let Some(dir) = sample_dir() else {
        eprintln!("skipped: no sample directory (set TRACKER_SAMPLES)");
        return;
    };
    let mut files = Vec::new();
    collect_wavs(&dir, &mut files);
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
