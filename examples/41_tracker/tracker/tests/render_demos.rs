//! Integration: render the demo songs end-to-end and check the audio is sane.

use dnb_tracker::{demo_songs, song_io, wav};

const SR: u32 = 48000;

fn stats(x: &[f32]) -> (f32, f32) {
    let peak = x.iter().fold(0.0f32, |m, v| m.max(v.abs()));
    let rms = (x.iter().map(|v| v * v).sum::<f32>() / x.len() as f32).sqrt();
    (peak, rms)
}

#[test]
fn demos_render_with_sane_levels() {
    for name in ["synth-dnb", "synth-phonk"] {
        let song = demo_songs::by_name(name).unwrap();
        let out = wav::render(song, 12.0, SR);
        assert_eq!(out.len(), 12 * SR as usize * 2);
        assert!(out.iter().all(|v| v.is_finite()), "{name}: NaN/inf");
        let (peak, rms) = stats(&out);
        assert!(peak < 1.0, "{name}: clipped {peak}");
        assert!((0.05..0.5).contains(&rms), "{name}: rms {rms}");
        // Stereo: the panned hats/bells must make L and R differ.
        let diff: f32 = out.chunks(2).map(|f| (f[0] - f[1]).abs()).sum();
        assert!(diff > 1.0, "{name}: mono output");
    }
}

#[test]
fn dnb_bass_enters_in_second_pattern() {
    // Pattern 0 is drums only, the reese starts with pattern 1 (64 rows at 174 BPM).
    let pat_secs = 64.0 * 60.0 / (174.0 * 4.0);
    let out = wav::render(demo_songs::demo_dnb(), 2.0 * pat_secs, SR);
    let half = out.len() / 2;
    let (_, rms_intro) = stats(&out[..half]);
    let (_, rms_main) = stats(&out[half..]);
    assert!(rms_main > rms_intro * 1.5, "{rms_intro} → {rms_main}");
}

#[test]
fn saved_song_renders_identically() {
    let dir = std::env::temp_dir().join(format!("dnb-int-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("phonk.txt");
    song_io::save(&path, &demo_songs::demo_phonk()).unwrap();
    let (loaded, warnings) = song_io::load(&path).unwrap();
    assert!(warnings.is_empty());
    let a = wav::render(demo_songs::demo_phonk(), 3.0, SR);
    let b = wav::render(loaded, 3.0, SR);
    assert_eq!(a, b);
    let wav_path = dir.join("out.wav");
    wav::write_wav(&wav_path, &a, SR).unwrap();
    let (mono, sr) = wav::load_wav(&wav_path).unwrap();
    assert_eq!((mono.len(), sr), (3 * SR as usize, SR as f32));
    std::fs::remove_dir_all(dir).ok();
}

#[test]
fn song_file_resolves_samples_relative_to_itself() {
    let dir = std::env::temp_dir().join(format!("dnb-smp-{}", std::process::id()));
    std::fs::create_dir_all(dir.join("smp")).unwrap();
    let tone: Vec<f32> = (0..9600).map(|i| (i as f32 * 0.03).sin() * 0.5).collect();
    wav::write_wav(&dir.join("smp/tone.wav"), &tone, SR).unwrap();
    let text = "bpm 174\norder 00\n\
                instr 00 kind=sample sample=smp/tone.wav name=Tone\n\
                instr 01 kind=sample sample=missing.wav name=Gone\n\
                pattern 00\n00 | C-4 00 .. ... |\n";
    let path = dir.join("song.txt");
    std::fs::write(&path, text).unwrap();
    let (song, warnings) = song_io::load(&path).unwrap();
    assert_eq!(song.instruments[0].sample.len(), 4800, "stereo → mono");
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].contains("missing.wav"));
    let (_, rms) = stats(&wav::render(song, 0.1, SR));
    assert!(rms > 0.05, "sample audible: {rms}");
    std::fs::remove_dir_all(dir).ok();
}
