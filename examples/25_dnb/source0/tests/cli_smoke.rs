use std::process::Command;

fn bin() -> Command {
    let mut c = Command::new(env!("CARGO_BIN_EXE_dark_dnb_synth"));
    c.env("ALSA_PCM_CARD", "0");
    c
}

#[test]
fn help_exits_zero() {
    let out = bin().arg("--help").output().expect("run --help");
    assert!(out.status.success());
}

#[test]
fn preset_roundtrip_and_dump() {
    let dir = std::env::temp_dir();
    let path = dir.join("dnb_s0_preset.toml");
    let out = bin()
        .args(["--save-preset"])
        .arg(&path)
        .output()
        .expect("save preset");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let text = std::fs::read_to_string(&path).expect("preset written");
    assert!(text.contains("ver = 1"));
    // Gespeichertes Preset steuert einen 0-Bar-Render.
    let wav = dir.join("dnb_s0_preset.wav");
    let out = bin()
        .args(["--preset"])
        .arg(&path)
        .args(["--bars", "0", "--render-wav"])
        .arg(&wav)
        .output()
        .expect("preset render");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    // Falsche Version wird abgelehnt (Exit != 0).
    let bad = dir.join("dnb_s0_preset_bad.toml");
    std::fs::write(&bad, "ver = 99\nbpm = 174.0\nbars = 1\ngain_db = -6.0\n").unwrap();
    let out = bin()
        .args(["--preset"])
        .arg(&bad)
        .output()
        .expect("bad preset");
    assert!(!out.status.success(), "bad ver must fail");
}

#[test]
fn tui_prints_sections_without_tty() {
    // Unter Test: stdout ist kein TTY -> Tabellen-Fallback, Exit 0.
    let out = Command::new(env!("CARGO_BIN_EXE_dnb_tui"))
        .output()
        .expect("run dnb_tui");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(text.contains("Tearout"), "sections table missing: {text}");
}

#[test]
fn render_silence_writes_valid_wav() {
    let path = std::env::temp_dir().join("dnb_s0_silence.wav");
    let out = bin()
        .args(["--render-wav"])
        .arg(&path)
        .args(["--bars", "0"])
        .output()
        .expect("run render");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let meta = std::fs::metadata(&path).expect("wav exists");
    // Stille = Header-only ist valide (hound schreibt 44-Byte-Header).
    assert!(meta.len() >= 44, "wav header missing, len={}", meta.len());
    let reader = hound::WavReader::open(&path).expect("wav readable");
    assert_eq!(reader.len(), 0);
}
