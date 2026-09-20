//! `cli_smoke` — CLI-Vertrag ohne Display/Modell (plus ein
//! ignorierter Xvfb-E2E mit Modell-Download, s. Task S4).
//!
//! Der E2E läuft nur explizit: unter Xvfb mit Netz und
//! `cargo test -- --ignored`.

use std::path::PathBuf;
use std::process::Command;

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_x11_rb_mq_viewer"))
}

#[test]
fn help_exits_zero() {
    let out = bin().arg("--help").output().expect("binary must run");
    assert!(out.status.success());
    let text = String::from_utf8_lossy(&out.stdout);
    for flag in [
        "--x",
        "--y",
        "--w",
        "--h",
        "--zoom",
        "--fps",
        "--model",
        "--save-frame",
        "--headless",
    ] {
        assert!(text.contains(flag), "missing {flag} in --help");
    }
}

#[test]
fn invalid_region_fails() {
    let out = bin().args(["--w", "0"]).output().expect("binary must run");
    assert!(!out.status.success());
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(err.contains("non-empty"), "unexpected stderr: {err}");
}

#[test]
fn bad_monitor_fails_without_display() {
    // Ohne X-Server scheitert die Monitor-Abfrage sauber (Exit ≠ 0),
    // nicht per Panic im Nirvana.
    let out = bin()
        .args(["--monitor", "7", "--save-frame", "/tmp/nope.png"])
        .output()
        .expect("binary must run");
    assert!(!out.status.success());
}

/// E2E unter Xvfb: ein annotierter Frame wird geschrieben.
///
/// Braucht: laufenden X-Server (`DISPLAY`), Netz (YOLOv8m-Download beim
/// ersten Lauf). Deshalb `#[ignore]` — Gate in S4/T2:
/// `Xvfb :99 & DISPLAY=:99 cargo test -- --ignored`.
/// Ungueltiges Modell scheitert mit klarer Meldung statt Panic.
///
/// Braucht wie oben Display + Netz (Monitor-Schritt zuerst).
#[test]
#[ignore]
fn bad_model_file_fails_under_xvfb() {
    let dir: PathBuf = std::env::temp_dir().join(format!("x11_yolo_bad_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let fake = dir.join("fake.onnx");
    std::fs::write(&fake, b"not an onnx model").unwrap();
    let out = bin()
        .args(["--model"])
        .arg(&fake)
        .args([
            "--x",
            "0",
            "--y",
            "0",
            "--w",
            "64",
            "--h",
            "64",
            "--save-frame",
        ])
        .arg(dir.join("nope.png"))
        .output()
        .expect("binary must run");
    assert!(!out.status.success());
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(!err.is_empty(), "expected an error message on stderr");
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
#[ignore]
fn save_frame_under_xvfb() {
    let dir: PathBuf = std::env::temp_dir().join(format!("x11_yolo_e2e_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let png = dir.join("e2e.png");
    let out = bin()
        .args([
            "--x",
            "0",
            "--y",
            "0",
            "--w",
            "320",
            "--h",
            "240",
            "--save-frame",
        ])
        .arg(&png)
        .output()
        .expect("binary must run");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let img = image::open(&png).expect("annotated png must exist");
    use image::GenericImageView;
    assert_eq!(img.dimensions(), (320, 240));
    std::fs::remove_dir_all(&dir).ok();
}
