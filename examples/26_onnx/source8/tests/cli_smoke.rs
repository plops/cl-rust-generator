//! CLI-Rauchtest ohne Modell/Display: saubere Fehler statt Panics.

use gui_detect::image::Rgb;
use std::process::Command;

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_gui_detect"))
}

#[test]
fn no_args_prints_usage_and_fails() {
    let out = bin().output().unwrap();
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("gui_detect detect"));
}

#[test]
fn garbage_model_fails_cleanly() {
    let dir = std::env::temp_dir().join(format!("gui_detect_smoke_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let ppm = dir.join("in.ppm");
    Rgb::filled(32, 16, [200, 10, 10])
        .write_ppm(std::fs::File::create(&ppm).unwrap())
        .unwrap();
    let model = dir.join("bad.onnx");
    std::fs::write(&model, b"kein onnx").unwrap();

    let out = bin()
        .args([
            "detect",
            model.to_str().unwrap(),
            ppm.to_str().unwrap(),
            "--device",
            "cpu",
        ])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1), "Exit 1 statt Panic/Abort");

    let missing = bin()
        .args(["detect", "fehlt.onnx", ppm.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(String::from_utf8_lossy(&missing.stderr).contains("fehlt.onnx"));
    std::fs::remove_dir_all(dir).unwrap();
}
