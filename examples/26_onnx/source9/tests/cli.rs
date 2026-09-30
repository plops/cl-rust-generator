//! CLI-Test: `bench` läuft als Binary, Exit 0, Tabellenkopf.
//!
//! Braucht echte Assets (Font + Modelle); kein stilles Überspringen.

use std::process::Command;

#[test]
fn bench_runs_and_prints_markdown_table() {
    let exe = env!("CARGO_BIN_EXE_unicode_ocr");
    let out = Command::new(exe)
        .args([
            "bench",
            "--lang",
            "de",
            "--gen",
            "pangram",
            "--samples",
            "1",
            "--lines",
            "1",
        ])
        .output()
        .expect("run bench binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "exit={} stderr={stderr}", out.status);
    assert!(stdout.contains("| Lang | Modell |"), "stdout={stdout}");
    assert!(stdout.contains("| de |"), "stdout={stdout}");
}

#[test]
fn bench_rejects_unknown_language() {
    let exe = env!("CARGO_BIN_EXE_unicode_ocr");
    let out = Command::new(exe)
        .args(["bench", "--lang", "xx"])
        .output()
        .expect("run bench");
    assert!(!out.status.success());
}
