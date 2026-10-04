//! GUI-Test unter `xvfb`: rendert Frames, speichert einen Screenshot und
//! prüft dessen Inhalt (läuft unter `cargo oxide test`, braucht Xvfb + GPU).

use std::process::Command;

#[test]
fn gui_screenshot_unter_xvfb() {
    let bin = env!("CARGO_BIN_EXE_sar_tdbp");
    let shot = std::env::temp_dir().join(format!("sar_gui_{}.png", std::process::id()));
    let out = Command::new("xvfb-run")
        .args([
            "-a",
            bin,
            "--phantom",
            "grid",
            "--size",
            "64",
            "--pulses",
            "64",
            "--frames",
            "5",
            "--screenshot",
        ])
        .arg(&shot)
        .output()
        .expect("xvfb-run starten");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(out.status.success(), "GUI-Exit: {}\n{stdout}", out.status);
    assert!(stdout.contains("Apertur-Pulse"), "Log:\n{stdout}");
    assert!(shot.is_file(), "Screenshot fehlt: {}", shot.display());

    // Inhalt: 800×600-Fenster mit nicht-trivialem Bild (Varianz > 0).
    let img = image::open(&shot).expect("PNG laden").to_luma8();
    assert_eq!((img.width(), img.height()), (800, 600));
    let px: Vec<f32> = img.pixels().map(|p| p.0[0] as f32).collect();
    let mean = px.iter().sum::<f32>() / px.len() as f32;
    let var = px.iter().map(|v| (v - mean).powi(2)).sum::<f32>() / px.len() as f32;
    assert!(var > 100.0, "Screenshot-Varianz {var} zu klein");
    // Linke Bildhälfte (SAR-Bild) unterscheidet sich von der rechten
    // Panel-Hälfte: beide nicht leer und nicht identisch.
    let (mut sl, mut sr) = (0.0f64, 0.0f64);
    for (i, v) in px.iter().enumerate() {
        if (i as u32 % 800) < 400 {
            sl += *v as f64;
        } else {
            sr += *v as f64;
        }
    }
    assert!(sl > 0.0 && sr > 0.0 && (sl - sr).abs() > 1.0);
    std::fs::remove_file(&shot).ok();
}
