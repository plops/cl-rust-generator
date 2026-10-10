//! Modell-Test (ignored): echte PP-OCRv6-Modelle aus `models/` (Symlink).
//! Laufen lassen mit:
//! `cargo test --release -p lbw-server --test models -- --ignored`
//! Mit `LBW_CPU=1` läuft er auf CPU (Vergleichsmessung); sonst muss CUDA
//! aktiv sein.

use lbw_server::detect::Provider;
use lbw_server::ocr::Ocr;

fn models_dir() -> String {
    format!("{}/../models", env!("CARGO_MANIFEST_DIR"))
}

#[test]
#[ignore]
fn detects_text_on_real_screenshot() {
    let dir = models_dir();
    let ppm = format!("{dir}/test_screen.ppm");
    assert!(std::path::Path::new(&ppm).exists(), "Testbild fehlt: {ppm}");
    let full = image::open(&ppm).unwrap().to_rgb8();
    // Wie der Server: festen 1280×720-Ausschnitt verwenden.
    let img = image::imageops::crop_imm(&full, 0, 0, 1280, 720).to_image();
    assert_eq!((img.width(), img.height()), (1280, 720));

    let cpu = std::env::var("LBW_CPU").is_ok();
    let provider = if cpu { Provider::Cpu } else { Provider::Auto };
    let mut ocr = Ocr::load(&dir, 8, provider).unwrap();
    assert_eq!(
        ocr.ep(),
        if cpu { "CPU" } else { "CUDA+CPU" },
        "falscher Execution-Provider"
    );
    let t = std::time::Instant::now();
    let texts = ocr.text(&img).unwrap();
    let total = t.elapsed().as_secs_f64() * 1000.0;
    let (det_ms, rec_ms) = ocr.last_ms;
    // Zweiter Durchlauf: ohne Kaltstart (CUDA-Kontext, cuDNN-Autotune).
    let texts2 = ocr.text(&img).unwrap();
    let (det2, rec2) = ocr.last_ms;
    assert_eq!(texts, texts2, "zweiter Durchlauf weicht ab");
    // Gleiches Bild → alle Zweit-Zugriffe aus dem Cache (1. Lauf Misses).
    let (hits, lookups) = ocr.cache_stats();
    assert!(hits > 0 && hits * 2 == lookups, "{hits}/{lookups}");
    assert!(!texts.is_empty(), "kein Text auf dem Testbild erkannt");
    let chars: usize = texts.iter().map(|t| t.text.chars().count()).sum();
    assert!(chars > 20, "zu wenig Text: {texts:?}");
    // Alle Boxen müssen im 720p-Original liegen (nicht in der 736er-Padzone).
    for x in &texts {
        assert!(
            x.rect.x2() <= 1280 && x.rect.y2() <= 720,
            "Box außerhalb: {:?}",
            x.rect
        );
    }
    eprintln!(
        "[models] EP={} {} Zeilen, {} Zeichen (kalt: det {det_ms:.1} ms, rec {rec_ms:.1} ms, gesamt {total:.1} ms; warm: det {det2:.1} ms, rec {rec2:.1} ms)",
        ocr.ep(),
        texts.len(),
        chars
    );
    for x in texts.iter().take(5) {
        eprintln!("[models] {:?} {:?}", x.rect, x.text);
    }
}
