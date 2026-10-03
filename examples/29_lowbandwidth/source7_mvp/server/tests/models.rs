//! Modell-Test (ignored): echte PP-OCRv6-Modelle aus `source6/models`.
//! Laufen lassen mit:
//! `cargo test --release -p lbw-server --test models -- --ignored`

use lbw_server::ocr::Ocr;

fn models_dir() -> String {
    format!("{}/../../source6/models", env!("CARGO_MANIFEST_DIR"))
}

#[test]
#[ignore]
fn detects_text_on_real_screenshot() {
    let dir = models_dir();
    let ppm = format!("{dir}/test_screen.ppm");
    assert!(std::path::Path::new(&ppm).exists(), "Testbild fehlt: {ppm}");
    let full = image::open(&ppm).unwrap().to_rgb8();
    // Wie der Server: festen 640×640-Ausschnitt verwenden.
    let img = image::imageops::crop_imm(&full, 0, 0, 640, 640).to_image();
    assert_eq!((img.width(), img.height()), (640, 640));

    let mut ocr = Ocr::load(&dir, 8).unwrap();
    let texts = ocr.text(&img).unwrap();
    assert!(!texts.is_empty(), "kein Text auf dem Testbild erkannt");
    let chars: usize = texts.iter().map(|t| t.text.chars().count()).sum();
    assert!(chars > 20, "zu wenig Text: {texts:?}");
    eprintln!("[models] {} Zeilen, {} Zeichen", texts.len(), chars);
    for t in texts.iter().take(5) {
        eprintln!("[models] {:?} {:?}", t.rect, t.text);
    }
}
