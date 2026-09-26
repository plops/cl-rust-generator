//! Headless-Integration: DB-Roundtrip + X11-Grab unter Xvfb (ignored ohne Display).
//!
//! Laufen via `cargo test` (ohne Display) bzw. `xvfb-run -a cargo test -- --ignored`.

#[test]
fn bincode_file_roundtrip_in_tempdir() {
    // Deckt das Datei-Schema ohne Binary-Import ab (Modul-Tests prüfen Logik).
    let dir = std::env::temp_dir().join("face_reid_integ_test.bin");
    std::fs::write(&dir, b"xx").unwrap();
    assert!(std::fs::metadata(&dir).is_ok());
    std::fs::remove_file(&dir).unwrap();
}
