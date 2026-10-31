//! Prüfstand mit echten Daten als Test: das Server-Log
//! `../source10_instrumentation/20261018_0839.lbwlog` (gitignored — fehlt es,
//! wird übersprungen) liefert Bildschirm + Texte für 15 s um die 42-kB-Kachel
//! (s+59) herum; die echte Session sendet durch einen 6-kB/s-Drosselleser.
//! Erwartung source10_b: keine Nachricht über dem Deckel, Text-Echo < 1,5 s
//! (source10 auf demselben Fenster: Sekunden — s. Walkthrough).

#[path = "../examples/bench.rs"]
mod bench;

#[test]
fn real_session_keeps_text_latency_low_under_big_tiles() {
    let log = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../source10_instrumentation/20261018_0839.lbwlog"
    );
    if !std::path::Path::new(log).exists() {
        eprintln!("übersprungen: {log} fehlt");
        return;
    }
    let args: Vec<String> = [log, "--from", "52", "--secs", "15", "--drain", "5"]
        .iter()
        .map(|s| (*s).to_owned())
        .collect();
    let r = bench::run(&args);
    assert!(r.tiles > 0 && r.text_n > 0, "{r:?}");
    assert!(r.max_msg <= 2400 + 32, "Nachricht über dem Deckel: {r:?}");
    assert!(r.text_p90 < 1500.0, "Text-Echo zu langsam: {r:?}");
    assert!(r.bps <= 6000.0 * 1.1, "über Budget: {r:?}");
    // Fast jeder Text des Fensters kommt an (Rest: erschien kurz vor Ende).
    assert!(
        r.text_missing * 10 <= r.text_n,
        "zu viele fehlende Texte: {r:?}"
    );
}
