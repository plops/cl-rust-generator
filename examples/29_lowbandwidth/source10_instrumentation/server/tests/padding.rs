//! Padding-Sweep (ignored): misst gute REC_PAD-/MASK_PAD-Werte auf einem
//! echten xterm-Screenshot. Startet ein eigenes Xvfb + xterm, braucht echte
//! Modelle und läuft nur im Release-Modus sinnvoll schnell:
//! `cargo test --release -p lbw-server --test padding -- --ignored --nocapture`
//!
//! Der Test druckt zwei Tabellen (Erkennung je `pad`, AV1-Bytes je
//! Masken-`pad`) und pinnt das Produktverhalten: Der ASCII-Marker muss über
//! den echten [`Ocr`]-Pfad gefunden werden, und die Default-Paddings dürfen
//! nicht schlechter sein als `0`.

use std::process::{Child, Command};
use std::time::Duration;

use clap::Parser;
use image::RgbImage;
use lbw_server::av1::encode_rgb;
use lbw_server::capture::{FrameSource, ScrapSource};
use lbw_server::config::Config;
use lbw_server::detect::{Detector, Provider, clip_to, pad_to_32};
use lbw_server::ocr::{Ocr, sample_colors};
use lbw_server::recognize::Recognizer;
use lbw_server::tiles::{MASK_PAD, crop_rgb, fill_rect, pad_rect};

/// Was das xterm anzeigt (ASCII-Anteil wird assertiert, Umlaute nur berichtet).
const MARKER_ASCII: &str = "PADDING-SWEEP-720";
/// Display für das eigene Xvfb (muss frei sein).
const DISPLAY: &str = ":97";
/// Wie `Ocr::text` (dort privat): Mindest-Konfidenz je Zeile.
const MIN_CONF: f32 = 0.5;

fn models_dir() -> String {
    format!("{}/../models", env!("CARGO_MANIFEST_DIR"))
}

/// Eigenes Xvfb + xterm; räumt beim Drop auf.
struct Xterm {
    xvfb: Child,
    xterm: Child,
}

impl Xterm {
    fn start() -> Self {
        let mut xvfb = Command::new("Xvfb")
            .args([DISPLAY, "-screen", "0", "1280x720x24"])
            .spawn()
            .expect("Xvfb fehlt (apt install xvfb) oder Display belegt");
        std::thread::sleep(Duration::from_secs(1));
        let line = format!("echo '{MARKER_ASCII} ÄÖÜäöüß'; exec sleep 120");
        // PADDING_XTERM_EXTRA="..." hängt weitere xterm-Argumente an (z. B.
        // "-fa Monospace -fs 14" für einen zweiten Messpunkt mit Skalierfont).
        let extra = std::env::var("PADDING_XTERM_EXTRA").unwrap_or_default();
        let mut args = vec!["-u8", "-geometry", "80x24+10+10"];
        args.extend(extra.split_whitespace());
        args.extend(["-e", "sh", "-c", &line]);
        let xterm = Command::new("xterm")
            .args(&args)
            .env("DISPLAY", DISPLAY)
            .env("LANG", "C.UTF-8")
            .spawn()
            .expect("xterm fehlt (apt install xterm)");
        std::thread::sleep(Duration::from_secs(2));
        if xvfb.try_wait().expect("Xvfb-Status").is_some() {
            panic!("Xvfb startete nicht (Display {DISPLAY} belegt?)");
        }
        Self { xvfb, xterm }
    }

    fn capture(&self) -> RgbImage {
        unsafe { std::env::set_var("DISPLAY", DISPLAY) };
        let mut src = ScrapSource::open(0, 0, 1280, 720).expect("Capture öffnen");
        src.grab().expect("Frame lesen")
    }
}

impl Drop for Xterm {
    fn drop(&mut self) {
        let _ = self.xterm.kill();
        let _ = self.xvfb.kill();
    }
}

#[test]
#[ignore]
fn sweep_padding_on_xterm() {
    let dir = models_dir();
    for f in [
        "PP-OCRv6_small_det.onnx",
        "PP-OCRv6_small_rec.onnx",
        "inference.yml",
    ] {
        assert!(
            std::path::Path::new(&format!("{dir}/{f}")).exists(),
            "Modell fehlt: {dir}/{f}"
        );
    }
    let xt = Xterm::start();
    let img = xt.capture();
    img.save("/tmp/padding_frame.png").unwrap();
    eprintln!("[padding] Capture nach /tmp/padding_frame.png geschrieben");

    // Produktpfad (mit REC_PAD-Default): ASCII-Marker muss gefunden werden.
    let mut ocr = Ocr::load(&dir, 4, Provider::Auto).unwrap();
    let items = ocr.text(&img).unwrap();
    let joined = items
        .iter()
        .map(|t| t.text.as_str())
        .collect::<Vec<_>>()
        .join(" ");
    eprintln!("[padding] Produkt: {} Zeilen: {joined:?}", items.len());
    assert!(
        joined.contains(MARKER_ASCII),
        "Marker {MARKER_ASCII:?} nicht erkannt in {joined:?}"
    );

    // Erkennungs-Sweep: Detektion einmal, Recognition je pad.
    let mut det =
        Detector::new(&format!("{dir}/PP-OCRv6_small_det.onnx"), 4, Provider::Auto).unwrap();
    let mut rec = Recognizer::new(
        &format!("{dir}/PP-OCRv6_small_rec.onnx"),
        &format!("{dir}/inference.yml"),
        4,
    )
    .unwrap();
    // Wie `Ocr::text`: auf 32er-Vielfache padden, Boxen zurückclippen.
    let (iw, ih) = (img.width(), img.height());
    let (padded, ow, oh) = pad_to_32(&img);
    let boxes: Vec<_> = det
        .detect(&padded)
        .unwrap()
        .into_iter()
        .filter_map(|b| clip_to(b, ow, oh))
        .collect();
    assert!(!boxes.is_empty(), "keine Box detektiert");
    eprintln!("[padding] {} Boxen detektiert", boxes.len());
    let mut rec_chars = Vec::new();
    for pad in [0u16, 2, 4, 6, 8] {
        let mut n = 0;
        let mut umlauts = 0;
        for b in &boxes {
            let r = pad_rect(*b, pad, iw, ih);
            let (t, c) = rec.recognize(&img, r).unwrap();
            if !t.trim().is_empty() && c >= MIN_CONF {
                n += t.chars().count();
                umlauts += t.chars().filter(|c| "ÄÖÜäöüß".contains(*c)).count();
            }
        }
        rec_chars.push((pad, n));
        eprintln!("[padding] rec_pad={pad}: {n} Zeichen, davon {umlauts} Umlaute/ß");
    }
    let at = |p: u16| rec_chars.iter().find(|(q, _)| *q == p).unwrap().1;
    assert!(at(4) >= at(0), "REC_PAD=4 schlechter als 0: {rec_chars:?}");

    // Masken-Sweep: Textregion maskieren, Rest als AV1 messen.
    // Echter Server-Default statt hartkodierter Zahl.
    let quantizer = Config::try_parse_from(["lbw-server"]).unwrap().quantizer;
    let mut mask_bytes = Vec::new();
    for mpad in [0u16, 2, 4, 6, 8, 10, 12] {
        let mut masked = img.clone();
        for b in &boxes {
            let r = pad_rect(*b, 4, iw, ih);
            let (_, bg) = sample_colors(&img, r);
            fill_rect(&mut masked, pad_rect(r, mpad, iw, ih), bg);
        }
        // Feste Vergleichsregion: Union aller Boxen (mit Max-Pad), gerade.
        let (iw16, ih16) = (iw as u16, ih as u16);
        let mut x0 = iw16;
        let mut y0 = ih16;
        let mut x1 = 0u16;
        let mut y1 = 0u16;
        for b in &boxes {
            let r = pad_rect(*b, 16, iw, ih);
            x0 = x0.min(r.x);
            y0 = y0.min(r.y);
            x1 = x1.max(r.x + r.w);
            y1 = y1.max(r.y + r.h);
        }
        let w = ((x1 - x0 + 1) as usize).max(16);
        let h = ((y1 - y0 + 1) as usize).max(16);
        let (w, h) = (w + w % 2, h + h % 2);
        let r = lbw_common::Rect::new(
            x0.min(iw16 - w as u16),
            y0.min(ih16 - h as u16),
            w as u16,
            h as u16,
        );
        let rgb = crop_rgb(&masked, r);
        let bytes = encode_rgb(&rgb, w, h, quantizer).unwrap().len();
        mask_bytes.push((mpad, bytes));
        eprintln!("[padding] mask_pad={mpad}: Region {w}x{h} = {bytes} B AV1");
    }
    let at = |p: u16| mask_bytes.iter().find(|(q, _)| *q == p).unwrap().1;
    assert!(
        at(MASK_PAD) <= at(0),
        "MASK_PAD schlechter als 0: {mask_bytes:?}"
    );
}
