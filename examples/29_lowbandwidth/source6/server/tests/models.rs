//! Nachweis mit echten Modellen (braucht `./scripts/fetch_models.sh`):
//! `cargo test --release -p lbw-server --test models -- --ignored --nocapture`

use lbw_server::analyze::{Analyzer, ModelPaths, icons};
use lbw_server::av1::{Av1Params, encode_rgb};
use lbw_server::image::Rgb;
use lbw_server::layout::mask;
use lbw_server::text_diff::TextState;

const M: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../models/");

fn paths() -> ModelPaths {
    let gui = format!("{M}gpa_640_int8.onnx");
    ModelPaths {
        det: format!("{M}PP-OCRv6_small_det.onnx"),
        rec: format!("{M}PP-OCRv6_small_rec.onnx"),
        dict: format!("{M}inference.yml"),
        gui: std::path::Path::new(&gui).exists().then_some(gui),
        threads: 8,
    }
}

fn screen() -> Rgb {
    let f = std::fs::File::open(format!("{M}test_screen.ppm")).expect("fetch_models.sh");
    let full = Rgb::read_ppm(std::io::BufReader::new(f)).unwrap();
    let mut img = Rgb::filled(640, 640, [0; 3]);
    for y in 0..640 {
        let s = y * full.w * 3;
        img.data[y * 640 * 3..(y + 1) * 640 * 3].copy_from_slice(&full.data[s..s + 640 * 3]);
    }
    img
}

#[test]
#[ignore = "braucht Modelle in source6/models"]
fn ocr_finds_text_and_masking_shrinks_av1() {
    let mut a = Analyzer::load(&paths()).unwrap();
    let img = screen();
    let texts = a.text(&img).unwrap();
    for t in texts.iter().take(12) {
        eprintln!("{:?} fg{:?} bg{:?} {}", t.rect, t.fg, t.bg, t.text);
    }
    eprintln!("{} Zeilen, {:?}", texts.len(), a.timings);
    assert!(texts.len() >= 10, "zu wenig Text erkannt");
    assert!(
        texts
            .iter()
            .any(|t| t.text.chars().filter(|c| c.is_alphabetic()).count() >= 8)
    );
    let rec_cold = a.timings.rec;
    assert_eq!(a.text(&img).unwrap(), texts, "Cache muss identisch liefern");
    eprintln!(
        "Erkennung kalt {rec_cold:.1} ms, mit Cache {:.1} ms",
        a.timings.rec
    );
    assert!(a.timings.rec < rec_cold / 5.0);

    let gui = a.gui_boxes(&img).unwrap();
    let mut st = TextState::new();
    let delta = st.update(texts);
    let mut masked = img.clone();
    mask(&mut masked, st.items());
    let icons = icons(&gui, st.items(), &masked);
    eprintln!(
        "{} GUI-Boxen, {} Icons: {:?} ({:?})",
        gui.len(),
        icons.len(),
        icons,
        a.timings
    );
    assert!(icons.len() < gui.len());
    let p = Av1Params::default();
    let raw = encode_rgb(&img.data, 640, 640, p).unwrap().len();
    let msk = encode_rgb(&masked.data, 640, 640, p).unwrap().len();
    let text_bytes = lbw_common::ServerMsg::Text {
        seq: 1,
        remove: vec![],
        add: delta.add,
    }
    .encode()
    .len();
    eprintln!("AV1 roh {raw} B, maskiert {msk} B, Text {text_bytes} B");
    assert!(msk < raw, "Maskierung muss AV1 verkleinern");
}
