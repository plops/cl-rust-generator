//! Parität Rust ↔ Ultralytics-Predictor (fp32-ONNX, conf 0.05, iou 0.7).
//! Braucht die Assets aus `scripts/export_models.sh` → `#[ignore]`;
//! ausführen mit `cargo test --release -- --ignored`.

use gui_detect::decode::iou;
use gui_detect::detector::Detector;
use gui_detect::image::Rgb;
use gui_detect::session::{Device, Model};
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

fn models() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("models")
}

fn reference(tag: &str) -> Vec<([f32; 4], f32)> {
    std::fs::read_to_string(models().join(format!("reference_{tag}.tsv")))
        .unwrap()
        .lines()
        .map(|l| {
            let v: Vec<f32> = l.split('\t').map(|x| x.parse().unwrap()).collect();
            ([v[0], v[1], v[2], v[3]], v[4])
        })
        .collect()
}

fn check(tag: &str) {
    let img = Rgb::read_ppm(BufReader::new(File::open(models().join("example_input.ppm")).unwrap())).unwrap();
    let bytes = std::fs::read(models().join(format!("gpa_{tag}_fp32.onnx"))).unwrap();
    let mut det = Detector::new(Model::load(&bytes, Device::Cpu, 0).unwrap());
    let (got, _) = det.detect(&img).unwrap();
    let refs = reference(tag);

    // Jede Referenzbox braucht ein Gegenstück mit IoU ≥ 0.9 und ähnlichem Score.
    let matched = refs
        .iter()
        .filter(|(b, s)| got.iter().any(|d| iou(b, &d.b) >= 0.9 && (d.score - s).abs() < 0.02))
        .count();
    let ratio = matched as f64 / refs.len() as f64;
    eprintln!("{tag}: {matched}/{} Referenzboxen getroffen, Rust {} Boxen", refs.len(), got.len());
    assert!(ratio >= 0.95, "{tag}: nur {ratio:.3} der Referenzboxen getroffen");
    assert!((got.len() as i64 - refs.len() as i64).abs() <= refs.len() as i64 / 20);
}

#[test]
#[ignore = "braucht models/ aus scripts/export_models.sh"]
fn parity_640() {
    check("640");
}

#[test]
#[ignore = "braucht models/ aus scripts/export_models.sh"]
fn parity_384x640() {
    check("384x640");
}
