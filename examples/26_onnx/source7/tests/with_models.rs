//! Modell-Vertragstests (ignored): prüfen SCRFD/ArcFace-Output-Layouts am echten `.onnx`.
//!
//! Braucht `models/*.onnx` (via `download_models.sh`). Lauf:
//! `cargo test --test with_models -- --ignored`.

use ort::{inputs, session::Session, value::TensorRef};

fn model(name: &str) -> Vec<u8> {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("models")
        .join(name);
    std::fs::read(&path)
        .unwrap_or_else(|_| panic!("Modell fehlt: {} (download_models.sh)", path.display()))
}

fn open(bytes: &[u8]) -> (Session, String) {
    let session = Session::builder()
        .unwrap()
        .commit_from_memory(bytes)
        .unwrap();
    let input = session.inputs()[0].name().to_string();
    (session, input)
}

#[test]
#[ignore]
fn scrfd_output_layout_matches_strides() {
    let bytes = model("det_500m.onnx");
    let (mut session, input_name) = open(&bytes);
    let mut input = vec![0.0f32; 3 * 640 * 640];
    for (i, x) in input.iter_mut().enumerate() {
        *x = (i as f32 % 255.0 - 127.5) / 128.0;
    }
    let outputs = session
        .run(inputs![input_name.as_str() =>
            TensorRef::from_array_view(([1, 3, 640, 640], &input[..])).unwrap()])
        .unwrap();
    assert_eq!(outputs.len(), 9, "3 Scores + 3 BBox + 3 Kps");
    for (i, &cells) in [80usize, 40, 20].iter().enumerate() {
        let n = cells * cells * 2; // _num_anchors = 2
        let (_, s) = outputs[i].try_extract_tensor::<f32>().unwrap();
        let (_, b) = outputs[i + 3].try_extract_tensor::<f32>().unwrap();
        let (_, k) = outputs[i + 6].try_extract_tensor::<f32>().unwrap();
        assert_eq!(s.len(), n, "scores Ebene {i}");
        assert_eq!(b.len(), n * 4, "bbox Ebene {i}");
        assert_eq!(k.len(), n * 10, "kps Ebene {i}");
    }
}

#[test]
#[ignore]
fn arcface_embeds_512d() {
    let bytes = model("w600k_mbf.onnx");
    let (mut session, input_name) = open(&bytes);
    let mut input = vec![0.0f32; 3 * 112 * 112];
    for (i, x) in input.iter_mut().enumerate() {
        *x = (i as f32 % 255.0 - 127.5) / 127.5;
    }
    let outputs = session
        .run(inputs![input_name.as_str() =>
            TensorRef::from_array_view(([1, 3, 112, 112], &input[..])).unwrap()])
        .unwrap();
    assert_eq!(outputs.len(), 1);
    let (_, d) = outputs[0].try_extract_tensor::<f32>().unwrap();
    assert_eq!(d.len(), 512);
    let norm: f32 = d.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!(norm > 1e-6, "Embedding darf nicht Null sein");
}
