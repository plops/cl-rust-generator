//! `geometry` — Koordinaten-Vertrag ohne Modell/X11/GPU.
//!
//! Letterbox-Rueckprojektion und NMS ueber die oeffentliche
//! lib-API (always-green-Basis, auch offline).

use ndarray::Array2;
use source1::{BoundingBox, Letterbox, decode};

/// Eine synthetische (84,)-Zeile im 640er-Raum.
fn row(xc: f32, yc: f32, w: f32, h: f32, class: usize, conf: f32) -> Vec<f32> {
    let mut r = vec![xc, yc, w, h];
    r.extend(std::iter::repeat_n(0.0, 80));
    r[4 + class] = conf;
    r
}

#[test]
fn full_frame_maps_to_full_source() {
    // 640er-Vollbild -> Quell-Vollbild (hier quadratisch, pad-frei).
    let lb = Letterbox::for_source(500, 500).unwrap();
    let full = BoundingBox {
        x1: 0.0,
        y1: 0.0,
        x2: 640.0,
        y2: 640.0,
    };
    let r = lb.back_project(full, 500, 500);
    assert!((r.x2 - r.x1 - 500.0).abs() < 1e-3);
    assert!((r.y2 - r.y1 - 500.0).abs() < 1e-3);
}

#[test]
fn wide_source_uses_vertical_padding() {
    // 800x400 -> scale 0.8, 640x320, pad_y = 160.
    let lb = Letterbox::for_source(800, 400).unwrap();
    assert!((lb.scale - 0.8).abs() < 1e-6);
    assert!((lb.pad_y - 160.0).abs() < 1e-6);
    let center = BoundingBox {
        x1: 310.0,
        y1: 310.0,
        x2: 330.0,
        y2: 330.0,
    };
    let r = lb.back_project(center, 800, 400);
    assert!((r.x1 + r.x2) / 2.0 - 400.0 < 1e-3);
    assert!((r.y1 + r.y2) / 2.0 - 200.0 < 1e-3);
}

#[test]
fn decode_end_to_end_through_public_api() {
    let lb = Letterbox::for_source(640, 480).unwrap();
    let rows = Array2::from_shape_vec(
        (3, 84),
        [
            row(320.0, 240.0, 200.0, 200.0, 0, 0.92),
            row(324.0, 244.0, 200.0, 200.0, 0, 0.7),
            row(100.0, 100.0, 30.0, 30.0, 1, 0.3),
        ]
        .concat(),
    )
    .unwrap();
    let det = decode(&rows, 0.5, 0.7, lb, 640, 480);
    assert_eq!(
        det.len(),
        1,
        "NMS muss Duplikat entfernen, Conf-Filter die schwache Box"
    );
    assert_eq!(det[0].label, "person");
    // Box liegt im Quellbild.
    assert!(det[0].bbox.x1 >= 0.0 && det[0].bbox.y1 >= 0.0);
    assert!(det[0].bbox.x2 <= 640.0 && det[0].bbox.y2 <= 480.0);
}
