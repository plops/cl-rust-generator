//! `03_infer` — YOLOv8-Inferenz via ort (S2).
//!
//! Preprocess (Letterbox 640×640), `output0`-Decode, Confidence-Filter,
//! NMS und Ruecktransformation auf Region-Koordinaten. Decode/NMS und
//! Geometrie sind rein und ohne Modell an synthetischen Tensoren
//! testbar; nur `load_session`/`infer_image` brauchen eine ONNX-Datei
//! (oder Netz fuer die URL).

use anyhow::{bail, Context, Result};
use image::{imageops::FilterType, RgbImage};
use ndarray::{s, Array2, Array4, ArrayViewD, Axis};
use ort::{
    inputs,
    session::{builder::GraphOptimizationLevel, Session},
    value::TensorRef,
};
use std::path::Path;

/// Modell-Eingabekante des YOLOv8-Exports.
pub const MODEL_SIZE: u32 = 640;
/// Letterbox-Fuellfarbe (YOLOv8-Konvention).
pub const LETTERBOX_PAD: u8 = 114;
/// Fallback-Eingabename, falls das Modell keinen nennt.
pub const FALLBACK_INPUT: &str = "images";
/// Ausgabename des YOLOv8-Exports.
pub const OUTPUT_NAME: &str = "output0";

/// 80 COCO-Klassen aus dem ort-Referenzbeispiel.
#[rustfmt::skip]
pub const CLASS_LABELS: [&str; 80] = [
    "person", "bicycle", "car", "motorcycle", "airplane", "bus", "train", "truck", "boat", "traffic light",
    "fire hydrant", "stop sign", "parking meter", "bench", "bird", "cat", "dog", "horse", "sheep", "cow", "elephant",
    "bear", "zebra", "giraffe", "backpack", "umbrella", "handbag", "tie", "suitcase", "frisbee", "skis", "snowboard",
    "sports ball", "kite", "baseball bat", "baseball glove", "skateboard", "surfboard", "tennis racket", "bottle",
    "wine glass", "cup", "fork", "knife", "spoon", "bowl", "banana", "apple", "sandwich", "orange", "broccoli",
    "carrot", "hot dog", "pizza", "donut", "cake", "chair", "couch", "potted plant", "bed", "dining table", "toilet",
    "tv", "laptop", "mouse", "remote", "keyboard", "cell phone", "microwave", "oven", "toaster", "sink", "refrigerator",
    "book", "clock", "vase", "scissors", "teddy bear", "hair drier", "toothbrush",
];

/// Achsenparallele Box in Quellbild-Koordinaten (Pixel).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BoundingBox {
    /// Linke Kante.
    pub x1: f32,
    /// Obere Kante.
    pub y1: f32,
    /// Rechte Kante (exklusiv gedacht, Clamp-robust).
    pub x2: f32,
    /// Untere Kante.
    pub y2: f32,
}

/// Eine Detektion: Box plus Klasse plus Confidence.
#[derive(Debug, Clone, PartialEq)]
pub struct Detection {
    /// Box in Quellbild-Pixeln.
    pub bbox: BoundingBox,
    /// Klassenname aus `CLASS_LABELS`.
    pub label: &'static str,
    /// Confidence 0..=1.
    pub conf: f32,
}

/// Letterbox-Parameter: Skalierung plus Padding auf 640×640.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Letterbox {
    /// Skalierung Quell- → Modellraum.
    pub scale: f32,
    /// Horizontales Padding (links) im Modellraum.
    pub pad_x: f32,
    /// Vertikales Padding (oben) im Modellraum.
    pub pad_y: f32,
}

impl Letterbox {
    /// Berechnet Skalierung + Padding fuer ein Quellbild.
    pub fn for_source(src_w: u32, src_h: u32) -> Result<Self> {
        if src_w == 0 || src_h == 0 {
            bail!("source image must be non-empty");
        }
        let size = MODEL_SIZE as f32;
        let scale = size / (src_w as f32).max(src_h as f32);
        let new_w = (src_w as f32 * scale).round();
        let new_h = (src_h as f32 * scale).round();
        Ok(Self {
            scale,
            pad_x: (size - new_w) / 2.0,
            pad_y: (size - new_h) / 2.0,
        })
    }

    /// Projiziert eine Box aus dem 640er-Modellraum zurueck aufs Quellbild
    /// (Padding abziehen, skalieren, auf Bildgrenzen clampen).
    pub fn back_project(&self, b: BoundingBox, src_w: u32, src_h: u32) -> BoundingBox {
        let unpad = |v: f32, pad: f32| (v - pad) / self.scale;
        let clamp = |v: f32, max: u32| v.clamp(0.0, max as f32);
        BoundingBox {
            x1: clamp(unpad(b.x1, self.pad_x), src_w),
            y1: clamp(unpad(b.y1, self.pad_y), src_h),
            x2: clamp(unpad(b.x2, self.pad_x), src_w),
            y2: clamp(unpad(b.y2, self.pad_y), src_h),
        }
    }
}

/// Schnittflaeche zweier Boxen (0 bei disjunkt/degeneriert).
pub fn intersection(a: &BoundingBox, b: &BoundingBox) -> f32 {
    let w = (a.x2.min(b.x2) - a.x1.max(b.x1)).max(0.0);
    let h = (a.y2.min(b.y2) - a.y1.max(b.y1)).max(0.0);
    w * h
}

/// Vereinigungsflaeche zweier Boxen.
pub fn union(a: &BoundingBox, b: &BoundingBox) -> f32 {
    let area = |b: &BoundingBox| ((b.x2 - b.x1).max(0.0)) * ((b.y2 - b.y1).max(0.0));
    area(a) + area(b) - intersection(a, b)
}

/// IoU zweier Boxen (0 bei leerer Vereinigung).
pub fn iou(a: &BoundingBox, b: &BoundingBox) -> f32 {
    let u = union(a, b);
    if u <= 0.0 {
        0.0
    } else {
        intersection(a, b) / u
    }
}

/// Resized das RGB-Bild per Letterbox auf 640×640 und normalisiert
/// nach NCHW-f32 (Werte 0..=1). Layout: `(1, 3, 640, 640)` mit Batch.
/// NO-OP for 640x640 input
pub fn preprocess(rgb: &[u8], src_w: u32, src_h: u32) -> Result<Array4<f32>> {
    let expect = (src_w as usize) * (src_h as usize) * 3;
    if rgb.len() != expect {
        bail!("rgb buffer size mismatch: got {}, need {expect}", rgb.len());
    }

    let size = MODEL_SIZE; // 640
    let mut input = Array4::<f32>::zeros((1, 3, size as usize, size as usize));
    let channel_len = (size * size) as usize;
    const INV_255: f32 = 1.0 / 255.0;

    let (r_plane, rest) = input.as_slice_mut().unwrap().split_at_mut(channel_len);
    let (g_plane, b_plane) = rest.split_at_mut(channel_len);

    // FAST PATH: If already 640x640, skip imageops::resize entirely!
    if src_w == size && src_h == size {
        for (i, px) in rgb.chunks_exact(3).enumerate() {
            r_plane[i] = f32::from(px[0]) * INV_255;
            g_plane[i] = f32::from(px[1]) * INV_255;
            b_plane[i] = f32::from(px[2]) * INV_255;
        }
        return Ok(input);
    }

    // SLOW PATH: Scale & pad to 640x640 (use FilterType::Nearest for speed)
    let lb = Letterbox::for_source(src_w, src_h)?;
    let src = RgbImage::from_raw(src_w, src_h, rgb.to_vec()).context("building source image")?;
    let new_w = ((src_w as f32 * lb.scale).round() as u32).max(1);
    let new_h = ((src_h as f32 * lb.scale).round() as u32).max(1);
    
    let resized = image::imageops::resize(&src, new_w, new_h, FilterType::Nearest);
    let mut canvas = RgbImage::from_pixel(size, size, image::Rgb([LETTERBOX_PAD; 3]));
    image::imageops::replace(
        &mut canvas,
        &resized,
        lb.pad_x.round() as i64,
        lb.pad_y.round() as i64,
    );

    for (i, px) in canvas.as_raw().chunks_exact(3).enumerate() {
        r_plane[i] = f32::from(px[0]) * INV_255;
        g_plane[i] = f32::from(px[1]) * INV_255;
        b_plane[i] = f32::from(px[2]) * INV_255;
    }

    Ok(input)
}


/// Holt die (N, 84)-Zeilen aus dem `output0`-Tensor (erwartet [1, C, N]).
///
/// Wie im Referenzbeispiel werden die Achsen transponiert und der
/// Batch-Index 0 gewaehlt.
pub fn extract_rows(output: ArrayViewD<'_, f32>) -> Result<Array2<f32>> {
    let shape = output.shape();
    if shape.len() != 3 || shape[0] != 1 {
        bail!("unexpected output0 shape {shape:?} (need [1, C, N])");
    }
    Ok(output.t().into_owned().slice(s![.., .., 0]).into_owned())
}

/// Decodiert (N, 4+Klassen)-Zeilen im 640er-Raum: Klassen-Maximum,
/// Confidence-Filter, NMS per IoU, Rueckprojektion aufs Quellbild.
pub fn decode(
    rows: &Array2<f32>,
    conf_min: f32,
    nms_iou: f32,
    lb: Letterbox,
    src_w: u32,
    src_h: u32,
) -> Vec<Detection> {
    let mut kept: Vec<(BoundingBox, &'static str, f32)> = Vec::new();
    for row in rows.axis_iter(Axis(0)) {
        let row: Vec<f32> = row.iter().copied().collect();
        if row.len() < 5 {
            continue;
        }
        let (class_id, conf) = row
            .iter()
            .skip(4)
            .enumerate()
            .map(|(i, v)| (i, *v))
            .reduce(|a, b| if b.1 > a.1 { b } else { a })
            .unwrap_or((0, 0.0));
        if conf < conf_min || class_id >= CLASS_LABELS.len() {
            continue;
        }
        let (xc, yc, w, h) = (row[0], row[1], row[2], row[3]);
        kept.push((
            BoundingBox {
                x1: xc - w / 2.0,
                y1: yc - h / 2.0,
                x2: xc + w / 2.0,
                y2: yc + h / 2.0,
            },
            CLASS_LABELS[class_id],
            conf,
        ));
    }
    kept.sort_by(|a, b| b.2.total_cmp(&a.2));
    // Greedy-NMS: staerkste Box behalten, stark ueberlappende verwerfen.
    let mut result: Vec<(BoundingBox, &'static str, f32)> = Vec::new();
    for cand in kept {
        if result
            .iter()
            .all(|kept: &(BoundingBox, &'static str, f32)| iou(&cand.0, &kept.0) < nms_iou)
        {
            result.push(cand);
        }
    }
    result
        .into_iter()
        .map(|(b, label, conf)| Detection {
            bbox: lb.back_project(b, src_w, src_h),
            label,
            conf,
        })
        .collect()
}

/// Laedt ein Modell: lokale Datei bevorzugt, sonst URL (mit ort-Cache).
pub fn load_session(model: &str) -> Result<Session> {
    let path = Path::new(model);
    if !path.is_file() && model.trim().is_empty() {
        bail!("--model must not be empty");
    }

    // Ermittle alle verfuegbaren logischen Kerne (Fallback auf 1)
    let cores = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    let mut builder = Session::builder()?
        .with_optimization_level(GraphOptimizationLevel::Level3)
        .map_err(|e| anyhow::anyhow!("{e}"))?
        .with_intra_threads(4) //cores)
        .map_err(|e| anyhow::anyhow!("{e}"))?;

    if path.is_file() {
        builder
            .commit_from_file(path)
            .with_context(|| format!("loading model file {model}"))
    } else {
        builder
            .commit_from_url(model)
            .with_context(|| format!("fetching model from {model}"))
    }
}

/// Ein Frame: Preprocess → Inferenz → Decode. Gibt Detektionen in
/// Quellbild-Pixeln zurueck.
pub fn infer_image(
    session: &mut Session,
    rgb: &[u8],
    src_w: u32,
    src_h: u32,
    conf: f32,
    nms: f32,
) -> Result<Vec<Detection>> {
    let lb = Letterbox::for_source(src_w, src_h)?;
    let input = preprocess(rgb, src_w, src_h)?;
    let name = session
        .inputs()
        .first()
        .map(|o| o.name().to_string())
        .unwrap_or_else(|| FALLBACK_INPUT.to_string());
    let outputs = session.run(inputs![name.as_str() => TensorRef::from_array_view(&input)?])?;
    let output = outputs
        .get(OUTPUT_NAME)
        .with_context(|| {
            let keys: Vec<&str> = outputs.keys().collect();
            format!("model output {OUTPUT_NAME:?} missing (got {keys:?}); is this a YOLOv8 model?")
        })?
        .try_extract_array::<f32>()?;
    let rows = extract_rows(output)?;
    Ok(decode(&rows, conf, nms, lb, src_w, src_h))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndarray::Array3;

    fn lb_800x600() -> (Letterbox, u32, u32) {
        (Letterbox::for_source(800, 600).unwrap(), 800, 600)
    }

    #[test]
    fn letterbox_params_800x600() {
        let (lb, _, _) = lb_800x600();
        assert!((lb.scale - 0.8).abs() < 1e-6);
        assert!(lb.pad_x.abs() < 1e-6);
        assert!((lb.pad_y - 80.0).abs() < 1e-6);
    }

    #[test]
    fn letterbox_rejects_empty() {
        assert!(Letterbox::for_source(0, 100).is_err());
        assert!(Letterbox::for_source(100, 0).is_err());
    }

    #[test]
    fn back_project_center_is_exact() {
        // 640er-Box um die Bildmitte -> Quellbild-Mitte (800x600, pad_y=80).
        let (lb, w, h) = lb_800x600();
        let b = BoundingBox {
            x1: 300.0,
            y1: 300.0,
            x2: 340.0,
            y2: 340.0,
        };
        let r = lb.back_project(b, w, h);
        assert!((r.x1 - 375.0).abs() < 1e-3);
        assert!((r.y1 - 275.0).abs() < 1e-3);
        assert!((r.x2 - 425.0).abs() < 1e-3);
        assert!((r.y2 - 325.0).abs() < 1e-3);
    }

    #[test]
    fn back_project_clamps_to_source() {
        let (lb, w, h) = lb_800x600();
        let b = BoundingBox {
            x1: -100.0,
            y1: -100.0,
            x2: 900.0,
            y2: 900.0,
        };
        let r = lb.back_project(b, w, h);
        assert_eq!((r.x1, r.y1), (0.0, 0.0));
        assert_eq!((r.x2, r.y2), (w as f32, h as f32));
    }

    #[test]
    fn iou_basics() {
        let a = BoundingBox {
            x1: 0.0,
            y1: 0.0,
            x2: 10.0,
            y2: 10.0,
        };
        assert!((iou(&a, &a) - 1.0).abs() < 1e-6);
        let b = BoundingBox {
            x1: 20.0,
            y1: 20.0,
            x2: 30.0,
            y2: 30.0,
        };
        assert_eq!(iou(&a, &b), 0.0);
        let c = BoundingBox {
            x1: 5.0,
            y1: 0.0,
            x2: 15.0,
            y2: 10.0,
        };
        assert!((iou(&a, &c) - 50.0 / 150.0).abs() < 1e-6);
    }

    /// Baut eine synthetische (N,84)-Zeile im 640er-Raum.
    fn row(xc: f32, yc: f32, w: f32, h: f32, class: usize, conf: f32) -> Vec<f32> {
        let mut r = vec![xc, yc, w, h];
        r.extend(std::iter::repeat_n(0.0, 80));
        r[4 + class] = conf;
        r
    }

    #[test]
    fn decode_filters_and_nms() {
        let (lb, w, h) = lb_800x600();
        // Zwei fast identische Personen-Boxen (eine muss per NMS fallen),
        // eine schwache Auto-Box (Conf-Filter), ein Auto weit weg (bleibt).
        let rows = Array2::from_shape_vec(
            (4, 84),
            [
                row(320.0, 320.0, 100.0, 100.0, 0, 0.9),
                row(322.0, 322.0, 100.0, 100.0, 0, 0.8),
                row(100.0, 100.0, 50.0, 50.0, 2, 0.4),
                row(500.0, 500.0, 40.0, 40.0, 2, 0.85),
            ]
            .concat(),
        )
        .unwrap();
        let det = decode(&rows, 0.5, 0.7, lb, w, h);
        assert_eq!(det.len(), 2);
        assert_eq!(det[0].label, "person");
        assert!((det[0].conf - 0.9).abs() < 1e-6);
        assert_eq!(det[1].label, "car");
    }

    #[test]
    fn decode_skips_short_rows() {
        let (lb, w, h) = lb_800x600();
        let rows = Array2::zeros((2, 3));
        assert!(decode(&rows, 0.5, 0.7, lb, w, h).is_empty());
    }

    #[test]
    fn extract_rows_transposes_batch() {
        // [1, 84, 2]-Tensor -> (2, 84)-Zeilen.
        let mut t = Array3::<f32>::zeros((1, 84, 2));
        t[[0, 0, 0]] = 320.0;
        t[[0, 4, 1]] = 0.9;
        let rows = extract_rows(t.view().into_dimensionality().unwrap()).unwrap();
        assert_eq!(rows.shape(), &[2, 84]);
        assert!((rows[[0, 0]] - 320.0).abs() < 1e-6);
        assert!((rows[[1, 4]] - 0.9).abs() < 1e-6);
    }

    #[test]
    fn extract_rows_rejects_bad_shape() {
        let t = Array3::<f32>::zeros((2, 84, 2));
        assert!(extract_rows(t.view().into_dimensionality().unwrap()).is_err());
    }

    #[test]
    fn preprocess_shape_and_range() {
        let rgb = vec![200u8; 64 * 48 * 3];
        let input = preprocess(&rgb, 64, 48).unwrap();
        assert_eq!(input.shape(), &[1, 3, 640, 640]);
        assert!(input.iter().all(|v| (0.0..=1.0).contains(v)));
        // Letterbox-Fuellung (114/255) muss in einer Ecke stehen.
        assert!((input[[0, 0, 0, 0]] - 114.0 / 255.0).abs() < 0.02);
    }

    #[test]
    fn preprocess_rejects_bad_length() {
        assert!(preprocess(&[0u8; 10], 64, 48).is_err());
    }

    #[test]
    fn labels_have_80_coco_entries() {
        assert_eq!(CLASS_LABELS.len(), 80);
        assert_eq!(CLASS_LABELS[0], "person");
        assert_eq!(CLASS_LABELS[79], "toothbrush");
    }
}
