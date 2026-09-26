//! `04_scrfd_detector` — SCRFD-Session, Preprocessing, Anchor-Decoding, NMS.
//!
//! 1:1 zum Referenzcode (`yakhyo/face-reidentification`): 9 Outputs
//! (Scores/BBox/Kps × Strides 8/16/32), `_num_anchors=2`, `distance2bbox`,
//! `distance2kps`, Greedy-NMS mit `+1`-Flächen. Nativ 640 → `det_scale=1`.

use crate::types::{BBox, FaceDetection, Landmarks5};
use ort::session::Session;

use ort::execution_providers::CPU;
#[cfg(feature = "cuda")]
use ort::execution_providers::CUDA;

/// FPN-Strides (Modell-Ebenen).
pub const STRIDES: [usize; 3] = [8, 16, 32];
/// Anker pro Location (Center dupliziert, wie im Referenzcode).
pub const NUM_ANCHORS: usize = 2;
/// Default-Konfidenzschwelle.
pub const CONF_DEFAULT: f32 = 0.5;
/// Default-NMS-IoU-Schwelle.
pub const IOU_DEFAULT: f32 = 0.4;
/// Native Modell-Kante (kein Resize nötig).
pub const INPUT_SIZE: usize = 640;

/// Baut eine Session: CUDA zuerst, bei Fehler CPU-Fallback. Gibt den
/// aktiven Provider-Namen fürs HUD zurück.
pub fn session_with_fallback(path: &str, _warmup: [usize; 4]) -> (Session, &'static str) {
    // Modell in den Speicher lesen (wie source2): `commit_from_memory`
    // braucht kein `std`-Feature in ort.
    let bytes = std::fs::read(path).unwrap();
    #[cfg(feature = "cuda")]
    if let Some(s) = try_cuda(&bytes, _warmup) {
        return (s, "CUDA");
    }
    let threads = std::thread::available_parallelism().map_or(0, std::num::NonZero::get);
    let mut builder = Session::builder()
        .unwrap()
        .with_execution_providers([CPU::default().build()])
        .unwrap()
        .with_intra_threads(threads)
        .unwrap();
    let session = builder.commit_from_memory(&bytes).unwrap();
    (session, "CPU")
}

/// Baut eine CUDA-Session und beweist per Warmup-Inferenz, dass der EP
/// wirklich rechnen kann (Commit allein reicht nicht: cuDNN fehlt z.B.
/// erst beim ersten Conv-Knoten auf). `None` → CPU-Fallback im Caller.
#[cfg(feature = "cuda")]
fn try_cuda(bytes: &[u8], warmup: [usize; 4]) -> Option<Session> {
    use ort::value::TensorRef;
    let mut s = Session::builder()
        .and_then(|b| Ok(b.with_execution_providers([CUDA::default().build()])?))
        .and_then(|mut b| b.commit_from_memory(bytes))
        .ok()?;
    let input_name = s.inputs()[0].name().to_string();
    let zeros = vec![0.0f32; warmup.iter().product()];
    s.run(ort::inputs![input_name.as_str() =>
        TensorRef::from_array_view((warmup, &zeros[..])).unwrap()])
        .ok()?;
    Some(s)
}

/// SCRFD-Detektor mit Schwellen und Session.
pub struct ScrfdDetector {
    session: Session,
    input_name: String,
    /// Konfidenzschwelle.
    pub conf_thres: f32,
    /// NMS-IoU-Schwelle.
    pub iou_thres: f32,
    /// Aktiver Provider (`CUDA`/`CPU`).
    pub provider: &'static str,
}

impl ScrfdDetector {
    /// Lädt das Modell (`det_500m.onnx`) mit EP-Fallback.
    pub fn open(path: &str) -> Self {
        let (session, provider) = session_with_fallback(path, [1, 3, INPUT_SIZE, INPUT_SIZE]);
        let input_name = session.inputs()[0].name().to_string();
        Self {
            session,
            input_name,
            conf_thres: CONF_DEFAULT,
            iou_thres: IOU_DEFAULT,
            provider,
        }
    }

    /// Detektiert Gesichter im 640×640-RGB-Frame.
    pub fn detect(&mut self, rgb: &[u8]) -> Vec<FaceDetection> {
        use ort::value::TensorRef;
        let input = preprocess_640(rgb);
        let outputs = self
            .session
            .run(ort::inputs![self.input_name.as_str() =>
                TensorRef::from_array_view(([1, 3, INPUT_SIZE, INPUT_SIZE], &input[..])).unwrap()])
            .unwrap();
        let mut flat: Vec<Vec<f32>> = Vec::with_capacity(9);
        for i in 0..9 {
            let (_, d) = outputs[i].try_extract_tensor::<f32>().unwrap();
            flat.push(d.to_vec());
        }
        let mut dets = Vec::new();
        for (idx, &stride) in STRIDES.iter().enumerate() {
            dets.extend(decode_stride(
                &flat[idx],
                &flat[idx + 3],
                &flat[idx + 6],
                stride,
                self.conf_thres,
            ));
        }
        nms_greedy(dets, self.iou_thres)
    }
}

/// `(x−127.5)/128`, RGB-planar (NCHW ohne Batch-Dim).
pub fn preprocess_640(rgb: &[u8]) -> Vec<f32> {
    let plane = INPUT_SIZE * INPUT_SIZE;
    let mut out = vec![0.0f32; 3 * plane];
    let (r, rest) = out.split_at_mut(plane);
    let (g, b) = rest.split_at_mut(plane);
    let (px3, _) = rgb.as_chunks::<3>();
    for (i, px) in px3.iter().take(plane).enumerate() {
        r[i] = (f32::from(px[0]) - 127.5) / 128.0;
        g[i] = (f32::from(px[1]) - 127.5) / 128.0;
        b[i] = (f32::from(px[2]) - 127.5) / 128.0;
    }
    out
}

/// Anchor-Center für eine Ebene (zeilenmajor, jedes ×2 dupliziert).
pub fn anchor_centers(stride: usize) -> Vec<[f32; 2]> {
    let n = INPUT_SIZE / stride;
    let mut v = Vec::with_capacity(n * n * NUM_ANCHORS);
    for y in 0..n {
        for x in 0..n {
            let p = [(x * stride) as f32, (y * stride) as f32];
            for _ in 0..NUM_ANCHORS {
                v.push(p);
            }
        }
    }
    v
}

/// Decodiert eine FPN-Ebene: Scores (N), BBox (N×4), Kps (N×10).
/// Regressionen werden mit dem Stride skaliert (`bbox_preds * stride`,
/// wie im Referenzcode) und als Center-Distanzen decodiert.
pub fn decode_stride(
    scores: &[f32],
    bbox: &[f32],
    kps: &[f32],
    stride: usize,
    threshold: f32,
) -> Vec<FaceDetection> {
    let centers = anchor_centers(stride);
    let st = stride as f32;
    let mut dets = Vec::new();
    for (i, &s) in scores.iter().enumerate() {
        if s < threshold {
            continue;
        }
        let c = centers[i];
        let b4 = &bbox[i * 4..i * 4 + 4];
        let k10 = &kps[i * 10..i * 10 + 10];
        let mut pts = [[0.0; 2]; 5];
        for k in 0..5 {
            pts[k] = [c[0] + k10[2 * k] * st, c[1] + k10[2 * k + 1] * st];
        }
        dets.push(FaceDetection {
            bbox: BBox {
                x1: c[0] - b4[0] * st,
                y1: c[1] - b4[1] * st,
                x2: c[0] + b4[2] * st,
                y2: c[1] + b4[3] * st,
                score: s,
            },
            landmarks: Landmarks5 { points: pts },
        });
    }
    dets
}

/// Greedy-NMS mit `+1`-Flächen wie im Referenzcode (sortiert intern).
pub fn nms_greedy(mut dets: Vec<FaceDetection>, iou_thres: f32) -> Vec<FaceDetection> {
    dets.sort_by(|a, b| b.bbox.score.total_cmp(&a.bbox.score));
    let area = |b: &BBox| (b.x2 - b.x1 + 1.0) * (b.y2 - b.y1 + 1.0);
    let mut keep = Vec::new();
    let mut suppressed = vec![false; dets.len()];
    for i in 0..dets.len() {
        if suppressed[i] {
            continue;
        }
        for (j, s) in suppressed.iter_mut().enumerate().skip(i + 1) {
            if *s {
                continue;
            }
            let (a, b) = (dets[i].bbox, dets[j].bbox);
            let w = (a.x2.min(b.x2) - a.x1.max(b.x1) + 1.0).max(0.0);
            let h = (a.y2.min(b.y2) - a.y1.max(b.y1) + 1.0).max(0.0);
            if w * h / (area(&a) + area(&b) - w * h) > iou_thres {
                *s = true;
            }
        }
        keep.push(dets[i]);
    }
    keep
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn anchor_count_matches_reference() {
        let total: usize = STRIDES.iter().map(|&s| anchor_centers(s).len()).sum();
        assert_eq!(total, (80 * 80 + 40 * 40 + 20 * 20) * 2);
        assert_eq!(anchor_centers(8)[0], [0.0, 0.0]);
        assert_eq!(anchor_centers(8)[1], [0.0, 0.0]); // Duplikat ×2
        assert_eq!(anchor_centers(8)[2], [8.0, 0.0]);
    }

    #[test]
    fn decode_computes_exact_box_and_kps() {
        // Ein Anker über Schwelle: Center (0,0) bei Stride 32 (erster).
        let n = 20 * 20 * 2;
        let mut scores = vec![0.0; n];
        scores[0] = 0.9;
        let mut bbox = vec![0.0; n * 4];
        bbox[0..4].copy_from_slice(&[10.0, 20.0, 30.0, 40.0]);
        let mut kps = vec![0.0; n * 10];
        kps[0..10].copy_from_slice(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]);
        let dets = decode_stride(&scores, &bbox, &kps, 32, 0.5);
        assert_eq!(dets.len(), 1);
        // Center (0,0), Regressionen × Stride 32 skaliert.
        assert_eq!((dets[0].bbox.x1, dets[0].bbox.y2), (-320.0, 1280.0));
        assert_eq!(dets[0].landmarks.points[4], [288.0, 320.0]);
    }

    #[test]
    fn nms_keeps_strongest_and_drops_overlap() {
        let mk = |x1: f32, score: f32| FaceDetection {
            bbox: BBox {
                x1,
                y1: 0.0,
                x2: x1 + 10.0,
                y2: 10.0,
                score,
            },
            landmarks: Landmarks5 {
                points: [[0.0; 2]; 5],
            },
        };
        let far = mk(100.0, 0.6);
        let kept = nms_greedy(vec![mk(1.0, 0.7), mk(0.0, 0.9), far], 0.4);
        assert_eq!(kept.len(), 2);
        assert_eq!(kept[0].bbox.score, 0.9);
        assert_eq!(kept[1].bbox.x1, 100.0);
    }

    #[test]
    fn preprocess_normalizes_mid_gray_to_zero() {
        let rgb = vec![127u8; 640 * 640 * 3];
        let p = preprocess_640(&rgb);
        assert!((p[0] - (127.0f32 - 127.5) / 128.0).abs() < 1e-6);
        assert_eq!(p.len(), 3 * 640 * 640);
    }
}
