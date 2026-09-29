//! `06_gui_detect` — GPA-GUI-Detector (YOLO11, 1 Klasse „GUI-Element“).
//!
//! Aus `26_onnx/source8` (Decode + NMS unverändert). Der Capture hat exakt
//! die Modell-Eingabegröße (640²), die Letterbox ist damit die Identität:
//! RGB/255, planar, keine Skalierung, Boxen direkt in Bildpixeln.

use lbw_common::Rect;
use ort::session::Session;
use ort::value::TensorRef;

use crate::image::Rgb;
use crate::ocr_detect::session;

/// Model-Card-Defaults.
pub const CONF: f32 = 0.05;
pub const IOU: f32 = 0.7;
pub const MAX_DET: usize = 300;
/// Mindestscore für die Layout-Entscheidung.
pub const USE_SCORE: f32 = 0.25;

/// Eine Detektion (`x1,y1,x2,y2`).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Det {
    pub b: [f32; 4],
    pub score: f32,
    pub class: usize,
}

impl Det {
    /// Ganzzahliges Rechteck, auf `w×h` begrenzt.
    #[must_use]
    pub fn rect(&self, w: usize, h: usize) -> Rect {
        let x0 = self.b[0].clamp(0.0, w as f32).floor();
        let y0 = self.b[1].clamp(0.0, h as f32).floor();
        let x1 = self.b[2].clamp(0.0, w as f32).ceil();
        let y1 = self.b[3].clamp(0.0, h as f32).ceil();
        Rect::new(x0 as u16, y0 as u16, (x1 - x0) as u16, (y1 - y0) as u16)
    }
}

/// YOLO-Detektor mit fester Eingabegröße.
pub struct GuiDetector {
    session: Session,
    pub in_w: usize,
    pub in_h: usize,
    input: Vec<f32>,
}

impl GuiDetector {
    pub fn new(path: &str, threads: usize) -> Result<Self, String> {
        let session = session(path, threads)?;
        let shape: Vec<i64> = session.inputs()[0]
            .dtype()
            .tensor_shape()
            .ok_or("GUI-Modell: Input ist kein Tensor")?
            .iter()
            .copied()
            .collect();
        let [1, 3, h, w] = shape[..] else {
            return Err(format!("GUI-Modell: erwarte [1,3,H,W], habe {shape:?}"));
        };
        if h <= 0 || w <= 0 {
            return Err("GUI-Modell: dynamische Größe nicht unterstützt".into());
        }
        Ok(Self {
            session,
            in_w: w as usize,
            in_h: h as usize,
            input: Vec::new(),
        })
    }

    /// GUI-Elemente mit Score ≥ [`USE_SCORE`], nach Score sortiert.
    pub fn detect(&mut self, img: &Rgb) -> Result<Vec<Det>, String> {
        if (img.w, img.h) != (self.in_w, self.in_h) {
            return Err(format!(
                "GUI-Modell erwartet {}x{}, Capture ist {}x{}",
                self.in_w, self.in_h, img.w, img.h
            ));
        }
        let plane = img.w * img.h;
        self.input.resize(3 * plane, 0.0);
        for (i, p) in img.data.as_chunks::<3>().0.iter().enumerate() {
            for (c, &v) in p.iter().enumerate() {
                self.input[c * plane + i] = f32::from(v) / 255.0;
            }
        }
        let out = self
            .session
            .run(ort::inputs![
                TensorRef::from_array_view(([1, 3, img.h, img.w], &self.input[..]))
                    .map_err(|e| e.to_string())?
            ])
            .map_err(|e| format!("gui: {e}"))?;
        let (shape, data) = out[0]
            .try_extract_tensor::<f32>()
            .map_err(|e| e.to_string())?;
        let [1, rows, n] = shape[..] else {
            return Err(format!("GUI-Output [1,4+nc,N] erwartet, habe {shape:?}"));
        };
        let dets = nms(decode(data, rows as usize, n as usize, CONF), IOU, MAX_DET);
        Ok(dets.into_iter().filter(|d| d.score >= USE_SCORE).collect())
    }
}

/// Kanal-major `[4+nc, N]` → Kandidaten mit Score `> conf`.
#[must_use]
pub fn decode(out: &[f32], rows: usize, n: usize, conf: f32) -> Vec<Det> {
    assert!(rows > 4 && out.len() >= rows * n);
    let at = |r: usize, i: usize| out[r * n + i];
    let mut dets = Vec::new();
    for i in 0..n {
        let (class, score) = (4..rows)
            .map(|r| (r - 4, at(r, i)))
            .fold((0, f32::MIN), |a, c| if c.1 > a.1 { c } else { a });
        if score > conf {
            let (cx, cy, hw, hh) = (at(0, i), at(1, i), at(2, i) / 2.0, at(3, i) / 2.0);
            dets.push(Det {
                b: [cx - hw, cy - hh, cx + hw, cy + hh],
                score,
                class,
            });
        }
    }
    dets
}

/// IoU zweier `xyxy`-Boxen.
#[must_use]
pub fn iou(a: &[f32; 4], b: &[f32; 4]) -> f32 {
    let iw = (a[2].min(b[2]) - a[0].max(b[0])).max(0.0);
    let ih = (a[3].min(b[3]) - a[1].max(b[1])).max(0.0);
    let inter = iw * ih;
    let area = |r: &[f32; 4]| (r[2] - r[0]) * (r[3] - r[1]);
    let union = area(a) + area(b) - inter;
    if union > 0.0 { inter / union } else { 0.0 }
}

/// Greedy-NMS, klassenweise.
#[must_use]
pub fn nms(mut dets: Vec<Det>, thr: f32, max_det: usize) -> Vec<Det> {
    dets.sort_by(|a, b| b.score.total_cmp(&a.score));
    let mut keep: Vec<Det> = Vec::new();
    for d in dets {
        if keep.len() == max_det {
            break;
        }
        if keep
            .iter()
            .all(|k| k.class != d.class || iou(&k.b, &d.b) <= thr)
        {
            keep.push(d);
        }
    }
    keep
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tensor(cols: &[[f32; 5]]) -> Vec<f32> {
        let n = cols.len();
        let mut t = vec![0.0; 5 * n];
        for (i, c) in cols.iter().enumerate() {
            for r in 0..5 {
                t[r * n + i] = c[r];
            }
        }
        t
    }

    #[test]
    fn decode_and_nms() {
        let t = tensor(&[
            [100.0, 50.0, 20.0, 10.0, 0.9],
            [101.0, 50.0, 20.0, 10.0, 0.8],
            [300.0, 50.0, 20.0, 10.0, 0.3],
            [1.0, 1.0, 1.0, 1.0, 0.01],
        ]);
        let d = nms(decode(&t, 5, 4, CONF), IOU, MAX_DET);
        assert_eq!(d.len(), 2);
        assert_eq!(d[0].b, [90.0, 45.0, 110.0, 55.0]);
        assert_eq!(d[1].score, 0.3);
    }

    #[test]
    fn det_rect_is_clamped() {
        let d = Det {
            b: [-5.0, 10.2, 700.0, 20.7],
            score: 1.0,
            class: 0,
        };
        assert_eq!(d.rect(640, 640), Rect::new(0, 10, 640, 11));
    }
}
