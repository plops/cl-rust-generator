//! `06_detector` — End-to-end-Detektor: Letterbox → Inferenz → Decode/NMS
//! → Rücktransformation ins Bild. Hält Buffer und misst jede Stufe.

use crate::decode::{Det, decode, nms};
use crate::image::Rgb;
use crate::letterbox::Letterbox;
use crate::session::Model;
use ort::value::TensorRef;
use std::time::Instant;

/// Model-Card-Defaults (`conf=0.05`, `iou=0.7`) plus Ultralytics `max_det`.
pub const CONF: f32 = 0.05;
pub const IOU: f32 = 0.7;
pub const MAX_DET: usize = 300;
/// Anzeigeschwelle für eingezeichnete Boxen (annotiertes PPM, Live-Fenster).
pub const SHOW: f32 = 0.25;

/// Laufzeiten einer Detektion in Millisekunden.
#[derive(Clone, Copy, Debug, Default)]
pub struct Timings {
    pub pre: f64,
    pub infer: f64,
    pub post: f64,
}

impl Timings {
    #[must_use]
    pub fn total(&self) -> f64 {
        self.pre + self.infer + self.post
    }
}

pub struct Detector {
    pub model: Model,
    pub conf: f32,
    pub iou: f32,
    input: Vec<f32>,
}

impl Detector {
    #[must_use]
    pub fn new(model: Model) -> Self {
        let input = vec![0.0; 3 * model.in_w * model.in_h];
        Self {
            model,
            conf: CONF,
            iou: IOU,
            input,
        }
    }

    /// Detektiert GUI-Elemente; Boxen in Bildkoordinaten, nach Score sortiert.
    pub fn detect(&mut self, img: &Rgb) -> Result<(Vec<Det>, Timings), String> {
        let (w, h) = (self.model.in_w, self.model.in_h);
        let t0 = Instant::now();
        let lb = Letterbox::new(img.w, img.h, w, h);
        lb.fill(img, &mut self.input);
        let t1 = Instant::now();

        let tensor = TensorRef::from_array_view(([1, 3, h, w], &self.input[..]))
            .map_err(|e| e.to_string())?;
        let out = self
            .model
            .session
            .run(ort::inputs![self.model.input_name.as_str() => tensor])
            .map_err(|e| e.to_string())?;
        let (shape, data) = out[0]
            .try_extract_tensor::<f32>()
            .map_err(|e| e.to_string())?;
        let t2 = Instant::now();

        let [1, rows, n] = shape[..] else {
            return Err(format!("erwarte Output [1,4+nc,N], habe {shape:?}"));
        };
        let mut dets = nms(
            decode(data, rows as usize, n as usize, self.conf),
            self.iou,
            MAX_DET,
        );
        for d in &mut dets {
            d.b = lb.to_source(d.b);
        }
        let t3 = Instant::now();

        let ms = |a: Instant, b: Instant| (b - a).as_secs_f64() * 1e3;
        Ok((
            dets,
            Timings {
                pre: ms(t0, t1),
                infer: ms(t1, t2),
                post: ms(t2, t3),
            },
        ))
    }
}
