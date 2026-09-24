//! `04_recognize` — PP-OCRv6-Erkennung (CTC) plus Wörterbuch (S3).
//!
//! Unveränderter Code aus `main.rs`; Konstanten und Schwellen identisch.
//! `stride` ist die Kantenlänge des RGBA-Puffers (bisher immer 640;
//! variable ROI folgt in S4 — bei 640 identisches Verhalten).

use ort::{inputs, session::Session, value::TensorRef};

use crate::detect::TextBox;
use crate::view::MODEL_SIZE;

/// Erkennungsmodell (PP-OCRv6 small, ONNX).
const REC_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_rec.onnx");
/// Wörterbuch-YAML mit `character_dict:`.
const DICT_YAML: &str = include_str!("../inference.yml");

/// Erkennungshöhe des Modells.
const REC_H: usize = 48;
/// Max. Zeilen pro Frame (Rechenzeit-Deckel).
const MAX_REC_LINES: usize = 64;

/// CTC-Erkenner mit wiederverwendbaren Buffern.
pub struct Recognizer {
    rec_session: Session,
    rec_in_name: String,
    dict: Vec<&'static str>,
    rec_input: Vec<f32>,
}

impl Recognizer {
    /// Lädt die Erkennungs-Session und das Wörterbuch.
    pub fn new() -> Self {
        let rec_session = Session::builder()
            .unwrap()
            .commit_from_memory(REC_BYTES)
            .unwrap();
        let rec_in_name = rec_session.inputs()[0].name().to_string();

        Self {
            rec_session,
            rec_in_name,
            dict: load_dict(DICT_YAML),
            rec_input: Vec::with_capacity(3 * REC_H * 960),
        }
    }

    /// Erkennt jede Bounding-Box im RGBA-Puffer (`stride` = Kantenlänge).
    pub fn recognize(&mut self, rgba: &[u8], boxes: &mut [TextBox], stride: usize) {
        let count = boxes.len().min(MAX_REC_LINES);
        for b in boxes.iter_mut().take(count) {
            let target_w = self.preprocess_crop(rgba, b, stride);
            let input_slice = &self.rec_input[..3 * REC_H * target_w];
            let rec_outs = self
                .rec_session
                .run(inputs![
                    self.rec_in_name.as_str() => TensorRef::from_array_view(([1, 3, REC_H, target_w], input_slice)).unwrap()
                ])
                .unwrap();

            let (shape, preds) = rec_outs[0].try_extract_tensor::<f32>().unwrap();
            b.text = ctc_decode(preds, shape, &self.dict);
        }
    }

    fn preprocess_crop(&mut self, rgba: &[u8], crop: &TextBox, stride: usize) -> usize {
        // Boxen liegen im 640-Raum der Detektion; `scale` bildet auf den
        // RGBA-Puffer ab. Bei `stride == MODEL_SIZE` ist `scale == 1.0`
        // (`x * 1.0 == x` exakt) und die Rechnung ist bit-identisch zu vorher.
        let scale = stride as f32 / MODEL_SIZE as f32;
        let (cw, ch) = (crop.w.max(1.0), crop.h.max(1.0));
        let raw_w = (REC_H as f32 * (cw / ch)).round() as usize;
        let target_w = (raw_w.div_ceil(32) * 32).clamp(32, 960);
        let resized_w = raw_w.min(target_w).max(1);

        let total = 3 * REC_H * target_w;
        if self.rec_input.len() < total {
            self.rec_input.resize(total, 0.0);
        }
        self.rec_input[..total].fill(0.0);

        let plane_stride = REC_H * target_w;
        let (sw, sh) = (cw * scale, ch * scale);
        let (ox, oy) = (crop.x * scale, crop.y * scale);
        for dy in 0..REC_H {
            let sy = (oy + (dy as f32 + 0.5) * (sh / REC_H as f32) - 0.5).round() as usize;
            let sy_c = sy.clamp(0, stride - 1);

            for dx in 0..resized_w {
                let sx = (ox + (dx as f32 + 0.5) * (sw / resized_w as f32) - 0.5).round() as usize;
                let sx_c = sx.clamp(0, stride - 1);

                let src = (sy_c * stride + sx_c) * 4;
                let dst = dy * target_w + dx;

                self.rec_input[dst] = rgba[src] as f32 / 127.5 - 1.0;
                self.rec_input[plane_stride + dst] = rgba[src + 1] as f32 / 127.5 - 1.0;
                self.rec_input[2 * plane_stride + dst] = rgba[src + 2] as f32 / 127.5 - 1.0;
            }
        }
        target_w
    }
}

/// Parst `character_dict:` aus dem YAML (ohne YAML-Dep).
pub fn load_dict(yaml: &'static str) -> Vec<&'static str> {
    let mut dict = Vec::new();
    let mut in_dict = false;

    for line in yaml.lines() {
        let t = line.trim();
        if t.starts_with("character_dict:") {
            in_dict = true;
        } else if in_dict {
            if let Some(item) = t.strip_prefix('-') {
                let s = item.trim();
                let clean = if (s.starts_with('\'') && s.ends_with('\''))
                    || (s.starts_with('"') && s.ends_with('"'))
                {
                    &s[1..s.len().saturating_sub(1)]
                } else {
                    s
                };
                dict.push(clean);
            } else if !t.is_empty() && !t.starts_with('#') {
                break;
            }
        }
    }
    dict
}

/// CTC-Dekodierung: Argmax pro Zeitschritt, Blanks (0) und Duplikate raus.
pub fn ctc_decode(data: &[f32], shape: &[i64], dict: &[&str]) -> String {
    let num_classes = *shape.last().unwrap() as usize;
    if num_classes == 0 {
        return String::new();
    }

    let mut text = String::new();
    let mut prev_idx = 0usize;

    for t in 0..(data.len() / num_classes) {
        let row = &data[t * num_classes..(t + 1) * num_classes];
        let max_idx = row
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.total_cmp(b))
            .map(|(idx, _)| idx)
            .unwrap_or(0);

        if max_idx != 0 && max_idx != prev_idx {
            if max_idx - 1 < dict.len() {
                text.push_str(dict[max_idx - 1]);
            } else if max_idx - 1 == dict.len() {
                text.push(' ');
            }
        }
        prev_idx = max_idx;
    }
    text
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dict_parses_mini_yaml() {
        let dict = load_dict("Global:\n  character_dict:\n  - 'a'\n  - b\n  - \"c\"\nOther: 1\n");
        assert_eq!(dict, vec!["a", "b", "c"]);
    }

    #[test]
    fn ctc_collapses_duplicates_and_blanks() {
        // Klassen: 0 = blank, 1 = 'a', 2 = 'b'. Sequenz: a, a, blank, b, b.
        let dict = ["a", "b"];
        let data = vec![
            0.1, 0.9, 0.0, //
            0.1, 0.8, 0.1, //
            0.9, 0.05, 0.05, //
            0.1, 0.1, 0.8, //
            0.2, 0.1, 0.7, //
        ];
        assert_eq!(ctc_decode(&data, &[5, 3], &dict), "ab");
    }

    #[test]
    fn ctc_all_blank_is_empty() {
        let dict = ["a"];
        let data = vec![0.9, 0.1, 0.8, 0.2];
        assert_eq!(ctc_decode(&data, &[2, 2], &dict), "");
    }

    #[test]
    fn ctc_empty_classes_is_empty() {
        let dict = ["a"];
        assert_eq!(ctc_decode(&[], &[4, 0], &dict), "");
    }
}
