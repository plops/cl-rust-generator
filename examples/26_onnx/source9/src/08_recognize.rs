//! `08_recognize` — CTC-Erkennung (PP-OCR) plus Wörterbuch.
//!
//! Crop → 48×W, Normalisierung `x/127.5-1`, `ctc_decode` mit Konfidenz
//! (aus source5; dort ohne Konfidenz und mit eingebettetem Modell).
//! Das Wörterbuch wird aus der `inference.yml` des jeweiligen Modells
//! geladen; YAML-Escapes (`''''` → `'`) werden aufgelöst.

use ort::{inputs, session::Session, value::TensorRef};
use std::path::Path;

use crate::detect::TextBox;
use crate::render::CANVAS;

/// Erkennungshöhe des Modells.
const REC_H: usize = 48;
/// Max. Zeilen pro Bild (Rechenzeit-Deckel).
const MAX_REC_LINES: usize = 64;

/// CTC-Erkenner mit wiederverwendbaren Buffern.
pub struct Recognizer {
    session: Session,
    in_name: String,
    dict: Vec<String>,
    input: Vec<f32>,
}

impl Recognizer {
    /// Lädt Session (`inference.onnx`) + Wörterbuch (`inference.yml`).
    pub fn open(onnx_path: &Path, dict: Vec<String>) -> Result<Self, String> {
        let session = Session::builder()
            .map_err(|e| e.to_string())?
            .commit_from_file(onnx_path)
            .map_err(|e| format!("{}: {e}", onnx_path.display()))?;
        let in_name = session.inputs()[0].name().to_string();
        Ok(Self {
            session,
            in_name,
            dict,
            input: Vec::with_capacity(3 * REC_H * 960),
        })
    }

    /// Wörterbuch (für den Charset-Schnitt in `03_corpus`).
    #[must_use]
    pub fn dict(&self) -> &[String] {
        &self.dict
    }

    /// Erkennt jede Box; schreibt `text` und gibt die Konfidenzen zurück.
    pub fn recognize(&mut self, rgba: &[u8], boxes: &mut [TextBox]) -> Result<Vec<f32>, String> {
        let mut confs = Vec::with_capacity(boxes.len().min(MAX_REC_LINES));
        for b in boxes.iter_mut().take(MAX_REC_LINES) {
            let target_w = self.preprocess_crop(rgba, b);
            let input_slice = &self.input[..3 * REC_H * target_w];
            let outs = self
                .session
                .run(inputs![
                    self.in_name.as_str() => TensorRef::from_array_view(([1, 3, REC_H, target_w], input_slice)).map_err(|e| e.to_string())?
                ])
                .map_err(|e| e.to_string())?;
            let (shape, preds) = outs[0]
                .try_extract_tensor::<f32>()
                .map_err(|e| e.to_string())?;
            let (text, conf) = ctc_decode(preds, shape, &self.dict);
            b.text = text;
            confs.push(conf);
        }
        Ok(confs)
    }

    fn preprocess_crop(&mut self, rgba: &[u8], crop: &TextBox) -> usize {
        let (cw, ch) = (crop.rect.w.max(1.0), crop.rect.h.max(1.0));
        let raw_w = (REC_H as f32 * (cw / ch)).round() as usize;
        let target_w = (raw_w.div_ceil(32) * 32).clamp(32, 960);
        let resized_w = raw_w.min(target_w).max(1);

        let total = 3 * REC_H * target_w;
        if self.input.len() < total {
            self.input.resize(total, 0.0);
        }
        self.input[..total].fill(0.0);

        let plane_stride = REC_H * target_w;
        let (ox, oy) = (crop.rect.x, crop.rect.y);
        for dy in 0..REC_H {
            let sy = (oy + (dy as f32 + 0.5) * (ch / REC_H as f32) - 0.5).round() as usize;
            let sy_c = sy.clamp(0, CANVAS - 1);

            for dx in 0..resized_w {
                let sx = (ox + (dx as f32 + 0.5) * (cw / resized_w as f32) - 0.5).round() as usize;
                let sx_c = sx.clamp(0, CANVAS - 1);

                let src = (sy_c * CANVAS + sx_c) * 4;
                let dst = dy * target_w + dx;

                self.input[dst] = f32::from(rgba[src]) / 127.5 - 1.0;
                self.input[plane_stride + dst] = f32::from(rgba[src + 1]) / 127.5 - 1.0;
                self.input[2 * plane_stride + dst] = f32::from(rgba[src + 2]) / 127.5 - 1.0;
            }
        }
        target_w
    }
}

/// Parst `character_dict:` aus dem YAML (ohne YAML-Dep).
///
/// Einfache Anführungszeichen escapen per Verdopplung (`''''` → `'`),
/// doppelte per Backslash (`"\""` → `"`).
pub fn load_dict(yaml: &str) -> Vec<String> {
    let mut dict = Vec::new();
    let mut in_dict = false;

    for line in yaml.lines() {
        let t = line.trim();
        if t.starts_with("character_dict:") {
            in_dict = true;
        } else if in_dict {
            if let Some(item) = t.strip_prefix('-') {
                dict.push(unquote(item.trim()));
            } else if !t.is_empty() && !t.starts_with('#') {
                break;
            }
        }
    }
    dict
}

fn unquote(s: &str) -> String {
    let b = s.as_bytes();
    if s.len() >= 2 && b[0] == b'\'' && b[s.len() - 1] == b'\'' {
        s[1..s.len() - 1].replace("''", "'")
    } else if s.len() >= 2 && b[0] == b'"' && b[s.len() - 1] == b'"' {
        s[1..s.len() - 1]
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
    } else {
        s.to_string()
    }
}

/// CTC-Dekodierung: Argmax pro Zeitschritt, Blanks (0) und Duplikate raus.
///
/// Gibt (Text, Konfidenz) zurück; Konfidenz = Mittel der Max-
/// Wahrscheinlichkeiten der emittierten Schritte (0 bei leerem Text).
/// Ist die letzte Klasse nicht im Wörterbuch, gilt sie als Leerzeichen
/// (`use_space_char`-Modelle).
pub fn ctc_decode(data: &[f32], shape: &[i64], dict: &[String]) -> (String, f32) {
    let num_classes = *shape.last().unwrap_or(&0) as usize;
    if num_classes == 0 {
        return (String::new(), 0.0);
    }

    let mut text = String::new();
    let mut prev_idx = 0usize;
    let mut conf_sum = 0.0f32;
    let mut conf_n = 0u32;

    for t in 0..(data.len() / num_classes) {
        let row = &data[t * num_classes..(t + 1) * num_classes];
        let (max_idx, max_p) = row
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.total_cmp(b))
            .map(|(idx, p)| (idx, *p))
            .unwrap_or((0, 0.0));

        if max_idx != 0 && max_idx != prev_idx {
            if max_idx - 1 < dict.len() {
                text.push_str(&dict[max_idx - 1]);
                conf_sum += max_p;
                conf_n += 1;
            } else if max_idx - 1 == dict.len() {
                text.push(' ');
                conf_sum += max_p;
                conf_n += 1;
            }
        }
        prev_idx = max_idx;
    }
    let conf = if conf_n > 0 {
        conf_sum / conf_n as f32
    } else {
        0.0
    };
    (text, conf)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dict(v: &[&str]) -> Vec<String> {
        v.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn dict_parses_mini_yaml() {
        let d = load_dict("Global:\n  character_dict:\n  - 'a'\n  - b\n  - \"c\"\nOther: 1\n");
        assert_eq!(d, ["a", "b", "c"]);
    }

    #[test]
    fn dict_unescapes_yaml_quotes() {
        let d = load_dict("  character_dict:\n  - ''''\n  - \"\\\"\"\n  - $\n");
        assert_eq!(d, ["'", "\"", "$"]);
    }

    #[test]
    fn ctc_collapses_duplicates_and_blanks() {
        // Klassen: 0 = blank, 1 = 'a', 2 = 'b'. Sequenz: a, a, blank, b, b.
        let d = dict(&["a", "b"]);
        let data = vec![
            0.1, 0.9, 0.0, //
            0.1, 0.8, 0.1, //
            0.9, 0.05, 0.05, //
            0.1, 0.1, 0.8, //
            0.2, 0.1, 0.7, //
        ];
        let (text, conf) = ctc_decode(&data, &[5, 3], &d);
        assert_eq!(text, "ab");
        assert!((conf - 0.85).abs() < 1e-6); // Mittel aus 0.9 und 0.8
    }

    #[test]
    fn ctc_last_class_is_space_and_empty_is_zero_conf() {
        let d = dict(&["a"]);
        let data = vec![0.05, 0.05, 0.9]; // Klasse 2 = dict.len() → Leerzeichen
        let (text, conf) = ctc_decode(&data, &[1, 3], &d);
        assert_eq!(text, " ");
        assert!((conf - 0.9).abs() < 1e-6);

        let (text, conf) = ctc_decode(&[0.9, 0.1, 0.8, 0.2], &[2, 2], &d);
        assert_eq!((text.as_str(), conf), ("", 0.0));
        assert_eq!(ctc_decode(&[], &[4, 0], &d), (String::new(), 0.0));
    }
}
