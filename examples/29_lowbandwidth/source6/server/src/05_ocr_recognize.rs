//! `05_ocr_recognize` — PP-OCRv6-Texterkennung (SVTR + CTC) je Box.
//!
//! Aus `26_onnx/source5/src/04_recognize.rs`: Crop → Höhe 48, Breite auf
//! Vielfaches von 32 (max. 960), CTC-Greedy-Dekodierung. Neu: Konfidenz
//! (Mittel der Argmax-Wahrscheinlichkeiten), damit Fehltreffer auf Bildern
//! nicht als Text maskiert werden.

use lbw_common::Rect;
use ort::session::Session;
use ort::value::TensorRef;

use crate::image::Rgb;
use crate::ocr_detect::session;

/// Eingabehöhe des Erkenners.
const REC_H: usize = 48;
/// Max. Eingabebreite.
const MAX_W: usize = 960;

/// CTC-Erkenner mit Wörterbuch.
pub struct Recognizer {
    session: Session,
    dict: Vec<String>,
    input: Vec<f32>,
}

impl Recognizer {
    /// `dict_path`: `inference.yml` mit `character_dict:`-Liste.
    pub fn new(path: &str, dict_path: &str, threads: usize) -> Result<Self, String> {
        let yaml = std::fs::read_to_string(dict_path).map_err(|e| format!("{dict_path}: {e}"))?;
        let dict = load_dict(&yaml);
        if dict.is_empty() {
            return Err(format!("{dict_path}: kein character_dict"));
        }
        Ok(Self {
            session: session(path, threads)?,
            dict,
            input: Vec::new(),
        })
    }

    /// Erkennt den Text in `r`; liefert (Text, Konfidenz 0..1).
    pub fn recognize(&mut self, img: &Rgb, r: Rect) -> Result<(String, f32), String> {
        let tw = self.preprocess(img, r);
        let out = self
            .session
            .run(ort::inputs![
                TensorRef::from_array_view(([1, 3, REC_H, tw], &self.input[..3 * REC_H * tw]))
                    .map_err(|e| e.to_string())?
            ])
            .map_err(|e| format!("rec: {e}"))?;
        let (shape, preds) = out[0]
            .try_extract_tensor::<f32>()
            .map_err(|e| e.to_string())?;
        Ok(ctc_decode(preds, shape, &self.dict))
    }

    /// Crop mit Nearest-Resize auf `REC_H × tw`, Werte in [-1, 1].
    fn preprocess(&mut self, img: &Rgb, r: Rect) -> usize {
        let (cw, ch) = (f32::from(r.w.max(1)), f32::from(r.h.max(1)));
        let raw_w = (REC_H as f32 * cw / ch).round() as usize;
        let tw = (raw_w.div_ceil(32) * 32).clamp(32, MAX_W);
        let rw = raw_w.clamp(1, tw);
        let plane = REC_H * tw;
        self.input.clear();
        self.input.resize(3 * plane, 0.0);
        for dy in 0..REC_H {
            let sy = (f32::from(r.y) + (dy as f32 + 0.5) * ch / REC_H as f32 - 0.5).round();
            let sy = (sy.max(0.0) as usize).min(img.h - 1);
            for dx in 0..rw {
                let sx = (f32::from(r.x) + (dx as f32 + 0.5) * cw / rw as f32 - 0.5).round();
                let sx = (sx.max(0.0) as usize).min(img.w - 1);
                let p = img.get(sx, sy);
                let d = dy * tw + dx;
                for (c, &v) in p.iter().enumerate() {
                    self.input[c * plane + d] = f32::from(v) / 127.5 - 1.0;
                }
            }
        }
        tw
    }
}

/// Parst `character_dict:` aus dem YAML (ohne YAML-Crate).
#[must_use]
pub fn load_dict(yaml: &str) -> Vec<String> {
    let mut dict = Vec::new();
    let mut in_dict = false;
    for line in yaml.lines() {
        let t = line.trim();
        if t.starts_with("character_dict:") {
            in_dict = true;
        } else if in_dict {
            if let Some(item) = t.strip_prefix('-') {
                let s = item.trim();
                let quoted = s.len() >= 2
                    && ((s.starts_with('\'') && s.ends_with('\''))
                        || (s.starts_with('"') && s.ends_with('"')));
                dict.push(if quoted { &s[1..s.len() - 1] } else { s }.to_owned());
            } else if !t.is_empty() && !t.starts_with('#') {
                break;
            }
        }
    }
    dict
}

/// CTC-Greedy: Argmax je Zeitschritt, Blank (0) und Wiederholungen raus.
/// Klasse `dict.len()+1` ist das Leerzeichen.
#[must_use]
pub fn ctc_decode(data: &[f32], shape: &[i64], dict: &[String]) -> (String, f32) {
    let n = shape.last().copied().unwrap_or(0).max(0) as usize;
    if n == 0 {
        return (String::new(), 0.0);
    }
    let (mut text, mut prev, mut conf, mut cnt) = (String::new(), 0usize, 0.0f32, 0usize);
    for row in data.chunks_exact(n) {
        let (idx, p) = row
            .iter()
            .copied()
            .enumerate()
            .max_by(|a, b| a.1.total_cmp(&b.1))
            .unwrap_or((0, 0.0));
        if idx != 0 && idx != prev {
            if let Some(s) = dict.get(idx - 1) {
                text.push_str(s);
            } else if idx - 1 == dict.len() {
                text.push(' ');
            }
            conf += p;
            cnt += 1;
        }
        prev = idx;
    }
    (text, if cnt > 0 { conf / cnt as f32 } else { 0.0 })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn d(v: &[&str]) -> Vec<String> {
        v.iter().map(|s| (*s).to_owned()).collect()
    }

    #[test]
    fn dict_parses_mini_yaml() {
        let dict =
            load_dict("Global:\n  character_dict:\n  - 'a'\n  - b\n  - \"c\"\n  - '''\nOther: 1\n");
        assert_eq!(dict, d(&["a", "b", "c", "'"]));
    }

    #[test]
    fn ctc_collapses_duplicates_blanks_and_reports_confidence() {
        let dict = d(&["a", "b"]);
        let data = [
            0.1, 0.9, 0.0, 0.0, //
            0.1, 0.8, 0.1, 0.0, //
            0.9, 0.05, 0.05, 0.0, //
            0.1, 0.1, 0.7, 0.1, //
            0.0, 0.0, 0.0, 1.0, //
        ];
        let (t, c) = ctc_decode(&data, &[5, 4], &dict);
        assert_eq!(t, "ab ");
        assert!((c - (0.9 + 0.7 + 1.0) / 3.0).abs() < 1e-6);
    }

    #[test]
    fn ctc_empty_inputs() {
        assert_eq!(ctc_decode(&[0.9, 0.1], &[1, 2], &d(&["a"])).0, "");
        assert_eq!(ctc_decode(&[], &[4, 0], &d(&["a"])), (String::new(), 0.0));
    }
}
