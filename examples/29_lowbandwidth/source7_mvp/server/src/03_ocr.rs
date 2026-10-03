//! `03_ocr` — PP-OCRv6-Texterkennung: DBNet-Detektion + SVTR/CTC-Erkennung.
//!
//! Aus `source6` (`04_ocr_detect`, `05_ocr_recognize`, Farb-Sampling aus
//! `07_layout`) übernommen, aber auf `image::RgbImage` umgestellt, das
//! Wörterbuch per `serde_yaml` gelesen und ohne Erkennungs-Cache (MVP).
//! OCR ist Pflicht: ohne Modelle startet der Server nicht (reines AV1-Textbild
//! würde das 6-kB/s-Budget sprengen).

use image::RgbImage;
use ort::session::Session;
use ort::session::builder::GraphOptimizationLevel;
use ort::value::TensorRef;

use lbw_common::{Rect, TextItem};

use crate::tiles::pad_rect;

/// Erkennungs-Rand je Seite: Detektions-Boxen schneiden Glyphen (z. B.
/// Umlaut-Punkte) haarscharf ab — mit Weißraum liest das Netz deutlich besser
/// (Befund aus `26_onnx`: CER 69→4 %). Per Xvfb/xterm-Sweep bestimmt.
const REC_PAD: u16 = 4;
/// Pixel-Schwelle für Textkandidaten.
const DET_THRESH: f32 = 0.3;
/// Mindest-Mittelscore einer Komponente.
const BOX_THRESH: f32 = 0.6;
/// Aufweitungsfaktor der Boxen.
const UNCLIP_RATIO: f32 = 1.5;
/// Mindest-Konfidenz, sonst bleibt die Zeile Bildinhalt.
const MIN_TEXT_CONF: f32 = 0.5;
/// Max. erkannte Zeilen je Frame (Rechenzeit-Deckel).
const MAX_LINES: usize = 160;
/// Eingabehöhe des Erkenners.
const REC_H: usize = 48;
/// Max. Eingabebreite des Erkenners.
const MAX_W: usize = 960;

/// Lädt ein ONNX-Modell (CPU, Level 3, `threads` Intra-Op-Threads; 0 = Default).
fn session(path: &str, threads: usize) -> Result<Session, String> {
    let e = |e: ort::Error| format!("{path}: {e}");
    let mut b = Session::builder()
        .map_err(e)?
        .with_optimization_level(GraphOptimizationLevel::Level3)
        .map_err(|x| format!("{path}: {x}"))?;
    if threads > 0 {
        b = b
            .with_intra_threads(threads)
            .map_err(|x| format!("{path}: {x}"))?;
    }
    b.commit_from_file(path).map_err(e)
}

fn px(img: &RgbImage, x: usize, y: usize) -> [u8; 3] {
    let i = (y * img.width() as usize + x) * 3;
    img.as_raw()[i..i + 3].try_into().unwrap()
}

/// DBNet-Detektor mit wiederverwendbaren Puffern.
pub struct Detector {
    session: Session,
    input: Vec<f32>,
    visited: Vec<u32>,
    tag: u32,
    queue: Vec<(usize, usize)>,
}

impl Detector {
    pub fn new(path: &str, threads: usize) -> Result<Self, String> {
        Ok(Self {
            session: session(path, threads)?,
            input: Vec::new(),
            visited: Vec::new(),
            tag: 0,
            queue: Vec::with_capacity(512),
        })
    }

    /// Textzeilen-Boxen im Bild (Breite/Höhe Vielfache von 32).
    pub fn detect(&mut self, img: &RgbImage) -> Result<Vec<Rect>, String> {
        let (w, h) = (img.width() as usize, img.height() as usize);
        if !w.is_multiple_of(32) || !h.is_multiple_of(32) {
            return Err(format!("DBNet braucht Vielfache von 32, nicht {w}x{h}"));
        }
        normalize_imagenet(img, &mut self.input);
        if self.visited.len() != w * h {
            self.visited = vec![0; w * h];
        }
        let out = self
            .session
            .run(ort::inputs![
                TensorRef::from_array_view(([1, 3, h, w], &self.input[..]))
                    .map_err(|e| e.to_string())?
            ])
            .map_err(|e| format!("det: {e}"))?;
        let (_, prob) = out[0]
            .try_extract_tensor::<f32>()
            .map_err(|e| e.to_string())?;
        self.tag = self.tag.wrapping_add(1);
        if self.tag == 0 {
            self.visited.fill(0);
            self.tag = 1;
        }
        Ok(postprocess(
            prob,
            w,
            h,
            &mut self.visited,
            self.tag,
            &mut self.queue,
        ))
    }
}

/// RGB8 → planar, ImageNet-normalisiert.
fn normalize_imagenet(img: &RgbImage, out: &mut Vec<f32>) {
    const MEAN: [f32; 3] = [0.485, 0.456, 0.406];
    const STD: [f32; 3] = [0.229, 0.224, 0.225];
    let plane = img.width() as usize * img.height() as usize;
    out.resize(3 * plane, 0.0);
    for (i, p) in img.as_raw().as_chunks::<3>().0.iter().enumerate() {
        for (c, &v) in p.iter().enumerate() {
            out[c * plane + i] = (f32::from(v) / 255.0 - MEAN[c]) / STD[c];
        }
    }
}

/// DBNet-Nachverarbeitung: verbundene Schwellen-Pixel → aufgeweitete Boxen,
/// sortiert nach Zeile (16-px-Bänder), dann x.
fn postprocess(
    prob: &[f32],
    w: usize,
    h: usize,
    visited: &mut [u32],
    tag: u32,
    queue: &mut Vec<(usize, usize)>,
) -> Vec<Rect> {
    let mut boxes = Vec::new();
    for y in 0..h {
        for x in 0..w {
            let idx = y * w + x;
            if prob[idx] < DET_THRESH || visited[idx] == tag {
                continue;
            }
            visited[idx] = tag;
            queue.clear();
            queue.push((x, y));
            let (mut x0, mut x1, mut y0, mut y1) = (x, x, y, y);
            let (mut sum, mut head) = (0.0f32, 0);
            while head < queue.len() {
                let (cx, cy) = queue[head];
                head += 1;
                x0 = x0.min(cx);
                x1 = x1.max(cx);
                y0 = y0.min(cy);
                y1 = y1.max(cy);
                sum += prob[cy * w + cx];
                for (dx, dy) in [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)] {
                    let (nx, ny) = (cx as isize + dx, cy as isize + dy);
                    if nx >= 0 && ny >= 0 && (nx as usize) < w && (ny as usize) < h {
                        let n = ny as usize * w + nx as usize;
                        if visited[n] != tag && prob[n] >= DET_THRESH {
                            visited[n] = tag;
                            queue.push((nx as usize, ny as usize));
                        }
                    }
                }
            }
            let bw = (x1 - x0 + 1) as f32;
            let bh = (y1 - y0 + 1) as f32;
            let avg = sum / queue.len() as f32;
            if queue.len() >= 16 && avg >= BOX_THRESH && bw >= 8.0 && bh >= 6.0 {
                let dist = (bw * bh * UNCLIP_RATIO) / (2.0 * (bw + bh));
                let dist_y = (dist * 0.4).min(bh * 0.15).max(1.0);
                let fx0 = (x0 as f32 - dist).max(0.0);
                let fy0 = (y0 as f32 - dist_y).max(0.0);
                let fx1 = (x1 as f32 + 1.0 + dist).min(w as f32);
                let fy1 = (y1 as f32 + 1.0 + dist_y).min(h as f32);
                let (rx, ry) = (fx0.floor() as u16, fy0.floor() as u16);
                boxes.push(Rect::new(
                    rx,
                    ry,
                    fx1.ceil() as u16 - rx,
                    fy1.ceil() as u16 - ry,
                ));
            }
        }
    }
    boxes.sort_by_key(|b| (b.y / 16, b.x));
    boxes
}

/// `PostProcess`-Abschnitt aus `inference.yml`.
#[derive(serde::Deserialize)]
struct InferenceYml {
    #[serde(rename = "PostProcess")]
    post_process: PostProcess,
}

#[derive(serde::Deserialize)]
struct PostProcess {
    character_dict: Vec<String>,
}

/// Liest `character_dict` aus dem YAML (per `serde_yaml`).
pub fn load_dict(yaml: &str) -> Result<Vec<String>, String> {
    let y: InferenceYml = serde_yaml::from_str(yaml).map_err(|e| e.to_string())?;
    Ok(y.post_process.character_dict)
}

/// CTC-Erkenner mit Wörterbuch.
pub struct Recognizer {
    session: Session,
    dict: Vec<String>,
    input: Vec<f32>,
}

impl Recognizer {
    /// `dict_path`: `inference.yml` mit `PostProcess.character_dict`.
    pub fn new(path: &str, dict_path: &str, threads: usize) -> Result<Self, String> {
        let yaml = std::fs::read_to_string(dict_path).map_err(|e| format!("{dict_path}: {e}"))?;
        let dict = load_dict(&yaml)?;
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
    pub fn recognize(&mut self, img: &RgbImage, r: Rect) -> Result<(String, f32), String> {
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
    fn preprocess(&mut self, img: &RgbImage, r: Rect) -> usize {
        let (cw, ch) = (f32::from(r.w.max(1)), f32::from(r.h.max(1)));
        let raw_w = (REC_H as f32 * cw / ch).round() as usize;
        let tw = (raw_w.div_ceil(32) * 32).clamp(32, MAX_W);
        let rw = raw_w.clamp(1, tw);
        let plane = REC_H * tw;
        self.input.clear();
        self.input.resize(3 * plane, 0.0);
        let (iw, ih) = (img.width() as usize, img.height() as usize);
        for dy in 0..REC_H {
            let sy = (f32::from(r.y) + (dy as f32 + 0.5) * ch / REC_H as f32 - 0.5).round();
            let sy = (sy.max(0.0) as usize).min(ih - 1);
            for dx in 0..rw {
                let sx = (f32::from(r.x) + (dx as f32 + 0.5) * cw / rw as f32 - 0.5).round();
                let sx = (sx.max(0.0) as usize).min(iw - 1);
                let p = px(img, sx, sy);
                let d = dy * tw + dx;
                for (c, &v) in p.iter().enumerate() {
                    self.input[c * plane + d] = f32::from(v) / 127.5 - 1.0;
                }
            }
        }
        tw
    }
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

fn dist2(a: [u8; 3], b: [u8; 3]) -> u32 {
    a.iter()
        .zip(b)
        .map(|(x, y)| u32::from(x.abs_diff(y)).pow(2))
        .sum()
}

fn acc(s: &mut [u32; 3], p: [u8; 3]) {
    for (a, v) in s.iter_mut().zip(p) {
        *a += u32::from(v);
    }
}

/// Dominante Hintergrund- und Schriftfarbe einer Textbox.
/// `bg` = Mittel der häufigsten (auf 4 bit quantisierten) Randfarbe;
/// `fg` = Mittel der Innenpixel mit ≥ 50 % der maximalen Distanz zu `bg`.
#[must_use]
pub fn sample_colors(img: &RgbImage, r: Rect) -> ([u8; 3], [u8; 3]) {
    let (w, h) = (img.width() as usize, img.height() as usize);
    let x0 = (r.x as usize).min(w.saturating_sub(1));
    let y0 = (r.y as usize).min(h.saturating_sub(1));
    let x1 = (x0 + r.w as usize).min(w).saturating_sub(1).max(x0);
    let y1 = (y0 + r.h as usize).min(h).saturating_sub(1).max(y0);
    let mut bins: std::collections::HashMap<u16, ([u32; 3], u32)> = Default::default();
    let mut add = |p: [u8; 3]| {
        let k = (u16::from(p[0] >> 4) << 8) | (u16::from(p[1] >> 4) << 4) | u16::from(p[2] >> 4);
        let e = bins.entry(k).or_default();
        acc(&mut e.0, p);
        e.1 += 1;
    };
    for x in x0..=x1 {
        add(px(img, x, y0));
        add(px(img, x, y1));
    }
    for y in y0..=y1 {
        add(px(img, x0, y));
        add(px(img, x1, y));
    }
    let (sum, n) = bins.values().max_by_key(|(_, n)| *n).copied().unwrap();
    let bg = sum.map(|s| ((s + n / 2) / n) as u8);

    let mut maxd = 0;
    for y in y0..=y1 {
        for x in x0..=x1 {
            maxd = maxd.max(dist2(px(img, x, y), bg));
        }
    }
    if maxd < 30 * 30 {
        // Kaum Kontrast: Schrift in Schwarz/Weiß je nach Helligkeit.
        let luma = u32::from(bg[0]) * 3 + u32::from(bg[1]) * 6 + u32::from(bg[2]);
        return (if luma > 1280 { [0; 3] } else { [255; 3] }, bg);
    }
    let (mut s, mut cnt) = ([0u32; 3], 0u32);
    for y in y0..=y1 {
        for x in x0..=x1 {
            let p = px(img, x, y);
            if dist2(p, bg) * 4 >= maxd {
                acc(&mut s, p);
                cnt += 1;
            }
        }
    }
    (s.map(|v| ((v + cnt / 2) / cnt) as u8), bg)
}

/// Geladene Texterkennung (Pflicht: ohne Modelle kein Serverstart).
pub struct Ocr {
    det: Detector,
    rec: Recognizer,
}

impl Ocr {
    /// Lädt `PP-OCRv6_small_{det,rec}.onnx` + `inference.yml` aus `dir`.
    /// Fehlt eine Datei, ist das ein harter Fehler (kein Fallback: ohne
    /// Textmaskierung sprengt AV1-Text das Bandbreiten-Budget).
    pub fn load(dir: &str, threads: usize) -> Result<Self, String> {
        let (det, rec, dict) = (
            format!("{dir}/PP-OCRv6_small_det.onnx"),
            format!("{dir}/PP-OCRv6_small_rec.onnx"),
            format!("{dir}/inference.yml"),
        );
        for p in [&det, &rec, &dict] {
            if !std::path::Path::new(p).exists() {
                return Err(format!("Modell fehlt: {p}"));
            }
        }
        Ok(Self {
            det: Detector::new(&det, threads)?,
            rec: Recognizer::new(&rec, &dict, threads)?,
        })
    }

    /// Textzeilen mit Farben; unsichere/leere Erkennungen fallen weg
    /// (die bleiben dann Bildinhalt). Das Rechteck ist bereits um [`REC_PAD`]
    /// erweitert — Erkennung, Farben, Maske und Client malen dasselbe.
    pub fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String> {
        let mut out = Vec::new();
        let boxes = self.det.detect(img)?;
        for r in boxes.into_iter().take(MAX_LINES) {
            let r = pad_rect(r, REC_PAD, img.width(), img.height());
            let (text, conf) = self.rec.recognize(img, r)?;
            let text = text.trim().to_owned();
            if text.is_empty() || conf < MIN_TEXT_CONF {
                continue;
            }
            let (fg, bg) = sample_colors(img, r);
            out.push(TextItem {
                rect: r,
                fg,
                bg,
                text,
            });
        }
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capture::solid;

    fn d(v: &[&str]) -> Vec<String> {
        v.iter().map(|s| (*s).to_owned()).collect()
    }

    #[test]
    fn dict_parses_yaml_structure() {
        let dict = load_dict("PostProcess:\n  name: CTCLabelDecode\n  character_dict:\n  - 'a'\n  - b\n  - \"c\"\n  - ''''\n").unwrap();
        assert_eq!(dict, d(&["a", "b", "c", "'"]));
    }

    #[test]
    fn dict_rejects_garbage() {
        assert!(load_dict("kein yaml: [").is_err());
        assert!(load_dict("PostProcess:\n  name: x\n").is_err());
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

    #[test]
    fn missing_models_are_an_error() {
        let Err(e) = Ocr::load("/pfad/den/es/nicht/gibt", 1) else {
            panic!("muss scheitern");
        };
        assert!(e.contains("Modell fehlt"), "{e}");
    }

    #[test]
    fn colors_follow_contrast() {
        // Weiß mit schwarzem Innenstrich: bg weiß, fg dunkel.
        let mut img = solid(16, 16, [255; 3]);
        for x in 4..12 {
            img.put_pixel(x, 8, image::Rgb([0; 3]));
        }
        let (fg, bg) = sample_colors(&img, Rect::new(0, 0, 16, 16));
        assert_eq!(bg, [255; 3]);
        assert!(fg[0] < 128, "{fg:?}");
        // Kontrastlos: Ersatz-Schriftfarbe nach Helligkeit.
        let (fg, _) = sample_colors(&solid(8, 8, [250; 3]), Rect::new(0, 0, 8, 8));
        assert_eq!(fg, [0; 3]);
    }

    #[test]
    fn postprocess_finds_solid_block() {
        let (w, h) = (96, 64);
        let mut prob = vec![0.0f32; w * h];
        for y in 20..32 {
            for x in 10..60 {
                prob[y * w + x] = 0.9;
            }
        }
        let mut visited = vec![0; w * h];
        let b = postprocess(&prob, w, h, &mut visited, 1, &mut Vec::new());
        assert_eq!(b.len(), 1);
        assert!(b[0].x <= 10 && b[0].x2() >= 60);
    }
}
