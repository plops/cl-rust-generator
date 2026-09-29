//! `04_ocr_detect` — PP-OCRv6-Textdetektion (DBNet) plus ONNX-Session-Helfer.
//!
//! Nachverarbeitung aus `26_onnx/source5/src/03_detect.rs` übernommen
//! (Schwellen identisch), aber für beliebige Größen (Vielfache von 32).

use lbw_common::Rect;
use ort::session::Session;
use ort::session::builder::GraphOptimizationLevel;
use ort::value::TensorRef;

use crate::image::Rgb;

/// Pixel-Schwelle für Textkandidaten.
const DET_THRESH: f32 = 0.3;
/// Mindest-Mittelscore einer Komponente.
const BOX_THRESH: f32 = 0.6;
/// Aufweitungsfaktor der Boxen.
const UNCLIP_RATIO: f32 = 1.5;

/// Lädt ein ONNX-Modell (CPU, Level 3, `threads` Intra-Op-Threads; 0 = Default).
pub fn session(path: &str, threads: usize) -> Result<Session, String> {
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
    pub fn detect(&mut self, img: &Rgb) -> Result<Vec<Rect>, String> {
        let (w, h) = (img.w, img.h);
        if w % 32 != 0 || h % 32 != 0 {
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

/// RGB8 → planar, ImageNet-normalisiert (wie source5 `prepare_native`).
pub fn normalize_imagenet(img: &Rgb, out: &mut Vec<f32>) {
    const MEAN: [f32; 3] = [0.485, 0.456, 0.406];
    const STD: [f32; 3] = [0.229, 0.224, 0.225];
    let plane = img.w * img.h;
    out.resize(3 * plane, 0.0);
    for (i, p) in img.data.as_chunks::<3>().0.iter().enumerate() {
        for (c, &v) in p.iter().enumerate() {
            out[c * plane + i] = (f32::from(v) / 255.0 - MEAN[c]) / STD[c];
        }
    }
}

/// DBNet-Nachverarbeitung: verbundene Schwellen-Pixel → aufgeweitete Boxen,
/// sortiert nach Zeile (16-px-Bänder), dann x.
pub fn postprocess(
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

#[cfg(test)]
mod tests {
    use super::*;

    fn run(prob: &[f32], w: usize, h: usize) -> Vec<Rect> {
        let mut visited = vec![0; w * h];
        postprocess(prob, w, h, &mut visited, 1, &mut Vec::new())
    }

    #[test]
    fn solid_block_yields_single_box_inside_image() {
        let (w, h) = (96, 64);
        let mut prob = vec![0.0f32; w * h];
        for y in 20..32 {
            for x in 10..60 {
                prob[y * w + x] = 0.9;
            }
        }
        let b = run(&prob, w, h);
        assert_eq!(b.len(), 1);
        let r = b[0];
        assert!(r.x <= 10 && r.x2() >= 60 && r.y <= 20 && r.y2() >= 32);
        assert!(r.x2() as usize <= w && r.y2() as usize <= h);
    }

    #[test]
    fn weak_and_tiny_regions_are_ignored() {
        let (w, h) = (64, 64);
        assert!(run(&vec![0.2; w * h], w, h).is_empty());
        let mut prob = vec![0.0; w * h];
        prob[5 * w + 5] = 0.99;
        prob[5 * w + 6] = 0.99;
        assert!(run(&prob, w, h).is_empty());
    }

    #[test]
    fn boxes_are_sorted_in_reading_order() {
        let (w, h) = (128, 96);
        let mut prob = vec![0.0f32; w * h];
        for (x0, y0) in [(70, 10), (10, 10), (10, 60)] {
            for y in y0..y0 + 10 {
                for x in x0..x0 + 30 {
                    prob[y * w + x] = 0.9;
                }
            }
        }
        let b = run(&prob, w, h);
        assert_eq!(b.len(), 3);
        assert!(b[0].x < b[1].x && b[1].y < b[2].y);
    }

    #[test]
    fn imagenet_normalization_layout() {
        let img = Rgb::filled(2, 1, [255, 0, 128]);
        let mut out = Vec::new();
        normalize_imagenet(&img, &mut out);
        assert_eq!(out.len(), 6);
        assert!((out[0] - (1.0 - 0.485) / 0.229).abs() < 1e-5);
        assert!((out[2] - (0.0 - 0.456) / 0.224).abs() < 1e-5);
    }
}
