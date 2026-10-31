//! `03_detect` — DBNet-Textdetektion: findet Textzeilen-Boxen im Bild.
//!
//! Voll-convolutional: akzeptiert jede 32er-Eingabegröße (1280×720 wird per
//! [`pad_to_32`] auf 1280×736 gebracht, Boxen danach per [`clip_to`]
//! zurückgeschnitten). Hier liegen auch die geteilten Bausteine:
//! [`session`] (ONNX-Session-Bau, CPU) und [`px`] (Pixelzugriff).

use image::RgbImage;
use ort::session::Session;
use ort::session::builder::GraphOptimizationLevel;
use ort::value::TensorRef;

use lbw_common::Rect;

/// Pixel-Schwelle für Textkandidaten.
const DET_THRESH: f32 = 0.3;
/// Mindest-Mittelscore einer Komponente.
const BOX_THRESH: f32 = 0.6;
/// Aufweitungsfaktor der Boxen.
const UNCLIP_RATIO: f32 = 1.5;

/// Prozessweite ONNX-Runtime-Umgebung (ab ort rc.15 Pflichtargument von
/// `Session::builder`; eine für alle Sessions).
fn env() -> Result<&'static ort::environment::Environment, String> {
    static ENV: std::sync::OnceLock<ort::environment::Environment> = std::sync::OnceLock::new();
    if let Some(e) = ENV.get() {
        return Ok(e);
    }
    let e = ort::init().build().map_err(|e| format!("ort: {e}"))?;
    Ok(ENV.get_or_init(|| e))
}

/// Lädt ein ONNX-Modell auf der CPU (Level 3, `threads` Intra-Op-Threads;
/// 0 = Default). CPU-only: Der CUDA-Pfad aus source10 ist in diesem
/// Container nicht testbar, und DBNet misst auf der CPU ~107 ms für
/// 1280×736 (8 Threads) — schnell genug für 10 fps-Takt mit Cache.
pub(crate) fn session(path: &str, threads: usize) -> Result<Session, String> {
    let e = |e: ort::Error| format!("{path}: {e}");
    let mut b = Session::builder(env()?)
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

pub(crate) fn px(img: &RgbImage, x: usize, y: usize) -> [u8; 3] {
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
        let session = session(path, threads)?;
        Ok(Self {
            session,
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

/// Füllt `img` per Kanten-Replikation auf 32er-Vielfache auf
/// (DBNet-Bedingung; 1280×720 → 1280×736). Replikation statt Füllfarbe:
/// keine künstliche Kante, die der Detektor als Text lesen könnte.
/// Liefert (Bild, Originalbreite, Originalhöhe).
#[must_use]
pub fn pad_to_32(img: &RgbImage) -> (RgbImage, u32, u32) {
    let (w, h) = (img.width(), img.height());
    let (pw, ph) = (w.div_ceil(32) * 32, h.div_ceil(32) * 32);
    let mut out = RgbImage::new(pw, ph);
    for y in 0..ph {
        let sy = y.min(h - 1);
        for x in 0..pw {
            out.put_pixel(x, y, *img.get_pixel(x.min(w - 1), sy));
        }
    }
    (out, w, h)
}

/// Schneidet `r` auf `w`×`h` zu; `None` bei Boxen ganz außerhalb
/// (z. B. in der Padding-Zone unterhalb von 720).
#[must_use]
pub fn clip_to(r: Rect, w: u32, h: u32) -> Option<Rect> {
    let (x, y) = (u32::from(r.x), u32::from(r.y));
    if x >= w || y >= h {
        return None;
    }
    Some(Rect::new(
        r.x,
        r.y,
        r.w.min((w - x) as u16),
        r.h.min((h - y) as u16),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capture::solid;

    #[test]
    fn pad_to_32_replicates_edges() {
        // 720p → 736 hoch, letzte Zeile/Spalte repliziert.
        let mut img = solid(40, 40, [9; 3]);
        img.put_pixel(39, 39, image::Rgb([1, 2, 3]));
        let (p, ow, oh) = pad_to_32(&img);
        assert_eq!(((p.width(), p.height()), (ow, oh)), ((64, 64), (40, 40)));
        assert_eq!(p.get_pixel(63, 63).0, [1, 2, 3]);
        assert_eq!(p.get_pixel(40, 0).0, [9; 3]);
        // Schon passende Größe: Maße bleiben, Inhalt gleich.
        let (q, _, _) = pad_to_32(&solid(32, 64, [7; 3]));
        assert_eq!((q.width(), q.height()), (32, 64));
        assert_eq!(q.get_pixel(31, 63).0, [7; 3]);
    }

    #[test]
    fn clip_to_cuts_padding_zone() {
        assert_eq!(
            clip_to(Rect::new(10, 10, 20, 8), 1280, 720),
            Some(Rect::new(10, 10, 20, 8))
        );
        // Über den Originalrand hinaus: klemmen.
        assert_eq!(
            clip_to(Rect::new(1270, 710, 20, 26), 1280, 720),
            Some(Rect::new(1270, 710, 10, 10))
        );
        // Ganz in der Padding-Zone (y ≥ 720): weg.
        assert_eq!(clip_to(Rect::new(0, 720, 50, 16), 1280, 720), None);
        assert_eq!(clip_to(Rect::new(1280, 0, 8, 8), 1280, 720), None);
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
