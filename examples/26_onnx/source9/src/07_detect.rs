//! `07_detect` — PP-OCRv6-Detektion (DBNet) auf dem 640×640-Canvas.
//!
//! Eingang ist RGBA aus `06_render` (source5 nahm BGRA vom Bildschirm).
//! `postprocess_dbnet` ist 1:1 aus source5 übernommen (nur `TextBox` nutzt
//! jetzt `render::Rect`).

use ort::{inputs, session::Session, value::TensorRef};
use std::path::Path;

use crate::render::{CANVAS, Rect};

/// Pixel-Schwelle für Textkandidaten.
const DET_THRESH: f32 = 0.3;
/// Mindest-Mittelscore einer Komponente.
const BOX_THRESH: f32 = 0.6;
/// Aufweitungsfaktor für die Boxen.
const UNCLIP_RATIO: f32 = 1.5;
/// Pixelzahl des Modell-Inputs.
const PLANE: usize = CANVAS * CANVAS;

/// Detektierte Textzeile (Canvas-Koordinaten).
#[derive(Clone, Debug, Default)]
pub struct TextBox {
    /// Aufgeweitete Box.
    pub rect: Rect,
    /// Erkannter Text (füllt `Recognizer`).
    pub text: String,
}

/// DBNet-Detektor mit wiederverwendbaren Buffern.
pub struct Detector {
    session: Session,
    in_name: String,
    /// Planar-normalisierter Input (`3×640×640`).
    input: Vec<f32>,
    visited: Vec<u32>,
    tag: u32,
    queue: Vec<(usize, usize)>,
}

impl Detector {
    /// Lädt die Detektions-Session aus einer ONNX-Datei.
    pub fn open(path: &Path) -> Result<Self, String> {
        let session = Session::builder()
            .map_err(|e| e.to_string())?
            .commit_from_file(path)
            .map_err(|e| format!("{}: {e}", path.display()))?;
        let in_name = session.inputs()[0].name().to_string();
        Ok(Self {
            session,
            in_name,
            input: vec![0.0f32; 3 * PLANE],
            visited: vec![0u32; PLANE],
            tag: 0,
            queue: Vec::with_capacity(512),
        })
    }

    /// Detektiert Textzeilen im Canvas-RGBA (`CANVAS²×4`).
    pub fn detect(&mut self, rgba: &[u8]) -> Result<Vec<TextBox>, String> {
        rgba_to_planar(rgba, &mut self.input);
        let outs = self
            .session
            .run(inputs![
                self.in_name.as_str() => TensorRef::from_array_view(([1, 3, CANVAS, CANVAS], &self.input[..])).map_err(|e| e.to_string())?
            ])
            .map_err(|e| e.to_string())?;
        self.tag = self.tag.wrapping_add(1);
        if self.tag == 0 {
            self.visited.fill(0);
            self.tag = 1;
        }
        let (_, prob) = outs[0]
            .try_extract_tensor::<f32>()
            .map_err(|e| e.to_string())?;
        Ok(postprocess_dbnet(
            prob,
            &mut self.visited,
            self.tag,
            &mut self.queue,
        ))
    }
}

/// RGBA (640²) → planar-normalisiert (ImageNet-Mittelwerte, aus source5).
pub fn rgba_to_planar(rgba: &[u8], planes: &mut [f32]) {
    const R_SCALE: f32 = 1.0 / (255.0 * 0.229);
    const R_OFF: f32 = 0.485 / 0.229;
    const G_SCALE: f32 = 1.0 / (255.0 * 0.224);
    const G_OFF: f32 = 0.456 / 0.224;
    const B_SCALE: f32 = 1.0 / (255.0 * 0.225);
    const B_OFF: f32 = 0.406 / 0.225;

    let (r_plane, rest) = planes.split_at_mut(PLANE);
    let (g_plane, b_plane) = rest.split_at_mut(PLANE);
    let (src, _) = rgba.as_chunks::<4>();
    for (i, s) in src.iter().take(PLANE).enumerate() {
        r_plane[i] = f32::from(s[0]) * R_SCALE - R_OFF;
        g_plane[i] = f32::from(s[1]) * G_SCALE - G_OFF;
        b_plane[i] = f32::from(s[2]) * B_SCALE - B_OFF;
    }
}

/// DBNet-Nachverarbeitung: verbundene Schwellen-Pixel → aufgeweitete Boxen.
pub fn postprocess_dbnet(
    prob: &[f32],
    visited: &mut [u32],
    tag: u32,
    queue: &mut Vec<(usize, usize)>,
) -> Vec<TextBox> {
    let mut boxes = Vec::new();

    for y in 0..CANVAS {
        for x in 0..CANVAS {
            let idx = y * CANVAS + x;
            if prob[idx] < DET_THRESH || visited[idx] == tag {
                continue;
            }

            visited[idx] = tag;
            queue.clear();
            queue.push((x, y));

            let (mut min_x, mut max_x, mut min_y, mut max_y) = (x, x, y, y);
            let mut score_sum = 0.0f32;
            let mut head = 0;

            while head < queue.len() {
                let (cx, cy) = queue[head];
                head += 1;

                min_x = min_x.min(cx);
                max_x = max_x.max(cx);
                min_y = min_y.min(cy);
                max_y = max_y.max(cy);
                score_sum += prob[cy * CANVAS + cx];

                for (dx, dy) in [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)] {
                    let nx = cx as isize + dx;
                    let ny = cy as isize + dy;
                    if nx >= 0 && nx < CANVAS as isize && ny >= 0 && ny < CANVAS as isize {
                        let n_idx = ny as usize * CANVAS + nx as usize;
                        if visited[n_idx] != tag && prob[n_idx] >= DET_THRESH {
                            visited[n_idx] = tag;
                            queue.push((nx as usize, ny as usize));
                        }
                    }
                }
            }

            let bw = (max_x - min_x + 1) as f32;
            let bh = (max_y - min_y + 1) as f32;
            let avg_score = score_sum / queue.len() as f32;

            if queue.len() >= 16 && avg_score >= BOX_THRESH && bw >= 8.0 && bh >= 6.0 {
                let dist = (bw * bh * UNCLIP_RATIO) / (2.0 * (bw + bh));
                let dist_y = (dist * 0.4).min(bh * 0.15).max(1.0);

                let x1 = (min_x as f32 - dist).max(0.0);
                let y1 = (min_y as f32 - dist_y).max(0.0);
                let x2 = (max_x as f32 + dist).min((CANVAS - 1) as f32);
                let y2 = (max_y as f32 + dist_y).min((CANVAS - 1) as f32);

                boxes.push(TextBox {
                    rect: Rect {
                        x: x1,
                        y: y1,
                        w: x2 - x1,
                        h: y2 - y1,
                    },
                    text: String::new(),
                });
            }
        }
    }

    boxes.sort_by(|a, b| {
        ((a.rect.y / 16.0) as i32)
            .cmp(&((b.rect.y / 16.0) as i32))
            .then_with(|| a.rect.x.total_cmp(&b.rect.x))
    });
    boxes
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(prob: &[f32]) -> Vec<TextBox> {
        let mut visited = vec![0u32; PLANE];
        let mut queue = Vec::new();
        postprocess_dbnet(prob, &mut visited, 1, &mut queue)
    }

    #[test]
    fn solid_block_yields_single_box() {
        // 20×20-Block über der Schwelle → genau eine Box (aus source5).
        let mut prob = vec![0.0f32; PLANE];
        for y in 100..120 {
            for x in 200..220 {
                prob[y * CANVAS + x] = 0.9;
            }
        }
        let boxes = run(&prob);
        assert_eq!(boxes.len(), 1);
        let r = &boxes[0].rect;
        assert!(r.x <= 200.0 && r.x + r.w >= 220.0);
        assert!(r.y <= 100.0 && r.y + r.h >= 120.0);
    }

    #[test]
    fn weak_pixels_yield_no_boxes() {
        assert!(run(&vec![0.2f32; PLANE]).is_empty());
    }

    #[test]
    fn tiny_specks_are_filtered() {
        // 2×2-Block: über der Schwelle, aber unter der Mindestgröße.
        let mut prob = vec![0.0f32; PLANE];
        for y in 50..52 {
            for x in 50..52 {
                prob[y * CANVAS + x] = 0.95;
            }
        }
        assert!(run(&prob).is_empty());
    }

    #[test]
    fn rgba_to_planar_matches_imagenet_norm() {
        let mut rgba = vec![0u8; 4 * PLANE];
        rgba[0..4].copy_from_slice(&[255, 0, 0, 255]); // rot
        let mut planes = vec![0.0f32; 3 * PLANE];
        rgba_to_planar(&rgba, &mut planes);
        assert!((planes[0] - (1.0 - 0.485) / 0.229).abs() < 1e-5);
        assert!((planes[PLANE] - (0.0 - 0.456) / 0.224).abs() < 1e-5);
        assert!((planes[2 * PLANE] - (0.0 - 0.406) / 0.225).abs() < 1e-5);
    }
}
