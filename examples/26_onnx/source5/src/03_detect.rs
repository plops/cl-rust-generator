//! `03_detect` — PP-OCRv6-Detektion (DBNet) plus Box-Nachverarbeitung (S3).
//!
//! Unveränderter Code aus `main.rs`; Konstanten und Schwellen identisch.
//! `Detector` besitzt Session und wiederverwendbare Buffer.

use ort::{inputs, session::Session, value::TensorRef};

use crate::view::MODEL_SIZE;

/// Detektionsmodell (PP-OCRv6 small, ONNX).
const DET_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_det.onnx");

/// Pixel-Schwelle für Textkandidaten.
const DET_THRESH: f32 = 0.3;
/// Mindest-Mittelscore einer Komponente.
const BOX_THRESH: f32 = 0.6;
/// Aufweitungsfaktor für die Boxen.
const UNCLIP_RATIO: f32 = 1.5;

/// Kantenlänge als `usize` (Schleifen-/Index-Arithmetik).
const SIZE: usize = MODEL_SIZE as usize;
/// Pixelzahl des Modell-Inputs.
const PLANE: usize = SIZE * SIZE;

/// Detektierte Textzeile (Koordinaten ROI-lokal, 640×640-Raum).
#[derive(Clone, Default)]
pub struct TextBox {
    /// Linke Kante.
    pub x: f32,
    /// Obere Kante.
    pub y: f32,
    /// Breite.
    pub w: f32,
    /// Höhe.
    pub h: f32,
    /// Erkannter Text (füllt `Recognizer`).
    pub text: String,
}

/// DBNet-Detektor mit wiederverwendbaren Buffern.
pub struct Detector {
    det_session: Session,
    det_in_name: String,
    /// Planar-normalisierter Input (`3×640×640`).
    pub det_input: Vec<f32>,
    visited: Vec<u32>,
    tag: u32,
    bfs_queue: Vec<(usize, usize)>,
}

impl Detector {
    /// Lädt die Detektions-Session.
    pub fn new() -> Self {
        let det_session = Session::builder()
            .unwrap()
            .commit_from_memory(DET_BYTES)
            .unwrap();
        let det_in_name = det_session.inputs()[0].name().to_string();

        Self {
            det_session,
            det_in_name,
            det_input: vec![0.0f32; 3 * PLANE],
            visited: vec![0u32; PLANE],
            tag: 0,
            bfs_queue: Vec::with_capacity(512),
        }
    }

    /// Führt die Detektion auf dem vorbereiteten Input aus.
    pub fn detect(&mut self) -> Vec<TextBox> {
        let det_outs = self
            .det_session
            .run(inputs![
                self.det_in_name.as_str() => TensorRef::from_array_view(([1, 3, SIZE, SIZE], &self.det_input[..])).unwrap()
            ])
            .unwrap();

        self.tag = self.tag.wrapping_add(1);
        if self.tag == 0 {
            self.visited.fill(0);
            self.tag = 1;
        }

        let (_, prob_map) = det_outs[0].try_extract_tensor::<f32>().unwrap();

        // Disjunkte Felder übergeben, statt `*self` zu leihen, solange `det_outs` lebt.
        postprocess_dbnet(prob_map, &mut self.visited, self.tag, &mut self.bfs_queue)
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

    for y in 0..SIZE {
        for x in 0..SIZE {
            let idx = y * SIZE + x;
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
                score_sum += prob[cy * SIZE + cx];

                for (dx, dy) in [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)] {
                    let nx = cx as isize + dx;
                    let ny = cy as isize + dy;
                    if nx >= 0 && nx < SIZE as isize && ny >= 0 && ny < SIZE as isize {
                        let n_idx = ny as usize * SIZE + nx as usize;
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
                let x2 = (max_x as f32 + dist).min((SIZE - 1) as f32);
                let y2 = (max_y as f32 + dist_y).min((SIZE - 1) as f32);

                boxes.push(TextBox {
                    x: x1,
                    y: y1,
                    w: x2 - x1,
                    h: y2 - y1,
                    text: String::new(),
                });
            }
        }
    }

    boxes.sort_by(|a, b| {
        ((a.y / 16.0) as i32)
            .cmp(&((b.y / 16.0) as i32))
            .then_with(|| a.x.total_cmp(&b.x))
    });
    boxes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solid_block_yields_single_box() {
        // 20×20-Block über der Schwelle → genau eine Box.
        let mut prob = vec![0.0f32; PLANE];
        for y in 100..120 {
            for x in 200..220 {
                prob[y * SIZE + x] = 0.9;
            }
        }
        let mut visited = vec![0u32; PLANE];
        let mut queue = Vec::new();
        let boxes = postprocess_dbnet(&prob, &mut visited, 1, &mut queue);
        assert_eq!(boxes.len(), 1);
        let b = &boxes[0];
        assert!(b.x <= 200.0 && b.x + b.w >= 220.0);
        assert!(b.y <= 100.0 && b.y + b.h >= 120.0);
    }

    #[test]
    fn weak_pixels_yield_no_boxes() {
        let prob = vec![0.2f32; PLANE];
        let mut visited = vec![0u32; PLANE];
        let mut queue = Vec::new();
        let boxes = postprocess_dbnet(&prob, &mut visited, 1, &mut queue);
        assert!(boxes.is_empty());
    }

    #[test]
    fn tiny_specks_are_filtered() {
        // 2×2-Block: über der Schwelle, aber unter der Mindestgröße.
        let mut prob = vec![0.0f32; PLANE];
        for y in 50..52 {
            for x in 50..52 {
                prob[y * SIZE + x] = 0.95;
            }
        }
        let mut visited = vec![0u32; PLANE];
        let mut queue = Vec::new();
        let boxes = postprocess_dbnet(&prob, &mut visited, 1, &mut queue);
        assert!(boxes.is_empty());
    }
}
