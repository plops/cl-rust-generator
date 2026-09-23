/// S1: ROI-State; Verdrahtung in den Loop erfolgt in S4.
#[allow(dead_code)]
#[path = "01_view.rs"]
mod view;

use macroquad::prelude::*;
use ort::{inputs, session::Session, value::TensorRef};
use std::time::Instant;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

const SIZE: usize = 640;
const PLANE: usize = SIZE * SIZE;
const REC_H: usize = 48;
const MAX_REC_LINES: usize = 64;

const DET_THRESH: f32 = 0.3;
const BOX_THRESH: f32 = 0.6;
const UNCLIP_RATIO: f32 = 1.5;

const DET_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_det.onnx");
const REC_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_rec.onnx");
const DICT_YAML: &str = include_str!("../inference.yml");
/// Sucht die Unifont-Datei an den bekannten System-Pfaden (APT-Paket
/// `fonts-unifont` installiert unter `opentype/`, nicht `unifont/`).
fn load_font_bytes() -> Vec<u8> {
    const CANDIDATES: &[&str] = &[
        "/usr/share/fonts/opentype/unifont/unifont.otf",
        "/usr/share/fonts/unifont/unifont.otf",
        "/usr/share/fonts/truetype/unifont/unifont.ttf",
    ];
    for path in CANDIDATES {
        if let Ok(bytes) = std::fs::read(path) {
            return bytes;
        }
    }
    panic!(
        "GNU Unifont not found (tried: {}); install it via `apt-get install fonts-unifont`",
        CANDIDATES.join(", ")
    );
}

fn window_conf() -> Conf {
    Conf {
        window_title: "PP-OCRv6 Live OCR".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
        ..Default::default()
    }
}

#[derive(Clone, Default)]
struct TextBox {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    text: String,
}

// ----------------------------------------------------------------------------
// OCR Engine (Detection + Recognition)
// ----------------------------------------------------------------------------

struct OcrEngine {
    det_session: Session,
    rec_session: Session,
    det_in_name: String,
    rec_in_name: String,
    dict: Vec<&'static str>,

    // Reusable scratch buffers
    det_input: Vec<f32>,
    rec_input: Vec<f32>,
    visited: Vec<u32>,
    tag: u32,
    bfs_queue: Vec<(usize, usize)>,
}

impl OcrEngine {
    fn new() -> Self {
        let det_session = Session::builder()
            .unwrap()
            .commit_from_memory(DET_BYTES)
            .unwrap();
        let det_in_name = det_session.inputs()[0].name().to_string();

        let rec_session = Session::builder()
            .unwrap()
            .commit_from_memory(REC_BYTES)
            .unwrap();
        let rec_in_name = rec_session.inputs()[0].name().to_string();

        Self {
            det_session,
            rec_session,
            det_in_name,
            rec_in_name,
            dict: load_dict(DICT_YAML),
            det_input: vec![0.0f32; 3 * PLANE],
            rec_input: Vec::with_capacity(3 * REC_H * 960),
            visited: vec![0u32; PLANE],
            tag: 0,
            bfs_queue: Vec::with_capacity(512),
        }
    }

    /// Prepares detection planar RGB floats and texture RGBA bytes from X11 BGRA
    fn prepare_inputs(&mut self, bgra_src: &[u8], rgba_dst: &mut [u8]) {
        let (r_plane, rest) = self.det_input.split_at_mut(PLANE);
        let (g_plane, b_plane) = rest.split_at_mut(PLANE);

        // Pre-calculated normalization multipliers: (1 / 255) / std
        const R_SCALE: f32 = 1.0 / (255.0 * 0.229);
        const R_OFF: f32 = 0.485 / 0.229;
        const G_SCALE: f32 = 1.0 / (255.0 * 0.224);
        const G_OFF: f32 = 0.456 / 0.224;
        const B_SCALE: f32 = 1.0 / (255.0 * 0.225);
        const B_OFF: f32 = 0.406 / 0.225;

        let (chunks, _) = bgra_src.as_chunks::<4>();
        for (i, px) in chunks.iter().take(PLANE).enumerate() {
            let (b, g, r) = (px[0], px[1], px[2]);
            let off = i * 4;
            rgba_dst[off..off + 4].copy_from_slice(&[r, g, b, 255]);

            r_plane[i] = r as f32 * R_SCALE - R_OFF;
            g_plane[i] = g as f32 * G_SCALE - G_OFF;
            b_plane[i] = b as f32 * B_SCALE - B_OFF;
        }
    }

    /// Runs DBNet text detector
    fn detect(&mut self) -> Vec<TextBox> {
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

        // Pass disjoint fields to avoid borrowing all of `*self` while `det_outs` is alive
        postprocess_dbnet(prob_map, &mut self.visited, self.tag, &mut self.bfs_queue)
    }

    /// Crops each bounding box and runs text recognition
    fn recognize(&mut self, rgba: &[u8], boxes: &mut [TextBox]) {
        let count = boxes.len().min(MAX_REC_LINES);
        for b in boxes.iter_mut().take(count) {
            let target_w = self.preprocess_crop(rgba, b);
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

    fn preprocess_crop(&mut self, rgba: &[u8], crop: &TextBox) -> usize {
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
        for dy in 0..REC_H {
            let sy = (crop.y + (dy as f32 + 0.5) * (ch / REC_H as f32) - 0.5).round() as usize;
            let sy_c = sy.clamp(0, SIZE - 1);

            for dx in 0..resized_w {
                let sx =
                    (crop.x + (dx as f32 + 0.5) * (cw / resized_w as f32) - 0.5).round() as usize;
                let sx_c = sx.clamp(0, SIZE - 1);

                let src = (sy_c * SIZE + sx_c) * 4;
                let dst = dy * target_w + dx;

                self.rec_input[dst] = rgba[src] as f32 / 127.5 - 1.0;
                self.rec_input[plane_stride + dst] = rgba[src + 1] as f32 / 127.5 - 1.0;
                self.rec_input[2 * plane_stride + dst] = rgba[src + 2] as f32 / 127.5 - 1.0;
            }
        }
        target_w
    }
}

// ----------------------------------------------------------------------------
// Post-Processing & Decoders
// ----------------------------------------------------------------------------

fn postprocess_dbnet(
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

fn load_dict(yaml: &'static str) -> Vec<&'static str> {
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

fn ctc_decode(data: &[f32], shape: &[i64], dict: &[&str]) -> String {
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

// ----------------------------------------------------------------------------
// Main Loop
// ----------------------------------------------------------------------------

#[macroquad::main(window_conf)]
async fn main() {
    let font_bytes = load_font_bytes();
    let font = load_ttf_font_from_bytes(&font_bytes).expect("Failed to parse font");
    let (conn, screen) = x11rb::connect(None).expect("Failed to connect to X11");
    let root = conn.setup().roots[screen].root;

    let mut engine = OcrEngine::new();
    let mut img = Image::gen_image_color(SIZE as u16, SIZE as u16, BLACK);
    let tex = Texture2D::from_image(&img);

    // Frame cache & deduplication buffers
    let mut prev_screen_bytes = Vec::new();
    let mut prev_printed_lines: Vec<String> = Vec::new();
    let mut cached_boxes: Vec<TextBox> = Vec::new();

    let mut det_ms = 0.0;
    let mut rec_ms = 0.0;

    while !is_key_down(KeyCode::Escape) {
        let reply = xproto::get_image(
            &conn,
            ImageFormat::Z_PIXMAP,
            root,
            0,
            0,
            SIZE as u16,
            SIZE as u16,
            u32::MAX,
        )
        .unwrap()
        .reply()
        .unwrap();

        // 1. Change Detector
        // High-throughput raw slice equality (SIMD-accelerated memcmp)
        let frame_changed = prev_screen_bytes != reply.data;
        let is_idle = !frame_changed;

        if frame_changed {
            // Convert X11 BGRA -> Planar RGB & RGBA texture
            engine.prepare_inputs(&reply.data, &mut img.bytes);
            tex.update(&img);

            // Run detection
            let t0 = Instant::now();
            let mut boxes = engine.detect();
            det_ms = t0.elapsed().as_secs_f64() * 1000.0;

            // Run recognition
            let t1 = Instant::now();
            engine.recognize(&img.bytes, &mut boxes);
            rec_ms = t1.elapsed().as_secs_f64() * 1000.0;

            // 2. Print output without duplicates
            let current_lines: Vec<String> = boxes
                .iter()
                .map(|b| b.text.trim())
                .filter(|t| !t.is_empty())
                .map(|t| t.to_string())
                .collect();

            if !current_lines.is_empty() && current_lines != prev_printed_lines {
                println!(
                    "--- [{}] Detected ({} lines) ---",
                    get_time() as u64,
                    current_lines.len()
                );
                for line in &current_lines {
                    println!("{line}");
                }
                prev_printed_lines = current_lines;
            }

            cached_boxes = boxes;
            prev_screen_bytes = reply.data; // O(1) buffer move
        }

        // 3. Render
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        for b in &cached_boxes {
            draw_rectangle_lines(b.x, b.y, b.w, b.h, 2.0, GREEN);

            if !b.text.is_empty() {
                let dims = measure_text(&b.text, Some(&font), 16, 1.0);
                let (pad, bw, bh) = (3.0, dims.width + 6.0, dims.height + 6.0);
                let bx = b.x.clamp(0.0, (SIZE as f32 - bw).max(0.0));
                let by = if b.y >= bh + 2.0 {
                    b.y - bh - 2.0
                } else {
                    b.y + b.h + 2.0
                };

                draw_rectangle(bx, by, bw, bh, Color::new(0.0, 0.0, 0.0, 0.85));
                draw_rectangle_lines(bx, by, bw, bh, 1.0, YELLOW);

                draw_text_ex(
                    &b.text,
                    bx + pad,
                    by + bh - pad - 2.0,
                    TextParams {
                        font: Some(&font),
                        font_size: 16,
                        color: WHITE,
                        ..Default::default()
                    },
                );
            }
        }

        // HUD overlay
        let status_color = if is_idle {
            Color::new(0.4, 0.8, 1.0, 1.0)
        } else {
            GREEN
        };
        let status_text = if is_idle { "PAUSED (STATIC)" } else { "ACTIVE" };

        draw_rectangle(0.0, 0.0, SIZE as f32, 24.0, Color::new(0.0, 0.0, 0.0, 0.75));
        draw_text_ex(
            format!(
                "[{status_text}] Lines: {} | Det: {det_ms:.1}ms | Rec: {rec_ms:.1}ms | FPS: {}",
                cached_boxes.len(),
                get_fps()
            ),
            10.0,
            17.0,
            TextParams {
                font: Some(&font),
                font_size: 16,
                color: status_color,
                ..Default::default()
            },
        );

        next_frame().await;
    }
}
