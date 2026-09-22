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
const UNIFONT_BYTES: &[u8] = include_bytes!("/usr/share/fonts/unifont/unifont.otf");

fn window_conf() -> Conf {
    Conf {
        window_title: "PP-OCRv6 Live OCR".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
        ..Default::default()
    }
}

struct TextBox {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    text: String,
}

// ----------------------------------------------------------------------------
// Dictionary & Decoder
// ----------------------------------------------------------------------------

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
// Vision & Post-Processing
// ----------------------------------------------------------------------------

fn postprocess_dbnet(prob: &[f32], visited: &mut [u32], tag: u32) -> Vec<TextBox> {
    let mut boxes = Vec::new();
    let mut q = Vec::with_capacity(512);

    for y in 0..SIZE {
        for x in 0..SIZE {
            let idx = y * SIZE + x;
            if prob[idx] < DET_THRESH || visited[idx] == tag {
                continue;
            }

            visited[idx] = tag;
            q.clear();
            q.push((x, y));

            let (mut min_x, mut max_x, mut min_y, mut max_y) = (x, x, y, y);
            let mut score_sum = 0.0f32;
            let mut head = 0;

            while head < q.len() {
                let (cx, cy) = q[head];
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
                            q.push((nx as usize, ny as usize));
                        }
                    }
                }
            }

            let bw = (max_x - min_x + 1) as f32;
            let bh = (max_y - min_y + 1) as f32;
            let avg_score = score_sum / q.len() as f32;

            if q.len() >= 16 && avg_score >= BOX_THRESH && bw >= 8.0 && bh >= 6.0 {
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

fn preprocess_crop(rgba: &[u8], crop: &TextBox, rec_buf: &mut Vec<f32>) -> usize {
    let (cw, ch) = (crop.w.max(1.0), crop.h.max(1.0));
    let raw_w = (REC_H as f32 * (cw / ch)).round() as usize;
    let target_w = (((raw_w + 31) / 32) * 32).clamp(32, 960);
    let resized_w = raw_w.min(target_w).max(1);

    let total = 3 * REC_H * target_w;
    if rec_buf.len() < total {
        rec_buf.resize(total, 0.0);
    }
    rec_buf[..total].fill(0.0);

    let plane_stride = REC_H * target_w;
    for dy in 0..REC_H {
        let sy = (crop.y + (dy as f32 + 0.5) * (ch / REC_H as f32) - 0.5).round() as usize;
        let sy_c = sy.clamp(0, SIZE - 1);

        for dx in 0..resized_w {
            let sx = (crop.x + (dx as f32 + 0.5) * (cw / resized_w as f32) - 0.5).round() as usize;
            let sx_c = sx.clamp(0, SIZE - 1);

            let src = (sy_c * SIZE + sx_c) * 4;
            let dst = dy * target_w + dx;

            rec_buf[dst] = rgba[src] as f32 / 127.5 - 1.0;
            rec_buf[plane_stride + dst] = rgba[src + 1] as f32 / 127.5 - 1.0;
            rec_buf[2 * plane_stride + dst] = rgba[src + 2] as f32 / 127.5 - 1.0;
        }
    }
    target_w
}

// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------

#[macroquad::main(window_conf)]
async fn main() {
    // Load GNU Unifont from system font directory
    //let font_bytes = std::fs::read("/usr/share/fonts/unifont/unifont.otf")
    //    .expect("Failed to read /usr/share/fonts/unifont/unifont.otf");
    //let font = load_ttf_font_from_bytes(&font_bytes)
    //    .expect("Failed to parse unifont.otf");
    let font = load_ttf_font_from_bytes(UNIFONT_BYTES).expect("Failed to parse font");
    let (conn, screen) = x11rb::connect(None).unwrap();
    let root = conn.setup().roots[screen].root;

    let mut det_session = Session::builder()
        .unwrap()
        .commit_from_memory(DET_BYTES)
        .unwrap();
    let det_in = det_session.inputs()[0].name().to_string();

    let mut rec_session = Session::builder()
        .unwrap()
        .commit_from_memory(REC_BYTES)
        .unwrap();
    let rec_in = rec_session.inputs()[0].name().to_string();

    let dict = load_dict(DICT_YAML);

    let mut img = Image::gen_image_color(SIZE as u16, SIZE as u16, BLACK);
    let tex = Texture2D::from_image(&img);

    let mut det_input = vec![0.0f32; 3 * PLANE];
    let mut rec_input = Vec::with_capacity(3 * REC_H * 960);
    let mut visited = vec![0u32; PLANE];
    let mut tag = 0u32;

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

        // Convert X11 BGRA -> Planar RGB (normalized) & Texture RGBA
        let (r_plane, rest) = det_input.split_at_mut(PLANE);
        let (g_plane, b_plane) = rest.split_at_mut(PLANE);
        for (i, px) in reply.data.chunks_exact(4).take(PLANE).enumerate() {
            let (b, g, r) = (px[0], px[1], px[2]);
            let off = i * 4;
            img.bytes[off..off + 4].copy_from_slice(&[r, g, b, 255]);

            r_plane[i] = (r as f32 / 255.0 - 0.485) / 0.229;
            g_plane[i] = (g as f32 / 255.0 - 0.456) / 0.224;
            b_plane[i] = (b as f32 / 255.0 - 0.406) / 0.225;
        }

        // 1. Detection
        let t0 = Instant::now();
        let det_outs = det_session
            .run(inputs![
                det_in.as_str() => TensorRef::from_array_view(([1, 3, SIZE, SIZE], &det_input[..])).unwrap()
            ])
            .unwrap();
        let det_ms = t0.elapsed().as_secs_f64() * 1000.0;

        tag = tag.wrapping_add(1);
        if tag == 0 {
            visited.fill(0);
            tag = 1;
        }

        let (_, prob_map) = det_outs[0].try_extract_tensor::<f32>().unwrap();
        let mut boxes = postprocess_dbnet(prob_map, &mut visited, tag);

        // 2. Recognition
        let t1 = Instant::now();
        let rec_count = boxes.len().min(MAX_REC_LINES);
        for b in boxes.iter_mut().take(rec_count) {
            let target_w = preprocess_crop(&img.bytes, b, &mut rec_input);
            let input_slice = &rec_input[..3 * REC_H * target_w];
            let rec_outs = rec_session
                .run(inputs![
                    rec_in.as_str() => TensorRef::from_array_view(([1, 3, REC_H, target_w], input_slice)).unwrap()
                ])
                .unwrap();
            let (shape, preds) = rec_outs[0].try_extract_tensor::<f32>().unwrap();
            b.text = ctc_decode(preds, shape, &dict);
        }
        let rec_ms = t1.elapsed().as_secs_f64() * 1000.0;

        // 3. Rendering
        tex.update(&img);
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        for b in &boxes {
            draw_rectangle_lines(b.x, b.y, b.w, b.h, 2.0, GREEN);

            if !b.text.is_empty() {
                // Measure text with unifont
                let dims = measure_text(&b.text, Some(&font), 16, 1.0);
                let (pad, bw, bh) = (3.0, dims.width + 6.0, dims.height + 6.0);
                let bx = b.x.clamp(0.0, (SIZE as f32 - bw).max(0.0));
                let by = if b.y >= bh + 2.0 { b.y - bh - 2.0 } else { b.y + b.h + 2.0 };

                draw_rectangle(bx, by, bw, bh, Color::new(0.0, 0.0, 0.0, 0.85));
                draw_rectangle_lines(bx, by, bw, bh, 1.0, YELLOW);

                // Draw text with unifont using draw_text_ex
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

        draw_rectangle(0.0, 0.0, SIZE as f32, 24.0, Color::new(0.0, 0.0, 0.0, 0.75));
        draw_text_ex(
            &format!("Lines: {} | Det: {det_ms:.1}ms | Rec: {rec_ms:.1}ms | FPS: {}", boxes.len(), get_fps()),
            10.0,
            17.0,
            TextParams {
                font: Some(&font),
                font_size: 16,
                color: GREEN,
                ..Default::default()
            },
        );

        next_frame().await;
    }
}