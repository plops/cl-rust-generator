use macroquad::prelude::*;
use ort::{inputs, session::Session, value::TensorRef};
use std::time::Instant;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

const SIZE: usize = 640;
const PLANE: usize = SIZE * SIZE;
const REC_H: usize = 48;
const MAX_REC_LINES: usize = 16;

const DET_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_det.onnx");
const REC_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_rec.onnx");


fn window_conf() -> Conf {
    Conf {
        window_title: "PP-OCRv6 End-to-End Live OCR".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
        ..Default::default()
    }
}

pub struct TextBox {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
    pub text: String,
    pub score: f32,
}

pub struct DbNetConfig {
    pub thresh: f32,
    pub box_thresh: f32,
    pub unclip_ratio: f32,
}

impl Default for DbNetConfig {
    fn default() -> Self {
        Self {
            thresh: 0.3,
            box_thresh: 0.6,
            unclip_ratio: 1.5,
        }
    }
}

// ============================================================================
// 1. DICTIONARY LOADER (Searches metadata, .yml, .json, and .txt)
// ============================================================================

fn load_dictionary(rec_session: &Session) -> (Vec<String>, bool) {
    // 1. Check if ONNX model metadata contains character dict
    if let Ok(meta) = rec_session.metadata() {
        for key in ["character", "character_dict", "keys"] {
            if let Some(val) = meta.custom(key) {
                let lines: Vec<String> = val.lines().map(|s| s.to_string()).collect();
                if lines.len() > 50 {
                    eprintln!("Loaded {} characters from ONNX metadata '{}'", lines.len(), key);
                    return (lines, true);
                }
            }
        }
    }

    // 2. Search common paths for inference.yml, inference.json, ppocr_keys_v1.txt
    let candidate_paths = [
        "inference.yml",
        "../inference.yml",
        "../../inference.yml",
        "rec.yml",
        "../rec.yml",
        "inference.json",
        "../inference.json",
        "ppocr_keys_v1.txt",
        "../ppocr_keys_v1.txt",
    ];

    for &path in &candidate_paths {
        if let Ok(content) = std::fs::read_to_string(path) {
            if path.ends_with(".yml") || path.ends_with(".yaml") {
                let dict = parse_yaml_dict(&content);
                if !dict.is_empty() {
                    eprintln!("Loaded {} characters from {}", dict.len(), path);
                    return (dict, true);
                }
            } else if path.ends_with(".json") {
                let dict = parse_json_dict(&content);
                if !dict.is_empty() {
                    eprintln!("Loaded {} characters from {}", dict.len(), path);
                    return (dict, true);
                }
            } else {
                let lines: Vec<String> = content
                    .lines()
                    .map(|l| l.trim_end_matches('\r').to_string())
                    .collect();
                if !lines.is_empty() {
                    eprintln!("Loaded {} characters from {}", lines.len(), path);
                    return (lines, true);
                }
            }
        }
    }

    eprintln!("⚠️ WARNING: No dictionary file found! OCR results will be incorrect.");
    eprintln!("   Please copy 'inference.yml' or 'inference.json' next to the binary or in project root.");
    let ascii: Vec<String> = (32u8..=126u8).map(|b| (b as char).to_string()).collect();
    (ascii, false)
}

fn parse_yaml_dict(content: &str) -> Vec<String> {
    let mut dict = Vec::new();
    let mut in_dict = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("character_dict:") {
            in_dict = true;
            continue;
        }
        if in_dict {
            if let Some(stripped) = trimmed.strip_prefix('-') {
                let raw = stripped.trim();
                let val = if (raw.starts_with('\'') && raw.ends_with('\'') && raw.len() >= 2)
                    || (raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2)
                {
                    &raw[1..raw.len() - 1]
                } else {
                    raw
                };
                dict.push(val.to_string());
            } else if !trimmed.is_empty() && !trimmed.starts_with('#') {
                break;
            }
        }
    }
    dict
}

fn parse_json_dict(content: &str) -> Vec<String> {
    let mut dict = Vec::new();
    let key = match content.find("\"character_dict\"").or_else(|| content.find("\"character\"")) {
        Some(idx) => idx,
        None => return dict,
    };

    if let Some(start_bracket) = content[key..].find('[') {
        let array_str = &content[key + start_bracket + 1..];
        let mut in_quote = false;
        let mut escaped = false;
        let mut cur = String::new();
        for c in array_str.chars() {
            if c == ']' && !in_quote {
                break;
            }
            if escaped {
                cur.push(c);
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                if in_quote {
                    dict.push(cur.clone());
                    cur.clear();
                    in_quote = false;
                } else {
                    in_quote = true;
                }
            } else if in_quote {
                cur.push(c);
            }
        }
    }
    dict
}

// ============================================================================
// 2. DETECTION POST-PROCESSING (Controlled Expansion to Avoid Line Bleed)
// ============================================================================

fn postprocess_dbnet(
    prob_map: &[f32],
    w: usize,
    h: usize,
    cfg: &DbNetConfig,
    visited: &mut [u32],
    tag: u32,
) -> Vec<TextBox> {
    let mut boxes = Vec::new();
    let mut queue = Vec::with_capacity(512);

    for y in 0..h {
        for x in 0..w {
            let idx = y * w + x;
            if prob_map[idx] >= cfg.thresh && visited[idx] != tag {
                visited[idx] = tag;
                queue.clear();
                queue.push((x, y));

                let mut min_x = x;
                let mut max_x = x;
                let mut min_y = y;
                let mut max_y = y;
                let mut score_sum = 0.0f32;
                let mut count = 0usize;

                let mut head = 0;
                while head < queue.len() {
                    let (cx, cy) = queue[head];
                    head += 1;

                    min_x = min_x.min(cx);
                    max_x = max_x.max(cx);
                    min_y = min_y.min(cy);
                    max_y = max_y.max(cy);

                    score_sum += prob_map[cy * w + cx];
                    count += 1;

                    for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                        let nx = cx as isize + dx;
                        let ny = cy as isize + dy;
                        if nx >= 0 && nx < w as isize && ny >= 0 && ny < h as isize {
                            let n_idx = ny as usize * w + nx as usize;
                            if visited[n_idx] != tag && prob_map[n_idx] >= cfg.thresh {
                                visited[n_idx] = tag;
                                queue.push((nx as usize, ny as usize));
                            }
                        }
                    }
                }

                let avg_score = score_sum / count as f32;
                let bw = (max_x - min_x + 1) as f32;
                let bh = (max_y - min_y + 1) as f32;

                if count >= 16 && avg_score >= cfg.box_thresh && bw >= 8.0 && bh >= 6.0 {
                    let perimeter = 2.0 * (bw + bh);
                    let area = bw * bh;
                    let distance = (area * cfg.unclip_ratio) / perimeter;

                    // Expand horizontally to capture full letters,
                    // but constrain vertical expansion to avoid capturing lines above/below.
                    let dist_x = distance;
                    let dist_y = (distance * 0.4).min(bh * 0.15).max(1.0);

                    let x1 = (min_x as f32 - dist_x).max(0.0);
                    let y1 = (min_y as f32 - dist_y).max(0.0);
                    let x2 = (max_x as f32 + dist_x).min((w - 1) as f32);
                    let y2 = (max_y as f32 + dist_y).min((h - 1) as f32);

                    boxes.push(TextBox {
                        x: x1,
                        y: y1,
                        w: x2 - x1,
                        h: y2 - y1,
                        text: String::new(),
                        score: avg_score,
                    });
                }
            }
        }
    }

    boxes.sort_by(|a, b| {
        let row_a = (a.y / 16.0) as i32;
        let row_b = (b.y / 16.0) as i32;
        row_a.cmp(&row_b).then_with(|| a.x.total_cmp(&b.x))
    });

    boxes
}

// ============================================================================
// 3. DYNAMIC-WIDTH RECOGNITION PREPROCESSING & DECODER
// ============================================================================

fn preprocess_crop(
    rgba: &[u8],
    frame_w: usize,
    frame_h: usize,
    crop: &TextBox,
    rec_buf: &mut Vec<f32>,
) -> usize {
    let cw = crop.w.max(1.0);
    let ch = crop.h.max(1.0);

    let ratio = cw / ch;
    let raw_w = (REC_H as f32 * ratio).round() as usize;

    // Align to multiple of 32 for CNN downsamplers; allow long lines up to 960px
    let target_w = (((raw_w + 31) / 32) * 32).clamp(32, 960);
    let resized_w = raw_w.min(target_w).max(1);

    let total_elements = 3 * REC_H * target_w;
    if rec_buf.len() < total_elements {
        rec_buf.resize(total_elements, 0.0);
    }
    rec_buf[..total_elements].fill(0.0);

    let plane_stride = REC_H * target_w;

    for dy in 0..REC_H {
        let sy = crop.y + (dy as f32 + 0.5) * (ch / REC_H as f32) - 0.5;
        let sy_c = (sy.round() as usize).clamp(0, frame_h - 1);

        for dx in 0..resized_w {
            let sx = crop.x + (dx as f32 + 0.5) * (cw / resized_w as f32) - 0.5;
            let sx_c = (sx.round() as usize).clamp(0, frame_w - 1);

            let src_idx = (sy_c * frame_w + sx_c) * 4;
            let r = rgba[src_idx] as f32;
            let g = rgba[src_idx + 1] as f32;
            let b = rgba[src_idx + 2] as f32;

            let dst_idx = dy * target_w + dx;
            rec_buf[dst_idx] = r / 127.5 - 1.0;
            rec_buf[plane_stride + dst_idx] = g / 127.5 - 1.0;
            rec_buf[2 * plane_stride + dst_idx] = b / 127.5 - 1.0;
        }
    }

    target_w
}

fn ctc_decode(data: &[f32], shape: &[i64], dict: &[String]) -> String {
    let num_classes = *shape.last().unwrap() as usize;
    if num_classes == 0 {
        return String::new();
    }
    let time_steps = data.len() / num_classes;

    let mut text = String::new();
    let mut prev_idx = 0usize;

    for t in 0..time_steps {
        let row = &data[t * num_classes..(t + 1) * num_classes];
        let mut max_idx = 0;
        let mut max_val = f32::NEG_INFINITY;
        for (i, &v) in row.iter().enumerate() {
            if v > max_val {
                max_val = v;
                max_idx = i;
            }
        }

        if max_idx != 0 && max_idx != prev_idx {
            if (max_idx - 1) < dict.len() {
                text.push_str(&dict[max_idx - 1]);
            } else if max_idx - 1 == dict.len() {
                text.push(' ');
            }
        }
        prev_idx = max_idx;
    }
    text
}

// ============================================================================
// 4. MAIN LOOP
// ============================================================================

#[macroquad::main(window_conf)]
async fn main() {
    let (conn, screen) = x11rb::connect(None).unwrap();
    let root = conn.setup().roots[screen].root;

    let mut det_session = Session::builder()
        .unwrap()
        .commit_from_memory(DET_BYTES)
        .unwrap();
    let det_in_name = det_session.inputs()[0].name().to_string();

    let mut rec_session = Session::builder()
        .unwrap()
        .commit_from_memory(REC_BYTES)
        .unwrap();
    let rec_in_name = rec_session.inputs()[0].name().to_string();

    let (dict, has_real_dict) = load_dictionary(&rec_session);
    let dbnet_cfg = DbNetConfig::default();

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

        // Detection
        let t_det0 = Instant::now();
        let det_outs = det_session
            .run(inputs![
                det_in_name.as_str() => TensorRef::from_array_view(([1, 3, SIZE, SIZE], &det_input[..])).unwrap()
            ])
            .unwrap();
        let det_ms = t_det0.elapsed().as_secs_f64() * 1000.0;

        tag = tag.wrapping_add(1);
        if tag == 0 {
            visited.fill(0);
            tag = 1;
        }

        let (_, prob_map) = det_outs[0].try_extract_tensor::<f32>().unwrap();
        let mut boxes = postprocess_dbnet(prob_map, SIZE, SIZE, &dbnet_cfg, &mut visited, tag);

        // Recognition (dynamic width per text line)
        let t_rec0 = Instant::now();
        let rec_count = boxes.len().min(MAX_REC_LINES);
        for b in boxes.iter_mut().take(rec_count) {
            let target_w = preprocess_crop(&img.bytes, SIZE, SIZE, b, &mut rec_input);
            let input_slice = &rec_input[..3 * REC_H * target_w];
            let rec_outs = rec_session
                .run(inputs![
                    rec_in_name.as_str() => TensorRef::from_array_view(([1, 3, REC_H, target_w], input_slice)).unwrap()
                ])
                .unwrap();
            let (shape, preds) = rec_outs[0].try_extract_tensor::<f32>().unwrap();
            b.text = ctc_decode(preds, shape, &dict);
        }
        let rec_ms = t_rec0.elapsed().as_secs_f64() * 1000.0;

        // Render
        tex.update(&img);
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        for b in &boxes {
            draw_rectangle_lines(b.x, b.y, b.w, b.h, 2.0, GREEN);

            if !b.text.is_empty() {
                let font_size = 16.0;
                let dims = measure_text(&b.text, None, font_size as u16, 1.0);
                let pad = 3.0;
                let bg_w = dims.width + pad * 2.0;
                let bg_h = dims.height + pad * 2.0;
                let bg_x = b.x.clamp(0.0, (SIZE as f32 - bg_w).max(0.0));
                let bg_y = if b.y >= bg_h + 2.0 {
                    b.y - bg_h - 2.0
                } else {
                    b.y + b.h + 2.0
                };

                draw_rectangle(bg_x, bg_y, bg_w, bg_h, Color::new(0.0, 0.0, 0.0, 0.85));
                draw_rectangle_lines(bg_x, bg_y, bg_w, bg_h, 1.0, YELLOW);
                draw_text(&b.text, bg_x + pad, bg_y + bg_h - pad - 2.0, font_size, WHITE);
            }
        }

        // Top HUD
        draw_rectangle(0.0, 0.0, SIZE as f32, 24.0, Color::new(0.0, 0.0, 0.0, 0.75));
        draw_text(
            format!(
                "Lines: {} | Det: {:.1}ms | Rec: {:.1}ms | FPS: {}",
                boxes.len(),
                det_ms,
                rec_ms,
                get_fps()
            ),
            10.0,
            17.0,
            15.0,
            GREEN,
        );

        if !has_real_dict {
            draw_rectangle(0.0, 24.0, SIZE as f32, 22.0, Color::new(0.8, 0.1, 0.1, 0.9));
            draw_text(
                "MISSING DICT: Copy 'inference.yml' to project folder to fix text!",
                10.0,
                40.0,
                15.0,
                WHITE,
            );
        }

        next_frame().await;
    }
}