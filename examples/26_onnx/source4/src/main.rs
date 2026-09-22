use macroquad::prelude::*;
use ort::{inputs, session::Session, value::TensorRef};
use std::time::Instant;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

const SIZE: usize = 640;
const PLANE: usize = SIZE * SIZE;

// Compiles the PP-OCRv6_tiny_det weights into .rodata (rename inference.onnx to det.onnx)
const MODEL_BYTES: &[u8] = include_bytes!("../det.onnx");

fn window_conf() -> Conf {
    Conf {
        window_title: "PP-OCRv6 Text Detector".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
        ..Default::default()
    }
}

pub struct DetBox {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
    pub score: f32,
}

/// Pure Rust DBNet postprocessing: converts the probability heatmap into bounding boxes.
fn postprocess_dbnet(
    prob_map: &[f32],
    w: usize,
    h: usize,
    thresh: f32,       // Official threshold: 0.3
    box_thresh: f32,   // Official box score threshold: 0.6
    unclip_ratio: f32, // Official text expansion ratio: 1.5
) -> Vec<DetBox> {
    let mut visited = vec![false; w * h];
    let mut boxes = Vec::new();

    for y in 0..h {
        for x in 0..w {
            let idx = y * w + x;
            if prob_map[idx] >= thresh && !visited[idx] {
                // BFS to find connected text components
                let mut queue = vec![(x, y)];
                visited[idx] = true;

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
                            if !visited[n_idx] && prob_map[n_idx] >= thresh {
                                visited[n_idx] = true;
                                queue.push((nx as usize, ny as usize));
                            }
                        }
                    }
                }

                let avg_score = score_sum / count as f32;
                let bw = (max_x - min_x + 1) as f32;
                let bh = (max_y - min_y + 1) as f32;

                // Filter noise: minimum pixel count and average confidence
                if count >= 12 && avg_score >= box_thresh && bw >= 6.0 && bh >= 6.0 {
                    // DBNet Unclip: expands the shrunk text mask back to full size
                    let perimeter = 2.0 * (bw + bh);
                    let area = bw * bh;
                    let distance = (area * unclip_ratio) / perimeter;

                    let x1 = (min_x as f32 - distance).max(0.0);
                    let y1 = (min_y as f32 - distance).max(0.0);
                    let x2 = (max_x as f32 + distance).min((w - 1) as f32);
                    let y2 = (max_y as f32 + distance).min((h - 1) as f32);

                    boxes.push(DetBox {
                        x: x1,
                        y: y1,
                        w: x2 - x1,
                        h: y2 - y1,
                        score: avg_score,
                    });
                }
            }
        }
    }
    boxes
}

#[macroquad::main(window_conf)]
async fn main() {
    let (conn, screen) = x11rb::connect(None).unwrap();
    let root = conn.setup().roots[screen].root;

    let mut session = Session::builder()
        .unwrap()
        .commit_from_memory(MODEL_BYTES)
        .unwrap();
    let input_name = session.inputs()[0].name().to_string();

    let mut img = Image::gen_image_color(SIZE as u16, SIZE as u16, BLACK);
    let tex = Texture2D::from_image(&img);
    let mut input = vec![0.0f32; 3 * PLANE];

    while !is_key_down(KeyCode::Escape) {
        // 1. Capture screen from X11
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

        // 2. Normalize RGB using ImageNet statistics (PaddleOCR det standard)
        let (r_plane, rest) = input.split_at_mut(PLANE);
        let (g_plane, b_plane) = rest.split_at_mut(PLANE);
        for (i, px) in reply.data.chunks_exact(4).take(PLANE).enumerate() {
            let (b, g, r) = (px[0], px[1], px[2]);
            let off = i * 4;
            img.bytes[off..off + 4].copy_from_slice(&[r, g, b, 255]);

            // Mean: [0.485, 0.456, 0.406], Std: [0.229, 0.224, 0.225]
            r_plane[i] = (r as f32 / 255.0 - 0.485) / 0.229;
            g_plane[i] = (g as f32 / 255.0 - 0.456) / 0.224;
            b_plane[i] = (b as f32 / 255.0 - 0.406) / 0.225;
        }

        // 3. Inference
        let t0 = Instant::now();
        let outputs = session
            .run(inputs![
                input_name.as_str() => TensorRef::from_array_view(([1, 3, SIZE, SIZE], &input[..])).unwrap()
            ])
            .unwrap();
        let infer_ms = t0.elapsed().as_secs_f64() * 1000.0;

        // Extract probability map [1, 1, 640, 640]
        let (_, prob_map) = outputs[0].try_extract_tensor::<f32>().unwrap();
        let boxes = postprocess_dbnet(prob_map, SIZE, SIZE, 0.3, 0.6, 1.5);

        // 4. Render
        tex.update(&img);
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        // Draw bounding boxes for detected text
        for b in &boxes {
            draw_rectangle_lines(b.x, b.y, b.w, b.h, 2.0, GREEN);
        }

        // HUD banner
        draw_rectangle(0.0, 0.0, SIZE as f32, 24.0, Color::new(0.0, 0.0, 0.0, 0.7));
        draw_text(
            &format!(
                "PP-OCRv6 Det | {} text lines | Infer: {:.1} ms | FPS: {}",
                boxes.len(),
                infer_ms,
                get_fps()
            ),
            10.0,
            17.0,
            16.0,
            GREEN,
        );

        next_frame().await;
    }
}