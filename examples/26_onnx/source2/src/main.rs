use macroquad::prelude::*;
use ndarray::Array4;
use ort::{inputs, session::Session, value::TensorRef};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

const SIZE: u16 = 640;
const MODEL_URL: &str =
    "https://github.com/ultralytics/assets/releases/download/v8.4.0/yolo26n.onnx";

fn window_conf() -> Conf {
    Conf {
        window_title: "yolo26n".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
        ..Default::default()
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    let (conn, screen) = x11rb::connect(None).unwrap();
    let root = conn.setup().roots[screen].root;

    // Load local file if present, otherwise download YOLO26n ONNX
    let mut session = if std::path::Path::new("yolo26n.onnx").is_file() {
        Session::builder().unwrap().commit_from_file("yolo26n.onnx")
    } else {
        Session::builder().unwrap().commit_from_url(MODEL_URL)
    }
    .unwrap();

    let input_name = session.inputs().first().map_or("images", |i| i.name()).to_string();
    let mut img = Image::gen_image_color(SIZE, SIZE, BLACK);
    let tex = Texture2D::from_image(&img);
    let mut input = Array4::<f32>::zeros((1, 3, SIZE as usize, SIZE as usize));

    while !is_key_down(KeyCode::Escape) {
        let reply = xproto::get_image(&conn, ImageFormat::Z_PIXMAP, root, 0, 0, SIZE, SIZE, u32::MAX)
            .unwrap()
            .reply()
            .unwrap();

        // Convert X11 BGRX -> RGBA (for Macroquad) and planar normalized float32 (for YOLO)
        let (r_plane, rest) = input.as_slice_mut().unwrap().split_at_mut((SIZE * SIZE) as usize);
        let (g_plane, b_plane) = rest.split_at_mut((SIZE * SIZE) as usize);

        for (i, px) in reply.data.chunks_exact(4).take((SIZE * SIZE) as usize).enumerate() {
            let (b, g, r) = (px[0], px[1], px[2]);
            let off = i * 4;
            img.bytes[off..off + 4].copy_from_slice(&[r, g, b, 255]);
            r_plane[i] = r as f32 / 255.0;
            g_plane[i] = g as f32 / 255.0;
            b_plane[i] = b as f32 / 255.0;
        }

        // YOLO26n end-to-end output shape: [1, 300, 6] -> [x1, y1, x2, y2, conf, class_id]
        let outputs = session
            .run(inputs![input_name.as_str() => TensorRef::from_array_view(&input).unwrap()])
            .unwrap();
        let output = outputs[0].try_extract_array::<f32>().unwrap();

        tex.update(&img);
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        // Draw detection rectangles directly on the screen
        for i in 0..output.shape()[1] {
            let conf = output[[0, i, 4]];
            if conf >= 0.5 {
                let (x1, y1, x2, y2) = (
                    output[[0, i, 0]],
                    output[[0, i, 1]],
                    output[[0, i, 2]],
                    output[[0, i, 3]],
                );
                draw_rectangle_lines(x1, y1, x2 - x1, y2 - y1, 2.0, RED);
            }
        }

        next_frame().await;
    }
}