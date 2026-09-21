use macroquad::prelude::*;
use ort::{inputs, session::Session, value::TensorRef};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

const SIZE: usize = 640;
const PLANE: usize = SIZE * SIZE;

// Compiles the ONNX weights directly into the binary's .rodata section
const MODEL_BYTES: &[u8] = include_bytes!("../yolo26n.onnx");

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

    // Load the model directly from embedded memory
    let mut session = Session::builder()
        .unwrap()
        .commit_from_memory(MODEL_BYTES)
        .unwrap();

    let input_name = session.inputs()[0].name().to_string();
    let mut img = Image::gen_image_color(SIZE as u16, SIZE as u16, BLACK);
    let tex = Texture2D::from_image(&img);

    let mut input = vec![0.0f32; 3 * PLANE];

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

        let (r_plane, rest) = input.split_at_mut(PLANE);
        let (g_plane, b_plane) = rest.split_at_mut(PLANE);

        for (i, px) in reply.data.chunks_exact(4).take(PLANE).enumerate() {
            let (b, g, r) = (px[0], px[1], px[2]);
            let off = i * 4;
            img.bytes[off..off + 4].copy_from_slice(&[r, g, b, 255]);
            r_plane[i] = r as f32 / 255.0;
            g_plane[i] = g as f32 / 255.0;
            b_plane[i] = b as f32 / 255.0;
        }

        let outputs = session
            .run(inputs![
                input_name.as_str() => TensorRef::from_array_view(([1, 3, SIZE, SIZE], &input[..])).unwrap()
            ])
            .unwrap();

        let (_, dets) = outputs[0].try_extract_tensor::<f32>().unwrap();

        tex.update(&img);
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        for det in dets.chunks_exact(6) {
            if det[4] >= 0.5 {
                draw_rectangle_lines(det[0], det[1], det[2] - det[0], det[3] - det[1], 2.0, RED);
            }
        }

        next_frame().await;
    }
}