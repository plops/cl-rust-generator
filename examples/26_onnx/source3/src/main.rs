use macroquad::prelude::*;
use tract::prelude::*;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

tract::impl_ndarray_interop!();

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

    // Resolve model path (using existing local file, or unpacking embedded bytes if standalone)
    let model_path = if std::path::Path::new("yolo26n.onnx").exists() {
        std::path::PathBuf::from("yolo26n.onnx")
    } else if std::path::Path::new("../yolo26n.onnx").exists() {
        std::path::PathBuf::from("../yolo26n.onnx")
    } else {
        let tmp = std::env::temp_dir().join("yolo26n.onnx");
        if !tmp.exists()
            || std::fs::metadata(&tmp).map(|m| m.len()).unwrap_or(0) != MODEL_BYTES.len() as u64
        {
            std::fs::write(&tmp, MODEL_BYTES).unwrap();
        }
        tmp
    };

    let mut model = tract::onnx()
        .unwrap()
        .load(&model_path)
        .unwrap();
    model
        .set_input_fact(0, format!("1,3,{SIZE},{SIZE},f32").as_str())
        .unwrap();
    let model = model.into_model().unwrap().into_runnable().unwrap();

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

        let input_view = ndarray::ArrayView4::from_shape((1, 3, SIZE, SIZE), &input).unwrap();
        let outputs = model.run([input_view.to_owned().tract().unwrap()]).unwrap();
        let dets = outputs[0].as_slice::<f32>().unwrap();

        tex.update(&img);
        clear_background(BLACK);
        draw_texture(&tex, 0.0, 0.0, WHITE);

        for det in dets.chunks_exact(6) {
            if det[4] >= 0.1 {
                draw_rectangle_lines(det[0], det[1], det[2] - det[0], det[3] - det[1], 2.0, RED);
            }
        }

        next_frame().await;
    }
}