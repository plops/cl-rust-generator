use glam::{Mat4, Vec3};
use glow::*;
use glutin::prelude::*;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    // 1. Setup Window & OpenGL Context using Winit + Glutin
    let event_loop = winit::event_loop::EventLoop::new();
    let window_builder = winit::window::WindowBuilder::new()
        .with_title("Minimal Rust EDL")
        .with_inner_size(winit::dpi::LogicalSize::new(1280.0, 720.0));

    // Template for creating the GL context
    let template = glutin::config::ConfigTemplateBuilder::new();
    let display_builder = glutin_winit::DisplayBuilder::new().with_window_builder(Some(window_builder));

    let (window, gl_config) = display_builder.build(&event_loop, template, |configs| {
        configs.reduce(|accum, config| if config.num_samples() > accum.num_samples() { config } else { accum }).unwrap()
    }).unwrap();

    let window = window.unwrap();
    let raw_handle = glutin_winit::finalize_window(&event_loop, &gl_config, &window).unwrap();
    let gl_context = unsafe {
        gl_config.display().create_context(&gl_config, &raw_handle).unwrap()
            .make_current(&raw_handle).unwrap()
    };

    // 2. Initialize Glow (Pure Rust OpenGL Bindings)
    let gl = unsafe { Context::from_loader_function(|s| gl_config.display().get_proc_address(std::ffi::CString::new(s).unwrap().as_c_str())) };

    // 3. Load & Center Point Cloud (XYZ)
    let points = load_xyz_minimal("input.xyz");

    unsafe {
        // Create VAO and VBO
        let vao = gl.create_vertex_array().unwrap();
        gl.bind_vertex_array(Some(vao));
        let vbo = gl.create_buffer().unwrap();
        gl.bind_buffer(ARRAY_BUFFER, Some(vbo));
        gl.buffer_data_u8_slice(ARRAY_BUFFER, bytemuck::cast_slice(&points), STATIC_DRAW);

        gl.enable_vertex_attrib_array(0);
        gl.vertex_attrib_pointer_f32(0, 3, FLOAT, false, 12, 0);

        // 4. Create Depth-Only FBO for Pass 1
        let depth_tex = gl.create_texture().unwrap();
        gl.bind_texture(TEXTURE_2D, Some(depth_tex));
        gl.tex_image_2d(TEXTURE_2D, 0, DEPTH_COMPONENT32F as i32, 1280, 720, 0, DEPTH_COMPONENT, FLOAT, None);

        let fbo = gl.create_framebuffer().unwrap();
        gl.bind_framebuffer(FRAMEBUFFER, Some(fbo));
        gl.framebuffer_texture_2d(FRAMEBUFFER, DEPTH_ATTACHMENT, TEXTURE_2D, Some(depth_tex), 0);
        gl.draw_buffer(NONE); // Optimization: No color buffer needed

        // --- Render Loop ---
        event_loop.run(move |event, _, control_flow| {
            match event {
                winit::event::Event::WindowEvent { event: winit::event::WindowEvent::CloseRequested, .. } => {
                    *control_flow = winit::event_loop::ControlFlow::Exit;
                }
                winit::event::Event::MainEventsCleared => {
                    // Pass 1: Render Points to Depth
                    gl.bind_framebuffer(FRAMEBUFFER, Some(fbo));
                    gl.clear(DEPTH_BUFFER_BIT);
                    gl.enable(DEPTH_TEST);
                    // [Insert Draw Calls for Point Cloud here]

                    // Pass 2: EDL on Fullscreen Quad
                    gl.bind_framebuffer(FRAMEBUFFER, None);
                    gl.clear(COLOR_BUFFER_BIT);
                    gl.disable(DEPTH_TEST);
                    gl.bind_texture(TEXTURE_2D, Some(depth_tex));
                    // [Insert Draw Calls for Post-Process Quad here]

                    gl_context.swap_buffers(&raw_handle).unwrap();
                }
                _ => (),
            }
        });
    }
}

fn load_xyz_minimal(path: &str) -> Vec<f32> {
    let mut data = Vec::new();
    if let Ok(file) = File::open(path) {
        let reader = BufReader::new(file);
        for line in reader.lines().flatten() {
            let pts: Vec<f32> = line.split_whitespace().filter_map(|s| s.parse().ok()).collect();
            if pts.len() >= 3 { data.extend_from_slice(&pts[0..3]); }
        }
    }
    data
}
