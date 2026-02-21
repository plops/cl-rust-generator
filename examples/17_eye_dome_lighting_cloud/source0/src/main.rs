use glium::{glutin, framebuffer, texture, Surface};
use nalgebra::{Matrix4, Point3, Vector3};
use std::fs::File;
use std::io::{BufRead, BufReader};

// --- SHADERS ---

const VERTEX_SHADER_SRC: &str = r#"
    #version 330 core
    in vec3 position;
    uniform mat4 perspective;
    uniform mat4 view;
    uniform float point_size;
    void main() {
        gl_Position = perspective * view * vec4(position, 1.0);
        gl_PointSize = point_size;
    }
"#;

const FRAGMENT_SHADER_SRC: &str = r#"
    #version 330 core
    void main() {
        // Just writing to depth buffer
    }
"#;

const EDL_POST_VERT: &str = r#"
    #version 330 core
    in vec2 position;
    out vec2 v_tex_coords;
    void main() {
        v_tex_coords = position * 0.5 + 0.5;
        gl_Position = vec4(position, 0.0, 1.0);
    }
"#;

const EDL_POST_FRAG: &str = r#"
    #version 330 core
    in vec2 v_tex_coords;
    out vec4 color;
    uniform sampler2D depth_tex;
    uniform vec2 res;

    float getDepth(vec2 uv) {
        return texture(depth_tex, uv).r;
    }

    void main() {
        float d = getDepth(v_tex_coords);
        if (d >= 1.0) discard; // Don't shade background

        float radius = 1.5;
        float strength = 10.0;

        // Sample 4 neighbors
        float sum = 0.0;
        sum += max(0.0, d - getDepth(v_tex_coords + vec2(radius, 0)/res));
        sum += max(0.0, d - getDepth(v_tex_coords + vec2(-radius, 0)/res));
        sum += max(0.0, d - getDepth(v_tex_coords + vec2(0, radius)/res));
        sum += max(0.0, d - getDepth(v_tex_coords + vec2(0, -radius)/res));

        float shade = exp(-sum * strength);
        // Map depth to a simple gray ramp + EDL shading
        color = vec4(vec3(0.5 + d * 0.5) * shade, 1.0);
    }
"#;

// --- DATA STRUCTURES ---

#[derive(Copy, Clone)]
struct Vertex { position: [f32; 3] }
glium::implement_vertex!(Vertex, position);

#[derive(Copy, Clone)]
struct QuadVertex { position: [f32; 2] }
glium::implement_vertex!(QuadVertex, position);

// --- HELPERS ---

fn load_xyz(path: &str) -> Vec<Vertex> {
    let file = File::open(path).expect("Failed to open file");
    let reader = BufReader::new(file);
    let mut points = Vec::new();
    let mut centroid = Vector3::new(0.0, 0.0, 0.0);

    for line in reader.lines().flatten() {
        let coords: Vec<f32> = line.split_whitespace()
            .filter_map(|s| s.parse().ok()).collect();
        if coords.len() >= 3 {
            points.push(Vertex { position: [coords[0], coords[1], coords[2]] });
            centroid += Vector3::new(coords[0], coords[1], coords[2]);
        }
    }

    // Normalize: Center the cloud to prevent floating point jitter
    centroid /= points.len() as f32;
    for p in &mut points {
        p.position[0] -= centroid.x;
        p.position[1] -= centroid.y;
        p.position[2] -= centroid.z;
    }
    points
}

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new().with_title("Rust EDL Point Cloud");
    let cb = glutin::ContextBuilder::new().with_depth_buffer(24);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();

    // Load Data
    let cloud_data = load_xyz("input.xyz");
    let v_buffer = glium::VertexBuffer::new(&display, &cloud_data).unwrap();
    let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

    // Quad for Post-Processing
    let quad_vbo = glium::VertexBuffer::new(&display, &[
        QuadVertex { position: [-1.0, -1.0] }, QuadVertex { position: [1.0, -1.0] },
        QuadVertex { position: [-1.0,  1.0] }, QuadVertex { position: [1.0,  1.0] },
    ]).unwrap();

    // Shaders
    let point_prog = glium::Program::from_source(&display, VERTEX_SHADER_SRC, FRAGMENT_SHADER_SRC, None).unwrap();
    let edl_prog = glium::Program::from_source(&display, EDL_POST_VERT, EDL_POST_FRAG, None).unwrap();

    event_loop.run(move |ev, _, control_flow| {
        let (width, height) = display.get_framebuffer_dimensions();

        // 1. Create/Resize Depth Texture for FBO
        let depth_tex = texture::DepthTexture2d::empty(&display, width, height).unwrap();
        let mut fbo = framebuffer::SimpleFrameBuffer::depth_only(&display, &depth_tex).unwrap();

        // 2. Camera Setup (Simple view matrix)
        let view = Matrix4::look_at_rh(
            &Point3::new(0.0, 0.0, 5.0),
            &Point3::origin(),
            &Vector3::y()
        );
        let perspective = Matrix4::new_perspective(width as f32 / height as f32, 0.1f32.to_radians() * 4500.0, 0.1, 1000.0);

        // 3. PASS 1: Render Points to FBO
        fbo.clear_depth(1.0);
        let params = glium::DrawParameters {
            depth: glium::Depth { test: glium::DepthTest::IfLess, write: true, ..Default::default() },
            point_size: Some(2.0),
            ..Default::default()
        };

        fbo.draw(&v_buffer, &indices, &point_prog,
                 &glium::uniform! { perspective: Into::<[[f32; 4]; 4]>::into(perspective), view: Into::<[[f32; 4]; 4]>::into(view), point_size: 2.0f32 },
                 &params).unwrap();

        // 4. PASS 2: EDL Post-processing to Screen
        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 0.0, 1.0);

        target.draw(&quad_vbo, glium::index::NoIndices(glium::index::PrimitiveType::TriangleStrip), &edl_prog,
                    &glium::uniform! {
                        depth_tex: &depth_tex,
                        res: [width as f32, height as f32]
                    },
                    &Default::default()).unwrap();

        target.finish().unwrap();
        *control_flow = glutin::event_loop::ControlFlow::Wait;
    });
}