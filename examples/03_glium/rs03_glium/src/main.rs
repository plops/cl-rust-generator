#[macro_use]
extern crate glium;
fn main() {
    #[allow(unused_imports)]
    use glium::{glutin, Surface};
    let mut event_loop = glium::glutin::event_loop::EventLoop::new();
    let mut wb = glium::glutin::window::WindowBuilder::new()
        .with_inner_size(glium::glutin::dpi::LogicalSize::new(512, 512))
        .with_title("vis");
    let mut cb = glium::glutin::ContextBuilder::new();
    let mut display = glium::Display::new(wb, cb, &event_loop).unwrap();
    #[derive(Copy, Clone)]
    struct Vertex {
        position: [f32; 2],
    };
    implement_vertex!(Vertex, position);
    let mut shape = vec![
        Vertex {
            position: [(-0.50), (-0.50)],
        },
        Vertex {
            position: [(0.0), (0.50)],
        },
        Vertex {
            position: [(0.50), (-0.250)],
        },
    ];
    let mut vertex_buffer = glium::VertexBuffer::new(&display, &shape).unwrap();
    let mut indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);
    event_loop.run(move |event, _, control_flow| {
        let mut next_frame_time =
            ((std::time::Instant::now()) + (std::time::Duration::from_nanos(16_666_667)));
        *control_flow = glium::glutin::event_loop::ControlFlow::WaitUntil(next_frame_time);
        match (event) {
            glutin::event::Event::WindowEvent { event, .. } => match (event) {
                glutin::event::WindowEvent::CloseRequested => {
                    *control_flow = glutin::event_loop::ControlFlow::Exit;
                    return;
                }
                _ => return,
            },
            glutin::event::Event::NewEvents(cause) => match (cause) {
                glutin::event::StartCause::ResumeTimeReached { .. } => (),
                glutin::event::StartCause::Init => (),
                _ => return,
            },
            _ => return,
        };
        let mut target = display.draw();
        target.clear_color((0.0), (0.0), (1.0), (1.0));
        target.finish().unwrap();
    });
}
