extern crate glium;
fn main() {
    #[allow(unused_imports)]
    use glium::{glutin, Surface};
    let mut event_loop = glium::glutin::event_loop::EventLoop::new();
    let wb = glium::glutin::window::WindowBuilder::new()
        .with_inner_size(glium::glutin::dpi::LogicalSize::new(512, 512))
        .with_title("vis");
    let cb = glium::glutin::ContextBuilder::new();
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();
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
