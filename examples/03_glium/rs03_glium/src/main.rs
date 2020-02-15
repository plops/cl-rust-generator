extern crate glium;
fn main() {
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
        let mut target = display.draw();
	target
        target.clear_color((0.0), (0.0), (1.0), (1.0));
        target.finish().unwrap();
    });
}
