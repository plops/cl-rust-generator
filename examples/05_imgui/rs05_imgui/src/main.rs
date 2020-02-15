use glium::glutin;
use glium::glutin::event::{Event, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::window::WindowBuilder;
use glium::{Display, Surface};
use imgui::*;
use imgui::{Context, FontConfig, FontGlyphRanges, FontSource, Ui};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use std::time::Instant;
struct System {
    event_loop: EventLoop<()>,
    display: glium::Display,
    imgui: Context,
    platform: WinitPlatform,
    renderer: Renderer,
    font_size: f32,
}
fn init(title: &str) -> System {
    let title = match (title.rfind("/")) {
        Some(idx) => title.split_at(((idx) + (1))).1,
        None => title,
    };
    let event_loop = EventLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let builder = WindowBuilder::new()
        .with_title(title.to_owned())
        .with_inner_size(glutin::dpi::LogicalSize::new(512f64, 512f64));
    let display =
        Display::new(builder, context, &event_loop).expect("failed to initialize display");
    let mut imgui = Context::create();
    imgui.set_ini_filename(None);
    let platform = WinitPlatform::init(&mut imgui);
    {
        let gl_window = display.gl_window();
        let window = gl_window.window();
        platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Rounded);
    };
    let renderer = Renderer::init(&mut imgui, &display).expect("failed to initialize renderer");
    return System {
        event_loop,
        display,
        imgui,
        platform,
        renderer,
        font_size: (12.),
    };
}
impl System {
    fn main_loop<F: FnMut(&mut bool, &mut Ui) + 'static>(self, mut run_ui: F) {
        let System {
            event_loop,
            display,
            mut imgui,
            mut platform,
            mut renderer,
            ..
        } = self;
        let mut last_frame = Instant::now();
        event_loop.run(move |event, _, control_flow| match (event) {
            Event::NewEvents(_) => {
                last_frame = imgui.io_mut().update_delta_time(last_frame);
            }
            Event::MainEventsCleared => {
                let gl_window = display.gl_window();
                platform
                    .prepare_frame(imgui.io_mut(), &(gl_window.window()))
                    .expect("failed to prepare frame");
                gl_window.window().request_redraw();
            }
            Event::RedrawRequested(_) => {
                let ui = imgui.frame();
                let run = true;
                run_ui(&mut run, &mut ui);
                if (!(run)) {
                    *control_flow = ControlFlow::Exit;
                };
                let gl_window = display.gl_window();
                let target = display.draw();
                target.clear_color_srgb((1.0), (1.0), (1.0), (1.0));
                platform.prepare_render(&ui, gl_window.window());
                let draw_data = ui.render();
                renderer
                    .render(&mut target, draw_data)
                    .expect("rendering failed");
                target.finish().expect("swap buffer failed");
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
            }
            event => {
                let gl_window = display.gl_window();
                platform.handle_event(imgui.io_mut(), gl_window.window(), &event);
            }
        });
    }
}
fn main() {
    let system = init(file!());
    system.main_loop(move |_, ui| {
        Window::new(im_str!("Hello world"))
            .size([(3.00e+2), (1.00e+2)], Condition::FirstUseEver)
            .build(ui, || {
                ui.text(im_str!("Hello World"));
                let mouse_pos = ui.io().mouse_pos;
                ui.text(format!("mouse: ({:.1},{:.1})", mouse_pos[0], mouse_pos[1]));
            });
    });
}
