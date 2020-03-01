#[allow(unused_parens)]
extern crate glfw;
use glfw::{Action, Context, Key};
fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    let (mut window, events) = glfw
        .create_window(512, 512, "glfw win", glfw::WindowMode::Windowed)
        .expect("failed to create glfw window");
    window.make_current();
    window.set_key_polling(true);
    while (!(window.should_close())) {
        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            println!("{:?}", event);
            match event {
                glfw::WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
                    window.set_should_close(true)
                }
                _ => {}
            }
        }
    }
}
