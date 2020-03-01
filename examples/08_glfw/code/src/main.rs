#[allow(unused_parens)]
use glfw::{Action, Context, Key};
fn main (){
            let mut glfw  = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    glfw.window_hint(glfw::WindowHint::ContextVersion(3, 3));
        let (mut window, events)  = glfw.create_window(512, 512, "glfw win", glfw::WindowMode::Windowed).expect("failed to create glfw window");
    window.make_current();
    window.set_key_polling(true);
    gl::load_with(| symbol|{
                return (window.get_proc_address(symbol) as *const);
});
    unsafe {
                gl::Enable(gl::BLEND);
                gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
                gl::Enable(gl::DEPTH_TEST);
                gl::DepthFunc(gl::LESS);
                gl::ClearColor(0.10    , 0.10    , 0.10    , 1.0    );
}
        let mut imgui  = imgui::Context::create();
    let mut imgui_glfw  = imgui_glfw_rs::ImguiGLFW::new(&mut imgui, &mut window);
    while (!(window.should_close())) {
                unsafe {
                        gl::Clear(((gl::COLOR_BUFFER_BIT) | (gl::DEPTH_BUFFER_BIT)));
}
                        let ui  = imgui_glfw.frame(&mut window, &mut imgui);
        ui.show_demo_window(&mut true);
        imgui_glfw.draw(ui, &mut window);
                window.swap_buffers();
                glfw.poll_events();
                for  (_, event) in glfw::flush_messages(&events) {
                        println!("{:?}", event);
                        match event {
                                glfw::WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
                                window.set_should_close(true)
},
                                _ => {
                                {}
},
}
}
};
}