#[allow(unused_parens)]
use chrono::{DateTime, Utc};
extern crate gl;
extern crate glutin;
use glutin::GLContext;
use gl::types::*;
use std::{mem, ptr, str};
use std::os::raw::c_void;
use std::ffi::CString;
fn main (){
            let mut events_loop  = glutin::event_loop::EventLoop::new();
        let window  = glutin::WindowBuilder::new().with_title("glutin thing").with_dimensions(512, 512);
    let context  = glutin::ContextBuilder::new().with_vsync(true);
    let gl_window  = glutin::GLWindow::new(window, context, &events_loop).unwrap();
    unsafe {
                gl_window.make_current().unwrap();
                gl::load_with(| symbol|{
                        (gl_window.get_proc_address(symbol) as *const);
                        return _;
});
};
}