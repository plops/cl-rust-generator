#![allow(unused_parens)]
#[macro_use]
extern crate rustacuda;
#[macro_use]
extern crate rustacuda_derive;
extern crate rustacuda_core;
use rustacuda::prelude::*;
use std::error::Error;
use std::ffi::CString;
fn main() -> Result<(), Box<dyn Error>> {
    rustacuda::init(CudaFlags::empty())?;
    let device = Device::get_device(0)?;
    let context = Context::create_and_push(
        ((ContextFlags::MAP_HOST) | (ContextFlags::SCHED_AUTO)),
        device,
    )?;
    let module_data = CString::new(include_str!("add.ptx"));
    let module = Module::load_from_string(&module_data)?;
    let stream = Stream::new(StreamFlags::NON_BLOCKING, None)?;
    let mut x = DeviceBox::new(&10.0f32)?;
    let mut y = DeviceBox::new(&20.0f32)?;
    let mut result = DeviceBox::new(&0.0f32)?;
    return Ok(());
}
