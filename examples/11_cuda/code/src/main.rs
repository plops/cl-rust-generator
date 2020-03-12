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
    let _ctx = Context::create_and_push(
        ((ContextFlags::MAP_HOST) | (ContextFlags::SCHED_AUTO)),
        device,
    )?;
    let ptx = CString::new(include_str!("add.ptx"))?;
    let module = Module::load_from_string(&ptx)?;
    let stream = Stream::new(StreamFlags::NON_BLOCKING, None)?;
    let mut in_x = DeviceBuffer::from_slice(&[1.0e32; 10])?;
    let mut in_y = DeviceBuffer::from_slice(&[2.0e32; 10])?;
    let mut out_1 = DeviceBuffer::from_slice(&[0.0; 10])?;
    let mut out_2 = DeviceBuffer::from_slice(&[0.0; 10])?;
    unsafe {
        let result = launch!(module.sum<<<1,1,0,stream>>>(in_x.as_device_ptr(), in_y.as_device_ptr(), out_1.as_device_ptr(), out_2.as_device_ptr()));
        result?;
    }
    stream.synchronize()?;
    let mut out_host = [0.0f32; 20];
    out_1.copy_to(&mut out_host[0..10])?;
    out_2.copy_to(&mut out_host[10..20])?;
    for x in out_host.iter() {
        assert_eq!((3.0 as u32), (*x as u32));
    }
    return Ok(());
}
