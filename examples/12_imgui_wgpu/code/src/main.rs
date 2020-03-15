#![allow(unused_parens)]
use wgpu;
fn main() {
    let adapter = wgpu::Adapter::request(
        &(wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::Default,
            backends: wgpu::BackendBit::PRIMARY,
        }),
    )
    .unwrap();
}
