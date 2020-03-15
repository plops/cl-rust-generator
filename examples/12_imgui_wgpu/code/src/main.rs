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
    let (device, queue) = adapter.request_device(
        &(wgpu::DeviceDescriptor {
            extensions: wgpu::Extensions {
                anisotropic_filtering: false,
            },
            limits: wgpu::Limits::default(),
        }),
    );
}
