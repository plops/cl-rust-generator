#![allow(unused_parens)]
use chrono::Utc;
use std::sync::Arc;
use vulkano::command_buffer::CommandBuffer;
use vulkano::descriptor::descriptor_set::PersistentDescriptorSet;
use vulkano::sync::GpuFuture;
use vulkano_win::VkSurfaceBuild;
use winit::EventsLoop;
use winit::WindowBuilder;
#[derive(Default, Copy, Clone)]
struct Vertex {
    position: [f32; 2],
}
vulkano::impl_vertex!(Vertex, position);
fn main() {
    {
        println!("{} {}:{} start ", Utc::now(), file!(), line!());
    }
    let mut event_loops = winit::EventsLoop::new();
    let instance = vulkano::instance::Instance::new(
        None,
        &vulkano::instance::InstanceExtensions::none(),
        None,
    )
    .expect("failed to create Vulkan instance");
    let physical = vulkano::instance::PhysicalDevice::enumerate(&instance)
        .next()
        .expect("no device available");
    let queue_family = physical
        .queue_families()
        .find(|&q| {
            return q.supports_graphics();
        })
        .expect("couldnt find graphical queue family");
    let (device, mut queues) = vulkano::device::Device::new(
        physical,
        &(vulkano::device::Features::none()),
        &(vulkano::device::DeviceExtensions::none()),
        [(queue_family, 0.50)].iter().cloned(),
    )
    .expect("failed to create device");
    let queue = queues.next().unwrap();
    {
        // render to window
        let surface = winit::WindowBuilder::new()
            .build_vk_surface(&event_loops, instance.clone())
            .unwrap();
        {
            println!("{} {}:{} queue ", Utc::now(), file!(), line!());
        };
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
