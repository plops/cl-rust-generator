#![allow(unused_parens)]
use chrono::Utc;
use std::sync::Arc;
use vulkano::command_buffer::CommandBuffer;
use vulkano::descriptor::descriptor_set::PersistentDescriptorSet;
use vulkano::sync::GpuFuture;
#[derive(Default, Copy, Clone)]
struct Vertex {
    position: [f32; 2],
}
vulkano::impl_vertex!(Vertex, position);
fn main() {
    {
        println!("{} {}:{} start ", Utc::now(), file!(), line!());
    }
    let mut event_loops = winit::event_loop::EventLoop::new();
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
        // render example
        let vertex_buffer = vulkano::buffer::CpuAccessibleBuffer::from_iter(
            device.clone(),
            vulkano::buffer::BufferUsage::all(),
            vec![
                Vertex {
                    position: [(-0.50), (-0.50)],
                },
                Vertex {
                    position: [0., 0.50],
                },
                Vertex {
                    position: [0.50, (-0.250)],
                },
            ]
            .into_iter(),
        )
        .expect("failed to create buffer");
        let render_pass = std::sync::Arc::new(
            vulkano::single_pass_renderpass!(device.clone(), attachments: {
                                    color: {
                                            load: Clear,
                                            store: Store,
                                            format: vulkano::format::Format::R8G8B8A8Unorm,
                                            samples: 1,
            }
            }, pass: {
                                    color: [color],
                                    depth_stencil: {}
            })
            .unwrap(),
        );
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
