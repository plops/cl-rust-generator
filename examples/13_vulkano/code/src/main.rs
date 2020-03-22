#![allow(unused_parens)]
use chrono::Utc;
use std::sync::Arc;
use vulkano::command_buffer::CommandBuffer;
use vulkano::descriptor::descriptor_set::PersistentDescriptorSet;
use vulkano::sync::GpuFuture;
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
        // image
        let image = vulkano::image::StorageImage::new(
            device.clone(),
            vulkano::image::Dimensions::Dim2d {
                width: 1024,
                height: 1024,
            },
            vulkano::format::Format::R8G8B8A8Unorm,
            Some(queue.family()),
        )
        .unwrap();
        let buf = vulkano::buffer::CpuAccessibleBuffer::from_iter(
            device.clone(),
            vulkano::buffer::BufferUsage::all(),
            (0..1024 * 1024 * 4).map(|_| 0u8),
        )
        .expect("failed to create buffer");
        let command_buffer =
            vulkano::command_buffer::AutoCommandBufferBuilder::new(device.clone(), queue.family())
                .unwrap()
                .clear_color_image(
                    image.clone(),
                    vulkano::format::ClearValue::Float([0., 0., 1.0, 1.0]),
                )
                .unwrap()
                .copy_image_to_buffer(image.clone(), buf.clone())
                .unwrap()
                .build()
                .unwrap();
        let finished = command_buffer.execute(queue.clone()).unwrap();
        finished
            .then_signal_fence_and_flush()
            .unwrap()
            .wait(None)
            .unwrap();
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
