#![allow(unused_parens)]
use chrono::Utc;
use vulkano::command_buffer::CommandBuffer;
fn main() {
    {
        println!("{} {}:{} start ", Utc::now(), file!(), line!());
    }
    let mut event_loops = winit::event_loop::EventLoop::new();
    let extensions = vulkano_win::required_extensions();
    let instance = vulkano::instance::Instance::new(None, &extensions, None)
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
    let data = 12;
    let buffer_src = vulkano::buffer::CpuAccessibleBuffer::from_data(
        device.clone(),
        vulkano::buffer::BufferUsage::all(),
        false,
        data,
    )
    .expect("failed to create buffer");
    let buffer_dst = vulkano::buffer::CpuAccessibleBuffer::from_data(
        device.clone(),
        vulkano::buffer::BufferUsage::all(),
        false,
        data,
    )
    .expect("failed to create buffer");
    let mut content = buffer_src.write().unwrap();
    *content = 2;
    let command_buffer =
        vulkano::command_buffer::AutoCommandBufferBuilder::new(device.clone(), queue.family())
            .unwrap()
            .copy_buffer(buffer_src.clone(), buffer_dst.clone())
            .unwrap()
            .build()
            .unwrap();
    let finished = command_buffer.execute(queue.clone());
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
