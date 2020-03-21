#![allow(unused_parens)]
use chrono::Utc;
use vulkano::command_buffer::CommandBuffer;
use vulkano::sync::GpuFuture;
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
    {
        let data_iter = 0..65535;
        let data_buffer = vulkano::buffer::CpuAccessibleBuffer::from_iter(
            device.clone(),
            vulkano::buffer::BufferUsage::all(),
            false,
            data_iter,
        )
        .expect("failed to create buffer");
        mod cs {
            vulkano_shaders::shader! {ty: "compute", src: r##"#version 450
layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;
layout(set = 0, binding = 0) buffer Data { uint data[]; }
buf;
void main() {
  uint idx = gl_GlobalInvocationID.x();
  buf.data[idx] = ((buf.data[idx]) * (12));
}
//"##}
        }
        let shader = cs::Shader::load(device.clone()).expect("failed to create shader");
    }
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
    {
        let mut content = buffer_src.write().unwrap();
        *content = 2;
    }
    let command_buffer =
        vulkano::command_buffer::AutoCommandBufferBuilder::new(device.clone(), queue.family())
            .unwrap()
            .copy_buffer(buffer_src.clone(), buffer_dst.clone())
            .unwrap()
            .build()
            .unwrap();
    let finished = command_buffer.execute(queue.clone()).unwrap();
    {
        println!("{} {}:{} copy .. ", Utc::now(), file!(), line!());
    }
    finished
        .then_signal_fence_and_flush()
        .unwrap()
        .wait(None)
        .unwrap();
    {
        let src_content = buffer_src.read().unwrap();
        let dst_content = buffer_dst.read().unwrap();
        {
            println!(
                "{} {}:{} after copy  *src_content={}  *dst_content={}",
                Utc::now(),
                file!(),
                line!(),
                *src_content,
                *dst_content
            );
        };
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
