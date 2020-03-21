#![allow(unused_parens)]
use chrono::Utc;
use vulkano::command_buffer::CommandBuffer;
use vulkano::descriptor::descriptor_set::PersistentDescriptorSet;
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
  uint idx = gl_GlobalInvocationID.x;
  buf.data[idx] = ((buf.data[idx]) * (12));
}
//"##}
        }
        let shader = cs::Shader::load(device.clone()).expect("failed to create shader");
        let compute_pipeline = std::sync::Arc::new(
            vulkano::pipeline::ComputePipeline::new(
                device.clone(),
                &shader.main_entry_point(),
                &(),
            )
            .expect("failed to create compute pipeline"),
        );
        let set = vulkano::descriptor::descriptor_set::PersistentDescriptorSet::start(
            compute_pipeline.clone(),
        )
        .add_buffer(data_buffer.clone())
        .unwrap()
        .build()
        .unwrap();
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
