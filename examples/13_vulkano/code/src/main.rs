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
        // store mandelbrot image
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
        mod cs {
            vulkano_shaders::shader! {ty: "compute", src: r##"#version 450
layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;
layout(set = 0, binding = 0, rgba8) uniform writeonly image2D img;
void main() {
  vec2 norm_coordinates = ((((vec2((0.50))) + (gl_GlobalInvocationID.xy))) /
                           (vec2(imageSize(img))));
  vec2 c = (((((2.0)) * (((norm_coordinates) - (vec2((0.50))))))) -
            (vec2((1.0), (0.))));
  vec2 z = vec2((0.), (0.));
  float i = (0.);
  for (i = (0.); i < (1.0); (i) += ((5.00e-3))) {
    z = vec2(((((z.x) * (z.x))) + (((-1) * (z.y) * (z.y))) + (c.x)),
             ((((z.x) * (z.y))) + (((z.x) * (z.y))) + (c.y)));
    if ((4.0) < length(z)) {
      break;
    };
  }
  vec4 to_write = vec4(vec3(i), (1.0));
  imageStore(img, ivec2(gl_GlobalInvocationID.xy), to_write);
}"##}
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
        let set = std::sync::Arc::new(
            vulkano::descriptor::descriptor_set::PersistentDescriptorSet::start(
                compute_pipeline.clone(),
                0,
            )
            .add_image(image.clone())
            .unwrap()
            .build()
            .unwrap(),
        );
        let buf = vulkano::buffer::CpuAccessibleBuffer::from_iter(
            device.clone(),
            vulkano::buffer::BufferUsage::all(),
            (0..((1024) * (1024) * (4))).map(|_| 0u8),
        )
        .expect("failed to create buffer");
        let command_buffer =
            vulkano::command_buffer::AutoCommandBufferBuilder::new(device.clone(), queue.family())
                .unwrap()
                .dispatch(
                    [((1024) / (8)), ((1024) / (8)), 1],
                    compute_pipeline.clone(),
                    set.clone(),
                    (),
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
        {
            // save image
            let buffer_content = buf.read().unwrap();
            let image =
                image::ImageBuffer::<image::Rgba<u8>, _>::from_raw(1024, 1024, &buffer_content[..])
                    .unwrap();
            image.save("image.png").unwrap();
        };
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
