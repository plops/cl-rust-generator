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
        let framebuffer = std::sync::Arc::new(
            vulkano::framebuffer::Framebuffer::start(render_pass.clone())
                .add(image.clone())
                .unwrap()
                .build()
                .unwrap(),
        );
        mod vs {
            vulkano_shaders::shader! {ty: "vertex", src: r##"#version 450
layout(location = 0) in vec2 position;
void main() { gl_Position = vec4(position, (0.), (1.0)); }"##}
        }
        mod fs {
            vulkano_shaders::shader! {ty: "fragment", src: r##"#version 450
layout(location = 0) out vec4 f_color;
void main() { f_color = vec4((1.0), (0.), (0.), (1.0)); }"##}
        }
        let vs = vs::Shader::load(device.clone()).expect("failed to create shader");
        let fs = fs::Shader::load(device.clone()).expect("failed to create shader");
        let pipeline = std::sync::Arc::new(
            vulkano::pipeline::GraphicsPipeline::start()
                .vertex_input_single_buffer::<Vertex>()
                .vertex_shader(vs.main_entry_point(), ())
                .viewports_dynamic_scissors_irrelevant(1)
                .fragment_shader(fs.main_entry_point(), ())
                .render_pass(vulkano::framebuffer::Subpass::from(render_pass.clone(), 0).unwrap())
                .build(device.clone())
                .unwrap(),
        );
        vulkano::command_buffer::AutoCommandBufferBuilder::new(device.clone(), queue.family())
            .unwrap()
            .begin_render_pass(framebuffer.clone(), false, vec![[0., 0., 1.0, 1.0].into()])
            .unwrap()
            .end_render_pass()
            .unwrap();
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
