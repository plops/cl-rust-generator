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
    let device_ext = vulkano::device::DeviceExtensions {
        khr_swapchain: true,
        ..vulkano::device::DeviceExtensions::none()
    };
    let (device, mut queues) = vulkano::device::Device::new(
        physical,
        physical.supported_features(),
        &device_ext,
        [(queue_family, 0.50)].iter().cloned(),
    )
    .expect("failed to create device");
    let queue = queues.next().unwrap();
    {
        // render to window
        let surface = winit::WindowBuilder::new()
            .build_vk_surface(&event_loops, instance.clone())
            .unwrap();
        let caps = surface
            .capabilities(physical)
            .expect("failed to get surface capabilities");
        let dimensions = caps.current_extent.unwrap_or([1280, 1024]);
        let alpha = caps.supported_composite_alpha.iter().next().unwrap();
        let format = caps.supported_formats[0].0;
        let (swapchain, images) = vulkano::swapchain::Swapchain::new(
            device.clone(),
            surface.clone(),
            caps.min_image_count,
            format,
            dimensions,
            1,
            caps.supported_usage_flags,
            &queue,
            vulkano::swapchain::SurfaceTransform::Identity,
            alpha,
            vulkano::swapchain::PresentMode::Fifo,
            true,
            None,
        )
        .expect("failed to create swapchain");
        let (image_num, acquire_future) =
            vulkano::swapchain::acquire_next_image(swapchain.clone(), None).unwrap();
        {
            println!("{} {}:{} swapchain  dimensions={:?}  alpha={:?}  format={:?}  caps.min_image_count={:?}  caps.supported_usage_flags={:?}  image_num={:?}", Utc::now(), file!(), line!(), dimensions, alpha, format, caps.min_image_count, caps.supported_usage_flags, image_num);
        }
        // render triangle to swapchain
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
                                            format: swapchain.format(),
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
        let buf = vulkano::buffer::CpuAccessibleBuffer::from_iter(
            device.clone(),
            vulkano::buffer::BufferUsage::all(),
            (0..1024 * 1024 * 4).map(|_| 0u8),
        )
        .expect("failed to create buffer");
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
        let dynamic_state = vulkano::command_buffer::DynamicState {
            line_width: None,
            viewports: None,
            scissors: None,
            compare_mask: None,
            write_mask: None,
            reference: None,
        };
        let command_buffer =
            vulkano::command_buffer::AutoCommandBufferBuilder::primary_one_time_submit(
                device.clone(),
                queue.family(),
            )
            .unwrap()
            .begin_render_pass(framebuffer.clone(), false, vec![[0., 0., 1.0, 1.0].into()])
            .unwrap()
            .draw(
                pipeline.clone(),
                &dynamic_state,
                vertex_buffer.clone(),
                (),
                (),
            )
            .unwrap()
            .end_render_pass()
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
        event_loops.run_forever(|event| match event {
            winit::Event::WindowEvent {
                event: winit::WindowEvent::CloseRequested,
                ..
            } => return winit::ControlFlow::Break,
            _ => return winit::ControlFlow::Continue,
        });
        {
            println!("{} {}:{} queue ", Utc::now(), file!(), line!());
        };
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
