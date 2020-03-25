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
    let _callback =
        vulkano::instance::debug::DebugCallback::errors_and_warnings(&instance, |msg| {
            println!(
                "{} {}:{} debug:  msg.description={:?}",
                Utc::now(),
                file!(),
                line!(),
                msg.description
            );
        })
        .ok();
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
        //  https://github.com/vulkano-rs/vulkano-examples/blob/master/src/bin/triangle.rs
        // vulkano 0.13 example:
        // https://github.com/vulkano-rs/vulkano-examples/blob/1cf9c37073a79a3a0cee60e83c8db8d967218e3e/src/bin/triangle.rs
        let surface = winit::WindowBuilder::new()
            .build_vk_surface(&event_loops, instance.clone())
            .unwrap();
        let window = surface.window();
        let caps = surface
            .capabilities(physical)
            .expect("failed to get surface capabilities");
        let dimensions = (|| {
            let window_size = window.get_inner_size();
            match window_size {
                Some(dimensions) => {
                    let ds: (u32, u32) = dimensions.to_physical(window.get_hidpi_factor()).into();
                    {
                        println!(
                            "{} {}:{} window size  ds={:?}",
                            Utc::now(),
                            file!(),
                            line!(),
                            ds
                        );
                    }
                    return [ds.0, ds.1];
                }
                _ => {
                    {
                        println!("{} {}:{} window size fail ", Utc::now(), file!(), line!());
                    }
                    panic!("win size")
                }
            };
        })();
        let alpha = caps.supported_composite_alpha.iter().next().unwrap();
        let format = caps.supported_formats[0].0;
        let (mut swapchain, images) = vulkano::swapchain::Swapchain::new(
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
            vulkano::swapchain::PresentMode::Immediate,
            true,
            None,
        )
        .expect("failed to create swapchain");
        {
            println!("{} {}:{} swapchain  dimensions={:?}  alpha={:?}  format={:?}  caps.min_image_count={:?}  caps.supported_usage_flags={:?}", Utc::now(), file!(), line!(), dimensions, alpha, format, caps.min_image_count, caps.supported_usage_flags);
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
        let mut dynamic_state = vulkano::command_buffer::DynamicState {
            viewports: Some(vec![vulkano::pipeline::viewport::Viewport {
                origin: [0., 0.],
                dimensions: [(dimensions[0] as f32), (dimensions[1] as f32)],
                depth_range: (0. ..1.0),
            }]),
            ..vulkano::command_buffer::DynamicState::none()
        };
        let mut framebuffers = images
            .iter()
            .map(|image| {
                return (std::sync::Arc::new(
                    vulkano::framebuffer::Framebuffer::start(render_pass.clone())
                        .add(image.clone())
                        .unwrap()
                        .build()
                        .unwrap(),
                )
                    as Arc<dyn vulkano::framebuffer::FramebufferAbstract + Send + Sync>);
            })
            .collect::<Vec<_>>();
        let mut recreate_swapchain = false;
        let mut previous_frame_end = Some(
            (Box::new(vulkano::sync::now(device.clone())) as Box<dyn vulkano::sync::GpuFuture>),
        );
        event_loops.run_forever(|event| {
            previous_frame_end.as_mut().unwrap().cleanup_finished();
            if recreate_swapchain {
                let dimensions = (|| {
                    let window_size = window.get_inner_size();
                    match window_size {
                        Some(dimensions) => {
                            let ds: (u32, u32) =
                                dimensions.to_physical(window.get_hidpi_factor()).into();
                            {
                                println!(
                                    "{} {}:{} window size  ds={:?}",
                                    Utc::now(),
                                    file!(),
                                    line!(),
                                    ds
                                );
                            }
                            return [ds.0, ds.1];
                        }
                        _ => {
                            {
                                println!(
                                    "{} {}:{} window size fail ",
                                    Utc::now(),
                                    file!(),
                                    line!()
                                );
                            }
                            panic!("win size")
                        }
                    };
                })();
                let (new_swapchain, new_images) =
                    match swapchain.recreate_with_dimension(dimensions) {
                        Ok(r) => r,
                        Err(vulkano::swapchain::SwapchainCreationError::UnsupportedDimensions) => {
                            return winit::ControlFlow::Continue
                        }
                        Err(e) => panic!("{:?}", e),
                    };
                {
                    println!(
                        "{} {}:{} swapchain needs recreation  dimensions={:?}",
                        Utc::now(),
                        file!(),
                        line!(),
                        dimensions
                    );
                }
                swapchain = new_swapchain;
                dynamic_state = vulkano::command_buffer::DynamicState {
                    viewports: Some(vec![vulkano::pipeline::viewport::Viewport {
                        origin: [0., 0.],
                        dimensions: [(dimensions[0] as f32), (dimensions[1] as f32)],
                        depth_range: (0. ..1.0),
                    }]),
                    ..vulkano::command_buffer::DynamicState::none()
                };
                framebuffers = new_images
                    .iter()
                    .map(|image| {
                        return (std::sync::Arc::new(
                            vulkano::framebuffer::Framebuffer::start(render_pass.clone())
                                .add(image.clone())
                                .unwrap()
                                .build()
                                .unwrap(),
                        )
                            as Arc<dyn vulkano::framebuffer::FramebufferAbstract + Send + Sync>);
                    })
                    .collect::<Vec<_>>();
                recreate_swapchain = false;
            };
            {
                println!("{} {}:{} next image ", Utc::now(), file!(), line!());
            }
            let (image_num, acquire_future) =
                match vulkano::swapchain::acquire_next_image(swapchain.clone(), None) {
                    Ok(r) => r,
                    Err(vulkano::swapchain::AcquireError::OutOfDate) => {
                        {
                            println!("{} {}:{} acquire error ", Utc::now(), file!(), line!());
                        }
                        recreate_swapchain = true;
                        return winit::ControlFlow::Continue;
                    }
                    Err(e) => {
                        {
                            println!(
                                "{} {}:{} failed to acquire next image  e={:?}",
                                Utc::now(),
                                file!(),
                                line!(),
                                e
                            );
                        }
                        panic!("fail")
                    }
                };
            let command_buffer =
                vulkano::command_buffer::AutoCommandBufferBuilder::primary_one_time_submit(
                    device.clone(),
                    queue.family(),
                )
                .unwrap()
                .begin_render_pass(
                    framebuffers[image_num].clone(),
                    false,
                    vec![[0., 0., 1.0, 1.0].into()],
                )
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
                .build()
                .unwrap();
            let future = previous_frame_end
                .take()
                .unwrap()
                .join(acquire_future)
                .then_execute(queue.clone(), command_buffer)
                .unwrap()
                .then_swapchain_present(queue.clone(), swapchain.clone(), image_num)
                .then_signal_fence_and_flush();
            match future {
                Ok(future) => {
                    previous_frame_end = Some((Box::new(future) as Box<_>));
                }
                Err(vulkano::sync::FlushError::OutOfDate) => {
                    {
                        println!("{} {}:{} out of date ", Utc::now(), file!(), line!());
                    }
                    recreate_swapchain = true;
                    previous_frame_end =
                        Some((Box::new(vulkano::sync::now(device.clone())) as Box<_>));
                }
                Err(e) => {
                    {
                        println!(
                            "{} {}:{} failed to flush future  e={:?}",
                            Utc::now(),
                            file!(),
                            line!(),
                            e
                        );
                    }
                    previous_frame_end =
                        Some((Box::new(vulkano::sync::now(device.clone())) as Box<_>));
                }
            };
            match event {
                winit::Event::WindowEvent {
                    event: winit::WindowEvent::CloseRequested,
                    ..
                } => return winit::ControlFlow::Break,
                winit::Event::WindowEvent {
                    event: winit::WindowEvent::Resized(_),
                    ..
                } => {
                    recreate_swapchain = true;
                    return winit::ControlFlow::Continue;
                }
                _ => return winit::ControlFlow::Continue,
            }
        });
        {
            println!("{} {}:{} queue ", Utc::now(), file!(), line!());
        };
    };
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!());
    }
}
