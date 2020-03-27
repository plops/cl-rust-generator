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
    position: [f32; 3],
}
vulkano::impl_vertex!(Vertex, position);
fn main() {
    {
        println!("{} {}:{} start ", Utc::now(), file!(), line!());
    }
    // https://github.com/vulkano-rs/vulkano-examples/blob/020c546562203f948111508cc7cb67ffd258ab87/src/bin/debug.rs
    let mut event_loops = winit::EventsLoop::new();
    let mut extensions = vulkano_win::required_extensions();
    extensions.ext_debug_report = true;
    let instance = vulkano::instance::Instance::new(
        None,
        &extensions,
        vec!["VK_LAYER_LUNARG_standard_validation"],
    )
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
                    position: [(-1.0), 1.0, 0.],
                },
                Vertex {
                    position: [1.0, 1.0, 0.],
                },
                Vertex {
                    position: [(-1.0), (-1.0), 0.],
                },
                Vertex {
                    position: [1.0, (-1.0), 0.],
                },
            ]
            .into_iter(),
        )
        .expect("failed to create buffer");
        let vertex_buffer2 = vulkano::buffer::CpuAccessibleBuffer::from_iter(
            device.clone(),
            vulkano::buffer::BufferUsage::all(),
            vec! [        Vertex {position: [0.    , 0.    , 0.250    ]};
        Vertex {position: [4.96673320e-2, 0.    , 0.245016650    ]};
        Vertex {position: [9.7354585e-2, 0.    , 0.230265240    ]};
        Vertex {position: [0.141160620    , 0.    , 0.20633390    ]};
        Vertex {position: [0.179339020    , 0.    , 0.174176680    ]};
        Vertex {position: [0.    , 8.1798680e-2, 0.250    ]};
        Vertex {position: [4.6933490e-2, 8.1798680e-2, 0.245016650    ]};
        Vertex {position: [9.1995890e-2, 8.1798680e-2, 0.230265240    ]};
        Vertex {position: [0.133390710    , 8.1798680e-2, 0.20633390    ]};
        Vertex {position: [0.169467660    , 8.1798680e-2, 0.174176680    ]};
        Vertex {position: [0.    , 0.154592450    , 0.250    ]};
        Vertex {position: [3.9032920e-2, 0.154592450    , 0.245016650    ]};
        Vertex {position: [7.6509720e-2, 0.154592450    , 0.230265240    ]};
        Vertex {position: [0.110936330    , 0.154592450    , 0.20633390    ]};
        Vertex {position: [0.140940250    , 0.154592450    , 0.174176680    ]};]
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
layout(location = 0) in vec3 position;
void main() { gl_Position = vec4(position, (1.0)); }"##}
        }
        mod fs {
            vulkano_shaders::shader! {ty: "fragment", src: r##"#version 450
layout(location = 0) out vec4 f_color;
//  https://www.youtube.com/watch?v=Cfe5UQ-1L9Q&t=1365s
layout(push_constant) uniform PushConstantData {
  uint timestamp;
  uint window_w;
  uint window_h;
  uint mouse_x;
  uint mouse_y;
}
pc;
float map(in vec3 pos) {
  float d1 = ((length(pos)) - ((0.250)));
  float d2 = ((pos.y) + ((0.250)));
  return min(d1, d2);
}
vec3 calcNormal(in vec3 pos) {
  vec2 e = vec2((1.00e-4), (0.));
  return normalize(vec3(((map(((pos) + (e.xyy)))) - (map(((pos) - (e.xyy))))),
                        ((map(((pos) + (e.yxy)))) - (map(((pos) - (e.yxy))))),
                        ((map(((pos) + (e.yyx)))) - (map(((pos) - (e.yyx)))))));
}
float castRay(in vec3 ro, vec3 rd) {
  float tau = (0.);
  for (int i = 0; i < 100; (i)++) {
    vec3 pos = ((ro) + (((tau) * (rd))));
    float h = map(pos);
    if (h < (1.00e-3)) {
      break;
    };
    (tau) += (h);
    if ((20.) < tau) {
      break;
    };
  };
  if ((20.) < tau) {
    tau = (-1.0);
  };
  return tau;
}
void main() {
  ivec2 iResolution = ivec2(pc.window_w, pc.window_h);
  vec2 p0 = (((((((2.0)) * (gl_FragCoord.xy))) - (iResolution.xy))) /
             (iResolution.y));
  vec2 p = vec2(p0.x, ((-1) * (p0.y)));
  float an = (((((10.)) * (pc.mouse_x))) / (pc.window_w));
  vec3 ro = vec3(sin(an), (((0.10)) * (sin((((1.00e-2)) * (pc.timestamp))))),
                 cos(an));
  vec3 ta = vec3((0.), (0.), (0.));
  vec3 ww = normalize(((ta) - (ro)));
  vec3 uu = normalize(cross(ww, vec3(0, 1, 0)));
  vec3 vv = normalize(cross(uu, ww));
  vec3 rd =
      normalize(((((p.x) * (uu))) + (((p.y) * (vv))) + ((((1.50)) * (ww)))));
  vec3 col = ((vec3((0.30), (0.50), (0.90))) - ((((0.50)) * (rd.y))));
  float tau = castRay(ro, rd);
  col = mix(col, vec3((0.70), (0.750), (0.80)), exp((((-10.)) * (rd.y))));
  if (0 < tau) {
    vec3 pos = ((ro) + (((tau) * (rd))));
    vec3 nor = calcNormal(pos);
    vec3 mate = vec3((0.20), (0.20), (0.20));
    vec3 sun_dir = normalize(vec3((0.80), (0.40), (0.20)));
    float sun_dif = clamp(dot(nor, sun_dir), (0.), (1.0));
    float sun_sha =
        step(castRay(((pos) + ((((1.00e-3)) * (nor)))), sun_dir), (0.));
    float sky_dif =
        clamp((((0.50)) + (dot(nor, vec3((0.), (1.0), (0.))))), (0.), (1.0));
    float bou_dif =
        clamp((((0.50)) + (dot(nor, vec3((0.), (-1.0), (0.))))), (0.), (1.0));
    col = ((mate) * (vec3((7.0), 5, 3)) * (sun_dif) * (sun_sha));
    (col) += (((mate) * (vec3((0.50), (0.80), (0.90))) * (sky_dif)));
    (col) += (((mate) * (vec3((0.70), (0.30), (0.20))) * (bou_dif)));
  };
  col = pow(col, vec3((0.45450)));
  f_color = vec4(col, (0.40));
}"##}
        }
        mod fs2 {
            vulkano_shaders::shader! {ty: "fragment", src: r##"#version 450
layout(location = 0) out vec4 f_color;
layout(push_constant) uniform PushConstantData {
  uint timestamp;
  uint window_w;
  uint window_h;
  uint mouse_x;
  uint mouse_y;
}
pc;
void main() { f_color = vec4((1.0), (0.20), (0.20), (0.70)); }"##}
        }
        let vs = vs::Shader::load(device.clone()).expect("failed to create shader");
        let fs = fs::Shader::load(device.clone()).expect("failed to create shader");
        let fs2 = fs::Shader::load(device.clone()).expect("failed to create shader");
        let pipeline = std::sync::Arc::new(
            vulkano::pipeline::GraphicsPipeline::start()
                .vertex_input_single_buffer::<Vertex>()
                .vertex_shader(vs.main_entry_point(), ())
                .triangle_strip()
                .viewports_dynamic_scissors_irrelevant(1)
                .fragment_shader(fs.main_entry_point(), ())
                .blend_alpha_blending()
                .render_pass(vulkano::framebuffer::Subpass::from(render_pass.clone(), 0).unwrap())
                .build(device.clone())
                .unwrap(),
        );
        let pipeline2 = std::sync::Arc::new(
            vulkano::pipeline::GraphicsPipeline::start()
                .vertex_input_single_buffer::<Vertex>()
                .vertex_shader(vs.main_entry_point(), ())
                .line_strip()
                .viewports_dynamic_scissors_irrelevant(1)
                .fragment_shader(fs2.main_entry_point(), ())
                .blend_alpha_blending()
                .render_pass(vulkano::framebuffer::Subpass::from(render_pass.clone(), 0).unwrap())
                .build(device.clone())
                .unwrap(),
        );
        let mut push_constants = fs::ty::PushConstantData {
            timestamp: 0,
            window_w: dimensions[0],
            window_h: dimensions[1],
            mouse_x: 0,
            mouse_y: 0,
        };
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
            push_constants.timestamp += 1;
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
                vulkano::command_buffer::AutoCommandBufferBuilder::primary_simultaneous_use(
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
                    push_constants.clone(),
                )
                .unwrap()
                .draw(
                    pipeline2.clone(),
                    &dynamic_state,
                    vertex_buffer2.clone(),
                    (),
                    push_constants.clone(),
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
                winit::Event::WindowEvent {
                    event:
                        winit::WindowEvent::CursorMoved {
                            device_id,
                            position,
                            ..
                        },
                    ..
                } => {
                    push_constants.mouse_x = (position.x as u32);
                    push_constants.mouse_y = (position.y as u32);
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
