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
            vec![
                Vertex {
                    position: [0., 0., 0.250],
                },
                Vertex {
                    position: [5.98289182571e-2, 0., 0.242735453818],
                },
                Vertex {
                    position: [0.1161807969966, 0., 0.221364004321],
                },
                Vertex {
                    position: [0.1657806652341, 0., 0.1871276864458],
                },
                Vertex {
                    position: [0.2057459715875, 0., 0.1420161792738],
                },
                Vertex {
                    position: [0.2337540625868, 0., 8.8651216710e-2],
                },
                Vertex {
                    position: [0.248177218742, 0., 3.013416827644e-2],
                },
                Vertex {
                    position: [0.248177215920, 0., (-3.013419151247e-2)],
                },
                Vertex {
                    position: [0.2337540542866, 0., (-8.86512385957e-2)],
                },
                Vertex {
                    position: [0.205745958291, 0., (-0.1420161985371)],
                },
                Vertex {
                    position: [0.1657806564740, 0., (-0.1871276942065)],
                },
                Vertex {
                    position: [0.1161807866338, 0., (-0.221364009760)],
                },
                Vertex {
                    position: [5.98289125755e-2, 0., (-0.242735455218)],
                },
                Vertex {
                    position: [0., 0., 0.250],
                },
                Vertex {
                    position: [3.73027185126e-2, 4.67761333522e-2, 0.242735453818],
                },
                Vertex {
                    position: [7.24375384544e-2, 9.08338076568e-2, 0.221364004321],
                },
                Vertex {
                    position: [0.1033625489180, 0.1296125474120, 0.1871276864458],
                },
                Vertex {
                    position: [0.1282805086037, 0.1608586831254, 0.1420161792738],
                },
                Vertex {
                    position: [0.1457432668327, 0.1827562911332, 8.8651216710e-2],
                },
                Vertex {
                    position: [0.1547359571533, 0.1940327690524, 3.013416827644e-2],
                },
                Vertex {
                    position: [0.1547359553942, 0.1940327668466, (-3.013419151247e-2)],
                },
                Vertex {
                    position: [0.1457432616576, 0.1827562846439, (-8.86512385957e-2)],
                },
                Vertex {
                    position: [0.1282805003135, 0.160858672730, (-0.1420161985371)],
                },
                Vertex {
                    position: [0.1033625434562, 0.1296125405631, (-0.1871276942065)],
                },
                Vertex {
                    position: [7.24375319933e-2, 9.08337995548e-2, (-0.221364009760)],
                },
                Vertex {
                    position: [3.7302714970e-2, 4.6776128910e-2, (-0.242735455218)],
                },
                Vertex {
                    position: [-0., 0., 0.250],
                },
                Vertex {
                    position: [(-1.331319144915e-2), 5.83288812961e-2, 0.242735453818],
                },
                Vertex {
                    position: [(-2.585266854540e-2), 0.1132678997768, 0.221364004321],
                },
                Vertex {
                    position: [(-3.6889681430e-2), 0.1616241948763, 0.1871276864458],
                },
                Vertex {
                    position: [(-4.57828018528e-2), 0.2005874868454, 0.1420161792738],
                },
                Vertex {
                    position: [(-5.20151906117e-2), 0.2278933560274, 8.8651216710e-2],
                },
                Vertex {
                    position: [(-5.52246459183e-2), 0.2419548932869, 3.013416827644e-2],
                },
                Vertex {
                    position: [(-5.5224645290e-2), 0.2419548905363, (-3.013419151247e-2)],
                },
                Vertex {
                    position: [(-5.20151887647e-2), 0.2278933479354, (-8.86512385957e-2)],
                },
                Vertex {
                    position: [(-4.57827988941e-2), 0.2005874738823, (-0.1420161985371)],
                },
                Vertex {
                    position: [(-3.68896794811e-2), 0.1616241863358, (-0.1871276942065)],
                },
                Vertex {
                    position: [(-2.585266623946e-2), 0.1132678896739, (-0.221364009760)],
                },
                Vertex {
                    position: [(-1.33131901849e-2), 5.83288757569e-2, (-0.242735455218)],
                },
                Vertex {
                    position: [-0., 0., 0.250],
                },
                Vertex {
                    position: [(-5.39039934444e-2), 2.59587933188e-2, 0.242735453818],
                },
                Vertex {
                    position: [(-0.104675282491), 5.04089558144e-2, 0.221364004321],
                },
                Vertex {
                    position: [(-0.14936322020), 7.1929530910e-2, 0.1871276864458],
                },
                Vertex {
                    position: [(-0.1853707174846), 8.92698265020e-2, 0.1420161792738],
                },
                Vertex {
                    position: [(-0.210605135849), 0.1014220810753, 8.8651216710e-2],
                },
                Vertex {
                    position: [(-0.223599950689), 0.107680053650, 3.013416827644e-2],
                },
                Vertex {
                    position: [(-0.2235999481473), 0.1076800524258, (-3.013419151247e-2)],
                },
                Vertex {
                    position: [(-0.210605128371), 0.1014220774740, (-8.86512385957e-2)],
                },
                Vertex {
                    position: [(-0.1853707055049), 8.92698207328e-2, (-0.1420161985371)],
                },
                Vertex {
                    position: [(-0.1493632123076), 7.19295271087e-2, (-0.1871276942065)],
                },
                Vertex {
                    position: [(-0.1046752731540), 5.04089513182e-2, (-0.221364009760)],
                },
                Vertex {
                    position: [(-5.39039883254e-2), 2.595879085364e-2, (-0.242735455218)],
                },
                Vertex {
                    position: [-0., -0., 0.250],
                },
                Vertex {
                    position: [(-5.39039885835e-2), (-2.595880341252e-2), 0.242735453818],
                },
                Vertex {
                    position: [(-0.1046752730513), (-5.04089754153e-2), 0.221364004321],
                },
                Vertex {
                    position: [(-0.149363206731), (-7.19295588784e-2), 0.1871276864458],
                },
                Vertex {
                    position: [(-0.185370700769), (-8.92698612133e-2), 0.1420161792738],
                },
                Vertex {
                    position: [(-0.2106051168578), (-0.1014221205118), 8.8651216710e-2],
                },
                Vertex {
                    position: [(-0.2235999305258), (-0.107680095520), 3.013416827644e-2],
                },
                Vertex {
                    position: [(-0.2235999279838), (-0.1076800942957), (-3.013419151247e-2)],
                },
                Vertex {
                    position: [(-0.210605109380), (-0.101422116911), (-8.86512385957e-2)],
                },
                Vertex {
                    position: [(-0.185370688789), (-8.92698554442e-2), (-0.1420161985371)],
                },
                Vertex {
                    position: [(-0.149363198839), (-7.19295550775e-2), (-0.1871276942065)],
                },
                Vertex {
                    position: [(-0.1046752637147), (-5.04089709190e-2), (-0.221364009760)],
                },
                Vertex {
                    position: [(-5.39039834645e-2), (-2.595880094735e-2), (-0.242735455218)],
                },
                Vertex {
                    position: [-0., -0., 0.250],
                },
                Vertex {
                    position: [(-1.331318052687e-2), (-5.83288837891e-2), 0.242735453818],
                },
                Vertex {
                    position: [(-2.585264733559e-2), (-0.1132679046178), 0.221364004321],
                },
                Vertex {
                    position: [(-3.68896511657e-2), (-0.1616242017840), 0.1871276864458],
                },
                Vertex {
                    position: [(-4.57827642921e-2), (-0.200587495418), 0.1420161792738],
                },
                Vertex {
                    position: [(-5.20151479378e-2), (-0.2278933657675), 8.8651216710e-2],
                },
                Vertex {
                    position: [(-5.52246006114e-2), (-0.241954903628), 3.013416827644e-2],
                },
                Vertex {
                    position: [
                        (-5.52245999836e-2),
                        (-0.2419549008773),
                        (-3.013419151247e-2),
                    ],
                },
                Vertex {
                    position: [(-5.20151460909e-2), (-0.2278933576754), (-8.86512385957e-2)],
                },
                Vertex {
                    position: [(-4.57827613334e-2), (-0.2005874824553), (-0.1420161985371)],
                },
                Vertex {
                    position: [(-3.68896492164e-2), (-0.1616241932436), (-0.1871276942065)],
                },
                Vertex {
                    position: [(-2.585264502965e-2), (-0.1132678945149), (-0.221364009760)],
                },
                Vertex {
                    position: [(-1.331317926258e-2), (-5.8328878250e-2), (-0.242735455218)],
                },
                Vertex {
                    position: [0., -0., 0.250],
                },
                Vertex {
                    position: [3.73027228921e-2, (-4.6776129860e-2), 0.242735453818],
                },
                Vertex {
                    position: [7.24375469589e-2, (-9.08338008747e-2), 0.221364004321],
                },
                Vertex {
                    position: [0.1033625610532, (-0.1296125377345), 0.1871276864458],
                },
                Vertex {
                    position: [0.1282805236644, (-0.1608586711149), 0.1420161792738],
                },
                Vertex {
                    position: [0.1457432839435, (-0.1827562774877), 8.8651216710e-2],
                },
                Vertex {
                    position: [0.154735975320, (-0.1940327545650), 3.013416827644e-2],
                },
                Vertex {
                    position: [0.154735973561, (-0.194032752359), (-3.013419151247e-2)],
                },
                Vertex {
                    position: [0.1457432787685, (-0.1827562710), (-8.86512385957e-2)],
                },
                Vertex {
                    position: [0.1282805153742, (-0.160858660719), (-0.1420161985371)],
                },
                Vertex {
                    position: [0.1033625555914, (-0.1296125308856), (-0.1871276942065)],
                },
                Vertex {
                    position: [7.24375404978e-2, (-9.08337927728e-2), (-0.221364009760)],
                },
                Vertex {
                    position: [3.7302719350e-2, (-4.67761254176e-2), (-0.242735455218)],
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
