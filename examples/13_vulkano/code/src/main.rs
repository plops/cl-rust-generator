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
        let vertex_buffer2  = vulkano::buffer::CpuAccessibleBuffer::from_iter(device.clone(), vulkano::buffer::BufferUsage::all(), vec! [Vertex {position: [0.0000000000000000000    , 0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [5.982891825713303000000d-2, 0.0000000000000000000    , 0.242735453817898820000    ]}, Vertex {position: [0.116180796996633280000    , 0.0000000000000000000    , 0.221364004321450340000    ]}, Vertex {position: [0.165780665234051050000    , 0.0000000000000000000    , 0.187127686445794280000    ]}, Vertex {position: [0.205745971587458200000    , 0.0000000000000000000    , 0.142016179273816700000    ]}, Vertex {position: [0.233754062586767420000    , 0.0000000000000000000    , 8.865121671010283000000d-2]}, Vertex {position: [0.248177218741541530000    , 0.0000000000000000000    , 3.013416827644401000000d-2]}, Vertex {position: [0.248177215920176000000    , 0.0000000000000000000    , (-3.013419151247201000000d-2)]}, Vertex {position: [0.233754054286640500000    , 0.0000000000000000000    , (-8.865123859573723000000d-2)]}, Vertex {position: [0.205745958290943030000    , 0.0000000000000000000    , (-0.142016198537144120000    )]}, Vertex {position: [0.165780656473971670000    , 0.0000000000000000000    , (-0.187127694206547100000    )]}, Vertex {position: [0.116180786633836210000    , 0.0000000000000000000    , (-0.221364009760265740000    )]}, Vertex {position: [5.982891257549974000000d-2, 0.0000000000000000000    , (-0.242735455218295650000    )]}, Vertex {position: [0.0000000000000000000    , 0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [3.730271851263469000000d-2, 4.677613335223260000000d-2, 0.242735453817898820000    ]}, Vertex {position: [7.243753845444576000000d-2, 9.083380765680588000000d-2, 0.221364004321450340000    ]}, Vertex {position: [0.103362548917986530000    , 0.129612547411974530000    , 0.187127686445794280000    ]}, Vertex {position: [0.128280508603721230000    , 0.160858683125399720000    , 0.142016179273816700000    ]}, Vertex {position: [0.145743266832663920000    , 0.182756291133195500000    , 8.865121671010283000000d-2]}, Vertex {position: [0.154735957153304460000    , 0.194032769052388900000    , 3.013416827644401000000d-2]}, Vertex {position: [0.154735955394211900000    , 0.194032766846556430000    , (-3.013419151247201000000d-2)]}, Vertex {position: [0.145743261657619700000    , 0.182756284643894730000    , (-8.865123859573723000000d-2)]}, Vertex {position: [0.128280500313480040000    , 0.160858672729765230000    , (-0.142016198537144120000    )]}, Vertex {position: [0.103362543456166650000    , 0.129612540563068450000    , (-0.187127694206547100000    )]}, Vertex {position: [7.243753199334779000000d-2, 9.083379955484462000000d-2, (-0.221364009760265740000    )]}, Vertex {position: [3.730271497019446000000d-2, 4.677612891015268000000d-2, (-0.242735455218295650000    )]}, Vertex {position: [-0.0000000000000000000    , 0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [(-1.331319144915384400000d-2), 5.832888129612022000000d-2, 0.242735453817898820000    ]}, Vertex {position: [(-2.585266854539942600000d-2), 0.113267899776832740000    , 0.221364004321450340000    ]}, Vertex {position: [(-3.688968143036530600000d-2), 0.161624194876295260000    , 0.187127686445794280000    ]}, Vertex {position: [(-4.578280185283858400000d-2), 0.200587486845443250000    , 0.142016179273816700000    ]}, Vertex {position: [(-5.201519061167547000000d-2), 0.227893356027439850000    , 8.865121671010283000000d-2]}, Vertex {position: [(-5.522464591829311000000d-2), 0.241954893286922600000    , 3.013416827644401000000d-2]}, Vertex {position: [(-5.522464529048000000000d-2), 0.241954890536294630000    , (-3.013419151247201000000d-2)]}, Vertex {position: [(-5.201518876472282000000d-2), 0.227893347935414580000    , (-8.865123859573723000000d-2)]}, Vertex {position: [(-4.578279889408457000000d-2), 0.200587473882299720000    , (-0.142016198537144120000    )]}, Vertex {position: [(-3.688967948106358000000d-2), 0.161624186335849500000    , (-0.187127694206547100000    )]}, Vertex {position: [(-2.585266623945934000000d-2), 0.113267889673852820000    , (-0.221364009760265740000    )]}, Vertex {position: [(-1.331319018487105400000d-2), 5.832887575693744000000d-2, (-0.242735455218295650000    )]}, Vertex {position: [-0.0000000000000000000    , 0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [(-5.390399344436628000000d-2), 2.595879331880475700000d-2, 0.242735453817898820000    ]}, Vertex {position: [(-0.104675282490522350000    ), 5.040895581444091000000d-2, 0.221364004321450340000    ]}, Vertex {position: [(-0.149363220200184000000    ), 7.192953090960519000000d-2, 0.187127686445794280000    ]}, Vertex {position: [(-0.185370717484648200000    ), 8.926982650198158000000d-2, 0.142016179273816700000    ]}, Vertex {position: [(-0.210605135849482700000    ), 0.101422081075273340000    , 8.865121671010283000000d-2]}, Vertex {position: [(-0.223599950689233250000    ), 0.107680053649965330000    , 3.013416827644401000000d-2]}, Vertex {position: [(-0.223599948147270700000    ), 0.107680052425820780000    , (-3.013419151247201000000d-2)]}, Vertex {position: [(-0.210605128371326640000    ), 0.101422077473983440000    , (-8.865123859573723000000d-2)]}, Vertex {position: [(-0.185370705504901840000    ), 8.926982073284018000000d-2, (-0.142016198537144120000    )]}, Vertex {position: [(-0.149363212307625100000    ), 7.192952710874940000000d-2, (-0.187127694206547100000    )]}, Vertex {position: [(-0.104675273153964700000    ), 5.040895131819202000000d-2, (-0.221364009760265740000    )]}, Vertex {position: [(-5.390398832539150000000d-2), 2.595879085363660400000d-2, (-0.242735455218295650000    )]}, Vertex {position: [-0.0000000000000000000    , -0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [(-5.390398858349008000000d-2), (-2.595880341251648500000d-2), 0.242735453817898820000    ]}, Vertex {position: [(-0.104675273051266420000    ), (-5.040897541525448000000d-2), 0.221364004321450340000    ]}, Vertex {position: [(-0.149363206731124000000    ), (-7.192955887839175000000d-2), 0.187127686445794280000    ]}, Vertex {position: [(-0.185370700768556270000    ), (-8.926986121329822000000d-2), 0.142016179273816700000    ]}, Vertex {position: [(-0.210605116857837980000    ), (-0.101422120511823280000    ), 8.865121671010283000000d-2]}, Vertex {position: [(-0.223599930525760940000    ), (-0.107680095519839900000    ), 3.013416827644401000000d-2]}, Vertex {position: [(-0.223599927983798600000    ), (-0.107680094295694860000    ), (-3.013419151247201000000d-2)]}, Vertex {position: [(-0.210605109379682580000    ), (-0.101422116910531980000    ), (-8.865123859573723000000d-2)]}, Vertex {position: [(-0.185370688788811000000    ), (-8.926985544415457000000d-2), (-0.142016198537144120000    )]}, Vertex {position: [(-0.149363198838565770000    ), (-7.192955507753449000000d-2), (-0.187127694206547100000    )]}, Vertex {position: [(-0.104675263714709600000    ), (-5.040897091900385000000d-2), (-0.221364009760265740000    )]}, Vertex {position: [(-5.390398346451577000000d-2), (-2.595880094734736600000d-2), (-0.242735455218295650000    )]}, Vertex {position: [-0.0000000000000000000    , -0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [(-1.331318052686562000000d-2), (-5.832888378906108000000d-2), 0.242735453817898820000    ]}, Vertex {position: [(-2.585264733558672000000d-2), (-0.113267904617833790000    ), 0.221364004321450340000    ]}, Vertex {position: [(-3.688965116566622600000d-2), (-0.161624201784014900000    ), 0.187127686445794280000    ]}, Vertex {position: [(-4.578276429212600000000d-2), (-0.200587495418430270000    ), 0.142016179273816700000    ]}, Vertex {position: [(-5.201514793784281000000d-2), (-0.227893365767463080000    ), 8.865121671010283000000d-2]}, Vertex {position: [(-5.522460061138814000000d-2), (-0.241954903627927380000    ), 3.013416827644401000000d-2]}, Vertex {position: [(-5.522459998357555000000d-2), (-0.241954900877299300000    ), (-3.013419151247201000000d-2)]}, Vertex {position: [(-5.201514609089168000000d-2), (-0.227893357675437470000    ), (-8.865123859573723000000d-2)]}, Vertex {position: [(-4.578276133337441700000d-2), (-0.200587482455286180000    ), (-0.142016198537144120000    )]}, Vertex {position: [(-3.688964921636609400000d-2), (-0.161624193243568780000    ), (-0.187127694206547100000    )]}, Vertex {position: [(-2.585264502964852000000d-2), (-0.113267894514853430000    ), (-0.221364009760265740000    )]}, Vertex {position: [(-1.331317926258386900000d-2), (-5.832887824987807000000d-2), (-0.242735455218295650000    )]}, Vertex {position: [0.0000000000000000000    , -0.0000000000000000000    , 0.250000000000000000000    ]}, Vertex {position: [3.730272289213218000000d-2, (-4.677612985969985500000d-2), 0.242735453817898820000    ]}, Vertex {position: [7.243754695892023000000d-2, (-9.083380087471371000000d-2), 0.221364004321450340000    ]}, Vertex {position: [0.103362561053188500000    , (-0.129612537734473760000    ), 0.187127686445794280000    ]}, Vertex {position: [0.128280523664397430000    , (-0.160858671114911100000    ), 0.142016179273816700000    ]}, Vertex {position: [0.145743283943542050000    , (-0.182756277487725330000    ), 8.865121671010283000000d-2]}, Vertex {position: [0.154735975319962580000    , (-0.194032754564962270000    ), 3.013416827644401000000d-2]}, Vertex {position: [0.154735973560869850000    , (-0.194032752359129950000    ), (-3.013419151247201000000d-2)]}, Vertex {position: [0.145743278768497210000    , (-0.182756270998425060000    ), (-8.865123859573723000000d-2)]}, Vertex {position: [0.128280515374155270000    , (-0.160858660719277350000    ), (-0.142016198537144120000    )]}, Vertex {position: [0.103362555591367980000    , (-0.129612530885568200000    ), (-0.187127694206547100000    )]}, Vertex {position: [7.243754049782150000000d-2, (-9.083379277275306000000d-2), (-0.221364009760265740000    )]}, Vertex {position: [3.730271934969152700000d-2, (-4.677612541762027000000d-2), (-0.242735455218295650000    )]}].into_iter()).expect("failed to create buffer");
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
