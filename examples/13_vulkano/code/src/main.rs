#![allow(unused_parens)]
use chrono::Utc;
use std::fs::File;
use std::io::prelude::*;
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
        println!("{} {}:{} start ", Utc::now(), file!(), line!())
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
            {
                println!(
                    "{} {}:{} debug:  msg.description={:?}",
                    Utc::now(),
                    file!(),
                    line!(),
                    msg.description
                )
            }
        })
        .ok();
    let physical = vulkano::instance::PhysicalDevice::enumerate(&instance)
        .next()
        .expect("no device available");
    let queue_family = physical
        .queue_families()
        .find(|&q| return q.supports_graphics())
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
                        )
                    }
                    return [ds.0, ds.1];
                }
                _ => {
                    {
                        println!("{} {}:{} window size fail ", Utc::now(), file!(), line!())
                    }
                    panic!("win size")
                }
            }
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
            println!("{} {}:{} swapchain  dimensions={:?}  alpha={:?}  format={:?}  caps.min_image_count={:?}  caps.supported_usage_flags={:?}", Utc::now(), file!(), line!(), dimensions, alpha, format, caps.min_image_count, caps.supported_usage_flags)
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
                    position: [0.14694631, 0., 0.20225425],
                },
                Vertex {
                    position: [0.2377641, 0., 7.725424e-2],
                },
                Vertex {
                    position: [0.2377641, 0., (-7.725427e-2)],
                },
                Vertex {
                    position: [0.14694631, 0., (-0.20225425)],
                },
                Vertex {
                    position: [0., 0., 0.250],
                },
                Vertex {
                    position: [0.13011450, 6.828936e-2, 0.20225425],
                },
                Vertex {
                    position: [0.21052969, 0.11049450, 7.725424e-2],
                },
                Vertex {
                    position: [0.21052967, 0.11049450, (-7.725427e-2)],
                },
                Vertex {
                    position: [0.13011450, 6.828935e-2, (-0.20225425)],
                },
                Vertex {
                    position: [0., 0., 0.250],
                },
                Vertex {
                    position: [8.34750e-2, 0.12093445, 0.20225425],
                },
                Vertex {
                    position: [0.13506540, 0.1956760, 7.725424e-2],
                },
                Vertex {
                    position: [0.13506540, 0.1956760, (-7.725427e-2)],
                },
                Vertex {
                    position: [8.34750e-2, 0.12093444, (-0.20225425)],
                },
                Vertex {
                    position: [0., 0., 0.250],
                },
                Vertex {
                    position: [1.7712420e-2, 0.1458749, 0.20225425],
                },
                Vertex {
                    position: [2.865930e-2, 0.23603056, 7.725424e-2],
                },
                Vertex {
                    position: [2.865930e-2, 0.23603055, (-7.725427e-2)],
                },
                Vertex {
                    position: [1.7712417e-2, 0.1458749, (-0.20225425)],
                },
                Vertex {
                    position: [-0., 0., 0.250],
                },
                Vertex {
                    position: [(-5.210789e-2), 0.13739718, 0.20225425],
                },
                Vertex {
                    position: [(-8.431234e-2), 0.22231331, 7.725424e-2],
                },
                Vertex {
                    position: [(-8.431233e-2), 0.22231331, (-7.725427e-2)],
                },
                Vertex {
                    position: [(-5.210789e-2), 0.13739718, (-0.20225425)],
                },
                Vertex {
                    position: [-0., 0., 0.250],
                },
                Vertex {
                    position: [(-0.1099909), 9.744343e-2, 0.20225425],
                },
                Vertex {
                    position: [(-0.1779690), 0.15766677, 7.725424e-2],
                },
                Vertex {
                    position: [(-0.1779690), 0.15766677, (-7.725427e-2)],
                },
                Vertex {
                    position: [(-0.1099909), 9.744341e-2, (-0.20225425)],
                },
                Vertex {
                    position: [-0., 0., 0.250],
                },
                Vertex {
                    position: [(-0.14267632), 3.5166556e-2, 0.20225425],
                },
                Vertex {
                    position: [(-0.2308551), 5.690067e-2, 7.725424e-2],
                },
                Vertex {
                    position: [(-0.2308551), 5.690067e-2, (-7.725427e-2)],
                },
                Vertex {
                    position: [(-0.14267631), 3.5166550e-2, (-0.20225425)],
                },
                Vertex {
                    position: [-0., -0., 0.250],
                },
                Vertex {
                    position: [(-0.14267631), (-3.516658e-2), 0.20225425],
                },
                Vertex {
                    position: [(-0.2308551), (-5.690072e-2), 7.725424e-2],
                },
                Vertex {
                    position: [(-0.2308551), (-5.690072e-2), (-7.725427e-2)],
                },
                Vertex {
                    position: [(-0.14267631), (-3.516658e-2), (-0.20225425)],
                },
                Vertex {
                    position: [-0., -0., 0.250],
                },
                Vertex {
                    position: [(-0.10999088), (-9.744345e-2), 0.20225425],
                },
                Vertex {
                    position: [(-0.1779690), (-0.15766680), 7.725424e-2],
                },
                Vertex {
                    position: [(-0.1779690), (-0.15766680), (-7.725427e-2)],
                },
                Vertex {
                    position: [(-0.10999087), (-9.744344e-2), (-0.20225425)],
                },
                Vertex {
                    position: [-0., -0., 0.250],
                },
                Vertex {
                    position: [(-5.210786e-2), (-0.13739720), 0.20225425],
                },
                Vertex {
                    position: [(-8.431230e-2), (-0.22231333), 7.725424e-2],
                },
                Vertex {
                    position: [(-8.431230e-2), (-0.22231333), (-7.725427e-2)],
                },
                Vertex {
                    position: [(-5.210786e-2), (-0.13739718), (-0.20225425)],
                },
                Vertex {
                    position: [0., -0., 0.250],
                },
                Vertex {
                    position: [1.7712432e-2, (-0.1458749), 0.20225425],
                },
                Vertex {
                    position: [2.865932e-2, (-0.23603056), 7.725424e-2],
                },
                Vertex {
                    position: [2.865932e-2, (-0.23603055), (-7.725427e-2)],
                },
                Vertex {
                    position: [1.7712432e-2, (-0.1458749), (-0.20225425)],
                },
                Vertex {
                    position: [0., -0., 0.250],
                },
                Vertex {
                    position: [8.34750e-2, (-0.12093444), 0.20225425],
                },
                Vertex {
                    position: [0.13506544, (-0.1956760), 7.725424e-2],
                },
                Vertex {
                    position: [0.13506542, (-0.1956760), (-7.725427e-2)],
                },
                Vertex {
                    position: [8.34750e-2, (-0.12093443), (-0.20225425)],
                },
                Vertex {
                    position: [0., -0., 0.250],
                },
                Vertex {
                    position: [0.13011450, (-6.828935e-2), 0.20225425],
                },
                Vertex {
                    position: [0.21052969, (-0.11049449), 7.725424e-2],
                },
                Vertex {
                    position: [0.21052969, (-0.11049449), (-7.725427e-2)],
                },
                Vertex {
                    position: [0.13011450, (-6.828935e-2), (-0.20225425)],
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
        let vs = {
            let mut f  = std::fs::File::open("/home/martin/stage/cl-rust-generator/examples/13_vulkano/code/src/trace.vert").expect("can't find /home/martin/stage/cl-rust-generator/examples/13_vulkano/code/src/trace.vert");
            let mut v = vec![];
            f.read_to_end(&mut v).unwrap();
            unsafe { vulkano::pipeline::shader::ShaderModule::new(device.clone(), &v) }.unwrap()
        };
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        struct ShaderInput_vs;
        unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ShaderInput_vs {
            type Iter = ShaderInputIter_vs;
            fn elements(&self) -> ShaderInputIter_vs {
                return ShaderInputIter_vs(0);
            }
        }
        #[derive(Debug, Copy, Clone)]
        struct ShaderInputIter_vs(u16);
        impl vulkano::pipeline::shader::Iterator for ShaderInputIter_vs {
            type Item = vulkano::pipeline::shader::ShaderInterfaceDefEntry;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                // - on entry per location, non-overlapping
                // - each element must not be longer than 128 bytes
                if (self.0) == (0) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 1..2,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("color")),
                    });
                }
                if (self.0) == (1) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 0..1,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("color")),
                    });
                }
                return None;
            }
            #[inline]
            fn size_hint(&mut self) -> (usize, Option<usize>) {
                // - exact number of entries left in iterator
                let len = ((2 - self.0) as usize);
                return (len, Some(len));
            }
        }
        impl vulkano::pipeline::shader::ExactSizeIterator for ShaderInputIter_vs {}
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        struct ShaderOutput_vs;
        unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ShaderInput_vs {
            type Iter = ShaderInputIter_vs;
            fn elements(&self) -> ShaderInputIter_vs {
                return ShaderInputIter_vs(0);
            }
        }
        #[derive(Debug, Copy, Clone)]
        struct ShaderOutputIter_vs(u16);
        impl vulkano::pipeline::shader::Iterator for ShaderOutputIter_vs {
            type Item = vulkano::pipeline::shader::ShaderInterfaceDefEntry;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                if (self.0) == (0) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 0..1,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("v_color")),
                    });
                }
                return None;
            }
            #[inline]
            fn size_hint(&mut self) -> (usize, Option<usize>) {
                // - exact number of entries left in iterator
                let len = ((2 - self.0) as usize);
                return (len, Some(len));
            }
        }
        impl vulkano::pipeline::shader::ExactSizeIterator for ShaderOutputIter_vs {}
        #[derive(Debug, Copy, Clone)]
        struct ShaderLayout_vs(ShaderStages);
        unsafe impl vulkano::pipeline::shader::PipelineLayoutDesc for ShaderLayout_vs {
            fn num_sets(&self) -> usize {
                return 0;
            }
            fn num_bindings_in_set(&self, _set: usize) -> Option<usize> {
                return None;
            }
            fn descriptor(&self, _set: usize, _binding: usize) -> Option<DescriptorDesc> {
                return None;
            }
            fn num_push_constants_ranges(&self) -> usize {
                return 0;
            }
            fn push_constants_range(&self, _num: usize) -> Option<PipelineLayoutDescPcRange> {
                return None;
            }
        }
        let fs = {
            let mut f  = std::fs::File::open("/home/martin/stage/cl-rust-generator/examples/13_vulkano/code/src/trace.frag").expect("can't find /home/martin/stage/cl-rust-generator/examples/13_vulkano/code/src/trace.frag");
            let mut v = vec![];
            f.read_to_end(&mut v).unwrap();
            unsafe { vulkano::pipeline::shader::ShaderModule::new(device.clone(), &v) }.unwrap()
        };
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        struct ShaderInput_fs;
        unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ShaderInput_fs {
            type Iter = ShaderInputIter_fs;
            fn elements(&self) -> ShaderInputIter_fs {
                return ShaderInputIter_fs(0);
            }
        }
        #[derive(Debug, Copy, Clone)]
        struct ShaderInputIter_fs(u16);
        impl vulkano::pipeline::shader::Iterator for ShaderInputIter_fs {
            type Item = vulkano::pipeline::shader::ShaderInterfaceDefEntry;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                // - on entry per location, non-overlapping
                // - each element must not be longer than 128 bytes
                if (self.0) == (0) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 1..2,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("color")),
                    });
                }
                if (self.0) == (1) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 0..1,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("color")),
                    });
                }
                return None;
            }
            #[inline]
            fn size_hint(&mut self) -> (usize, Option<usize>) {
                // - exact number of entries left in iterator
                let len = ((2 - self.0) as usize);
                return (len, Some(len));
            }
        }
        impl vulkano::pipeline::shader::ExactSizeIterator for ShaderInputIter_fs {}
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        struct ShaderOutput_fs;
        unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ShaderInput_fs {
            type Iter = ShaderInputIter_fs;
            fn elements(&self) -> ShaderInputIter_fs {
                return ShaderInputIter_fs(0);
            }
        }
        #[derive(Debug, Copy, Clone)]
        struct ShaderOutputIter_fs(u16);
        impl vulkano::pipeline::shader::Iterator for ShaderOutputIter_fs {
            type Item = vulkano::pipeline::shader::ShaderInterfaceDefEntry;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                if (self.0) == (0) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 0..1,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("v_color")),
                    });
                }
                return None;
            }
            #[inline]
            fn size_hint(&mut self) -> (usize, Option<usize>) {
                // - exact number of entries left in iterator
                let len = ((2 - self.0) as usize);
                return (len, Some(len));
            }
        }
        impl vulkano::pipeline::shader::ExactSizeIterator for ShaderOutputIter_fs {}
        #[derive(Debug, Copy, Clone)]
        struct ShaderLayout_fs(ShaderStages);
        unsafe impl vulkano::pipeline::shader::PipelineLayoutDesc for ShaderLayout_fs {
            fn num_sets(&self) -> usize {
                return 0;
            }
            fn num_bindings_in_set(&self, _set: usize) -> Option<usize> {
                return None;
            }
            fn descriptor(&self, _set: usize, _binding: usize) -> Option<DescriptorDesc> {
                return None;
            }
            fn num_push_constants_ranges(&self) -> usize {
                return 0;
            }
            fn push_constants_range(&self, _num: usize) -> Option<PipelineLayoutDescPcRange> {
                return None;
            }
        }
        let fs2 = {
            let mut f  = std::fs::File::open("/home/martin/stage/cl-rust-generator/examples/13_vulkano/code/src/trace2.frag").expect("can't find /home/martin/stage/cl-rust-generator/examples/13_vulkano/code/src/trace2.frag");
            let mut v = vec![];
            f.read_to_end(&mut v).unwrap();
            unsafe { vulkano::pipeline::shader::ShaderModule::new(device.clone(), &v) }.unwrap()
        };
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        struct ShaderInput_fs2;
        unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ShaderInput_fs2 {
            type Iter = ShaderInputIter_fs2;
            fn elements(&self) -> ShaderInputIter_fs2 {
                return ShaderInputIter_fs2(0);
            }
        }
        #[derive(Debug, Copy, Clone)]
        struct ShaderInputIter_fs2(u16);
        impl vulkano::pipeline::shader::Iterator for ShaderInputIter_fs2 {
            type Item = vulkano::pipeline::shader::ShaderInterfaceDefEntry;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                // - on entry per location, non-overlapping
                // - each element must not be longer than 128 bytes
                if (self.0) == (0) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 1..2,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("color")),
                    });
                }
                if (self.0) == (1) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 0..1,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("color")),
                    });
                }
                return None;
            }
            #[inline]
            fn size_hint(&mut self) -> (usize, Option<usize>) {
                // - exact number of entries left in iterator
                let len = ((2 - self.0) as usize);
                return (len, Some(len));
            }
        }
        impl vulkano::pipeline::shader::ExactSizeIterator for ShaderInputIter_fs2 {}
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        struct ShaderOutput_fs2;
        unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ShaderInput_fs2 {
            type Iter = ShaderInputIter_fs2;
            fn elements(&self) -> ShaderInputIter_fs2 {
                return ShaderInputIter_fs2(0);
            }
        }
        #[derive(Debug, Copy, Clone)]
        struct ShaderOutputIter_fs2(u16);
        impl vulkano::pipeline::shader::Iterator for ShaderOutputIter_fs2 {
            type Item = vulkano::pipeline::shader::ShaderInterfaceDefEntry;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                if (self.0) == (0) {
                    self.0 += 1;
                    return Some(vulkano::pipeline::shader::ShaderInterfaceDefEntry {
                        location: 0..1,
                        format: Format::R32G32B32Sfloat,
                        name: Some(std::borrow::Cow::Borrowed("v_color")),
                    });
                }
                return None;
            }
            #[inline]
            fn size_hint(&mut self) -> (usize, Option<usize>) {
                // - exact number of entries left in iterator
                let len = ((2 - self.0) as usize);
                return (len, Some(len));
            }
        }
        impl vulkano::pipeline::shader::ExactSizeIterator for ShaderOutputIter_fs2 {}
        #[derive(Debug, Copy, Clone)]
        struct ShaderLayout_fs2(ShaderStages);
        unsafe impl vulkano::pipeline::shader::PipelineLayoutDesc for ShaderLayout_fs2 {
            fn num_sets(&self) -> usize {
                return 0;
            }
            fn num_bindings_in_set(&self, _set: usize) -> Option<usize> {
                return None;
            }
            fn descriptor(&self, _set: usize, _binding: usize) -> Option<DescriptorDesc> {
                return None;
            }
            fn num_push_constants_ranges(&self) -> usize {
                return 0;
            }
            fn push_constants_range(&self, _num: usize) -> Option<PipelineLayoutDescPcRange> {
                return None;
            }
        }
        let pipeline = std::sync::Arc::new(
            vulkano::pipeline::GraphicsPipeline::start()
                .vertex_input_single_buffer::<Vertex>()
                .vertex_shader(
                    unsafe {
                        vs.graphics_entry_point(std::ffi::CStr::from_bytes_with_nul_unchecked(
                            b"main\0",
                        ))
                    },
                    (),
                )
                .triangle_strip()
                .viewports_dynamic_scissors_irrelevant(1)
                .blend_alpha_blending()
                .render_pass(vulkano::framebuffer::Subpass::from(render_pass.clone(), 0).unwrap())
                .build(device.clone())
                .unwrap(),
        );
        let pipeline2 = std::sync::Arc::new(
            vulkano::pipeline::GraphicsPipeline::start()
                .vertex_input_single_buffer::<Vertex>()
                .line_strip()
                .viewports_dynamic_scissors_irrelevant(1)
                .blend_alpha_blending()
                .render_pass(vulkano::framebuffer::Subpass::from(render_pass.clone(), 0).unwrap())
                .build(device.clone())
                .unwrap(),
        );
        let ep = fs2.main_entry_point();
        {
            println!(
                "{} {}:{} fs2  ep.input={:?}  ep.output={:?}",
                Utc::now(),
                file!(),
                line!(),
                ep.input,
                ep.output
            )
        }
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
                                )
                            }
                            return [ds.0, ds.1];
                        }
                        _ => {
                            {
                                println!("{} {}:{} window size fail ", Utc::now(), file!(), line!())
                            }
                            panic!("win size")
                        }
                    }
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
                    )
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
                println!("{} {}:{} next image ", Utc::now(), file!(), line!())
            }
            let (image_num, acquire_future) =
                match vulkano::swapchain::acquire_next_image(swapchain.clone(), None) {
                    Ok(r) => r,
                    Err(vulkano::swapchain::AcquireError::OutOfDate) => {
                        {
                            println!("{} {}:{} acquire error ", Utc::now(), file!(), line!())
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
                            )
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
                        println!("{} {}:{} out of date ", Utc::now(), file!(), line!())
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
                        )
                    }
                    previous_frame_end =
                        Some((Box::new(vulkano::sync::now(device.clone())) as Box<_>));
                }
            }
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
            println!("{} {}:{} queue ", Utc::now(), file!(), line!())
        }
    }
    {
        println!("{} {}:{} end ", Utc::now(), file!(), line!())
    }
}
