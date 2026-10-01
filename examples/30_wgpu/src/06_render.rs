//! Vulkan-Renderer: zwei instanziierte Pipelines (Rechtecke + Text).
//!
//! [`WgpuState`] hällt Device, Surface und alle GPU-Ressourcen. Rechtecke
//! (Treemap + Header-Hintergrund) laufen über `treemap.wgsl` mit
//! Cushion-Shading, die Headerzeile über `text.wgsl` mit Atlas-Sampling.
//! Kein Vertex-Buffer: Quad-Ecken entstehen im Shader aus `vertex_index`.

use std::borrow::Cow;
use std::sync::Arc;

use crate::text::{ATLAS_H, ATLAS_W, GlyphInstance, build_atlas_rgba};

/// Shader-Quellen (WGSL, zur Compilezeit eingebettet).
const TREEMAP_SHADER: &str = include_str!("shaders/treemap.wgsl");
const TEXT_SHADER: &str = include_str!("shaders/text.wgsl");

/// Obergrenzen der Instanz-Buffer (schützen vor Riesen-Bäumen).
pub const MAX_RECTS: usize = 65_536;
pub const MAX_GLYPHS: usize = 512;

/// `RectInstance`-Flags (müssen zu `treemap.wgsl` passen).
pub const FLAG_FLAT: u32 = 1;
pub const FLAG_HI: u32 = 2;

/// Hintergrund (Clear-Farbe, linear): dunkles Blaugrau.
const BG_COLOR: wgpu::Color = wgpu::Color {
    r: 0.08,
    g: 0.09,
    b: 0.12,
    a: 1.0,
};

/// Eine Rechteck-Instanz: Pixel-Geometrie, Farbe, Shader-Flags (32 Byte).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RectInstance {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub flags: u32,
}

/// Reinterpretiert ein Slice als Bytes (kein `bytemuck` nötig).
fn slice_bytes<T>(s: &[T]) -> &[u8] {
    let bytes = std::mem::size_of_val(s);
    unsafe { std::slice::from_raw_parts(s.as_ptr().cast::<u8>(), bytes) }
}

/// Minimaler Blockier-Executor für die wgpu-Initialisierung (statt `pollster`).
/// Parkt den Thread bis der Waker ihn aufweckt — exakt das Muster aller
/// wgpu-Beispiele, nur ohne zusätzliche Dependency.
pub fn block_on<F: std::future::Future>(future: F) -> F::Output {
    use std::task::{Context, Poll, Wake, Waker};
    struct ThreadWaker(std::thread::Thread);
    impl Wake for ThreadWaker {
        fn wake(self: Arc<Self>) {
            self.0.unpark();
        }
        fn wake_by_ref(self: &Arc<Self>) {
            self.0.unpark();
        }
    }
    let mut future = Box::pin(future);
    let waker: Waker = Arc::new(ThreadWaker(std::thread::current())).into();
    let mut cx = Context::from_waker(&waker);
    loop {
        match future.as_mut().poll(&mut cx) {
            Poll::Ready(value) => return value,
            Poll::Pending => std::thread::park(),
        }
    }
}

/// Kompletter GPU-Zustand: Surface, Device, Pipelines, Buffer, Zähler.
pub struct WgpuState {
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    rect_pipeline: wgpu::RenderPipeline,
    text_pipeline: wgpu::RenderPipeline,
    uniform_buf: wgpu::Buffer,
    rect_buf: wgpu::Buffer,
    rect_bind: wgpu::BindGroup,
    hl_buf: wgpu::Buffer,
    hl_bind: wgpu::BindGroup,
    glyph_buf: wgpu::Buffer,
    text_bind: wgpu::BindGroup,
    rect_count: u32,
    hl_count: u32,
    glyph_count: u32,
}

impl WgpuState {
    /// Erzeugt Instance (Vulkan-only), Surface, Device und alle Ressourcen.
    /// `width`/`height` ist die Fenster-Innengröße (niemals 0 übergeben).
    pub fn new(
        window: Arc<winit::window::Window>,
        width: u32,
        height: u32,
    ) -> Result<Self, String> {
        let mut instance_desc = wgpu::InstanceDescriptor::new_without_display_handle();
        instance_desc.backends = wgpu::Backends::VULKAN;
        let instance = wgpu::Instance::new(instance_desc);
        let surface: wgpu::Surface<'static> = instance
            .create_surface(window)
            .map_err(|e| format!("create_surface: {e:?}"))?;
        let adapter = block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            force_fallback_adapter: false,
            compatible_surface: Some(&surface),
            apply_limit_buckets: false,
        }))
        .map_err(|e| format!("request_adapter: {e:?}"))?;
        let (device, queue) = block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("treemap"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            experimental_features: wgpu::ExperimentalFeatures::default(),
            memory_hints: wgpu::MemoryHints::Performance,
            trace: wgpu::Trace::default(),
        }))
        .map_err(|e| format!("request_device: {e:?}"))?;

        let mut config = surface
            .get_default_config(&adapter, width.max(1), height.max(1))
            .ok_or_else(|| "no surface config".to_string())?;
        config.present_mode = wgpu::PresentMode::Fifo;
        surface.configure(&device, &config);

        let uniform_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("screen"),
            size: 8,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        queue.write_buffer(&uniform_buf, 0, slice_bytes(&[width as f32, height as f32]));

        let rect_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("rect"),
            entries: &[
                uniform_entry(0, wgpu::ShaderStages::VERTEX),
                storage_entry(1),
            ],
        });
        let rect_buf = storage_buffer(&device, "rects", (MAX_RECTS * 32) as u64);
        let hl_buf = storage_buffer(&device, "highlight", 32);
        let rect_bind = rect_bind_group(&device, &rect_layout, &uniform_buf, &rect_buf);
        let hl_bind = rect_bind_group(&device, &rect_layout, &uniform_buf, &hl_buf);

        let treemap_mod = shader_module(&device, "treemap", TREEMAP_SHADER);
        let rect_pipeline = rect_pipeline(&device, &rect_layout, &treemap_mod, config.format);

        // Text-Pipeline: Uniform + Glyphen + Atlas-Textur + Sampler.
        let atlas_bytes = build_atlas_rgba();
        let atlas = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("atlas"),
            size: wgpu::Extent3d {
                width: ATLAS_W,
                height: ATLAS_H,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8Unorm,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &atlas,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &atlas_bytes,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(ATLAS_W * 4),
                rows_per_image: Some(ATLAS_H),
            },
            wgpu::Extent3d {
                width: ATLAS_W,
                height: ATLAS_H,
                depth_or_array_layers: 1,
            },
        );
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("atlas"),
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            mipmap_filter: wgpu::MipmapFilterMode::Nearest,
            ..Default::default()
        });
        let text_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("text"),
            entries: &[
                uniform_entry(0, wgpu::ShaderStages::VERTEX),
                storage_entry(1),
                wgpu::BindGroupLayoutEntry {
                    binding: 2,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 3,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });
        let glyph_buf = storage_buffer(
            &device,
            "glyphs",
            (MAX_GLYPHS * std::mem::size_of::<GlyphInstance>()) as u64,
        );
        let atlas_view = atlas.create_view(&wgpu::TextureViewDescriptor::default());
        let text_bind = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("text"),
            layout: &text_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: uniform_buf.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: glyph_buf.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: wgpu::BindingResource::TextureView(&atlas_view),
                },
                wgpu::BindGroupEntry {
                    binding: 3,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });
        let text_mod = shader_module(&device, "text", TEXT_SHADER);
        let text_pipeline = text_pipeline(&device, &text_layout, &text_mod, config.format);

        Ok(Self {
            surface,
            device,
            queue,
            config,
            rect_pipeline,
            text_pipeline,
            uniform_buf,
            rect_buf,
            rect_bind,
            hl_buf,
            hl_bind,
            glyph_buf,
            text_bind,
            rect_count: 0,
            hl_count: 0,
            glyph_count: 0,
        })
    }

    /// Passt Swapchain und Screen-Uniform an (Null-Größen ignorieren).
    pub fn resize(&mut self, width: u32, height: u32) {
        if width == 0 || height == 0 {
            return;
        }
        self.config.width = width;
        self.config.height = height;
        self.surface.configure(&self.device, &self.config);
        self.queue.write_buffer(
            &self.uniform_buf,
            0,
            slice_bytes(&[width as f32, height as f32]),
        );
    }

    /// Lädt die Treemap-Rechtecke hoch (kürzt auf `MAX_RECTS`).
    pub fn upload_rects(&mut self, rects: &[RectInstance]) {
        let n = rects.len().min(MAX_RECTS);
        if n > 0 {
            self.queue
                .write_buffer(&self.rect_buf, 0, slice_bytes(&rects[..n]));
        }
        self.rect_count = n as u32;
    }

    /// Setzt das Hover-Highlight (`None` = ausblenden).
    pub fn set_highlight(&mut self, rect: Option<RectInstance>) {
        match rect {
            Some(r) => {
                self.queue.write_buffer(&self.hl_buf, 0, slice_bytes(&[r]));
                self.hl_count = 1;
            }
            None => self.hl_count = 0,
        }
    }

    /// Lädt die Header-Glyphen hoch (kürzt auf `MAX_GLYPHS`).
    pub fn upload_glyphs(&mut self, glyphs: &[GlyphInstance]) {
        let n = glyphs.len().min(MAX_GLYPHS);
        if n > 0 {
            self.queue
                .write_buffer(&self.glyph_buf, 0, slice_bytes(&glyphs[..n]));
        }
        self.glyph_count = n as u32;
    }

    /// Rendert einen Frame; Ok bei Erfolg oder überspringbarem Zustand.
    pub fn render(&self) -> Result<(), String> {
        use wgpu::CurrentSurfaceTexture as Tex;
        let frame = match self.surface.get_current_texture() {
            Tex::Success(f) | Tex::Suboptimal(f) => f,
            Tex::Timeout | Tex::Occluded | Tex::Validation => return Ok(()),
            Tex::Outdated | Tex::Lost => {
                self.surface.configure(&self.device, &self.config);
                return Ok(());
            }
        };
        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("frame"),
            });
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("treemap"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(BG_COLOR),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            pass.set_pipeline(&self.rect_pipeline);
            pass.set_bind_group(0, &self.rect_bind, &[]);
            pass.draw(0..6, 0..self.rect_count);
            pass.set_bind_group(0, &self.hl_bind, &[]);
            pass.draw(0..6, 0..self.hl_count);
            pass.set_pipeline(&self.text_pipeline);
            pass.set_bind_group(0, &self.text_bind, &[]);
            pass.draw(0..6, 0..self.glyph_count);
        }
        self.queue.submit([encoder.finish()]);
        self.queue.present(frame);
        Ok(())
    }
}

fn uniform_entry(binding: u32, visibility: wgpu::ShaderStages) -> wgpu::BindGroupLayoutEntry {
    wgpu::BindGroupLayoutEntry {
        binding,
        visibility,
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Uniform,
            has_dynamic_offset: false,
            min_binding_size: None,
        },
        count: None,
    }
}

fn storage_entry(binding: u32) -> wgpu::BindGroupLayoutEntry {
    wgpu::BindGroupLayoutEntry {
        binding,
        visibility: wgpu::ShaderStages::VERTEX,
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Storage { read_only: true },
            has_dynamic_offset: false,
            min_binding_size: None,
        },
        count: None,
    }
}

fn storage_buffer(device: &wgpu::Device, label: &str, size: u64) -> wgpu::Buffer {
    device.create_buffer(&wgpu::BufferDescriptor {
        label: Some(label),
        size,
        usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    })
}

fn rect_bind_group(
    device: &wgpu::Device,
    layout: &wgpu::BindGroupLayout,
    uniform: &wgpu::Buffer,
    storage: &wgpu::Buffer,
) -> wgpu::BindGroup {
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("rect"),
        layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: storage.as_entire_binding(),
            },
        ],
    })
}

fn shader_module(device: &wgpu::Device, label: &str, source: &str) -> wgpu::ShaderModule {
    device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some(label),
        source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(source)),
    })
}

fn pipeline_layout(device: &wgpu::Device, bgl: &wgpu::BindGroupLayout) -> wgpu::PipelineLayout {
    device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: Some("treemap"),
        bind_group_layouts: &[Some(bgl)],
        immediate_size: 0,
    })
}

fn rect_pipeline(
    device: &wgpu::Device,
    bgl: &wgpu::BindGroupLayout,
    module: &wgpu::ShaderModule,
    format: wgpu::TextureFormat,
) -> wgpu::RenderPipeline {
    let layout = pipeline_layout(device, bgl);
    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("rect"),
        layout: Some(&layout),
        vertex: wgpu::VertexState {
            module,
            entry_point: Some("vs"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            buffers: &[],
        },
        primitive: wgpu::PrimitiveState::default(),
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        fragment: Some(wgpu::FragmentState {
            module,
            entry_point: Some("fs"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            targets: &[Some(wgpu::ColorTargetState {
                format,
                blend: None,
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        multiview_mask: None,
        cache: None,
    })
}

fn text_pipeline(
    device: &wgpu::Device,
    bgl: &wgpu::BindGroupLayout,
    module: &wgpu::ShaderModule,
    format: wgpu::TextureFormat,
) -> wgpu::RenderPipeline {
    let layout = pipeline_layout(device, bgl);
    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("text"),
        layout: Some(&layout),
        vertex: wgpu::VertexState {
            module,
            entry_point: Some("vs"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            buffers: &[],
        },
        primitive: wgpu::PrimitiveState::default(),
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        fragment: Some(wgpu::FragmentState {
            module,
            entry_point: Some("fs"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            targets: &[Some(wgpu::ColorTargetState {
                format,
                blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        multiview_mask: None,
        cache: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rect_instance_is_32_bytes() {
        assert_eq!(std::mem::size_of::<RectInstance>(), 32);
    }

    #[test]
    fn slice_bytes_roundtrip() {
        let rects = [RectInstance {
            x: 1.0,
            y: 2.0,
            w: 3.0,
            h: 4.0,
            r: 0.5,
            g: 0.25,
            b: 0.125,
            flags: FLAG_HI,
        }];
        let bytes = slice_bytes(&rects);
        assert_eq!(bytes.len(), 32);
        assert_eq!(f32::from_le_bytes(bytes[0..4].try_into().unwrap()), 1.0);
        assert_eq!(
            u32::from_le_bytes(bytes[28..32].try_into().unwrap()),
            FLAG_HI
        );
    }

    #[test]
    fn block_on_ready_and_pending() {
        assert_eq!(block_on(async { 40 + 2 }), 42);
        // Future, die genau einmal Pending liefert und sich selbst weckt.
        struct WakeOnce(bool);
        impl std::future::Future for WakeOnce {
            type Output = u32;
            fn poll(
                mut self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<u32> {
                if self.0 {
                    std::task::Poll::Ready(7)
                } else {
                    self.0 = true;
                    cx.waker().wake_by_ref();
                    std::task::Poll::Pending
                }
            }
        }
        assert_eq!(block_on(WakeOnce(false)), 7);
    }
}
