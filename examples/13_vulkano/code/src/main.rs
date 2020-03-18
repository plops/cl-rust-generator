#![allow(unused_parens)]
use chrono::Utc;
use vulkano_win::VkSurfaceBuild;
use winit::{EventsLoop, WindowBuilder};
fn main() -> Result<(), Box<dyn Error>> {
    let mut events_loop = EventsLoop::new();
    let extensions = vulkano_win::required_extensions();
    let instance =
        Instance::new(None, &extensions, None).expect("failed to create Vulkan instance");
    let surface = WindowBuilder::new()
        .build_vk_surface(&events_loop, instance.clone())
        .unwrap();
    return Ok(());
}
