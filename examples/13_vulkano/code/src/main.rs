#![allow(unused_parens)]
use chrono::Utc;
use vulkano::instance::{Instance, InstanceExtensions};
use vulkano_win::VkSurfaceBuild;
fn main() {
    let extensions = vulkano_win::required_extensions();
    let instance =
        Instance::new(None, &extensions, None).expect("failed to create Vulkan instance");
}
