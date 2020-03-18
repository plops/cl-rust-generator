#![allow(unused_parens)]
use chrono::Utc;
use vulkano_win::VkSurfaceBuild;
fn main() {
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
    let (device, mut queues) = vulkano::device::Device::new(
        physical,
        &(vulkano::device::Features::none()),
        &(vulkano::device::DeviceExtensions::none()),
        [(queue_family, 0.50)].iter().cloned(),
    )
    .expect("failed to create device");
    let queue = queues.next().unwrap();
}
