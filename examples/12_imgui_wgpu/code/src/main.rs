#![allow(unused_parens)]
use imgui::*;
use imgui_wgpu::Renderer;
use imgui_winit_support;
use std::time::Instant;
use winit::{
    dpi::LogicalSize, ElementState, Event, EventsLoop, KeywordInput, VirtualKeyCode, WindowBuilder,
    WindowEvent,
};
fn main() {
    let instance = wgpu::Instance::new();
    let adapter = instance.get_adapter(
        &(wgpu::AdapterDescriptor {
            power_preference: wgpu::PowerPreference::HighPerformance,
        }),
    );
}
