use std::sync::{Arc, Mutex};
use proto_def::graphical_proxy::LinkBox;

// Client image struct to avoid naming conflicts
#[derive(Clone)]
pub struct ClientImage {
    pub width: u16,
    pub height: u16,
    pub bytes: Vec<u8>,
}

impl From<ClientImage> for macroquad::prelude::Image {
    fn from(img: ClientImage) -> Self {
        macroquad::prelude::Image {
            width: img.width as u16,
            height: img.height as u16,
            bytes: img.bytes,
        }
    }
}

#[derive(Default)]
pub struct ClientState {
    // Video data
    pub latest_frame: Option<ClientImage>,
    pub frame_width: u16,
    pub frame_height: u16,
    pub dirty: bool,
    pub packet_count: u64,
    pub last_packet_size: usize,
    
    // AV1 decoder
    pub decoder: Option<crate::video::decoder::Av1Decoder>,
    
    // Spatial metadata
    pub links: Vec<LinkBox>,
    pub doc_width: u32,
    pub doc_height: u32,
    pub page_title: String,
    
    // Virtual framebuffer
    pub server_viewport_y: i32,
    pub local_scroll_y: i32,
    
    // Debug parameters
    pub debug_y_offset: i32,
    pub verbose_coords: bool,
}
