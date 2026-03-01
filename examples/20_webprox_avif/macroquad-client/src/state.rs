use std::sync::{Arc, Mutex};

pub struct PageState {
    pub latest_frame: Option<Vec<u8>>, // RGB pixels
    pub width: u32,
    pub height: u32,
    pub server_viewport_y: i32,
    pub local_scroll_y: i32,
    pub doc_height: u32,
}

impl Default for PageState {
    fn default() -> Self {
        Self {
            latest_frame: None,
            width: 1280,
            height: 720,
            server_viewport_y: 0,
            local_scroll_y: 0,
            doc_height: 1000,
        }
    }
}

pub type SharedState = Arc<Mutex<PageState>>;
