use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;

use crate::state::{ClientState, ClientImage};
use crate::render::ui;
use proto_def::graphical_proxy::ClientEvent;

pub async fn run_render_loop(
    state: Arc<Mutex<ClientState>>,
    event_tx: tokio::sync::mpsc::UnboundedSender<ClientEvent>
) {
    let mut texture: Option<Texture2D> = None;
    let mut last_mouse_wheel_y = 0.0;
    let mut last_w_key_state = false;
    let mut last_s_key_state = false;
    let mut last_pageup_key_state = false;
    let mut last_pagedown_key_state = false;
    
    loop {
        // 1. Process local input & send to event_tx
        ui::handle_input_and_send_events(&event_tx, &mut last_mouse_wheel_y, &mut last_w_key_state, &mut last_s_key_state, &mut last_pageup_key_state, &mut last_pagedown_key_state);
        
        // 2. Lock state to fetch latest texture / metadata
        {
            let mut lock = match state.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            
            // Update texture if new frame available
            if lock.dirty {
                if let Some(client_image) = &lock.latest_frame {
                    let image = macroquad::prelude::Image::from(client_image.clone());
                    if texture.is_none() {
                        texture = Some(Texture2D::from_image(&image));
                    } else {
                        texture.as_mut().unwrap().update(&image);
                    }
                }
                lock.dirty = false;
            }
        }
        
        // 3. Draw texture & UI
        ui::draw_ui_and_frame(&state, texture.as_ref());
        
        next_frame().await;
    }
}
