use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use log::{info, error, debug};
use tokio::sync::mpsc;

mod state;
mod network;
mod video;

use state::{PageState, SharedState};

#[macroquad::main("AV1 Remote Browser - Client")]
async fn main() {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));
    info!("Starting Client...");

    let shared_state = Arc::new(Mutex::new(PageState::default()));
    let state_clone = shared_state.clone();

    // Channel for sending events from UI to Network task
    let (event_tx, event_rx) = mpsc::channel(64);

    // Background Network Task
    std::thread::spawn(move || {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap();
        
        rt.block_on(async {
            if let Err(e) = network::network_loop(state_clone, event_rx).await {
                error!("Network loop error: {:?}", e);
            }
        });
    });

    // Render Loop
    render_loop(shared_state, event_tx).await;
}

async fn render_loop(state: SharedState, _event_tx: mpsc::Sender<proto_def::ClientEvent>) {
    let mut video_texture: Option<Texture2D> = None;
    let mut last_decoded_frame: Option<Vec<u8>> = None;

    loop {
        clear_background(color_u8!(30, 30, 30, 255));

        let local_y = {
            let mut lock = state.lock().unwrap();
            
            // Check for new decoded frame
            if let Some(rgb_data) = lock.latest_frame.take() {
                last_decoded_frame = Some(rgb_data);
            }
            
            // Simple Scroll simulation
            let (_, mouse_wheel_y) = mouse_wheel();
            if mouse_wheel_y != 0.0 {
                lock.local_scroll_y -= (mouse_wheel_y * 30.0) as i32;
                lock.local_scroll_y = lock.local_scroll_y.clamp(0, lock.doc_height as i32);
            }

            lock.local_scroll_y
        };

        // Update Texture if needed
        if let Some(rgba) = &last_decoded_frame {
            if video_texture.is_none() {
                video_texture = Some(Texture2D::from_rgba8(1280, 720, rgba));
            } else {
                video_texture.as_mut().unwrap().update(rgba);
            }
        }

        // Draw Frame
        if let Some(tex) = video_texture {
            draw_texture(tex, 0.0, -local_y as f32, WHITE);
        }

        // Skeleton UI (Placeholder for Milestone 1)
        draw_text("AV1 Mock Stream (Milestone 1)", 20.0, 30.0, 30.0, YELLOW);
        draw_text(&format!("Scroll Y: {}", local_y), 20.0, 60.0, 20.0, GREEN);

        next_frame().await;
    }
}
