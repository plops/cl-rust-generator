use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use futures::StreamExt;
use tonic::Request;

use proto_def::graphical_proxy::{
    remote_browser_client::RemoteBrowserClient,
    ClientEvent, server_update,
};

#[derive(Default)]
struct ClientState {
    latest_frame: Option<Image>,
    frame_width: u16,
    frame_height: u16,
    dirty: bool,
    data_size: usize,
}

#[macroquad::main("AV1 Remote Browser")]
async fn main() {
    let state = Arc::new(Mutex::new(ClientState {
        frame_width: 1280,
        frame_height: 720,
        ..Default::default()
    }));
    
    let state_clone = state.clone();
    
    // Background gRPC client
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let mut client = RemoteBrowserClient::connect("http://[::1]:50051").await.expect("Could not connect to server");
            
            let client_events: Vec<ClientEvent> = vec![
                ClientEvent { event: None }
            ];
            let request = Request::new(futures::stream::iter(client_events));
            
            let mut stream = client.stream_session(request).await.expect("Stream session failed").into_inner();
            
            while let Some(Ok(update)) = stream.next().await {
                if let Some(server_update::Update::Frame(frame)) = update.update {
                    let mut lock = state_clone.lock().unwrap();
                    let w = lock.frame_width;
                    let h = lock.frame_height;
                    
                    // Decode Visualizer: Create a visualization of the AV1 bitstream data
                    // This proves we are receiving real packets.
                    let mut rgba_data = vec![0u8; w as usize * h as usize * 4];
                    let packet_len = frame.av1_data.len();
                    
                    // Simple pattern based on packet data to show activity
                    for i in 0..packet_len.min(w as usize * h as usize) {
                        let val = frame.av1_data[i];
                        let idx = i * 4;
                        rgba_data[idx] = val;
                        rgba_data[idx+1] = 128;
                        rgba_data[idx+2] = 200;
                        rgba_data[idx+3] = 255;
                    }
                    
                    lock.latest_frame = Some(Image {
                        width: w,
                        height: h,
                        bytes: rgba_data,
                    });
                    lock.data_size = packet_len;
                    lock.dirty = true;
                }
            }
        });
    });

    let mut texture: Option<Texture2D> = None;
    let mut last_size = 0;

    loop {
        clear_background(BLACK);

        {
            let mut lock = state.lock().unwrap();
            if lock.dirty {
                if let Some(image) = &lock.latest_frame {
                    if texture.is_none() {
                        texture = Some(Texture2D::from_image(image));
                    } else {
                        texture.as_mut().unwrap().update(image);
                    }
                }
                last_size = lock.data_size;
                lock.dirty = false;
            }
        }

        if let Some(tex) = &texture {
            draw_texture_ex(
                tex,
                0.0,
                0.0,
                WHITE,
                DrawTextureParams {
                    dest_size: Some(vec2(screen_width(), screen_height())),
                    ..Default::default()
                },
            );
            draw_text(&format!("Packet Size: {} bytes", last_size), 20.0, 20.0, 20.0, GREEN);
        } else {
            draw_text("Connecting to server...", screen_width() / 2.0 - 100.0, screen_height() / 2.0, 30.0, WHITE);
        }

        next_frame().await;
    }
}
