use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use futures::StreamExt;
use tonic::Request;
use tokio::sync::mpsc;

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
    packet_count: u64,
    last_packet_size: usize,
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
            println!("[Client] Connecting to server...");
            let mut client = match RemoteBrowserClient::connect("http://[::1]:50051").await {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("[Client] Connection failed: {:?}", e);
                    return;
                }
            };
            
            // Use a channel to keep the client-to-server stream open
            let (tx_events, rx_events) = mpsc::channel(10);
            let _ = tx_events.send(ClientEvent { event: None }).await;
            
            let request = Request::new(tokio_stream::wrappers::ReceiverStream::new(rx_events));
            let mut stream = client.stream_session(request).await.expect("Stream session failed").into_inner();
            
            println!("[Client] Stream established. Waiting for packets...");
            
            while let Some(item) = stream.next().await {
                match item {
                    Ok(update) => {
                        if let Some(server_update::Update::Frame(frame)) = update.update {
                            let mut lock = state_clone.lock().unwrap();
                            let w = lock.frame_width;
                            let h = lock.frame_height;
                            
                            lock.packet_count += 1;
                            lock.last_packet_size = frame.av1_data.len();
                            
                            // Visualization: Fill the screen with bits from the packet
                            let mut rgba_data = vec![0u8; w as usize * h as usize * 4];
                            let data_len = frame.av1_data.len();
                            
                            // Spread packet data across the visualization buffer
                            for i in 0..(w as usize * h as usize) {
                                let byte_idx = i % data_len;
                                let val = frame.av1_data[byte_idx];
                                let pixel_idx = i * 4;
                                // Make it colorful and obvious
                                rgba_data[pixel_idx] = val;
                                rgba_data[pixel_idx + 1] = (val as u16 * 2 % 255) as u8;
                                rgba_data[pixel_idx + 2] = 200;
                                rgba_data[pixel_idx + 3] = 255;
                            }
                            
                            lock.latest_frame = Some(Image {
                                width: w,
                                height: h,
                                bytes: rgba_data,
                            });
                            lock.dirty = true;
                        }
                    }
                    Err(e) => {
                        eprintln!("[Client] Stream error: {:?}", e);
                        break;
                    }
                }
            }
        });
    });

    let mut texture: Option<Texture2D> = None;
    let mut last_size = 0;
    let mut count = 0;

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
                last_size = lock.last_packet_size;
                count = lock.packet_count;
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
            
            // Draw a semi-transparent overlay for status
            draw_rectangle(10.0, 10.0, 300.0, 80.0, Color::new(0.0, 0.0, 0.0, 0.7));
            draw_text(&format!("Packets Received: {}", count), 20.0, 40.0, 20.0, GREEN);
            draw_text(&format!("Last Packet: {} bytes", last_size), 20.0, 70.0, 20.0, SKYBLUE);
        } else {
            draw_text("Waiting for AV1 stream...", screen_width() / 2.0 - 150.0, screen_height() / 2.0, 30.0, WHITE);
            draw_text("Check server terminal for logs", screen_width() / 2.0 - 120.0, screen_height() / 2.0 + 40.0, 20.0, GRAY);
        }

        next_frame().await;
    }
}
