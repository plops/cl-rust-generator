use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use futures::StreamExt;
use tonic::Request;
use tokio::sync::mpsc;

use proto_def::graphical_proxy::{
    remote_browser_client::RemoteBrowserClient,
    ClientEvent, server_update, client_event,
};

// Import rav1d for AV1 decoding with Rust API
use rav1d::{Decoder, Settings, Picture, PlanarImageComponent, Rav1dError};
use image::{ImageBuffer, Rgba};

// AV1 Decoder wrapper using the clean Rust API
struct Av1Decoder {
    decoder: Option<Decoder>,
}

impl Av1Decoder {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Av1Decoder { decoder: None })
    }
    
    fn decode_frame(&mut self, av1_data: &[u8]) -> Result<Option<ImageBuffer<Rgba<u8>, Vec<u8>>>, Box<dyn std::error::Error>> {
        // Initialize decoder if needed
        if self.decoder.is_none() {
            let mut settings = Settings::new();
            settings.set_n_threads(1);
            
            match Decoder::with_settings(&settings) {
                Ok(decoder) => {
                    self.decoder = Some(decoder);
                    println!("[Client] AV1 decoder initialized successfully");
                }
                Err(e) => {
                    return Err(format!("Failed to initialize AV1 decoder: {:?}", e).into());
                }
            }
        }
        
        if let Some(ref mut decoder) = self.decoder {
            // Send AV1 data to decoder
            let data_copy = av1_data.to_vec().into_boxed_slice();
            match decoder.send_data(data_copy, None, None, None) {
                Ok(()) => {
                    // Try to get a picture
                    match decoder.get_picture() {
                        Ok(picture) => {
                            // Convert YUV to RGBA
                            let width = picture.width();
                            let height = picture.height();
                            let y_plane = picture.plane_data(PlanarImageComponent::Y);
                            let u_plane = picture.plane_data(PlanarImageComponent::U);
                            let v_plane = picture.plane_data(PlanarImageComponent::V);
                            
                            let y_stride = picture.stride(PlanarImageComponent::Y) as usize;
                            let u_stride = picture.stride(PlanarImageComponent::U) as usize;
                            let v_stride = picture.stride(PlanarImageComponent::V) as usize;
                            
                            let mut rgba_data = vec![0u8; (width * height * 4) as usize];
                            
                            // YUV420 to RGB conversion
                            for y in 0..height {
                                for x in 0..width {
                                    let y_idx = (y as usize * y_stride) + x as usize;
                                    let u_idx = (y as usize / 2 * u_stride) + (x as usize / 2);
                                    let v_idx = (y as usize / 2 * v_stride) + (x as usize / 2);
                                    
                                    let y_val = y_plane[y_idx];
                                    let u_val = u_plane[u_idx];
                                    let v_val = v_plane[v_idx];
                                    
                                    // YUV to RGB conversion (BT.601)
                                    let r = (1.164 * (y_val as f32 - 16.0) + 1.596 * (v_val as f32 - 128.0))
                                        .max(0.0).min(255.0) as u8;
                                    let g = (1.164 * (y_val as f32 - 16.0) - 0.813 * (v_val as f32 - 128.0) - 0.391 * (u_val as f32 - 128.0))
                                        .max(0.0).min(255.0) as u8;
                                    let b = (1.164 * (y_val as f32 - 16.0) + 2.018 * (u_val as f32 - 128.0))
                                        .max(0.0).min(255.0) as u8;
                                    
                                    let rgba_idx = ((y * width + x) * 4) as usize;
                                    rgba_data[rgba_idx] = r;
                                    rgba_data[rgba_idx + 1] = g;
                                    rgba_data[rgba_idx + 2] = b;
                                    rgba_data[rgba_idx + 3] = 255;
                                }
                            }
                            
                            println!("[Client] Successfully decoded AV1 frame: {}x{}", width, height);
                            return Ok(Some(ImageBuffer::from_raw(width, height, rgba_data)
                                .ok_or("Failed to create image buffer")?));
                        }
                        Err(Rav1dError::TryAgain) => {
                            // No frame ready yet, need more data
                            return Ok(None);
                        }
                        Err(e) => {
                            return Err(format!("Failed to get picture: {:?}", e).into());
                        }
                    }
                }
                Err(Rav1dError::TryAgain) => {
                    // Need to send pending data first
                    if let Err(e) = decoder.send_pending_data() {
                        return Err(format!("Failed to send pending data: {:?}", e).into());
                    }
                    return Ok(None);
                }
                Err(e) => {
                    return Err(format!("Failed to send data: {:?}", e).into());
                }
            }
        }
        
        Err("Decoder not initialized".into())
    }
}

#[derive(Default)]
struct ClientState {
    // Video data
    latest_frame: Option<Image>,
    frame_width: u16,
    frame_height: u16,
    dirty: bool,
    packet_count: u64,
    last_packet_size: usize,
    
    // AV1 decoder
    decoder: Option<Av1Decoder>,
    
    // Spatial metadata
    links: Vec<proto_def::graphical_proxy::LinkBox>,
    doc_width: u32,
    doc_height: u32,
    page_title: String,
    
    // Virtual framebuffer
    server_viewport_y: i32,
    local_scroll_y: i32,
    
    // Client event sender
    event_sender: Option<tokio::sync::mpsc::Sender<ClientEvent>>,
}

#[macroquad::main("AV1 Remote Browser")]
async fn main() {
    let state = Arc::new(Mutex::new(ClientState {
        frame_width: 1280,
        frame_height: 720,
        server_viewport_y: 0,
        local_scroll_y: 0,
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
            
            // Store the event sender in the state
            {
                let mut state_lock = match state_clone.lock() {
                    Ok(guard) => guard,
                    Err(poisoned) => poisoned.into_inner(),
                };
                state_lock.event_sender = Some(tx_events.clone());
            }
            
            // Send initial event to establish stream
            let _ = tx_events.send(ClientEvent { event: None }).await;
            
            let request = Request::new(tokio_stream::wrappers::ReceiverStream::new(rx_events));
            let mut stream = client.stream_session(request).await.expect("Stream session failed").into_inner();
            
            println!("[Client] Stream established. Waiting for packets...");
            
            while let Some(item) = stream.next().await {
                match item {
                    Ok(update) => {
                        match update.update {
                            Some(server_update::Update::Frame(frame)) => {
                                let mut lock = match state_clone.lock() {
                                    Ok(guard) => guard,
                                    Err(poisoned) => poisoned.into_inner(),
                                };
                                
                                lock.packet_count += 1;
                                lock.last_packet_size = frame.av1_data.len();
                                lock.server_viewport_y = frame.viewport_y;
                                
                                // Decode AV1 frame to actual image using rav1d
                                let w = lock.frame_width;
                                let h = lock.frame_height;
                                
                                // Initialize decoder if needed
                                if lock.decoder.is_none() {
                                    match Av1Decoder::new() {
                                        Ok(decoder) => {
                                            lock.decoder = Some(decoder);
                                        }
                                        Err(e) => {
                                            eprintln!("[Client] Failed to initialize AV1 decoder: {}", e);
                                            // Fall back to visualization
                                            continue;
                                        }
                                    }
                                }
                                
                                // Try to decode the AV1 frame
                                if let Some(ref mut decoder) = lock.decoder {
                                    match decoder.decode_frame(&frame.av1_data) {
                                        Ok(Some(image_buffer)) => {
                                            let (width, height) = image_buffer.dimensions();
                                            let rgba_data = image_buffer.into_raw();
                                            
                                            println!("[Client] Successfully decoded AV1 frame: {}x{}, {} bytes, keyframe: {}", 
                                                    width, height, frame.av1_data.len(), frame.is_keyframe);
                                            
                                            lock.latest_frame = Some(Image {
                                                width: width as u16,
                                                height: height as u16,
                                                bytes: rgba_data,
                                            });
                                            lock.dirty = true;
                                            continue;
                                        }
                                        Ok(None) => {
                                            // No frame ready yet, try again next time
                                            continue;
                                        }
                                        Err(e) => {
                                            eprintln!("[Client] AV1 decode failed: {}, falling back to visualization", e);
                                        }
                                    }
                                }
                                
                                // Fallback to visualization if decoding fails
                                let mut rgba_data = vec![0u8; w as usize * h as usize * 4];
                                let data_len = frame.av1_data.len();
                                
                                // Create a browser-like visualization pattern
                                for i in 0..(w as usize * h as usize) {
                                    let byte_idx = i % data_len;
                                    let val = frame.av1_data[byte_idx];
                                    let pixel_idx = i * 4;
                                    
                                    let x = i % w as usize;
                                    let y = i / w as usize;
                                    
                                    // Create a browser-like visualization pattern
                                    let content_type = (val as usize + x + y) % 4;
                                    
                                    match content_type {
                                        0 => { // White/light background (like page background)
                                            rgba_data[pixel_idx] = 240;
                                            rgba_data[pixel_idx + 1] = 240;
                                            rgba_data[pixel_idx + 2] = 240;
                                        }
                                        1 => { // Dark text
                                            rgba_data[pixel_idx] = 20;
                                            rgba_data[pixel_idx + 1] = 20;
                                            rgba_data[pixel_idx + 2] = 20;
                                        }
                                        2 => { // Blue links
                                            rgba_data[pixel_idx] = 0;
                                            rgba_data[pixel_idx + 1] = 0;
                                            rgba_data[pixel_idx + 2] = 200;
                                        }
                                        3 => { // UI elements (gray)
                                            rgba_data[pixel_idx] = 180;
                                            rgba_data[pixel_idx + 1] = 180;
                                            rgba_data[pixel_idx + 2] = 180;
                                        }
                                        _ => { // Default
                                            rgba_data[pixel_idx] = val;
                                            rgba_data[pixel_idx + 1] = val;
                                            rgba_data[pixel_idx + 2] = val;
                                        }
                                    }
                                    rgba_data[pixel_idx + 3] = 255; // A
                                }
                                
                                println!("[Client] Using visualization for AV1 frame: {} bytes, keyframe: {}", 
                                        frame.av1_data.len(), frame.is_keyframe);
                                
                                lock.latest_frame = Some(Image {
                                    width: w,
                                    height: h,
                                    bytes: rgba_data,
                                });
                                lock.dirty = true;
                            }
                            Some(server_update::Update::SpatialData(metadata)) => {
                                let mut lock = match state_clone.lock() {
                                    Ok(guard) => guard,
                                    Err(poisoned) => poisoned.into_inner(),
                                };
                                lock.page_title = metadata.title;
                                lock.doc_width = metadata.document_width;
                                lock.doc_height = metadata.document_height;
                                lock.links = metadata.links;
                                println!("[Client] Received spatial metadata: {} links", lock.links.len());
                            }
                            Some(server_update::Update::Status(status)) => {
                                println!("[Client] Status: {}", status.message);
                            }
                            _ => {}
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

        let (server_y, local_y, links, doc_width, doc_height, page_title) = {
            let mut lock = match state.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            
            // Handle local scrolling (mouse wheel)
            let (_, mouse_wheel_y) = mouse_wheel();
            if mouse_wheel_y != 0.0 {
                lock.local_scroll_y -= mouse_wheel_y as i32 * 30; // 30px per tick
                lock.local_scroll_y = lock.local_scroll_y.clamp(0, lock.doc_height as i32);
                
                // Send scroll event to server
                if let Some(sender) = &lock.event_sender {
                    let scroll_event = ClientEvent {
                        event: Some(client_event::Event::Scroll(proto_def::graphical_proxy::ScrollInput {
                            delta_y: -(mouse_wheel_y as i32 * 30),
                        })),
                    };
                    // Note: This would need to be sent from the main thread, 
                    // but for now we'll just update local state
                    println!("[Client] Scroll to: {}", lock.local_scroll_y);
                }
            }
            
            // Update texture if new frame available
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
            
            (lock.server_viewport_y, lock.local_scroll_y, lock.links.clone(), lock.doc_width, lock.doc_height, lock.page_title.clone())
        };

        // Draw video frame with viewport offset
        if let Some(tex) = &texture {
            // Calculate the render offset based on virtual framebuffer
            let render_offset_y = (server_y - local_y) as f32;
            
            draw_texture_ex(
                tex,
                0.0,
                render_offset_y,
                WHITE,
                DrawTextureParams {
                    dest_size: Some(vec2(screen_width(), screen_height())),
                    ..Default::default()
                },
            );
            
            // Draw link overlays (blue boxes)
            for link in &links {
                let screen_y = (link.y - local_y) as f32;
                let screen_x = link.x as f32;

                // Check if link is visible
                if screen_y > -link.height as f32 && screen_y < screen_height() {
                    // Draw semi-transparent blue box
                    draw_rectangle(screen_x, screen_y, link.width as f32, link.height as f32, Color::new(0.0, 0.5, 1.0, 0.3));
                    draw_rectangle_lines(screen_x, screen_y, link.width as f32, link.height as f32, 2.0, BLUE);
                    
                    // Draw link label (truncated)
                    let label = if link.label.len() > 30 {
                        format!("{}...", &link.label[..30])
                    } else {
                        link.label.clone()
                    };
                    draw_text(&label, screen_x + 2.0, screen_y + 15.0, 14.0, WHITE);
                    
                    // Handle mouse clicks on links
                    if is_mouse_button_pressed(MouseButton::Left) {
                        let (mx, my) = mouse_position();
                        if mx >= screen_x && mx <= screen_x + link.width as f32 &&
                           my >= screen_y && my <= screen_y + link.height as f32 {
                            println!("[Client] Link clicked: {}", link.url);
                            // TODO: Send navigate event to server
                        }
                    }
                }
            }
            
            // Draw status overlay
            draw_rectangle(10.0, 10.0, 350.0, 120.0, Color::new(0.0, 0.0, 0.0, 0.7));
            draw_text(&format!("Page: {}", page_title.chars().take(40).collect::<String>()), 20.0, 35.0, 16.0, GREEN);
            draw_text(&format!("Packets: {} | Size: {} bytes", count, last_size), 20.0, 60.0, 16.0, SKYBLUE);
            draw_text(&format!("Viewport: Server={} Local={}", server_y, local_y), 20.0, 85.0, 16.0, YELLOW);
            draw_text(&format!("Document: {}x{} | Links: {}", doc_width, doc_height, links.len()), 20.0, 110.0, 16.0, ORANGE);
            
            // Draw scrollbar
            if doc_height > 0 {
                let scroll_ratio = local_y as f32 / doc_height as f32;
                let scrollbar_height = 50.0;
                let scrollbar_y = scroll_ratio * (screen_height() - scrollbar_height);
                draw_rectangle(screen_width() - 15.0, scrollbar_y, 10.0, scrollbar_height, GRAY);
            }
        } else {
            draw_text("Connecting to AV1 Remote Browser...", screen_width() / 2.0 - 180.0, screen_height() / 2.0, 30.0, WHITE);
            draw_text("Make sure the server is running on [::1]:50051", screen_width() / 2.0 - 200.0, screen_height() / 2.0 + 40.0, 20.0, GRAY);
        }

        next_frame().await;
    }
}
