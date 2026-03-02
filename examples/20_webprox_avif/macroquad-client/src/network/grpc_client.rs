use std::sync::{Arc, Mutex};
use futures::StreamExt;
use tonic::Request;
use tokio::sync::mpsc;
use log::{info, warn, error, debug};

use proto_def::graphical_proxy::{
    remote_browser_client::RemoteBrowserClient,
    ClientEvent, server_update,
};

use crate::state::{ClientState, ClientImage};
use crate::video::decoder::Av1Decoder;

pub async fn start_grpc_client(
    state: Arc<Mutex<ClientState>>,
    mut rx_events: tokio::sync::mpsc::UnboundedReceiver<ClientEvent>,
    cli: &crate::ClientCli
) {
    info!("Connecting to server...");
    let mut client = match RemoteBrowserClient::connect("http://[::1]:50051").await {
        Ok(c) => {
            info!("Successfully connected to server");
            c
        }
        Err(e) => {
            error!("Connection failed: {:?}", e);
            return;
        }
    };
    
    // Use a channel to keep the client-to-server stream open
    let (tx_events, rx_events_internal) = mpsc::channel(10);
    
    // Send initial stream configuration
    info!("Sending initial stream configuration");
    let config_event = ClientEvent {
        event: Some(proto_def::graphical_proxy::client_event::Event::Config(
            proto_def::graphical_proxy::StreamConfig {
                enable_video: true,
                enable_spatial_links: true,
                force_keyframes: true, // Simplified mode for single-frame AVIF decoder
                full_page: cli.full_page,
                raw_rgb: cli.raw_rgb,
            }
        ))
    };
    let _ = tx_events.send(config_event).await;
    
    // Bridge UI events to internal channel
    tokio::spawn(async move {
        info!("Starting UI event bridge");
        while let Some(event) = rx_events.recv().await {
            debug!("Forwarding UI event to server: {:?}", event.event);
            if let Err(e) = tx_events.send(event).await {
                error!("Failed to forward UI event: {:?}", e);
                break;
            }
        }
        info!("UI event bridge ended");
    });
    
    let request = Request::new(tokio_stream::wrappers::ReceiverStream::new(rx_events_internal));
    let mut stream = client.stream_session(request).await.expect("Stream session failed").into_inner();
    
    info!("Stream established. Waiting for packets...");
    
    while let Some(item) = stream.next().await {
        match item {
            Ok(update) => {
                debug!("Received server update");
                match update.update {
                    Some(server_update::Update::Frame(frame)) => {
                        info!("Received video frame: {} bytes AV1, {} bytes RGB, keyframe: {}, viewport: ({},{})", 
                            frame.av1_data.len(), frame.raw_rgb_data.len(), frame.is_keyframe, frame.viewport_x, frame.viewport_y);
                        
                        // Handle timestamp for latency measurement
                        if !frame.timestamp_text.is_empty() {
                            if let Ok(timestamp_ms) = frame.timestamp_text.parse::<u64>() {
                                let current_time_ms = std::time::SystemTime::now()
                                    .duration_since(std::time::UNIX_EPOCH)
                                    .unwrap()
                                    .as_millis() as u64;
                                let latency_ms = current_time_ms.saturating_sub(timestamp_ms);
                                info!("🕒 LATENCY: {}ms (timestamp: {}, current: {})", 
                                    latency_ms, timestamp_ms, current_time_ms);
                            }
                        }
                        
                        let mut lock = match state.lock() {
                            Ok(guard) => guard,
                            Err(poisoned) => poisoned.into_inner(),
                        };
                        
                        lock.packet_count += 1;
                        lock.last_packet_size = frame.av1_data.len();
                        lock.server_viewport_y = frame.viewport_y;
                        
                        // Handle raw RGB mode or AV1 decoding
                        if !frame.raw_rgb_data.is_empty() {
                            info!("Processing raw RGB frame: {}x{}, {} bytes", 
                                frame.frame_width, frame.frame_height, frame.raw_rgb_data.len());
                            
                            // Convert RGB to RGBA by adding alpha channel
                            let mut rgba_data = Vec::with_capacity(frame.raw_rgb_data.len() / 3 * 4);
                            for chunk in frame.raw_rgb_data.chunks_exact(3) {
                                rgba_data.extend_from_slice(&[chunk[0], chunk[1], chunk[2], 255]); // R, G, B, A
                            }
                            
                            lock.latest_frame = Some(ClientImage {
                                width: frame.frame_width as u16,
                                height: frame.frame_height as u16,
                                bytes: rgba_data,
                            });
                            lock.dirty = true;
                            
                            // Update frame dimensions
                            lock.frame_width = frame.frame_width as u16;
                            lock.frame_height = frame.frame_height as u16;
                            
                            // Check if this is the first successful frame
                            if !lock.first_frame_received {
                                info!("🎉 FIRST FRAME RECEIVED! Client is now ready for scrolling testing");
                                lock.first_frame_received = true;
                            }
                            
                            continue;
                        }
                        
                        // Original AV1 decoding path
                        let _w = lock.frame_width;
                        let _h = lock.frame_height;
                        
                        // Initialize decoder if needed
                        if lock.decoder.is_none() {
                            info!("Initializing AV1 decoder");
                            match Av1Decoder::new() {
                                Ok(decoder) => {
                                    lock.decoder = Some(decoder);
                                    info!("AV1 decoder initialized successfully");
                                }
                                Err(e) => {
                                    error!("Failed to initialize AV1 decoder: {}", e);
                                    // Fall back to visualization
                                    continue;
                                }
                            }
                        }
                        
                        // Try to decode AV1 frame
                        if let Some(ref mut decoder) = lock.decoder {
                            match decoder.decode_frame(&frame.av1_data) {
                                Ok(Some(image_buffer)) => {
                                    let (width, height) = image_buffer.dimensions();
                                    let rgba_data = image_buffer.into_raw();
                                    
                                    // Check if this is the first successful AV1 frame
                                    debug!("First frame check: first_frame_received={}", lock.first_frame_received);
                                    if !lock.first_frame_received {
                                        info!("🎉 FIRST FRAME RECEIVED! Client is now ready for scrolling testing");
                                        lock.first_frame_received = true;
                                    }
                                    
                                    info!("Successfully decoded AV1 frame: {}x{}, {} bytes", 
                                            width, height, frame.av1_data.len());
                                    
                                    lock.latest_frame = Some(ClientImage {
                                        width: width as u16,
                                        height: height as u16,
                                        bytes: rgba_data,
                                    });
                                    lock.dirty = true;
                                    continue;
                                }
                                Ok(None) => {
                                    debug!("No frame ready yet, try again next time");
                                    continue;
                                }
                                Err(e) => {
                                    warn!("AV1 decode failed: {}, using visualization", e);
                                    
                                    // Fallback to visualization
                                    let w = lock.frame_width;
                                    let h = lock.frame_height;
                                    let mut rgba_data = vec![0u8; w as usize * h as usize * 4];
                                    let data_len = frame.av1_data.len();
                                    
                                    for i in 0..(w as usize * h as usize) {
                                        let byte_idx = i % data_len;
                                        let val = frame.av1_data[byte_idx];
                                        let pixel_idx = i * 4;
                                        
                                        let x = i % w as usize;
                                        let y = i / w as usize;
                                        
                                        let content_type = (val as usize + x + y) % 4;
                                        
                                        match content_type {
                                            0 => { // White/light background
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
                                    
                                    lock.latest_frame = Some(ClientImage {
                                        width: w,
                                        height: h,
                                        bytes: rgba_data,
                                    });
                                    lock.dirty = true;
                                }
                            }
                        }
                    }
                    Some(server_update::Update::SpatialData(metadata)) => {
                        info!("Received spatial metadata: {} links, title: '{}'", metadata.links.len(), metadata.title);
                        let mut lock = match state.lock() {
                            Ok(guard) => guard,
                            Err(poisoned) => poisoned.into_inner(),
                        };
                        lock.page_title = metadata.title;
                        lock.doc_width = metadata.document_width;
                        lock.doc_height = metadata.document_height;
                        lock.links = metadata.links;
                    }
                    Some(server_update::Update::Status(status)) => {
                        info!("Status: {}", status.message);
                    }
                    _ => {
                        debug!("Received unknown update type");
                    }
                }
            }
            Err(e) => {
                error!("Stream error: {:?}", e);
                break;
            }
        }
    }
    
    info!("Stream ended, client disconnecting");
}
