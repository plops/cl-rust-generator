use std::sync::{Arc, Mutex};
use futures::StreamExt;
use log::{info, warn, error, debug};

use proto_def::graphical_proxy::{
    server_update,
};

use crate::state::{ClientState, ClientImage};
use crate::video::decoder::Av1Decoder;

pub struct StreamHandler;

impl StreamHandler {
    pub fn handle_video_frame(
        frame: &proto_def::graphical_proxy::VideoFrame,
        state: &Arc<Mutex<ClientState>>
    ) -> Result<(), Box<dyn std::error::Error>> {
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
            Self::handle_raw_rgb_frame(frame, &mut lock)?;
            return Ok(());
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
                    return Ok(());
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
                    return Ok(());
                }
                Ok(None) => {
                    debug!("No frame ready yet, try again next time");
                    return Ok(());
                }
                Err(e) => {
                    warn!("AV1 decode failed: {}, using visualization", e);
                    Self::create_visualization_fallback(frame, &mut lock);
                    return Ok(());
                }
            }
        }
        
        Ok(())
    }
    
    fn handle_raw_rgb_frame(
        frame: &proto_def::graphical_proxy::VideoFrame,
        lock: &mut ClientState
    ) -> Result<(), Box<dyn std::error::Error>> {
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
        
        Ok(())
    }
    
    fn create_visualization_fallback(
        frame: &proto_def::graphical_proxy::VideoFrame,
        lock: &mut ClientState
    ) {
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
    
    pub fn handle_spatial_metadata(
        metadata: &proto_def::graphical_proxy::SpatialMetadata,
        state: &Arc<Mutex<ClientState>>
    ) -> Result<(), Box<dyn std::error::Error>> {
        info!("Received spatial metadata: {} links, title: '{}'", metadata.links.len(), metadata.title);
        let mut lock = match state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        lock.page_title = metadata.title.clone();
        lock.doc_width = metadata.document_width;
        lock.doc_height = metadata.document_height;
        lock.links = metadata.links.clone();
        Ok(())
    }
    
    pub fn handle_status_message(
        status: &proto_def::graphical_proxy::StatusMessage,
        _state: &Arc<Mutex<ClientState>>
    ) -> Result<(), Box<dyn std::error::Error>> {
        info!("Status: {}", status.message);
        Ok(())
    }
}
