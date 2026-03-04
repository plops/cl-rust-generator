use rav1e::prelude::*;
use log::{info, warn, debug, trace};

use proto_def::graphical_proxy::{ServerUpdate, VideoFrame};
use crate::ServerConfig;
use std::default::Default;

pub struct EncoderConfig {
    pub quantizer: usize,
    pub min_quantizer: usize,
    pub speed_preset: u8,
    pub threads: usize,
    pub tile_cols: usize,
    pub tile_rows: usize,
    pub low_latency: bool,
    pub still_picture: bool,
    pub rdo_lookahead_frames: usize,
    pub min_keyint: usize,
    pub keyint: usize,
    pub switch_frame_interval: usize,
    pub reservoir_frame_delay: usize,
    pub error_resilient: bool,
}

pub struct FrameResult {
    pub updates: Vec<ServerUpdate>,
}

pub struct Rav1eEncoder {
    ctx: Context<u8>,
    width: usize,
    height: usize,
    viewport_y_queue: std::collections::VecDeque<i32>,
    frame_count: usize,
}

impl Rav1eEncoder {
    pub fn new(width: usize, height: usize, config: &ServerConfig) -> Result<Self, Box<dyn std::error::Error>> {
        // Configure rav1e encoder with actual dimensions and CLI parameters
        let mut enc = rav1e::prelude::EncoderConfig::default();
        enc.width = width;
        enc.height = height;
        enc.bit_depth = 8;
        enc.chroma_sampling = ChromaSampling::Cs420;
        
        // Use config parameters for encoder configuration
        enc.quantizer = config.quantizer as usize;
        enc.min_quantizer = config.min_quantizer;
        enc.speed_settings = SpeedSettings::from_preset(config.speed_preset as u8);
        enc.tile_cols = config.tile_cols;
        enc.tile_rows = config.tile_rows;
        
        // Apply latency-focused configuration from config parameters
        enc.low_latency = !config.disable_low_latency;
        enc.still_picture = !config.disable_still_picture;
        enc.speed_settings.rdo_lookahead_frames = config.rdo_lookahead_frames;
        enc.min_key_frame_interval = config.min_keyint;
        enc.max_key_frame_interval = config.keyint;
        enc.switch_frame_interval = config.switch_frame_interval;
        enc.reservoir_frame_delay = Some(config.reservoir_frame_delay);
        enc.error_resilient = !config.disable_error_resilient;
        
        info!("Encoder config: quantizer={}, min_quantizer={}, speed_preset={}, tiles={}x{}, threads={}", 
            enc.quantizer, enc.min_quantizer, config.speed_preset, enc.tile_cols, enc.tile_rows, config.threads);
        
        // Additional speed optimizations for real-time
        enc.width = 640;  // Reduce resolution from 800x600 to 640x480 for faster encoding
        enc.height = 480;
        
        let cfg = Config::new().with_encoder_config(enc).with_threads(config.threads);
        let ctx: Context<u8> = cfg.new_context().map_err(|e| format!("Encoder error: {:?}", e))?;
        
        Ok(Self {
            ctx,
            width,
            height,
            viewport_y_queue: std::collections::VecDeque::new(),
            frame_count: 0,
        })
    }
    
    pub async fn encode_frame(
        &mut self,
        screenshot: &image::RgbaImage,
        current_viewport_y: i32,
        full_page_mode: bool,
        force_keyframes: bool,
        timestamp_text: String,
    ) -> Result<FrameResult, Box<dyn std::error::Error>> {
        let mut updates = Vec::new();
        
        // Convert RGBA to YUV using core-utils
        let yuv_start = std::time::Instant::now();
        debug!("[Server] Converting to YUV...");
        let yuv_image = core_utils::rgba_to_yuv420(screenshot, self.width as u32, self.height as u32)
            .map_err(|e| format!("YUV conversion error: {:?}", e))?;
        let yuv_time = yuv_start.elapsed();
        debug!("[Server] YUV conversion complete in {:?}", yuv_time);

        let mut packet_count = 0;
        let mut found_packet = false;
        let pump_iterations = 0;

        loop {
            info!("[Server] Creating AV1 frame...");
            let mut frame = self.ctx.new_frame();
            frame.planes[0].copy_from_raw_u8(&yuv_image.y, yuv_image.y_stride as usize, 1);
            frame.planes[1].copy_from_raw_u8(&yuv_image.u, yuv_image.u_stride as usize, 1);
            frame.planes[2].copy_from_raw_u8(&yuv_image.v, yuv_image.v_stride as usize, 1);
            
            debug!("[Server] Sending frame to encoder...");
            let encode_start = std::time::Instant::now();
            match self.ctx.send_frame(frame) {
                Ok(_) => {
                    let send_time = encode_start.elapsed();
                    trace!("[Server] Frame sent to encoder in {:?}", send_time);
                    // Store the current state to guarantee metadata perfectly aligns with the encoded image
                    self.viewport_y_queue.push_back(current_viewport_y);
                },
                Err(e) => {
                    warn!("[Server] Send frame error: {:?}", e);
                }
            }
            
            info!("[Server] Receiving encoded packets...");
            let receive_start = std::time::Instant::now();
            
            // Try to receive packets with proper EncoderStatus handling
            loop {
                match self.ctx.receive_packet() {
                    Ok(packet) => {
                        found_packet = true;
                        let receive_time = receive_start.elapsed();
                        // Pop the viewport_y belonging exactly to the frame that finished encoding
                        let packet_viewport_y = self.viewport_y_queue.pop_front().unwrap_or(current_viewport_y);
                        
                        // Calculate compression metrics
                        let original_size = self.width * self.height * 3; // YUV420 approx 3 bytes per pixel
                        let compressed_size = packet.data.len();
                        let compression_ratio = original_size as f64 / compressed_size as f64;
                        let size_reduction_percent = (1.0 - (compressed_size as f64 / original_size as f64)) * 100.0;
                        
                        info!("[Server] Sending packet {} with {} bytes (encoding latency: {:?}, compression: {:.1}x, {:.1}% size reduction)", 
                            packet_count, compressed_size, receive_time, compression_ratio, size_reduction_percent);
                        
                        let update = ServerUpdate {
                            update: Some(proto_def::graphical_proxy::server_update::Update::Frame(VideoFrame {
                                av1_data: packet.data,
                                is_keyframe: force_keyframes || packet.frame_type == FrameType::KEY,
                                viewport_x: if full_page_mode { 0 } else { 0 },
                                viewport_y: if full_page_mode { 0 } else { packet_viewport_y },
                                raw_rgb_data: Vec::new(), // Empty in AV1 mode
                                frame_width: self.width as u32,
                                frame_height: self.height as u32,
                                timestamp_text: timestamp_text.clone(),
                            })),
                        };
                        trace!("[Server] Sending frame {} with viewport_y={}", packet_count, packet_viewport_y);
                        updates.push(update);
                        packet_count += 1;
                    }
                    Err(rav1e::EncoderStatus::NeedMoreData) => {
                        // This is normal - encoder needs more frames before producing output
                        debug!("[Server] Encoder needs more data - this is normal for early frames");
                        break;
                    }
                    Err(rav1e::EncoderStatus::Encoded) => {
                        // Frame was encoded but no packet output - continue trying
                        debug!("[Server] Frame encoded but no packet output - continuing");
                        continue;
                    }
                    Err(rav1e::EncoderStatus::LimitReached) => {
                        // No more packets available
                        debug!("[Server] Encoder limit reached - no more packets");
                        break;
                    }
                    Err(status) => {
                        warn!("[Server] Unexpected encoder status: {:?}", status);
                        break;
                    }
                }
            } // inner loop

            if found_packet || !true { // TODO: get pump_encoder_mode from config
                break;
            }

            info!("[Server] Pumping encoder: no packets received, sending copy of frame (iteration {})", pump_iterations);
        } // outer loop
        
        if !found_packet {
            warn!("[Server] No packets received from encoder - check low_latency configuration!");
        }
        
        info!("[Server] No more packets, frame {} complete. Sent {} packets", self.frame_count, packet_count);
        self.frame_count += 1;
        
        Ok(FrameResult { updates })
    }
}
