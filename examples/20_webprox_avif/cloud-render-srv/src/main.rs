use std::pin::Pin;
use tokio::sync::mpsc;
use tokio_stream::{Stream, StreamExt, wrappers::ReceiverStream};
use tonic::{transport::Server, Request, Response, Status};
use clap::{Parser, Subcommand};
use log::{info, warn, error, debug, trace};

use proto_def::graphical_proxy::{
    remote_browser_server::{RemoteBrowser, RemoteBrowserServer},
    ClientEvent, ServerUpdate, VideoFrame, SpatialMetadata,
};

use rav1e::prelude::*;
use std::time::Duration;

mod browser;
use browser::{ChromeRunner, CdpStream, extract_spatial_metadata};

mod session;

// Initialize logging at startup
fn init_logging(level: &str) {
    core_utils::logging::init_logging_with_level("cloud-render-srv", level)
        .expect("Failed to initialize logging");
}

#[derive(Parser, Debug, Clone)]
#[command(author, version, about = "AV1 Remote Browser Server")]
struct Cli {
    /// Log level (trace, debug, info, warn, error)
    #[arg(long, default_value = "info")]
    log_level: String,
    
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug, Clone)]
enum Commands {
    /// Test Chrome extraction - capture screenshot and extract link metadata
    TestScreencast {
        #[arg(long)]
        url: String,
        #[arg(long)]
        output: Option<String>,
    },
    /// Start the gRPC server
    #[command(
        after_help = "ENCODING STRATEGIES:
  1. RAW RGB (Lowest Latency, Highest Bandwidth): Use --raw-rgb
  2. Still Picture (Low Latency, Intra-only AVIF): [Default]
  3. Low Latency Video (Low Latency, Inter-prediction): Use --disable-still-picture
  4. Max Efficiency Video (Higher Latency, Best Compression): Use --disable-still-picture --disable-low-latency --rdo-lookahead-frames 20"
    )]
    Serve {
        /// Rav1e quantizer (0-255, higher=lower quality/smaller size, default: 230)
        #[arg(long, default_value = "230")]
        quantizer: u8,
        
        /// Rav1e minimum quantizer (0-255, default: 190)
        #[arg(long, default_value = "190")]
        min_quantizer: u8,
        
        /// Rav1e speed preset (0-10, higher=faster/lower quality, default: 10)
        #[arg(long, default_value = "10")]
        speed_preset: usize,
        
        /// Number of encoder threads (0=auto CPUs, default: 12)
        #[arg(long, default_value = "12")]
        threads: usize,
        
        /// Tile columns (power of 2, max 64, default: 2)
        #[arg(long, default_value = "2")]
        tile_cols: usize,
        
        /// Tile rows (power of 2, max 64, default: 1)
        #[arg(long, default_value = "1")]
        tile_rows: usize,
        
        /// Enable full-page screenshots (captures entire document instead of viewport)
        #[arg(long)]
        full_page: bool,
        
        /// Use raw RGB transmission instead of AV1 encoding (bypasses encoder for lower latency)
        #[arg(long)]
        raw_rgb: bool,
        
        /// Stream loop delay in milliseconds. Controls the base time interval between frame captures. Default: 2500.
        #[arg(long, default_value = "2500")]
        loop_delay: u64,
        
        /// Disable low_latency mode (enables frame reordering like B-frames). Increases encoding efficiency but adds significant frame delay. Default: low_latency is enabled.
        #[arg(long)]
        disable_low_latency: bool,
        
        /// Disable still_picture mode (enables temporal inter-prediction). When enabled (default), forces standalone frames (intra-only) with screen content tools. Disabling this trades independence for better compression.
        #[arg(long)]
        disable_still_picture: bool,
        
        /// Number of lookahead frames for Rate-Distortion Optimization (1-40). Lower values reduce initial buffering delay. Default: 1 (minimum delay).
        #[arg(long, default_value = "1")]
        rdo_lookahead_frames: usize,
        
        /// Minimum interval between keyframes. Default: 1.
        #[arg(long, default_value = "1")]
        min_keyint: u64,
        
        /// Maximum interval between keyframes. Setting to 1 forces all frames to be keyframes. Default: 1.
        #[arg(long, default_value = "1")]
        keyint: u64,
        
        /// Interval for S-frames within a GOP (requires low-latency enabled). 0 disables. Default: 0.
        #[arg(long, default_value = "0")]
        switch_frame_interval: u64,
        
        /// Rate control buffer size (12-131072). This controls bitrate smoothing span, not output delay. Default: 12 (minimum allowed).
        #[arg(long, default_value = "12")]
        reservoir_frame_delay: i32,
        
        /// Disable error_resilient mode. When enabled (default), frames are independently decodable without prior context blocking.
        #[arg(long)]
        disable_error_resilient: bool,
        
        /// Testing flag: Shutdown the server after sending this many frames.
        #[arg(long)]
        max_frames: Option<usize>,
        
        /// Continuously feed copies of the current frame into the encoder until it yields a packet
        #[arg(long)]
        pump_encoder: bool,
    },
}

#[derive(Debug, Clone)]
pub struct ServerConfig {
    log_level: String,
    quantizer: u8,
    min_quantizer: u8,
    speed_preset: usize,
    threads: usize,
    tile_cols: usize,
    tile_rows: usize,
    full_page: bool,
    raw_rgb: bool,
    loop_delay: u64,
    disable_low_latency: bool,
    disable_still_picture: bool,
    rdo_lookahead_frames: usize,
    min_keyint: u64,
    keyint: u64,
    switch_frame_interval: u64,
    reservoir_frame_delay: i32,
    disable_error_resilient: bool,
    max_frames: Option<usize>,
    pump_encoder: bool,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            log_level: "info".to_string(),
            quantizer: 230,
            min_quantizer: 190,
            speed_preset: 10,
            threads: 12,
            tile_cols: 2,
            tile_rows: 1,
            full_page: false,
            raw_rgb: false,
            loop_delay: 2500,
            disable_low_latency: false,
            disable_still_picture: false,
            rdo_lookahead_frames: 1,
            min_keyint: 1,
            keyint: 1,
            switch_frame_interval: 0,
            reservoir_frame_delay: 12,
            disable_error_resilient: false,
            max_frames: None,
            pump_encoder: false,
        }
    }
}

pub struct RemoteBrowserImpl {
    config: ServerConfig,
}

impl RemoteBrowserImpl {
    pub fn new(config: ServerConfig) -> Self {
        Self { config }
    }
}

#[tonic::async_trait]
impl RemoteBrowser for RemoteBrowserImpl {
    type StreamSessionStream = Pin<Box<dyn Stream<Item = Result<ServerUpdate, Status>> + Send>>;

    async fn stream_session(
        &self,
        request: Request<tonic::Streaming<ClientEvent>>,
    ) -> Result<Response<Self::StreamSessionStream>, Status> {
        info!("New client connection received");
        let client_stream = request.into_inner();
        let (tx, rx) = mpsc::channel(128);

        // Background task to handle client events and run browser session
        let config_clone = self.config.clone();
        tokio::spawn(async move {
            info!("Spawning browser session task");
            if let Err(e) = run_browser_session(client_stream, tx, &config_clone).await {
                error!("Browser session error: {:?}", e);
            }
            info!("Browser session task ended");
        });

        let output_stream = ReceiverStream::new(rx);
        info!("Stream session established, returning response");
        Ok(Response::new(Box::pin(output_stream)))
    }
}

async fn run_browser_session(
    mut client_stream: tonic::Streaming<ClientEvent>,
    tx: tokio::sync::mpsc::Sender<Result<ServerUpdate, Status>>,
    config: &ServerConfig,
) -> Result<(), Box<dyn std::error::Error>> {
    info!("Starting browser session");
    let mut _width = 1280;
    let mut _height = 720;
    let mut _integration_test_mode = false;
    
    // Check first for test image event to determine if we should skip browser initialization
    info!("Waiting for first client event...");
    if let Some(Ok(first_event)) = client_stream.next().await {
        debug!("Received first event: {:?}", first_event.event);
        if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &first_event.event {
            _integration_test_mode = true;
            info!("Integration test mode detected - skipping browser initialization");
            
            // Handle the test image event
            let _ = session::handle_test_image_event(first_event, &tx).await;
            
            // Continue processing events in test mode
            loop {
                if let Some(Ok(event)) = client_stream.next().await {
                    if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &event.event {
                        let _ = session::handle_test_image_event(event, &tx).await;
                    }
                } else {
                    break;
                }
            }
            return Ok(());
        }
    } else {
        warn!("No first event received, client may have disconnected");
        return Ok(());
    }
    
    // Normal browser mode - initialize Chrome browser
    info!("Initializing Chrome browser...");
    let chrome = ChromeRunner::new(true).await?;
    let page = chrome.new_page().await?;
    info!("Chrome browser initialized successfully");
    
    // Navigate to calibration page
    info!("Navigating to calibration page...");
    let calibration_path = format!("file://{}/calibration.html", std::env::current_dir()?.display());
    CdpStream::navigate_to(&page, &calibration_path).await?;
    
    // Wait for page to load
    tokio::time::sleep(Duration::from_secs(3)).await;
    
    // Capture first screenshot to get actual dimensions
    info!("Capturing initial screenshot...");
    let first_screenshot = CdpStream::capture_screenshot(&page, config.full_page).await?;
    let width = first_screenshot.width() as usize;
    let height = first_screenshot.height() as usize;
    
    info!("Actual screenshot dimensions: {}x{}", width, height);
    
    // Configure rav1e encoder with actual dimensions and CLI parameters
    let mut enc = EncoderConfig::default();
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
    let mut ctx: Context<u8> = cfg.new_context().map_err(|e| format!("Encoder error: {:?}", e))?;
    
    // Send initial spatial metadata
    let metadata = extract_spatial_metadata(&page).await?;
    let spatial_update = ServerUpdate {
        update: Some(proto_def::graphical_proxy::server_update::Update::SpatialData(SpatialMetadata {
            title: metadata.title,
            document_width: metadata.document_width,
            document_height: metadata.document_height,
            links: metadata.links.into_iter().map(|link| proto_def::graphical_proxy::LinkBox {
                id: link.id,
                url: link.url,
                label: link.label,
                x: link.x,
                y: link.y,
                width: link.width,
                height: link.height,
            }).collect(),
        })),
    };
    
    if tx.send(Ok(spatial_update)).await.is_err() {
        return Ok(()); // Client disconnected
    }
    
    let mut frame_count = 0;
    let mut current_viewport_y = 0i32;
    let mut force_keyframes = true; // Default to simplified mode
    let mut full_page_mode = config.full_page; // Get from config
    let mut raw_rgb_mode = config.raw_rgb; // Get from config
    let pump_encoder_mode = config.pump_encoder; // Get from config
    let mut viewport_y_queue = std::collections::VecDeque::new(); // Tracks exact Y for buffered frames
    let mut last_scroll_time = std::time::Instant::now(); // Track when last scroll was processed
    
    loop {
        info!("[Server] Starting frame {} iteration", frame_count);
        
        // Handle client events (non-blocking with timeout)
        info!("[Server] Checking for client events...");
        match tokio::time::timeout(Duration::from_millis(10), client_stream.next()).await {
            Ok(Some(Ok(event))) => {
            // Check for test image event first to avoid borrow issues
            if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &event.event {
                let _ = session::handle_test_image_event(event, &tx).await;
                continue;
            }
            
            if let Some(client_event) = event.event {
                match client_event {
                    proto_def::graphical_proxy::client_event::Event::Config(config) => {
                        force_keyframes = config.force_keyframes;
                        full_page_mode = config.full_page;
                        raw_rgb_mode = config.raw_rgb;
                        println!("Client updated stream config - force_keyframes: {}, full_page: {}, raw_rgb: {}", 
                            force_keyframes, full_page_mode, raw_rgb_mode);
                    }
                    proto_def::graphical_proxy::client_event::Event::Navigate(navigate) => {
                        println!("Navigating to: {}", navigate.url);
                        CdpStream::navigate_to(&page, &navigate.url).await?;
                        tokio::time::sleep(Duration::from_secs(2)).await;
                        
                        // Send updated metadata
                        let metadata = extract_spatial_metadata(&page).await?;
                        let spatial_update = ServerUpdate {
                            update: Some(proto_def::graphical_proxy::server_update::Update::SpatialData(SpatialMetadata {
                                title: metadata.title,
                                document_width: metadata.document_width,
                                document_height: metadata.document_height,
                                links: metadata.links.into_iter().map(|link| proto_def::graphical_proxy::LinkBox {
                                    id: link.id,
                                    url: link.url,
                                    label: link.label,
                                    x: link.x,
                                    y: link.y,
                                    width: link.width,
                                    height: link.height,
                                }).collect(),
                            })),
                        };
                        
                        if tx.send(Ok(spatial_update)).await.is_err() {
                            return Ok(());
                        }
                    }
                    proto_def::graphical_proxy::client_event::Event::Scroll(scroll) => {
                        let scroll_start_time = std::time::Instant::now();
                        let old_viewport_y = current_viewport_y;
                        
                        // Always use absolute positioning
                        current_viewport_y = scroll.absolute_y.max(0);
                        info!("[Server] Scroll event received: absolute_y={}, old_viewport={}, new_viewport={}", 
                            scroll.absolute_y, old_viewport_y, current_viewport_y);
                        
                        // Execute instant scroll in browser and return actual clamped scroll position
                        let scroll_script = format!(
                            "(() => {{ window.scrollTo({{top: {}, behavior: 'instant'}}); return window.scrollY; }})()", 
                            current_viewport_y
                        );
                        debug!("[Server] Executing scroll script: {}", scroll_script);
                        
                        let browser_scroll_start = std::time::Instant::now();
                        if let Ok(res) = page.evaluate(scroll_script.as_str()).await {
                            if let Ok(actual_y) = res.into_value::<f64>() {
                                current_viewport_y = actual_y as i32;
                            }
                        }
                        let browser_scroll_time = browser_scroll_start.elapsed();
                        info!("[Server] Browser scroll execution took: {:?}", browser_scroll_time);
                        
                        // Question the 100ms delay - TESTING: Set to 0ms to measure impact
                        let delay_start = std::time::Instant::now();
                        tokio::time::sleep(Duration::from_millis(0)).await;
                        let delay_time = delay_start.elapsed();
                        info!("[Server] Fixed 0ms delay completed: {:?}", delay_time);
                        
                        let total_scroll_time = scroll_start_time.elapsed();
                        info!("[Server] Total scroll processing time: {:?}", total_scroll_time);
                        info!("[Server] Browser scroll completed, viewport now at: {}", current_viewport_y);
                        
                        // Update last scroll time for correlation with frame captures
                        last_scroll_time = std::time::Instant::now();
                    }
                    _ => {}
                }
            }
        }
        Ok(Some(Err(e))) => {
            warn!("[Server] Client event error: {:?}", e);
        }
        Ok(None) => {
            info!("[Server] Client disconnected");
            return Ok(());
        }
        Err(_) => {
            // Timeout - no event available, continue with frame capture
            info!("[Server] No client event available, continuing with frame capture");
        }
        }
        
        info!("[Server] Proceeding with frame capture...");
        
        // Extract timestamp before screenshot for latency measurement
        let timestamp_text = if let Ok(result) = page.evaluate("document.getElementById('timestamp').textContent").await {
            if let Ok(full_text) = result.into_value::<String>() {
                // Extract Unix timestamp between delimiters
                if let Some(start) = full_text.find("__TIMESTAMP_START__") {
                    if let Some(end) = full_text.find("__TIMESTAMP_END__") {
                        let timestamp_part = &full_text[start + 19..end]; // 19 = len("__TIMESTAMP_START__")
                        info!("[Server] Extracted timestamp: {}", timestamp_part);
                        Some(timestamp_part.to_string())
                    } else {
                        info!("[Server] Extracted timestamp: {}", full_text);
                        Some(full_text)
                    }
                } else {
                    info!("[Server] Extracted timestamp: {}", full_text);
                    Some(full_text)
                }
            } else {
                None
            }
        } else {
            None
        };
        
        // Capture screenshot from browser
        let screenshot_start = std::time::Instant::now();
        debug!("[Server] Capturing screenshot...");
        let screenshot = CdpStream::capture_screenshot(&page, full_page_mode).await?;
        let screenshot_time = screenshot_start.elapsed();
        info!("[Server] Screenshot captured: {}x{} in {:?}", screenshot.width(), screenshot.height(), screenshot_time);
        
        // Track if this frame was triggered by a recent scroll event
        let time_since_last_scroll = screenshot_start.duration_since(last_scroll_time);
        if time_since_last_scroll < Duration::from_millis(500) {
            info!("[Server] This screenshot appears to be scroll-related (captured {:?} after scroll processing)", time_since_last_scroll);
        }
        
        // Handle raw RGB mode or AV1 encoding
        if raw_rgb_mode {
            info!("[Server] Using raw RGB transmission mode");
            
            // Convert RGBA to RGB (strip alpha channel)
            let rgb_start = std::time::Instant::now();
            let mut rgb_data = Vec::with_capacity(screenshot.width() as usize * screenshot.height() as usize * 3);
            for pixel in screenshot.as_raw().chunks_exact(4) {
                rgb_data.extend_from_slice(&[pixel[0], pixel[1], pixel[2]]); // R, G, B only
            }
            let rgb_time = rgb_start.elapsed();
            info!("[Server] RGB conversion completed in {:?}", rgb_time);
            
            // Send raw RGB frame
            let update = ServerUpdate {
                update: Some(proto_def::graphical_proxy::server_update::Update::Frame(VideoFrame {
                    av1_data: Vec::new(), // Empty when using raw RGB
                    is_keyframe: true, // Always keyframe in raw mode
                    viewport_x: if full_page_mode { 0 } else { 0 },
                    viewport_y: if full_page_mode { 0 } else { current_viewport_y },
                    raw_rgb_data: rgb_data,
                    frame_width: screenshot.width(),
                    frame_height: screenshot.height(),
                    timestamp_text: timestamp_text.unwrap_or_default(),
                })),
            };
            
            if tx.send(Ok(update)).await.is_err() {
                error!("[Server] Failed to send raw RGB frame to client");
                return Ok(());
            }
            info!("[Server] Raw RGB frame sent successfully");
        } else {
            // Original AV1 encoding path
            info!("[Server] Using AV1 encoding mode");
            
            // Convert RGBA to YUV using core-utils
        let yuv_start = std::time::Instant::now();
        debug!("[Server] Converting to YUV...");
        let yuv_image = core_utils::rgba_to_yuv420(&screenshot, width as u32, height as u32)
            .map_err(|e| format!("YUV conversion error: {:?}", e))?;
        let yuv_time = yuv_start.elapsed();
        debug!("[Server] YUV conversion complete in {:?}", yuv_time);

        let mut packet_count = 0;
        let mut found_packet = false;
        let mut pump_iterations = 0;

        loop {
            info!("[Server] Creating AV1 frame...");
            let mut frame = ctx.new_frame();
            frame.planes[0].copy_from_raw_u8(&yuv_image.y, yuv_image.y_stride as usize, 1);
            frame.planes[1].copy_from_raw_u8(&yuv_image.u, yuv_image.u_stride as usize, 1);
            frame.planes[2].copy_from_raw_u8(&yuv_image.v, yuv_image.v_stride as usize, 1);
            
            debug!("[Server] Sending frame to encoder...");
            let encode_start = std::time::Instant::now();
            match ctx.send_frame(frame) {
                Ok(_) => {
                    let send_time = encode_start.elapsed();
                    trace!("[Server] Frame sent to encoder in {:?}", send_time);
                    // Store the current state to guarantee metadata perfectly aligns with the encoded image
                    viewport_y_queue.push_back(current_viewport_y);
                },
                Err(e) => {
                    warn!("[Server] Send frame error: {:?}", e);
                }
            }
            
            info!("[Server] Receiving encoded packets...");
            let receive_start = std::time::Instant::now();
            
            // Try to receive packets with proper EncoderStatus handling
            loop {
                match ctx.receive_packet() {
                    Ok(packet) => {
                        found_packet = true;
                        let receive_time = receive_start.elapsed();
                        // Pop the viewport_y belonging exactly to the frame that finished encoding
                        let packet_viewport_y = viewport_y_queue.pop_front().unwrap_or(current_viewport_y);
                        
                        // Calculate compression metrics
                        let original_size = width * height * 3; // YUV420 approx 3 bytes per pixel
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
                                frame_width: width as u32,
                                frame_height: height as u32,
                                timestamp_text: timestamp_text.clone().unwrap_or_default(),
                            })),
                        };
                        trace!("[Server] Sending frame {} with viewport_y={}", packet_count, packet_viewport_y);
                        if tx.send(Ok(update)).await.is_err() {
                            error!("[Server] Failed to send frame {} to client", packet_count);
                            break;
                        }
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

            if found_packet || !pump_encoder_mode {
                break;
            }

            pump_iterations += 1;
            info!("[Server] Pumping encoder: no packets received, sending copy of frame (iteration {})", pump_iterations);
        } // outer loop
        
        if !found_packet {
            warn!("[Server] No packets received from encoder - check low_latency configuration!");
        }
        
        info!("[Server] No more packets, frame {} complete. Sent {} packets", frame_count, packet_count);
        } // End of AV1 encoding else block

        frame_count += 1;
        
        // Check if we've reached the maximum frame limit for testing
        if let Some(max_frames) = config.max_frames {
            if frame_count >= max_frames {
                info!("[Server] Reached maximum frame limit ({}), shutting down", max_frames);
                break Ok(());
            }
        }
        
        info!("[Server] Sleeping for {}ms (configured loop delay)", config.loop_delay);
        tokio::time::sleep(Duration::from_millis(config.loop_delay)).await;
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    
    match cli.command {
        Some(Commands::TestScreencast { url, output }) => {
            init_logging(&cli.log_level);
            info!("Testing Chrome extraction for URL: {}", url);
            test_chrome_extraction(&url, output.as_deref()).await?;
        }
        Some(Commands::Serve { 
            quantizer, min_quantizer, speed_preset, threads, tile_cols, tile_rows,
            full_page, raw_rgb, loop_delay, disable_low_latency, disable_still_picture,
            rdo_lookahead_frames, min_keyint, keyint, switch_frame_interval,
            reservoir_frame_delay, disable_error_resilient, max_frames, pump_encoder 
        }) => {
            let config = ServerConfig {
                log_level: cli.log_level.clone(),
                quantizer,
                min_quantizer,
                speed_preset,
                threads,
                tile_cols,
                tile_rows,
                full_page,
                raw_rgb,
                loop_delay,
                disable_low_latency,
                disable_still_picture,
                rdo_lookahead_frames,
                min_keyint,
                keyint,
                switch_frame_interval,
                reservoir_frame_delay,
                disable_error_resilient,
                max_frames,
                pump_encoder,
            };
            
            init_logging(&config.log_level);
            let addr = "[::1]:50051".parse()?;
            let browser_service = RemoteBrowserImpl::new(config);

            info!("Starting gRPC server at {}", addr);

            Server::builder()
                .add_service(RemoteBrowserServer::new(browser_service))
                .serve(addr)
                .await?;
        }
        None => {
            init_logging(&cli.log_level);
            let config = ServerConfig::default();
            let addr = "[::1]:50051".parse()?;
            let browser_service = RemoteBrowserImpl::new(config);

            info!("Starting gRPC server at {}", addr);

            Server::builder()
                .add_service(RemoteBrowserServer::new(browser_service))
                .serve(addr)
                .await?;
        }
    }

    Ok(())
}

async fn test_chrome_extraction(url: &str, output_path: Option<&str>) -> Result<(), Box<dyn std::error::Error>> {
    println!("Starting Chrome extraction test...");
    
    // Initialize Chrome runner
    let mut chrome = ChromeRunner::new(true).await?;
    let page = chrome.new_page().await?;
    
    println!("Navigating to URL: {}", url);
    CdpStream::navigate_to(&page, url).await?;
    
    println!("Capturing screenshot...");
    let screenshot = CdpStream::capture_screenshot(&page, false).await?;
    
    // Save screenshot if output path provided
    if let Some(output) = output_path {
        screenshot.save(output)?;
        println!("Screenshot saved to: {}", output);
    }
    
    println!("Extracting spatial metadata...");
    let metadata = extract_spatial_metadata(&page).await?;
    
    println!("Page Title: {}", metadata.title);
    println!("Document Size: {}x{}", metadata.document_width, metadata.document_height);
    println!("Found {} links:", metadata.links.len());
    
    for (i, link) in metadata.links.iter().take(10).enumerate() {
        println!("  {}. {} at ({},{}) size {}x{}", 
                 i + 1, 
                 link.label.chars().take(30).collect::<String>(),
                 link.x, link.y, link.width, link.height);
    }
    
    if metadata.links.len() > 10 {
        println!("  ... and {} more links", metadata.links.len() - 10);
    }
    
    // Save metadata as JSON
    let metadata_manual = serde_json::json!({
        "title": metadata.title,
        "document_width": metadata.document_width,
        "document_height": metadata.document_height,
        "links": metadata.links.iter().map(|link| {
            serde_json::json!({
                "id": link.id,
                "url": link.url,
                "label": link.label,
                "x": link.x,
                "y": link.y,
                "width": link.width,
                "height": link.height
            })
        }).collect::<Vec<_>>()
    });
    
    let metadata_json = serde_json::to_string_pretty(&metadata_manual)?;
    let json_path = output_path
        .map(|p| format!("{}.json", &p[..p.rfind('.').unwrap_or(p.len())]))
        .unwrap_or_else(|| "metadata.json".to_string());
    
    std::fs::write(&json_path, metadata_json)?;
    println!("Metadata saved to: {}", json_path);
    
    // Clean up
    chrome.close().await?;
    
    println!("Chrome extraction test completed successfully!");
    Ok(())
}
