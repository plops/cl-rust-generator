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
use image::DynamicImage;
use std::time::Duration;

mod browser;
use browser::{ChromeRunner, CdpStream, extract_spatial_metadata, ExtractedMetadata};

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
    Serve,
}

pub struct RemoteBrowserImpl {
    cli: Cli,
}

impl RemoteBrowserImpl {
    pub fn new(cli: Cli) -> Self {
        Self { cli }
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
        let cli_clone = self.cli.clone();
        tokio::spawn(async move {
            info!("Spawning browser session task");
            if let Err(e) = run_browser_session(client_stream, tx, &cli_clone).await {
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
    cli: &Cli,
) -> Result<(), Box<dyn std::error::Error>> {
    info!("Starting browser session");
    let mut width = 1280;
    let mut height = 720;
    let mut integration_test_mode = false;
    
    // Check first for test image event to determine if we should skip browser initialization
    info!("Waiting for first client event...");
    if let Some(Ok(first_event)) = client_stream.next().await {
        debug!("Received first event: {:?}", first_event.event);
        if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &first_event.event {
            integration_test_mode = true;
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
    let first_screenshot = CdpStream::capture_screenshot(&page).await?;
    width = first_screenshot.width() as usize;
    height = first_screenshot.height() as usize;
    
    info!("Actual screenshot dimensions: {}x{}", width, height);
    
    // Configure rav1e encoder with actual dimensions and CLI parameters
    let mut enc = EncoderConfig::default();
    enc.width = width;
    enc.height = height;
    enc.bit_depth = 8;
    enc.chroma_sampling = ChromaSampling::Cs420;
    
    // Use CLI parameters for encoder configuration
    enc.quantizer = cli.quantizer as usize;
    enc.min_quantizer = cli.min_quantizer;
    enc.speed_settings = SpeedSettings::from_preset(cli.speed_preset as u8);
    enc.tile_cols = cli.tile_cols;
    enc.tile_rows = cli.tile_rows;
    
    info!("Encoder config: quantizer={}, min_quantizer={}, speed_preset={}, tiles={}x{}, threads={}", 
        enc.quantizer, enc.min_quantizer, cli.speed_preset, enc.tile_cols, enc.tile_rows, cli.threads);
    
    // Force keyframe mode for client compatibility
    enc.min_key_frame_interval = 1;  // Every frame is a keyframe
    enc.max_key_frame_interval = 1;  // Force all frames to be keyframes
    enc.low_latency = true;          // Crucial: disables frame reordering to minimize delay
    
    // Real-time streaming settings - disable lookahead for immediate packet output
    enc.speed_settings.rdo_lookahead_frames = 1;  // Minimal lookahead for real-time
    enc.speed_settings.scene_detection_mode = rav1e::prelude::SceneDetectionSpeed::None;  // Disable scene detection
    
    // Additional speed optimizations for real-time
    enc.width = 640;  // Reduce resolution from 800x600 to 640x480 for faster encoding
    enc.height = 480;
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(cli.threads);
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
                        println!("Client updated stream config - force_keyframes: {}", force_keyframes);
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
        
        // Capture screenshot from browser
        let screenshot_start = std::time::Instant::now();
        debug!("[Server] Capturing screenshot...");
        let screenshot = CdpStream::capture_screenshot(&page).await?;
        let screenshot_time = screenshot_start.elapsed();
        info!("[Server] Screenshot captured: {}x{} in {:?}", screenshot.width(), screenshot.height(), screenshot_time);
        
        // Track if this frame was triggered by a recent scroll event
        let time_since_last_scroll = screenshot_start.duration_since(last_scroll_time);
        if time_since_last_scroll < Duration::from_millis(500) {
            info!("[Server] This screenshot appears to be scroll-related (captured {:?} after scroll processing)", time_since_last_scroll);
        }
        
        // Convert RGBA to YUV using core-utils
        let yuv_start = std::time::Instant::now();
        debug!("[Server] Converting to YUV...");
        let yuv_image = core_utils::rgba_to_yuv420(&screenshot, width as u32, height as u32)
            .map_err(|e| format!("YUV conversion error: {:?}", e))?;
        let yuv_time = yuv_start.elapsed();
        debug!("[Server] YUV conversion complete in {:?}", yuv_time);

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
        // Receive encoded packets
        // NOTE: With low_latency=true and minimal lookahead, rav1e should output packets immediately
        // If no packets are received, it indicates a configuration issue
        let mut packet_count = 0;
        let mut found_packet = false;
        
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
                            viewport_x: 0,
                            viewport_y: packet_viewport_y,
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
        }
        
        if !found_packet {
            warn!("[Server] No packets received from encoder - check low_latency configuration!");
        }
        
        info!("[Server] No more packets, frame {} complete. Sent {} packets", frame_count, packet_count);

        frame_count += 1;
        info!("[Server] Sleeping for 2500ms (0.4 FPS) - matching actual encoding speed");
        tokio::time::sleep(Duration::from_millis(2500)).await; // 0.4 FPS to match 2.5s encoding time
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    init_logging(&cli.log_level);
    
    match cli.command {
        Some(Commands::TestScreencast { url, output }) => {
            info!("Testing Chrome extraction for URL: {}", url);
            test_chrome_extraction(&url, output.as_deref()).await?;
        }
        Some(Commands::Serve) | None => {
            let addr = "[::1]:50051".parse()?;
            let browser_service = RemoteBrowserImpl::new(cli.clone());

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
    let screenshot = CdpStream::capture_screenshot(&page).await?;
    
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
