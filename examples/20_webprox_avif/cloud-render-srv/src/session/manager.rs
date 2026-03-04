use log::{info, warn, debug, error};
use tokio_stream::StreamExt;

use proto_def::graphical_proxy::{
    ClientEvent, ServerUpdate, VideoFrame, SpatialMetadata,
};
use tonic::Status;

use crate::browser::{ChromeRunner, CdpStream, extract_spatial_metadata};
use crate::encoder::rav1e_enc::Rav1eEncoder;
use crate::ServerConfig;

pub struct SessionManager {
    client_stream: tonic::Streaming<ClientEvent>,
    tx: tokio::sync::mpsc::Sender<Result<ServerUpdate, Status>>,
    config: ServerConfig,
}

impl SessionManager {
    pub fn new(
        client_stream: tonic::Streaming<ClientEvent>,
        tx: tokio::sync::mpsc::Sender<Result<ServerUpdate, Status>>,
        config: ServerConfig,
    ) -> Self {
        Self {
            client_stream,
            tx,
            config,
        }
    }

    pub async fn run(mut self) -> Result<(), Box<dyn std::error::Error>> {
        info!("Starting browser session");
        let mut _width = 1280;
        let mut _height = 720;
        let mut _integration_test_mode = false;
        
        // Check first for test image event to determine if we should skip browser initialization
        info!("Waiting for first client event...");
        if let Some(Ok(first_event)) = self.client_stream.next().await {
            debug!("Received first event: {:?}", first_event.event);
            if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &first_event.event {
                _integration_test_mode = true;
                info!("Integration test mode detected - skipping browser initialization");
                
                // Handle the test image event
                let _ = crate::session::handle_test_image_event(first_event, &self.tx).await;
                
                // Continue processing events in test mode
                loop {
                    if let Some(Ok(event)) = self.client_stream.next().await {
                        if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &event.event {
                            let _ = crate::session::handle_test_image_event(event, &self.tx).await;
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
        tokio::time::sleep(std::time::Duration::from_secs(3)).await;
        
        // Capture first screenshot to get actual dimensions
        info!("Capturing initial screenshot...");
        let first_screenshot = CdpStream::capture_screenshot(&page, self.config.full_page).await?;
        let width = first_screenshot.width() as usize;
        let height = first_screenshot.height() as usize;
        
        info!("Actual screenshot dimensions: {}x{}", width, height);
        
        // Initialize encoder
        let mut encoder = Rav1eEncoder::new(width, height, &self.config)?;
        
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
        
        if self.tx.send(Ok(spatial_update) as Result<ServerUpdate, Status>).await.is_err() {
            return Ok(()); // Client disconnected
        }
        
        let mut frame_count = 0;
        let mut current_viewport_y = 0i32;
        let mut force_keyframes = true; // Default to simplified mode
        let mut full_page_mode = self.config.full_page; // Get from config
        let mut raw_rgb_mode = self.config.raw_rgb; // Get from config
        let pump_encoder_mode = self.config.pump_encoder; // Get from config
        let mut viewport_y_queue: std::collections::VecDeque<i32> = std::collections::VecDeque::new(); // Tracks exact Y for buffered frames
        let mut last_scroll_time = std::time::Instant::now(); // Track when last scroll was processed
        
        loop {
            info!("[Server] Starting frame {} iteration", frame_count);
            
            // Handle client events (non-blocking with timeout)
            info!("[Server] Checking for client events...");
            match tokio::time::timeout(std::time::Duration::from_millis(10), self.client_stream.next()).await {
                Ok(Some(Ok(event))) => {
                    // Check for test image event first to avoid borrow issues
                    if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(_)) = &event.event {
                        let _ = crate::session::handle_test_image_event(event, &self.tx).await;
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
                                tokio::time::sleep(std::time::Duration::from_secs(2)).await;
                                
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
                                
                                if self.tx.send(Ok(spatial_update) as Result<ServerUpdate, Status>).await.is_err() {
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
                                tokio::time::sleep(std::time::Duration::from_millis(0)).await;
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
            if time_since_last_scroll < std::time::Duration::from_millis(500) {
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
                
                if self.tx.send(Ok(update) as Result<ServerUpdate, Status>).await.is_err() {
                    error!("[Server] Failed to send raw RGB frame to client");
                    return Ok(());
                }
                info!("[Server] Raw RGB frame sent successfully");
            } else {
                // Use encoder for AV1 encoding
                info!("[Server] Using AV1 encoding mode");
                
                let frame_result = encoder.encode_frame(
                    &screenshot, 
                    current_viewport_y, 
                    full_page_mode, 
                    force_keyframes,
                    timestamp_text.clone().unwrap_or_default()
                ).await?;
                
                for update in frame_result.updates {
                    if self.tx.send(Ok(update) as Result<ServerUpdate, Status>).await.is_err() {
                        error!("[Server] Failed to send frame to client");
                        return Ok(());
                    }
                }
            }

            frame_count += 1;
            
            // Check if we've reached the maximum frame limit for testing
            if let Some(max_frames) = self.config.max_frames {
                if frame_count >= max_frames {
                    info!("[Server] Reached maximum frame limit ({}), shutting down", max_frames);
                    break Ok(());
                }
            }
            
            info!("[Server] Sleeping for {}ms (configured loop delay)", self.config.loop_delay);
            tokio::time::sleep(std::time::Duration::from_millis(self.config.loop_delay)).await;
        }
    }
}
