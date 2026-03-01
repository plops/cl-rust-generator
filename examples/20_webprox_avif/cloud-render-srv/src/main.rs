use std::pin::Pin;
use tokio::sync::mpsc;
use tokio_stream::{Stream, StreamExt, wrappers::ReceiverStream};
use tonic::{transport::Server, Request, Response, Status};
use clap::{Parser, Subcommand};
use log::{info, warn, error, debug};

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
fn init_logging() {
    core_utils::logging::init_logging("cloud-render-srv")
        .expect("Failed to initialize logging");
}

#[derive(Parser, Debug)]
#[command(author, version, about = "AV1 Remote Browser Server")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
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

pub struct RemoteBrowserImpl {}

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
        tokio::spawn(async move {
            info!("Spawning browser session task");
            if let Err(e) = run_browser_session(tx, client_stream).await {
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
    tx: mpsc::Sender<Result<ServerUpdate, Status>>,
    mut client_stream: tonic::Streaming<ClientEvent>,
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
    
    // Navigate to a default page
    info!("Navigating to Wikipedia page...");
    CdpStream::navigate_to(&page, "https://en.wikipedia.org/wiki/Rust_(programming_language)").await?;
    
    // Wait for page to load
    tokio::time::sleep(Duration::from_secs(3)).await;
    
    // Capture first screenshot to get actual dimensions
    info!("Capturing initial screenshot...");
    let first_screenshot = CdpStream::capture_screenshot(&page).await?;
    width = first_screenshot.width() as usize;
    height = first_screenshot.height() as usize;
    
    info!("Actual screenshot dimensions: {}x{}", width, height);
    
    // Configure rav1e encoder with actual dimensions
    let mut enc = EncoderConfig::default();
    enc.width = width;
    enc.height = height;
    enc.bit_depth = 8;
    enc.chroma_sampling = ChromaSampling::Cs420;
    enc.speed_settings = SpeedSettings::from_preset(10);
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(2);
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
                        current_viewport_y = (current_viewport_y + scroll.delta_y).max(0);
                        // Execute scroll in browser
                        let scroll_script = format!("window.scrollTo(0, {});", current_viewport_y);
                        let _ = page.evaluate(scroll_script.as_str()).await?;
                        tokio::time::sleep(Duration::from_millis(100)).await;
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
        println!("[Server] Capturing screenshot...");
        let screenshot = CdpStream::capture_screenshot(&page).await?;
        println!("[Server] Screenshot captured: {}x{}", screenshot.width(), screenshot.height());
        
        // Convert RGBA to YUV using core-utils
        println!("[Server] Converting to YUV...");
        let yuv_image = core_utils::rgba_to_yuv420(&screenshot, width as u32, height as u32)
            .map_err(|e| format!("YUV conversion error: {:?}", e))?;
        println!("[Server] YUV conversion complete");

        info!("[Server] Creating AV1 frame...");
        let mut frame = ctx.new_frame();
        frame.planes[0].copy_from_raw_u8(&yuv_image.y, yuv_image.y_stride as usize, 1);
        frame.planes[1].copy_from_raw_u8(&yuv_image.u, yuv_image.u_stride as usize, 1);
        frame.planes[2].copy_from_raw_u8(&yuv_image.v, yuv_image.v_stride as usize, 1);
        
        println!("[Server] Sending frame to encoder...");
        ctx.send_frame(frame).map_err(|e| format!("Send frame error: {:?}", e))?;
        println!("[Server] Frame sent to encoder");
        
        info!("[Server] Receiving encoded packets...");
        // Receive encoded packets
        let mut packet_count = 0;
        let mut found_packet = false;
        
        // Try to receive packets with a timeout
        while let Ok(packet) = ctx.receive_packet() {
            found_packet = true;
            println!("[Server] Sending packet {} with {} bytes", packet_count, packet.data.len());
            let update = ServerUpdate {
                update: Some(proto_def::graphical_proxy::server_update::Update::Frame(VideoFrame {
                    av1_data: packet.data,
                    is_keyframe: force_keyframes || packet.frame_type == FrameType::KEY,
                    viewport_x: 0,
                    viewport_y: current_viewport_y,
                })),
            };
            if tx.send(Ok(update)).await.is_err() {
                println!("[Server] Client disconnected");
                return Ok(()); // Client disconnected
            }
            packet_count += 1;
        }
        
        if !found_packet {
            warn!("[Server] No packets received from encoder - this may be normal for early frames");
        }
        
        info!("[Server] No more packets, frame {} complete. Sent {} packets", frame_count, packet_count);

        frame_count += 1;
        info!("[Server] Sleeping for 33ms (30 FPS)...");
        tokio::time::sleep(Duration::from_millis(33)).await; // 30 FPS
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    init_logging();
    
    let cli = Cli::parse();
    
    match cli.command {
        Some(Commands::TestScreencast { url, output }) => {
            info!("Testing Chrome extraction for URL: {}", url);
            test_chrome_extraction(&url, output.as_deref()).await?;
        }
        Some(Commands::Serve) | None => {
            let addr = "[::1]:50051".parse()?;
            let browser_service = RemoteBrowserImpl {};

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
