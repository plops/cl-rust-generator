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
mod encoder;

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
            let session_manager = session::SessionManager::new(client_stream, tx, config_clone);
            if let Err(e) = session_manager.run().await {
                error!("Browser session error: {:?}", e);
            }
            info!("Browser session task ended");
        });

        let output_stream = ReceiverStream::new(rx);
        info!("Stream session established, returning response");
        Ok(Response::new(Box::pin(output_stream)))
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
