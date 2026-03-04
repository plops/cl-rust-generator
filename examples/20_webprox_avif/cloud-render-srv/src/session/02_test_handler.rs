use anyhow::{Result, anyhow};
use tokio::sync::mpsc;

use proto_def::graphical_proxy::{
    ClientEvent, ServerUpdate, TestFrameResult,
};

use crate::encoder::rav1e_enc::Rav1eEncoder;
use crate::ServerConfig;

/// Handle RawImageData client events for integration testing
pub async fn handle_raw_image_data(
    width: u32,
    height: u32,
    rgb_data: Vec<u8>,
    tx: &mpsc::Sender<Result<ServerUpdate, tonic::Status>>,
) -> Result<()> {
    println!("[Server] Received test image: {}x{}, {} bytes", width, height, rgb_data.len());
    
    // Validate RGB data size
    let expected_size = (width * height * 3) as usize;
    if rgb_data.len() != expected_size {
        return Err(anyhow!("RGB data size mismatch: expected {}, got {}", expected_size, rgb_data.len()));
    }
    
    // Convert RGB to RGBA (add alpha channel)
    let mut rgba_data = Vec::with_capacity((width * height * 4) as usize);
    for chunk in rgb_data.chunks_exact(3) {
        rgba_data.extend_from_slice(&[chunk[0], chunk[1], chunk[2], 255]); // Add alpha = 255
    }
    
    // Create RGBA image from the data
    let image = image::RgbaImage::from_raw(width, height, rgba_data)
        .ok_or_else(|| anyhow!("Failed to create RGBA image from data"))?;
    
    // Create test-specific encoder configuration
    let mut test_config = ServerConfig::default();
    test_config.quantizer = 150;  // Increased from default 100 to reduce file size
    test_config.min_quantizer = 80;  // Set minimum quality floor
    test_config.speed_preset = 8;  // Balanced speed/quality
    test_config.tile_cols = 1;  // Minimal tiling to avoid errors
    test_config.tile_rows = 1;
    test_config.threads = 1;
    test_config.disable_low_latency = false;
    test_config.disable_still_picture = false;  // Enable still picture mode for test frames
    
    // Create encoder for test frame
    let mut encoder = Rav1eEncoder::new(width as usize, height as usize, &test_config)
        .map_err(|e| anyhow!("Failed to create test encoder: {:?}", e))?;
    
    // Encode the test frame
    let frame_result = encoder.encode_frame(
        &image,
        0,  // viewport_y (not relevant for test frames)
        false,  // full_page_mode (not relevant for test frames)
        true,   // force_keyframes (always keyframe for test frames)
        "test_frame".to_string(),  // timestamp_text
    ).await.map_err(|e| anyhow!("Test frame encoding failed: {:?}", e))?;
    
    // Extract the AV1 data from the frame result
    if frame_result.updates.is_empty() {
        return Err(anyhow!("No encoded data received from test encoder"));
    }
    
    let av1_data = if let Some(proto_def::graphical_proxy::server_update::Update::Frame(video_frame)) = 
        &frame_result.updates[0].update {
        video_frame.av1_data.clone()
    } else {
        return Err(anyhow!("Expected Frame update but got different type"));
    };
    
    println!("[Server] Successfully encoded test frame: {} bytes", av1_data.len());
    
    // Save AVIF data to file for debugging
    if let Err(e) = std::fs::write("debug_output.avif", &av1_data) {
        println!("[Server] Failed to save debug AVIF: {:?}", e);
    } else {
        println!("[Server] Saved AVIF to debug_output.avif");
    }
    
    // Send test frame result back to client
    let test_result = TestFrameResult {
        av1_data: av1_data,
        is_keyframe: true, // Force keyframe for simplified decoding
    };
    
    let update = ServerUpdate {
        update: Some(proto_def::graphical_proxy::server_update::Update::TestFrame(test_result)),
    };
    
    if tx.send(Ok(update)).await.is_err() {
        return Err(anyhow!("Failed to send test result to client"));
    }
    
    println!("[Server] Test frame sent successfully");
    Ok(())
}

/// Handle test image events from client
pub async fn handle_test_image_event(
    event: ClientEvent,
    tx: &mpsc::Sender<Result<ServerUpdate, tonic::Status>>,
) -> Result<bool> {
    println!("[Server] Processing test image event...");
    
    if let Some(proto_def::graphical_proxy::client_event::Event::TestImage(raw_image)) = event.event {
        println!("[Server] Received test image: {}x{}, {} bytes", raw_image.width, raw_image.height, raw_image.rgb_data.len());
        
        let width = raw_image.width;
        let height = raw_image.height;
        let rgb_data = raw_image.rgb_data;
        
        match handle_raw_image_data(width, height, rgb_data, tx).await {
            Ok(()) => {
                println!("[Server] Test image processed successfully");
                Ok(true)
            }
            Err(e) => {
                eprintln!("[Server] Test image handling error: {:?}", e);
                let status_update = ServerUpdate {
                    update: Some(proto_def::graphical_proxy::server_update::Update::Status(
                        proto_def::graphical_proxy::SystemStatus {
                            message: format!("Test image error: {}", e),
                        }
                    )),
                };
                let _ = tx.send(Ok(status_update)).await;
                Ok(true)
            }
        }
    } else {
        println!("[Server] Not a test image event");
        Ok(false) // Not a test image event
    }
}
