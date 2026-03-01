use anyhow::{Result, anyhow};
use rav1e::prelude::*;
use tokio::sync::mpsc;

use proto_def::graphical_proxy::{
    ClientEvent, ServerUpdate, TestFrameResult,
};

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
    
    // Convert to YUV using core-utils (expects raw bytes)
    let yuv_image = core_utils::rgba_to_yuv420(&rgba_data, width, height)
        .map_err(|e| anyhow!("YUV conversion error: {:?}", e))?;
    
    // Configure rav1e encoder for single frame
    let mut enc = EncoderConfig::default();
    enc.width = width as usize;
    enc.height = height as usize;
    enc.bit_depth = 8;
    enc.chroma_sampling = ChromaSampling::Cs420;
    enc.speed_settings = SpeedSettings::from_preset(10);
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(1);
    let mut ctx: Context<u8> = cfg.new_context()
        .map_err(|e| anyhow!("Encoder context creation failed: {:?}", e))?;
    
    // Create frame and send to encoder
    let mut frame = ctx.new_frame();
    frame.planes[0].copy_from_raw_u8(&yuv_image.y, yuv_image.y_stride as usize, 1);
    frame.planes[1].copy_from_raw_u8(&yuv_image.u, yuv_image.u_stride as usize, 1);
    frame.planes[2].copy_from_raw_u8(&yuv_image.v, yuv_image.v_stride as usize, 1);
    
    ctx.send_frame(frame)
        .map_err(|e| anyhow!("Send frame error: {:?}", e))?;
    
    // Flush the encoder to get the packet
    ctx.flush();
    
    // Receive encoded packet
    let mut av1_data = Vec::new();
    let mut packet_count = 0;
    
    // Keep receiving packets until we get data
    while packet_count < 10 { // Limit attempts to avoid infinite loop
        match ctx.receive_packet() {
            Ok(packet) => {
                if !packet.data.is_empty() {
                    av1_data = packet.data;
                    println!("[Server] Successfully encoded packet: {} bytes", av1_data.len());
                    break;
                }
                packet_count += 1;
            }
            Err(e) => {
                println!("[Server] Encoder packet receive error: {:?}", e);
                // Try flushing again
                ctx.flush();
                packet_count += 1;
                if packet_count >= 5 {
                    break;
                }
            }
        }
    }
    
    if av1_data.is_empty() {
        return Err(anyhow!("No encoded data received"));
    }
    
    // Get AVIF-compatible sequence header from rav1e
    let sequence_header = ctx.container_sequence_header();
    
    // Send raw AV1 frame data directly - let the client handle both AVIF and raw AV1
    println!("[Server] Sending raw AV1 frame data: {} bytes", av1_data.len());
    
    // Save AVIF data to file for debugging
    if let Err(e) = std::fs::write("debug_output.avif", &av1_data) {
        println!("[Server] Failed to save debug AVIF: {:?}", e);
    } else {
        println!("[Server] Saved AVIF to debug_output.avif");
    }
    
    // Send test frame result back to client
    let test_result = TestFrameResult {
        av1_data: av1_data, // Send just the raw AV1 frame data
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

