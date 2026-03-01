use anyhow::{Result, anyhow};
use tokio::sync::mpsc;
use futures::StreamExt;
use tonic::Request;

use proto_def::graphical_proxy::{
    remote_browser_client::RemoteBrowserClient,
    ClientEvent, server_update, client_event,
};

use crate::integration_test::{
    TestImageData, load_test_image, compare_images, report_results, 
    TestOutcome, ComparisonResult
};

pub async fn run_integration_test(
    test_image_path: &std::path::Path,
    server_addr: &str,
    tolerance: f32,
    output_result: Option<&std::path::Path>,
) -> Result<()> {
    println!("[Client] Starting integration test");
    println!("  Test image: {}", test_image_path.display());
    println!("  Server: {}", server_addr);
    println!("  Tolerance: {:.2}", tolerance);
    
    // Load test image
    let test_data = load_test_image(test_image_path)?;
    let original_rgb = test_data.rgb_data.clone();
    
    // Connect to server
    println!("[Client] Connecting to server...");
    let mut client = RemoteBrowserClient::connect(format!("http://{}", server_addr)).await
        .map_err(|e| anyhow!("Failed to connect to server: {}", e))?;
    
    println!("[Client] Connected successfully");
    
    // Setup gRPC streaming
    let (tx_events, rx_events) = mpsc::channel(10);
    let request = Request::new(tokio_stream::wrappers::ReceiverStream::new(rx_events));
    let mut stream = client.stream_session(request).await
        .map_err(|e| anyhow!("Failed to establish stream: {}", e))?
        .into_inner();
    
    // Send test image data
    println!("[Client] Sending test image data...");
    let test_image_event = ClientEvent {
        event: Some(client_event::Event::TestImage(
            proto_def::graphical_proxy::RawImageData {
                width: test_data.width,
                height: test_data.height,
                rgb_data: test_data.rgb_data,
            }
        )),
    };
    
    if tx_events.send(test_image_event).await.is_err() {
        return Err(anyhow!("Failed to send test image event"));
    }
    
    println!("[Client] Waiting for AVIF response...");
    
    // Wait for test frame result
    let mut avif_data = None;
    
    // Use a simple timeout approach
    let start_time = std::time::Instant::now();
    let timeout_duration = std::time::Duration::from_secs(30);
    
    while avif_data.is_none() && start_time.elapsed() < timeout_duration {
        match stream.next().await {
            Some(Ok(update)) => {
                match update.update {
                    Some(server_update::Update::TestFrame(test_frame)) => {
                        println!("[Client] Received test frame: {} bytes", test_frame.av1_data.len());
                        avif_data = Some(test_frame.av1_data);
                        break;
                    }
                    Some(server_update::Update::Status(status)) => {
                        println!("[Client] Server status: {}", status.message);
                        if status.message.contains("error") {
                            return Err(anyhow!("Server reported error: {}", status.message));
                        }
                    }
                    _ => {
                        println!("[Client] Received unexpected update type");
                    }
                }
            }
            Some(Err(e)) => {
                return Err(anyhow!("Stream error: {}", e));
            }
            None => {
                return Err(anyhow!("Stream ended unexpectedly"));
            }
        }
    }
    
    if avif_data.is_none() {
        return Err(anyhow!("Timeout waiting for test frame response"));
    }
    
    // Decode AVIF data
    let avif_data = avif_data.ok_or_else(|| anyhow!("No AVIF data received"))?;
    println!("[Client] Decoding AVIF data...");
    
    let decoded_image = decode_avif(&avif_data).await
        .map_err(|e| anyhow!("AVIF decoding failed: {}", e))?;
    
    println!("[Client] AVIF decoded successfully: {}x{}", decoded_image.width(), decoded_image.height());
    
    // Compare images
    println!("[Client] Comparing images...");
    let comparison = compare_images(
        &original_rgb,
        &decoded_image.into_raw(),
        test_data.width,
        test_data.height,
        tolerance,
    )?;
    
    // Report results
    let outcome = if comparison.passed {
        TestOutcome::Passed
    } else {
        TestOutcome::Failed(format!(
            "Image comparison failed: {}/{} pixels differ, max difference: {:.2}",
            comparison.different_pixels,
            comparison.total_pixels,
            comparison.max_difference
        ))
    };
    
    report_results(
        outcome,
        Some(comparison),
        &test_image_path.to_string_lossy(),
        server_addr,
        output_result,
    )?;
    
    Ok(())
}

async fn decode_avif(avif_data: &[u8]) -> Result<image::RgbaImage> {
    use aom_decode::{avif::{Avif, Image}, Config};
    
    // First try to decode as AVIF (with proper container)
    let config = Config {
        threads: std::thread::available_parallelism()?.get(),
    };
    
    match Avif::decode(avif_data, &config) {
        Ok(mut avif) => {
            println!("[Client] Decoded as proper AVIF");
            // Convert to RGB image
            let image = avif.convert()
                .map_err(|e| anyhow!("AVIF color conversion error: {}", e))?;
            
            // Convert to RGBA image buffer
            let rgba_image = match image {
                Image::RGB8(img) => {
                    println!("[Client] Converting RGB8 to RGBA");
                    let mut rgba_data = Vec::with_capacity(img.width() * img.height() * 4);
                    for pixel in img.buf() {
                        rgba_data.extend_from_slice(&[pixel.r, pixel.g, pixel.b, 255]);
                    }
                    image::RgbaImage::from_raw(img.width() as u32, img.height() as u32, rgba_data)
                        .ok_or_else(|| anyhow!("Failed to create RGBA image from RGB"))?
                }
                Image::RGBA8(img) => {
                    println!("[Client] Using RGBA8 directly");
                    let rgba_data: Vec<u8> = img.buf().iter()
                        .flat_map(|pixel| [pixel.r, pixel.g, pixel.b, pixel.a])
                        .collect();
                    image::RgbaImage::from_raw(img.width() as u32, img.height() as u32, rgba_data)
                        .ok_or_else(|| anyhow!("Failed to create RGBA image"))?
                }
                other => {
                    return Err(anyhow!("Unsupported image format"));
                }
            };
            Ok(rgba_image)
        }
        Err(e) => {
            println!("[Client] AVIF decode failed: {}, trying raw AV1...", e);
            // Try to decode as raw AV1 data
            decode_raw_av1(avif_data, &config).await
        }
    }
}

async fn decode_raw_av1(av1_data: &[u8], config: &aom_decode::Config) -> Result<image::RgbaImage> {
    use aom_decode::{Decoder, FrameTempRef, RowsIters};
    use aom_decode::color;
    use yuv::convert::RGBConvert;
    use yuv::YUV;
    
    let mut decoder = Decoder::new(config)?;
    let frame = decoder.decode_frame(av1_data)
        .map_err(|e| anyhow!("Raw AV1 decode failed: {}", e))?;
    
    println!("[Client] Decoded raw AV1 frame");
    
    let range = frame.range();
    let rgba_image = match frame.rows_iter()? {
        RowsIters::YuvPlanes8 {y, u, v, chroma_sampling} => {
            println!("[Client] Converting YUV420 to RGBA");
            let mc = frame.matrix_coefficients().unwrap_or(color::MatrixCoefficients::BT709);
            let conv = RGBConvert::<u8>::new(range, mc)?;
            let width = y.width();
            let height = y.height();
            
            let mut rgba_data = Vec::with_capacity(width * height * 4);
            
            // Collect all YUV data first
            let y_data: Vec<u8> = y.flat_map(|row| row.iter().copied()).collect();
            let u_data: Vec<u8> = u.flat_map(|row| row.iter().copied()).collect();
            let v_data: Vec<u8> = v.flat_map(|row| row.iter().copied()).collect();
            
            // Simple chroma subsampling handling for YUV420
            for row_idx in 0..height {
                for col_idx in 0..width {
                    let y_idx = row_idx * width + col_idx;
                    let y_val = y_data[y_idx];
                    
                    // Chroma subsampling: U and V are half resolution
                    let u_idx = (row_idx / 2) * (width / 2) + (col_idx / 2);
                    let v_idx = (row_idx / 2) * (width / 2) + (col_idx / 2);
                    
                    let u_val = u_data.get(u_idx).copied().unwrap_or(128);
                    let v_val = v_data.get(v_idx).copied().unwrap_or(128);
                    
                    let yuv = YUV { y: y_val, u: u_val, v: v_val };
                    let rgb = conv.to_rgb(yuv);
                    rgba_data.extend_from_slice(&[rgb.r, rgb.g, rgb.b, 255]);
                }
            }
            
            image::RgbaImage::from_raw(width as u32, height as u32, rgba_data)
                .ok_or_else(|| anyhow!("Failed to create RGBA image from YUV"))?
        }
        RowsIters::Mono8(y) => {
            println!("[Client] Converting grayscale to RGBA");
            let width = y.width();
            let height = y.height();
            let mut rgba_data = Vec::with_capacity(width * height * 4);
            
            for y_val in y.flat_map(|row| row.iter().copied()) {
                rgba_data.extend_from_slice(&[y_val, y_val, y_val, 255]);
            }
            
            image::RgbaImage::from_raw(width as u32, height as u32, rgba_data)
                .ok_or_else(|| anyhow!("Failed to create RGBA image from grayscale"))?
        }
        _other => {
            return Err(anyhow!("Unsupported YUV format"));
        }
    };
    
    Ok(rgba_image)
}
