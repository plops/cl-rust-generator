use std::pin::Pin;
use tokio::sync::mpsc;
use tokio_stream::{Stream, StreamExt, wrappers::ReceiverStream};
use tonic::{transport::Server, Request, Response, Status};

use proto_def::graphical_proxy::{
    remote_browser_server::{RemoteBrowser, RemoteBrowserServer},
    ClientEvent, ServerUpdate, VideoFrame,
};

use rav1e::prelude::*;
use image::{RgbaImage, Rgba};
use std::time::Duration;

pub struct RemoteBrowserImpl {}

#[tonic::async_trait]
impl RemoteBrowser for RemoteBrowserImpl {
    type StreamSessionStream = Pin<Box<dyn Stream<Item = Result<ServerUpdate, Status>> + Send>>;

    async fn stream_session(
        &self,
        request: Request<tonic::Streaming<ClientEvent>>,
    ) -> Result<Response<Self::StreamSessionStream>, Status> {
        let mut client_stream = request.into_inner();
        let (tx, rx) = mpsc::channel(128);

        // Background task to read client events
        tokio::spawn(async move {
            while let Some(Ok(event)) = client_stream.next().await {
                println!("Received client event: {:?}", event);
            }
        });

        // Background task to send mock AV1 frames
        tokio::spawn(async move {
            if let Err(e) = run_mock_encoder(tx).await {
                eprintln!("Mock encoder error: {:?}", e);
            }
        });

        let output_stream = ReceiverStream::new(rx);
        Ok(Response::new(Box::pin(output_stream)))
    }
}

async fn run_mock_encoder(tx: mpsc::Sender<Result<ServerUpdate, Status>>) -> Result<(), Box<dyn std::error::Error>> {
    let width = 1280;
    let height = 720;
    
    // Configure rav1e
    let mut enc = EncoderConfig::default();
    enc.width = width;
    enc.height = height;
    enc.bit_depth = 8;
    enc.chroma_sampling = ChromaSampling::Cs420;
    enc.speed_settings = SpeedSettings::from_preset(10);
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(2);
    let mut ctx: Context<u8> = cfg.new_context().map_err(|e| format!("Encoder error: {:?}", e))?;
    
    let mut frame_count = 0;
    loop {
        // Generate mock frame (rotating rectangle)
        let mut img = RgbaImage::new(width as u32, height as u32);
        let angle = (frame_count as f32 * 0.1) % (2.0 * std::f32::consts::PI);
        let rect_x = (width as f32 / 2.0 + 200.0 * angle.cos()) as u32;
        let rect_y = (height as f32 / 2.0 + 200.0 * angle.sin()) as u32;

        for x in 0..100 {
            for y in 0..100 {
                if rect_x + x < width as u32 && rect_y + y < height as u32 {
                    img.put_pixel(rect_x + x, rect_y + y, Rgba([255, 0, 0, 255]));
                }
            }
        }

        // Convert RGBA to YUV using core-utils
        let yuv_image = core_utils::rgba_to_yuv420(&img, width as u32, height as u32)
            .map_err(|e| format!("YUV conversion error: {:?}", e))?;

        let mut frame = ctx.new_frame();
        frame.planes[0].copy_from_raw_u8(&yuv_image.y, yuv_image.y_stride as usize, 1);
        frame.planes[1].copy_from_raw_u8(&yuv_image.u, yuv_image.u_stride as usize, 1);
        frame.planes[2].copy_from_raw_u8(&yuv_image.v, yuv_image.v_stride as usize, 1);
        
        ctx.send_frame(frame).map_err(|e| format!("Send frame error: {:?}", e))?;
        
        while let Ok(packet) = ctx.receive_packet() {
            let update = ServerUpdate {
                update: Some(proto_def::graphical_proxy::server_update::Update::Frame(VideoFrame {
                    av1_data: packet.data,
                    is_keyframe: packet.frame_type == FrameType::KEY,
                    viewport_x: 0,
                    viewport_y: 0,
                })),
            };
            if tx.send(Ok(update)).await.is_err() {
                return Ok(()); // Client disconnected
            }
        }

        frame_count += 1;
        tokio::time::sleep(Duration::from_millis(33)).await; // 30 FPS
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:50051".parse()?;
    let browser_service = RemoteBrowserImpl {};

    println!("Starting gRPC server at {}", addr);

    Server::builder()
        .add_service(RemoteBrowserServer::new(browser_service))
        .serve(addr)
        .await?;

    Ok(())
}
