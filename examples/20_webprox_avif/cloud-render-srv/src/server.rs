use crate::video::encoder::Av1Encoder;
use proto_def::{RemoteBrowser, ClientEvent, ServerUpdate, VideoFrame};
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{Request, Response, Status};
use log::{info, error, debug};
use std::pin::Pin;
use std::time::Duration;
use futures::Stream;

pub struct RemoteBrowserServer {
    // Session state would go here
}

impl RemoteBrowserServer {
    pub fn new() -> Self {
        Self {}
    }
}

#[tonic::async_trait]
impl RemoteBrowser for RemoteBrowserServer {
    type StreamSessionStream = Pin<Box<dyn Stream<Item = Result<ServerUpdate, Status>> + Send>>;

    async fn StreamSession(
        &self,
        request: Request<tonic::Streaming<ClientEvent>>,
    ) -> Result<Response<Self::StreamSessionStream>, Status> {
        info!("New client session started.");
        let mut in_stream = request.into_inner();
        let (tx, rx) = mpsc::channel(128);

        // TODO: Handle client events (navigation, scroll) in a separate task
        tokio::spawn(async move {
            while let Some(event) = in_stream.message().await.ok().flatten() {
                debug!("Received client event: {:?}", event);
                // In Milestone 1, we mostly ignore these but could log them.
            }
        });

        // The Mock Stream Task (Milestone 1)
        tokio::spawn(async move {
            let width = 1280;
            let height = 720;
            let mut encoder = match Av1Encoder::new(width, height, 8) {
                Ok(enc) => enc,
                Err(e) => {
                    error!("Failed to initialize encoder: {:?}", e);
                    return;
                }
            };

            let mut frame_count = 0;
            let mut interval = tokio::time::interval(Duration::from_millis(33)); // ~30 FPS

            loop {
                interval.tick().await;

                // 1. Generate Mock Image (Moving Rectangle)
                let mut rgba = vec![0u8; width * height * 4];
                let rect_x = (frame_count * 5) % (width - 200);
                let rect_y = (frame_count * 2) % (height - 200);

                for y in rect_y..(rect_y + 200) {
                    for x in rect_x..(rect_x + 200) {
                        let offset = (y * width + x) * 4;
                        rgba[offset] = 255;     // R
                        rgba[offset + 1] = 0;   // G
                        rgba[offset + 2] = 255; // B
                        rgba[offset + 3] = 255; // A
                    }
                }

                // 2. Encode
                match encoder.encode_rgba_frame(&rgba) {
                    Ok(av1_data) => {
                        if !av1_data.is_empty() {
                            let update = ServerUpdate {
                                update: Some(proto_def::server_update::Update::Frame(VideoFrame {
                                    av1_data,
                                    is_keyframe: frame_count % 60 == 0,
                                    viewport_x: 0,
                                    viewport_y: 0,
                                })),
                            };
                            if tx.send(Ok(update)).await.is_err() {
                                break; // Client disconnected
                            }
                        }
                    }
                    Err(e) => {
                        error!("Encoding failed: {:?}", e);
                        break;
                    }
                }

                frame_count += 1;
            }
        });

        let out_stream = ReceiverStream::new(rx);
        Ok(Response::new(Box::pin(out_stream)))
    }
}
