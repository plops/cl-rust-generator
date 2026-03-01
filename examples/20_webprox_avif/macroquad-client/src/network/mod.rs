use crate::state::SharedState;
use crate::video::decoder::Av1Decoder;
use proto_def::{RemoteBrowserClient, ClientEvent, ServerUpdate};
use tokio::sync::mpsc;
use tokio_stream::StreamExt;
use log::{info, error, debug};
use std::sync::Arc;

pub async fn network_loop(
    state: SharedState,
    mut event_rx: mpsc::Receiver<ClientEvent>,
) -> Result<(), Box<dyn std::error::Error>> {
    info!("Network loop starting, connecting to server...");

    let mut client = RemoteBrowserClient::connect("http://127.0.0.1:50051").await?;
    info!("Connected to gRPC server.");

    // Create the stream of client events
    let event_stream = async_stream::stream! {
        while let Some(event) = event_rx.recv().await {
            yield event;
        }
    };

    let mut server_stream = client.StreamSession(event_stream).await?.into_inner();

    let mut decoder = Av1Decoder::new()?;

    while let Some(update) = server_stream.next().await {
        match update {
            Ok(server_update) => {
                if let Some(event) = server_update.update {
                    match event {
                        proto_def::server_update::Update::Frame(frame) => {
                            debug!("Received AV1 frame packet: {} bytes", frame.av1_data.len());
                            
                            // 1. Decode AV1
                            match decoder.decode_av1_packet(&frame.av1_data) {
                                Ok(Some(rgb_data)) => {
                                    // 2. Put into state
                                    let mut lock = state.lock().unwrap();
                                    lock.latest_frame = Some(rgb_data);
                                    lock.server_viewport_y = frame.viewport_y;
                                }
                                Ok(None) => {
                                    // Not enough data for a complete frame yet
                                }
                                Err(e) => {
                                    error!("AV1 decoding failed: {:?}", e);
                                }
                            }
                        }
                        _ => debug!("Unhandled server update type"),
                    }
                }
            }
            Err(e) => {
                error!("gRPC stream error: {:?}", e);
                break;
            }
        }
    }

    Ok(())
}
