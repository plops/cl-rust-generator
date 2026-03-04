use std::sync::{Arc, Mutex};
use futures::StreamExt;
use log::{info, warn, error, debug};

use proto_def::graphical_proxy::{ClientEvent, server_update};

use crate::state::ClientState;
use super::ConnectionManager;
use super::StreamHandler;

pub async fn start_grpc_client(
    state: Arc<Mutex<ClientState>>,
    mut rx_events: tokio::sync::mpsc::UnboundedReceiver<ClientEvent>,
    cli: &crate::ClientCli
) {
    // Establish connection using ConnectionManager
    let mut connection_manager = match ConnectionManager::connect().await {
        Ok(cm) => cm,
        Err(e) => {
            error!("Failed to establish connection: {:?}", e);
            return;
        }
    };
    
    // Create stream using ConnectionManager
    let mut stream = match connection_manager.create_stream(rx_events, cli).await {
        Ok(stream) => stream,
        Err(e) => {
            error!("Failed to create stream: {:?}", e);
            return;
        }
    };
    
    info!("Stream established. Waiting for packets...");
    
    while let Some(item) = stream.next().await {
        match item {
            Ok(update) => {
                debug!("Received server update");
                match update.update {
                    Some(server_update::Update::Frame(frame)) => {
                        if let Err(e) = StreamHandler::handle_video_frame(&frame, &state) {
                            error!("Failed to handle video frame: {:?}", e);
                        }
                    }
                    Some(server_update::Update::SpatialData(metadata)) => {
                        if let Err(e) = StreamHandler::handle_spatial_metadata(&metadata, &state) {
                            error!("Failed to handle spatial metadata: {:?}", e);
                        }
                    }
                    Some(server_update::Update::Status(status)) => {
                        if let Err(e) = StreamHandler::handle_status_message(&status, &state) {
                            error!("Failed to handle status message: {:?}", e);
                        }
                    }
                    _ => {
                        debug!("Received unknown update type");
                    }
                }
            }
            Err(e) => {
                error!("Stream error: {:?}", e);
                break;
            }
        }
    }
    
    info!("Stream ended, client disconnecting");
}
