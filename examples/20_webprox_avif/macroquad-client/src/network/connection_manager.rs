use tonic::Request;
use tokio::sync::mpsc;
use log::{info, warn, error, debug};

use proto_def::graphical_proxy::{
    remote_browser_client::RemoteBrowserClient,
    ClientEvent,
};

pub struct ConnectionManager {
    client: RemoteBrowserClient<tonic::transport::Channel>,
}

impl ConnectionManager {
    pub async fn connect() -> Result<Self, Box<dyn std::error::Error>> {
        info!("Connecting to server...");
        let client = match RemoteBrowserClient::connect("http://[::1]:50051").await {
            Ok(c) => {
                info!("Successfully connected to server");
                c
            }
            Err(e) => {
                error!("Connection failed: {:?}", e);
                return Err(format!("Connection failed: {:?}", e).into());
            }
        };
        
        Ok(Self { client })
    }
    
    pub async fn create_stream(
        &mut self,
        mut rx_events: tokio::sync::mpsc::UnboundedReceiver<ClientEvent>,
        cli: &crate::ClientCli
    ) -> Result<tonic::Streaming<proto_def::graphical_proxy::ServerUpdate>, Box<dyn std::error::Error>> {
        // Use a channel to keep the client-to-server stream open
        let (tx_events, rx_events_internal) = mpsc::channel(10);
        
        // Send initial stream configuration
        info!("Sending initial stream configuration");
        let config_event = ClientEvent {
            event: Some(proto_def::graphical_proxy::client_event::Event::Config(
                proto_def::graphical_proxy::StreamConfig {
                    enable_video: true,
                    enable_spatial_links: true,
                    force_keyframes: true, // Simplified mode for single-frame AVIF decoder
                    full_page: cli.full_page,
                    raw_rgb: cli.raw_rgb,
                }
            ))
        };
        let _ = tx_events.blocking_send(config_event);
        
        // Bridge UI events to internal channel
        tokio::spawn(async move {
            info!("Starting UI event bridge");
            while let Some(event) = rx_events.recv().await {
                debug!("Forwarding UI event to server: {:?}", event.event);
                if let Err(e) = tx_events.send(event).await {
                    error!("Failed to forward UI event: {:?}", e);
                    break;
                }
            }
            info!("UI event bridge ended");
        });
        
        let request = Request::new(tokio_stream::wrappers::ReceiverStream::new(rx_events_internal));
        let stream = self.client.stream_session(request).await?.into_inner();
        
        Ok(stream)
    }
}
