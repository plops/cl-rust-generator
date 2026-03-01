use iced::futures::StreamExt;
use iced_futures::subscription::{self, Subscription};
use futures::stream::unfold;
use tonic::Request;
use tokio::sync::mpsc as tokio_mpsc;
use tokio_stream::wrappers::ReceiverStream;

use proto_def::graphical_proxy::{
    remote_browser_client::RemoteBrowserClient,
    ClientEvent, ServerUpdate, SpatialMetadata, VideoFrame,
    client_event, StreamConfig, ScrollInput, NavigateRequest,
    server_update,
};

#[derive(Debug, Clone)]
pub enum GrpcMessage {
    Frame(Vec<u8>, FrameMetadata),
    SpatialMetadata(SpatialMetadata),
    Status(String),
    Connected,
    Disconnected,
    Error(String),
}

#[derive(Debug, Clone)]
pub struct FrameMetadata {
    pub width: u32,
    pub height: u32,
    pub viewport_x: i32,
    pub viewport_y: i32,
    pub is_keyframe: bool,
}

impl From<VideoFrame> for FrameMetadata {
    fn from(frame: VideoFrame) -> Self {
        Self {
            width: 1280, // Default, will be updated by decoder
            height: 720,
            viewport_x: frame.viewport_x,
            viewport_y: frame.viewport_y,
            is_keyframe: frame.is_keyframe,
        }
    }
}

pub struct GrpcSubscription {
    // For sending events to server
    event_sender: Option<tokio_mpsc::Sender<ClientEvent>>,
}

impl GrpcSubscription {
    pub fn subscription() -> Subscription<Result<GrpcMessage, String>> {
        Subscription::run_with_id("grpc_stream", async {
            unfold(State::Connecting, |state| async move {
                match state {
                    State::Connecting => connect_to_server().await,
                    State::Connected { mut stream, event_sender } => {
                        match handle_stream(&mut stream, &event_sender).await {
                            Ok(Some(message)) => (Some(Ok(message)), State::Connected { stream, event_sender }),
                            Ok(None) => (Some(Ok(GrpcMessage::Disconnected)), State::Connecting),
                            Err(error) => (Some(Err(error)), State::Connecting),
                        }
                    }
                }
            }).flatten()
        })
    }
    
    pub fn send_scroll_event(&mut self, delta_y: i32) {
        if let Some(ref mut sender) = self.event_sender {
            let scroll_event = ClientEvent {
                event: Some(client_event::Event::Scroll(ScrollInput {
                    delta_y: -delta_y,
                })),
            };
            
            if let Err(e) = sender.try_send(scroll_event) {
                eprintln!("[Client] Failed to send scroll event: {}", e);
            }
        }
    }
    
    pub fn send_navigate_event(&mut self, url: String) {
        if let Some(ref mut sender) = self.event_sender {
            let nav_event = ClientEvent {
                event: Some(client_event::Event::Navigate(NavigateRequest {
                    url,
                })),
            };
            
            if let Err(e) = sender.try_send(nav_event) {
                eprintln!("[Client] Failed to send navigate event: {}", e);
            }
        }
    }
}

#[derive(Debug)]
enum State {
    Connecting,
    Connected {
        stream: tonic::Streaming<ServerUpdate>,
        event_sender: tokio_mpsc::Sender<ClientEvent>,
    },
}

async fn connect_to_server() -> (Result<GrpcMessage, String>, State) {
    println!("[Client] Connecting to server...");
    
    match RemoteBrowserClient::connect("http://[::1]:50051").await {
        Ok(mut client) => {
            println!("[Client] Connected to server");
            
            // Create channel for client-to-server communication
            let (tx_events, rx_events) = tokio_mpsc::channel(10);
            
            // Send initial stream configuration
            let config_event = ClientEvent {
                event: Some(client_event::Event::Config(StreamConfig {
                    enable_video: true,
                    enable_spatial_links: true,
                    force_keyframes: true,
                })),
            };
            
            if let Err(e) = tx_events.send(config_event).await {
                return (Err(format!("Failed to send config: {}", e)), State::Connecting);
            }
            
            // Start streaming session
            let request = Request::new(ReceiverStream::new(rx_events));
            match client.stream_session(request).await {
                Ok(response) => {
                    let stream = response.into_inner();
                    (Ok(GrpcMessage::Connected), State::Connected { stream, event_sender: tx_events })
                }
                Err(e) => {
                    let error = format!("Failed to start stream: {}", e);
                    eprintln!("[Client] {}", error);
                    (Err(error), State::Connecting)
                }
            }
        }
        Err(e) => {
            let error = format!("Connection failed: {}", e);
            eprintln!("[Client] {}", error);
            (Err(error), State::Connecting)
        }
    }
}

async fn handle_stream(
    _stream: &mut tonic::Streaming<ServerUpdate>,
    _event_sender: &tokio_mpsc::Sender<ClientEvent>,
) -> Result<Option<GrpcMessage>, String> {
    match _stream.next().await {
        Some(Ok(update)) => {
            match update.update {
                Some(server_update::Update::Frame(frame)) => {
                    let metadata = FrameMetadata::from(frame);
                    Ok(Some(GrpcMessage::Frame(frame.av1_data, metadata)))
                }
                Some(server_update::Update::SpatialData(metadata)) => {
                    Ok(Some(GrpcMessage::SpatialMetadata(metadata)))
                }
                Some(server_update::Update::Status(status)) => {
                    Ok(Some(GrpcMessage::Status(status.message)))
                }
                _ => Ok(None),
            }
        }
        Some(Err(e)) => {
            let error = format!("Stream error: {}", e);
            eprintln!("[Client] {}", error);
            Err(error)
        }
        None => {
            println!("[Client] Stream ended");
            Ok(None)
        }
    }
}

impl Default for GrpcSubscription {
    fn default() -> Self {
        Self {
            event_sender: None,
        }
    }
}
