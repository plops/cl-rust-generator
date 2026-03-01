use iced::{
    widget::{canvas, column, container, scrollable, text},
    Element, Length, Subscription, Theme, Command,
};
use iced::widget::image;

mod canvas_renderer;
mod decoder;
mod grpc_client;

use canvas_renderer::VideoCanvas;
use decoder::AvifDecoder;
use grpc_client::{GrpcSubscription, FrameMetadata};

use proto_def::graphical_proxy::{
    SpatialMetadata, LinkBox,
};

#[derive(Debug, Clone)]
enum Message {
    // gRPC events
    FrameReceived(Vec<u8>, FrameMetadata),
    SpatialDataReceived(SpatialMetadata),
    StatusReceived(String),
    
    // User interactions
    Scrolled(i32),
    LinkClicked(String),
    NavigateRequested(String),
    
    // Canvas events
    CanvasRedraw,
    
    // Connection events
    Connected,
    Disconnected,
    Error(String),
}

#[derive(Default)]
struct AvifClient {
    // Video state
    latest_frame: Option<image::Handle>,
    frame_metadata: Option<FrameMetadata>,
    decoder: AvifDecoder,
    
    // Spatial data
    links: Vec<LinkBox>,
    doc_dimensions: (u32, u32),
    page_title: String,
    
    // Scrolling state
    scroll_offset: i32,
    server_viewport: i32,
    
    // Connection state
    is_connected: bool,
    connection_status: String,
    
    // gRPC subscription
    grpc_subscription: Option<GrpcSubscription>,
}

impl AvifClient {
    fn handle_frame_received(&mut self, av1_data: Vec<u8>, metadata: FrameMetadata) {
        match self.decoder.decode_frame(&av1_data) {
            Ok((width, height, rgba_data)) => {
                let updated_metadata = FrameMetadata {
                    width,
                    height,
                    ..metadata
                };
                
                let handle = image::Handle::from_pixels(width, height, rgba_data);
                self.latest_frame = Some(handle);
                self.frame_metadata = Some(updated_metadata);
                self.server_viewport = metadata.viewport_y;
            }
            Err(e) => {
                eprintln!("[Client] AVIF decode failed: {}", e);
                // Fallback to visualization pattern could be implemented here
            }
        }
    }
    
    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::FrameReceived(av1_data, metadata) => {
                self.handle_frame_received(av1_data, metadata);
                Command::request_redraw()
            }
            
            Message::SpatialDataReceived(metadata) => {
                self.page_title = metadata.title;
                self.doc_dimensions = (metadata.document_width, metadata.document_height);
                self.links = metadata.links;
                Command::none()
            }
            
            Message::StatusReceived(status) => {
                self.connection_status = status;
                Command::none()
            }
            
            Message::Scrolled(delta_y) => {
                self.scroll_offset = (self.scroll_offset + delta_y)
                    .clamp(0, self.doc_dimensions.1 as i32);
                
                // Send scroll event to server via subscription
                if let Some(ref mut subscription) = self.grpc_subscription {
                    subscription.send_scroll_event(delta_y);
                }
                
                Command::none()
            }
            
            Message::LinkClicked(url) => {
                if let Some(ref mut subscription) = self.grpc_subscription {
                    subscription.send_navigate_event(url);
                }
                Command::none()
            }
            
            Message::NavigateRequested(url) => {
                if let Some(ref mut subscription) = self.grpc_subscription {
                    subscription.send_navigate_event(url);
                }
                Command::none()
            }
            
            Message::Connected => {
                self.is_connected = true;
                self.connection_status = "Connected".to_string();
                Command::none()
            }
            
            Message::Disconnected => {
                self.is_connected = false;
                self.connection_status = "Disconnected".to_string();
                Command::none()
            }
            
            Message::Error(error) => {
                self.connection_status = format!("Error: {}", error);
                Command::none()
            }
            
            Message::CanvasRedraw => {
                Command::request_redraw()
            }
        }
    }

    fn view(&self) -> Element<Message> {
        let content = column![
            // Video canvas
            canvas(VideoCanvas {
                frame_handle: self.latest_frame.clone(),
                links: self.links.clone(),
                scroll_offset: self.scroll_offset,
                server_viewport: self.server_viewport,
                doc_dimensions: self.doc_dimensions,
            })
            .width(Length::Fill)
            .height(Length::Fill),
            
            // Status overlay
            container(
                column![
                    text(format!("Page: {}", &self.page_title)).size(16),
                    text(format!("Links: {}", self.links.len())).size(14),
                    text(format!("Viewport: {}", self.scroll_offset)).size(14),
                    text(format!("Status: {}", self.connection_status)).size(14),
                ]
            )
            .padding(10)
            .style(container::Style::solid(iced::Color::new(0.0, 0.0, 0.0, 0.7))),
        ];

        scrollable(content)
            .direction(scrollable::Direction::Both(
                scrollable::Scrollbar::new(),
                scrollable::Scrollbar::new(),
            ))
            .into()
    }

    fn subscription(&self) -> Subscription<Message> {
        Subscription::batch(vec![
            // gRPC streaming subscription
            GrpcSubscription::subscription().map(|result| match result {
                Ok(update) => match update {
                    grpc_client::GrpcMessage::Frame(data, metadata) => {
                        Message::FrameReceived(data, metadata)
                    }
                    grpc_client::GrpcMessage::SpatialMetadata(metadata) => {
                        Message::SpatialDataReceived(metadata)
                    }
                    grpc_client::GrpcMessage::Status(status) => {
                        Message::StatusReceived(status)
                    }
                    grpc_client::GrpcMessage::Connected => Message::Connected,
                    grpc_client::GrpcMessage::Disconnected => Message::Disconnected,
                    grpc_client::GrpcMessage::Error(error) => Message::Error(error),
                },
                Err(error) => Message::Error(format!("gRPC error: {}", error)),
            }),
        ])
    }
}

fn main() -> iced::Result {
    iced::run("AV1 Remote Browser", AvifClient::update, AvifClient::view, AvifClient::subscription)
}
