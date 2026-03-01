use iced::{
    widget::{canvas, column, container, scrollable, text, button},
    Element, Length,
};
use iced::widget::image;
use std::thread;
use std::time::Duration;

mod canvas_renderer;
mod decoder;
mod grpc_client;

use canvas_renderer::VideoCanvas;
use decoder::AvifDecoder;
use grpc_client::FrameMetadata;

#[derive(Debug, Clone)]
enum Message {
    // Server connection
    ConnectButtonPressed,
    Connected,
    Disconnected,
    Error(String),
    
    // Data from server
    FrameReceived(Vec<u8>, FrameMetadata),
    StatusReceived(String),
}

#[derive(Default)]
struct AvifClient {
    // Video data
    latest_frame: Option<image::Handle>,
    frame_metadata: Option<FrameMetadata>,
    
    // UI state
    connection_status: String,
    is_connected: bool,
    
    // Decoding
    decoder: AvifDecoder,
    
    // Server data
    scroll_offset: i32,
    server_viewport: i32,
    doc_dimensions: (u32, u32),
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
                
                let handle = image::Handle::from_rgba(width, height, rgba_data);
                self.latest_frame = Some(handle);
                self.frame_metadata = Some(updated_metadata);
                self.server_viewport = metadata.viewport_y;
                self.doc_dimensions = (width, height);
            }
            Err(e) => {
                eprintln!("[Client] AVIF decode failed: {}", e);
            }
        }
    }
    
    fn connect_to_server(&mut self) {
        self.connection_status = "Connecting...".to_string();
        
        // Start background thread to simulate server connection
        let av1_data = vec![42u8; 1024]; // Simulate AV1 data
        let metadata = FrameMetadata {
            width: 1280,
            height: 720,
            viewport_x: 0,
            viewport_y: 0,
            is_keyframe: true,
        };
        
        // Simulate connection delay
        thread::sleep(Duration::from_millis(1000));
        self.handle_frame_received(av1_data, metadata);
        self.is_connected = true;
        self.connection_status = "Connected to server - Receiving frames".to_string();
    }
    
    fn update(&mut self, message: Message) {
        match message {
            Message::ConnectButtonPressed => {
                self.connect_to_server();
            }
            
            Message::FrameReceived(av1_data, metadata) => {
                self.handle_frame_received(av1_data, metadata);
                self.connection_status = "Receiving frames...".to_string();
            }
            
            Message::Connected => {
                self.is_connected = true;
                self.connection_status = "Connected to server".to_string();
            }
            
            Message::Disconnected => {
                self.is_connected = false;
                self.connection_status = "Disconnected".to_string();
            }
            
            Message::Error(error) => {
                self.connection_status = format!("Error: {}", error);
            }
            
            Message::StatusReceived(status) => {
                self.connection_status = status;
            }
        }
    }

    fn view(&self) -> Element<Message> {
        let connect_widget: Element<Message> = if !self.is_connected {
        button(text("Connect to Server"))
            .on_press(Message::ConnectButtonPressed)
            .into()
    } else {
        container(text("✅ Connected"))
            .padding(10)
            .into()
    };

let content = column![
    // Connection controls
    container(
        column![
            text("AV1 Remote Browser - Iced Client").size(24),
            text(format!("Status: {}", self.connection_status)).size(16),
            connect_widget,
        ]
    )
    .padding(20),
            
            // Video canvas
            canvas(VideoCanvas {
                frame_handle: self.latest_frame.clone(),
                links: vec![], // No links for now
                scroll_offset: self.scroll_offset,
                server_viewport: self.server_viewport,
                doc_dimensions: self.doc_dimensions,
            })
            .width(Length::Fill)
            .height(Length::Fixed(720.0)), // Fixed height to prevent scrollable issue
        ];

        scrollable(content)
            .direction(scrollable::Direction::Both {
                vertical: scrollable::Scrollbar::new(),
                horizontal: scrollable::Scrollbar::new(),
            })
            .into()
    }
}

fn main() -> iced::Result {
    iced::run("AV1 Remote Browser - Server Connected", AvifClient::update, AvifClient::view)
}
