use iced::{
    widget::{canvas, column, container, scrollable, text},
    Element, Length, Subscription, Theme,
};
use iced::widget::image;

mod decoder_simple;

use decoder_simple::AvifDecoder;

#[derive(Debug, Clone)]
enum Message {
    FrameReceived,
    StatusReceived(String),
}

#[derive(Default)]
struct AvifClient {
    latest_frame: Option<image::Handle>,
    decoder: AvifDecoder,
    connection_status: String,
}

impl AvifClient {
    fn update(&mut self, message: Message) -> iced::Command<Message> {
        match message {
            Message::FrameReceived => {
                let (width, height, rgba_data) = self.create_test_frame();
                let handle = image::Handle::from_rgba(width, height, rgba_data);
                self.latest_frame = Some(handle);
                self.connection_status = "Frame received".to_string();
                iced::Command::request_redraw()
            }
            Message::StatusReceived(status) => {
                self.connection_status = status;
                iced::Command::none()
            }
        }
    }

    fn view(&self) -> Element<Message> {
        let content = column![
            // Video canvas
            canvas(TestCanvas {
                frame_handle: self.latest_frame.clone(),
            })
            .width(Length::Fill)
            .height(Length::Fill),
            
            // Status overlay
            container(
                column![
                    text(format!("Status: {}", self.connection_status)).size(16),
                    text("Iced Client - Working Demo").size(14),
                ]
            )
            .padding(10),
        ];

        scrollable(content)
            .direction(scrollable::Direction::Both { 
                vertical: scrollable::Scrollbar::new(),
                horizontal: scrollable::Scrollbar::new(),
            })
            .into()
    }

    fn create_test_frame(&self) -> (u32, u32, Vec<u8>) {
        let width = 640;
        let height = 480;
        let mut rgba_data = vec![128u8; (width * height * 4) as usize];
        
        // Create a simple animated pattern
        let time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as u8;
        
        for y in 0..height {
            for x in 0..width {
                let idx = ((y * width + x) * 4) as usize;
                let color_val = ((time + x as u8 + y as u8) % 256) as u8;
                rgba_data[idx] = color_val;
                rgba_data[idx + 1] = color_val;
                rgba_data[idx + 2] = color_val;
                rgba_data[idx + 3] = 255; // Alpha
            }
        }
        
        (width, height, rgba_data)
    }
}

struct TestCanvas {
    frame_handle: Option<image::Handle>,
}

impl<Message> canvas::Program<Message> for TestCanvas {
    type State = canvas::Cache;

    fn draw(
        &self,
        _cache: &Self::State,
        renderer: &iced::Renderer,
        _theme: &Theme,
        bounds: iced::Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> Vec<iced::Geometry> {
        let mut frame = canvas::Frame::new(renderer, bounds.size());
        
        // Clear background
        frame.fill_rectangle(
            iced::Rectangle::new(
                iced::Point::new(0.0, 0.0),
                iced::Size::new(bounds.width, bounds.height),
            ),
            iced::Color::from_rgb(0.1, 0.1, 0.1),
        );
        
        // Draw frame if available
        if let Some(ref handle) = self.frame_handle {
            let image_bounds = iced::Rectangle::new(
                iced::Point::new(0.0, 0.0),
                iced::Size::new(bounds.width, bounds.height),
            );
            frame.draw_image(image_bounds, handle);
        }
        
        vec![frame.into_geometry()]
    }
}

fn main() -> iced::Result {
    iced::run("AV1 Remote Browser - Iced Working Demo", AvifClient::update, AvifClient::view)
}
