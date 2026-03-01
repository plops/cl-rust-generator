use iced::{
    widget::canvas::{self, Cache, Geometry},
    widget::image, Color, Rectangle, Size, Theme, Renderer,
};
use proto_def::graphical_proxy::LinkBox;

pub struct VideoCanvas {
    pub frame_handle: Option<image::Handle>,
    pub links: Vec<LinkBox>,
    pub scroll_offset: i32,
    pub server_viewport: i32,
    pub doc_dimensions: (u32, u32),
}

impl<Message> canvas::Program<Message> for VideoCanvas {
    type State = Cache;

    fn draw(
        &self,
        cache: &Self::State,
        renderer: &Renderer,
        theme: &Theme,
        bounds: Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> Vec<Geometry> {
        let mut frame = canvas::Frame::new(renderer, bounds.size());
        
        // Clear background
        frame.fill_rectangle(
            iced::Point::new(0.0, 0.0),
            Size::new(bounds.width, bounds.height),
            Color::from_rgb(0.1, 0.1, 0.1)
        );
        
        // Draw video frame if available
        if let Some(ref handle) = self.frame_handle {
            // Calculate render offset based on viewport difference
            let render_offset_y = (self.server_viewport - self.scroll_offset) as f32;
            
            let image_bounds = Rectangle::new(
                iced::Point::new(0.0, render_offset_y),
                Size::new(bounds.width, bounds.height),
            );
            
            frame.draw_image(image_bounds, handle);
        }
        
        // Draw link overlays
        for link in &self.links {
            let screen_y = (link.y as i32 - self.scroll_offset) as f32;
            let screen_x = link.x as f32;
            
            // Check if link is visible
            if screen_y > -link.height as f32 && screen_y < bounds.height {
                let link_bounds = Rectangle::new(
                    iced::Point::new(screen_x, screen_y),
                    Size::new(link.width as f32, link.height as f32),
                );
                
                // Draw semi-transparent blue overlay
                frame.fill_rectangle(
                    iced::Point::new(link_bounds.x, link_bounds.y),
                    Size::new(link_bounds.width, link_bounds.height),
                    Color::new(0.0, 0.5, 1.0, 0.3)
                );
                
                // Draw border
                frame.stroke_rectangle(
                    iced::Point::new(link_bounds.x, link_bounds.y),
                    Size::new(link_bounds.width, link_bounds.height),
                    canvas::Stroke::default().with_color(Color::from_rgb(0.0, 0.0, 1.0)).with_width(2.0),
                );
                
                // Draw link label (truncated if too long)
                let label = if link.label.len() > 30 {
                    format!("{}...", &link.label[..30])
                } else {
                    link.label.clone()
                };
                
                // Note: Text rendering in canvas is more complex, this is a placeholder
                // In a full implementation, you'd use text rendering capabilities
                // or overlay Iced text widgets on top of the canvas
            }
        }
        
        // Draw scrollbar if document is larger than viewport
        if self.doc_dimensions.1 > bounds.height as u32 {
            let scroll_ratio = self.scroll_offset as f32 / self.doc_dimensions.1 as f32;
            let scrollbar_height = 50.0;
            let scrollbar_y = scroll_ratio * (bounds.height - scrollbar_height);
            let scrollbar_width = 10.0;
            let scrollbar_x = bounds.width - scrollbar_width;
            
            let scrollbar_bounds = Rectangle::new(
                iced::Point::new(scrollbar_x, scrollbar_y),
                Size::new(scrollbar_width, scrollbar_height),
            );
            
            frame.fill_rectangle(
                iced::Point::new(scrollbar_bounds.x, scrollbar_bounds.y),
                Size::new(scrollbar_bounds.width, scrollbar_bounds.height),
                Color::from_rgb(0.5, 0.5, 0.5)
            );
        }
        
        vec![frame.into_geometry()]
    }
}
