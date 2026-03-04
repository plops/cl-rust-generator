use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use log::{trace, info};
use crate::state::ClientState;
use proto_def::graphical_proxy::LinkBox;

pub fn draw_ui_and_frame(state: &Arc<Mutex<ClientState>>, texture: Option<&Texture2D>) {
    let (server_y, local_y, links, doc_width, doc_height, page_title, debug_y_offset, verbose_coords) = {
        let mut lock = match state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        
        // Mark dirty flag check
        let was_dirty = lock.dirty;
        if was_dirty {
            lock.dirty = false;
        }
        
        (lock.server_viewport_y, lock.local_scroll_y, lock.links.clone(), lock.doc_width, lock.doc_height, lock.page_title.clone(), lock.debug_y_offset, lock.verbose_coords)
    };

    clear_background(BLACK);

    // Draw video frame with viewport offset
    if let Some(tex) = texture {
        // Calculate the render offset based on virtual framebuffer
        let render_offset_y = (server_y - local_y + debug_y_offset) as f32;
        
        draw_texture_ex(
            tex,
            0.0,
            render_offset_y,
            WHITE,
            DrawTextureParams {
                // Ensure 1:1 rendering scale to perfectly match the unscaled DOM metadata
                dest_size: Some(vec2(tex.width(), tex.height())),
                ..Default::default()
            },
        );
        
        // Draw link overlays (blue boxes)
        draw_link_overlays(&links, local_y, debug_y_offset, verbose_coords, render_offset_y);
        
        // Draw status overlay
        draw_status_overlay(&state, server_y, local_y, doc_width, doc_height, &links, &page_title);
        
        // Add controls help
        draw_text("Controls: Mouse Wheel=Request Viewport", 20.0, 135.0, 14.0, GREEN);
        
        // Draw scrollbar
        draw_scrollbar(local_y, doc_height);
    } else {
        draw_text("Connecting to AV1 Remote Browser...", screen_width() / 2.0 - 180.0, screen_height() / 2.0, 30.0, WHITE);
        draw_text("Make sure the server is running on [::1]:50051", screen_width() / 2.0 - 200.0, screen_height() / 2.0 + 40.0, 20.0, GRAY);
    }
}

fn draw_link_overlays(links: &[LinkBox], local_y: i32, debug_y_offset: i32, verbose_coords: bool, render_offset_y: f32) {
    for link in links {
        let screen_y = (link.y - local_y + debug_y_offset) as f32;
        let screen_x = link.x as f32;

        if verbose_coords {
            trace!("[Client] Link: id={}, link_y={}, local_y={}, screen_y={}, render_offset_y={}", 
                link.id, link.y, local_y, screen_y, render_offset_y);
        }

        // Check if link is visible
        if screen_y > -link.height as f32 && screen_y < screen_height() {
            // Draw semi-transparent blue box
            draw_rectangle(screen_x, screen_y, link.width as f32, link.height as f32, Color::new(0.0, 0.5, 1.0, 0.3));
            draw_rectangle_lines(screen_x, screen_y, link.width as f32, link.height as f32, 2.0, BLUE);
            
            // Draw link label (truncated)
            let label = if link.label.len() > 30 {
                format!("{}...", &link.label[..30])
            } else {
                link.label.clone()
            };
            draw_text(&label, screen_x + 2.0, screen_y + 15.0, 14.0, WHITE);
        }
    }
}

fn draw_status_overlay(state: &Arc<Mutex<ClientState>>, server_y: i32, local_y: i32, doc_width: u32, doc_height: u32, links: &[LinkBox], page_title: &str) {
    // Draw status overlay background
    draw_rectangle(10.0, 10.0, 350.0, 120.0, Color::new(0.0, 0.0, 0.0, 0.7));
    
    // Draw page title
    draw_text(&format!("Page: {}", page_title.chars().take(40).collect::<String>()), 20.0, 35.0, 16.0, GREEN);
    
    // Get packet statistics
    let (packet_count, last_packet_size) = {
        let lock = match state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        (lock.packet_count, lock.last_packet_size)
    };
    
    draw_text(&format!("Packets: {} | Size: {} bytes", packet_count, last_packet_size), 20.0, 60.0, 16.0, SKYBLUE);
    draw_text(&format!("Viewport: Server={} Local={}", server_y, local_y), 20.0, 85.0, 16.0, YELLOW);
    draw_text(&format!("Document: {}x{} | Links: {}", doc_width, doc_height, links.len()), 20.0, 110.0, 16.0, ORANGE);
}

fn draw_scrollbar(local_y: i32, doc_height: u32) {
    if doc_height > 0 {
        let scroll_ratio = local_y as f32 / doc_height as f32;
        let scrollbar_height = 50.0;
        let scrollbar_y = scroll_ratio * (screen_height() - scrollbar_height);
        draw_rectangle(screen_width() - 15.0, scrollbar_y, 10.0, scrollbar_height, GRAY);
    }
}

pub fn check_link_clicks(links: &[LinkBox], local_y: i32, debug_y_offset: i32) -> Option<String> {
    if is_mouse_button_pressed(MouseButton::Left) {
        let (mx, my) = mouse_position();
        
        for link in links {
            let screen_y = (link.y - local_y + debug_y_offset) as f32;
            let screen_x = link.x as f32;

            // Check if link is visible and clicked
            if screen_y > -link.height as f32 && screen_y < screen_height() &&
               mx >= screen_x && mx <= screen_x + link.width as f32 &&
               my >= screen_y && my <= screen_y + link.height as f32 {
                info!("[Client] Link clicked: {}", link.url);
                return Some(link.url.clone());
            }
        }
    }
    None
}
