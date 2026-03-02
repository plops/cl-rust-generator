use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use log::{trace, debug, info};
use crate::state::ClientState;

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
        for link in &links {
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
                
                // Handle mouse clicks on links
                if is_mouse_button_pressed(MouseButton::Left) {
                    let (mx, my) = mouse_position();
                    if mx >= screen_x && mx <= screen_x + link.width as f32 &&
                       my >= screen_y && my <= screen_y + link.height as f32 {
                        info!("[Client] Link clicked: {}", link.url);
                        // TODO: Send navigate event to server via event_tx
                    }
                }
            }
        }
        
        // Draw status overlay
        draw_rectangle(10.0, 10.0, 350.0, 120.0, Color::new(0.0, 0.0, 0.0, 0.7));
        draw_text(&format!("Page: {}", page_title.chars().take(40).collect::<String>()), 20.0, 35.0, 16.0, GREEN);
        
        let packet_count = {
            let lock = match state.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            lock.packet_count
        };
        
        let last_packet_size = {
            let lock = match state.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            lock.last_packet_size
        };
        
        draw_text(&format!("Packets: {} | Size: {} bytes", packet_count, last_packet_size), 20.0, 60.0, 16.0, SKYBLUE);
        draw_text(&format!("Viewport: Server={} Local={}", server_y, local_y), 20.0, 85.0, 16.0, YELLOW);
        draw_text(&format!("Document: {}x{} | Links: {}", doc_width, doc_height, links.len()), 20.0, 110.0, 16.0, ORANGE);
        
        // Add controls help
        draw_text("Controls: Mouse Wheel=Request Viewport", 20.0, 135.0, 14.0, GREEN);
        
        // Draw scrollbar
        if doc_height > 0 {
            let scroll_ratio = local_y as f32 / doc_height as f32;
            let scrollbar_height = 50.0;
            let scrollbar_y = scroll_ratio * (screen_height() - scrollbar_height);
            draw_rectangle(screen_width() - 15.0, scrollbar_y, 10.0, scrollbar_height, GRAY);
        }
    } else {
        draw_text("Connecting to AV1 Remote Browser...", screen_width() / 2.0 - 180.0, screen_height() / 2.0, 30.0, WHITE);
        draw_text("Make sure the server is running on [::1]:50051", screen_width() / 2.0 - 200.0, screen_height() / 2.0 + 40.0, 20.0, GRAY);
    }
}

pub fn handle_input_and_send_events(
    state: &Arc<Mutex<ClientState>>,
    event_tx: &tokio::sync::mpsc::UnboundedSender<proto_def::graphical_proxy::ClientEvent>, 
    last_mouse_wheel_y: &mut f32,
) {
    let (_, mouse_wheel_y) = mouse_wheel();
    debug!("[Client] Mouse wheel raw: wheel_y={}, last_wheel_y={}", mouse_wheel_y, *last_mouse_wheel_y);
    
    // Also check keyboard keys as fallback for testing
    let up_key = is_key_pressed(KeyCode::Up);
    let down_key = is_key_pressed(KeyCode::Down);
    
    // Use mouse wheel or keyboard as scroll input
    let scroll_input = if mouse_wheel_y != 0.0 { mouse_wheel_y } else if up_key { -1.0 } else if down_key { 1.0 } else { 0.0 };
    
    // Handle scroll input
    if scroll_input != 0.0 {
        debug!("[Client] Scroll triggered: input={}, source={}", scroll_input, 
                if mouse_wheel_y != 0.0 { "mouse" } else { "keyboard" });
        *last_mouse_wheel_y = scroll_input;
        
        // Handle local scroll first for immediate feedback
        let current_local_scroll_y = {
            let mut lock = match state.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            lock.local_scroll_y -= scroll_input as i32 * 30; // 30px per tick
            lock.local_scroll_y = lock.local_scroll_y.clamp(0, lock.doc_height as i32);
            debug!("[Client] Scroll to: {}", lock.local_scroll_y);
            lock.local_scroll_y
        };
        
        debug!("[Client] Mouse Wheel: Requesting server viewport at absolute position {}", current_local_scroll_y);
        let scroll_event = proto_def::graphical_proxy::ClientEvent {
            event: Some(proto_def::graphical_proxy::client_event::Event::Scroll(
                proto_def::graphical_proxy::ScrollInput {
                    absolute_y: current_local_scroll_y,    // Send absolute position
                }
            )),
        };
        
        // Send scroll event to network thread
        if let Err(e) = event_tx.send(scroll_event) {
            eprintln!("[Client] Failed to send scroll event: {:?}", e);
        } else {
            trace!("[Client] ✓ Absolute scroll event sent to position {}", current_local_scroll_y);
        }
    }
}
