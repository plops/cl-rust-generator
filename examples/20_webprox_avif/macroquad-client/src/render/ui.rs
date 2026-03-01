use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use crate::state::ClientState;

pub fn draw_ui_and_frame(state: &Arc<Mutex<ClientState>>, texture: Option<&Texture2D>) {
    let (server_y, local_y, links, doc_width, doc_height, page_title, debug_y_offset, verbose_coords) = {
        let mut lock = match state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        
        // Handle local scrolling (mouse wheel)
        let (_, mouse_wheel_y) = mouse_wheel();
        if mouse_wheel_y != 0.0 {
            lock.local_scroll_y -= mouse_wheel_y as i32 * 30; // 30px per tick
            lock.local_scroll_y = lock.local_scroll_y.clamp(0, lock.doc_height as i32);
            println!("[Client] Scroll to: {}", lock.local_scroll_y);
        }
        
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
                dest_size: Some(vec2(screen_width(), screen_height())),
                ..Default::default()
            },
        );
        
        // Draw link overlays (blue boxes)
        for link in &links {
            let screen_y = (link.y - local_y + debug_y_offset) as f32;
            let screen_x = link.x as f32;

            if verbose_coords {
                println!("[Client] Link: id={}, link_y={}, local_y={}, screen_y={}, render_offset_y={}", 
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
                        println!("[Client] Link clicked: {}", link.url);
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
        
        // Add keyboard controls help
        draw_text("Controls: Mouse Wheel=Local Scroll | W/PageUp=Server Up | S/PageDown=Server Down", 20.0, 135.0, 14.0, GREEN);
        
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
    event_tx: &tokio::sync::mpsc::UnboundedSender<proto_def::graphical_proxy::ClientEvent>, 
    last_mouse_wheel_y: &mut f32,
    last_w_key_state: &mut bool,
    last_s_key_state: &mut bool,
    last_pageup_key_state: &mut bool,
    last_pagedown_key_state: &mut bool
) {
    let (_, mouse_wheel_y) = mouse_wheel();
    if mouse_wheel_y != 0.0 && mouse_wheel_y != *last_mouse_wheel_y {
        *last_mouse_wheel_y = mouse_wheel_y;
        println!("[Client] Mouse Wheel: Local scroll ({}px)", -(mouse_wheel_y as i32 * 30));
        let scroll_event = proto_def::graphical_proxy::ClientEvent {
            event: Some(proto_def::graphical_proxy::client_event::Event::Scroll(
                proto_def::graphical_proxy::ScrollInput {
                    delta_y: -(mouse_wheel_y as i32 * 30),
                }
            )),
        };
        
        // Send scroll event to network thread
        if let Err(e) = event_tx.send(scroll_event) {
            eprintln!("[Client] Failed to send scroll event: {:?}", e);
        } else {
            println!("[Client] ✓ Local scroll event sent");
        }
    }
    
    // Handle keyboard input for independent server viewport control
    let current_w_key_state = is_key_pressed(KeyCode::W);
    let current_s_key_state = is_key_pressed(KeyCode::S);
    let current_pageup_key_state = is_key_pressed(KeyCode::PageUp);
    let current_pagedown_key_state = is_key_pressed(KeyCode::PageDown);
    
    // W or PageUp - just pressed (was false, now true)
    if (!*last_w_key_state && current_w_key_state) || (!*last_pageup_key_state && current_pageup_key_state) {
        println!("[Client] Keyboard: Server scroll UP (-50px)");
        let server_scroll_event = proto_def::graphical_proxy::ClientEvent {
            event: Some(proto_def::graphical_proxy::client_event::Event::Scroll(
                proto_def::graphical_proxy::ScrollInput {
                    delta_y: -50, // Scroll server up by 50px
                }
            )),
        };
        
        if let Err(e) = event_tx.send(server_scroll_event) {
            eprintln!("[Client] Failed to send server scroll event: {:?}", e);
        } else {
            println!("[Client] ✓ Server scroll UP event sent");
        }
    }
    
    // S or PageDown - just pressed (was false, now true)
    if (!*last_s_key_state && current_s_key_state) || (!*last_pagedown_key_state && current_pagedown_key_state) {
        println!("[Client] Keyboard: Server scroll DOWN (+50px)");
        let server_scroll_event = proto_def::graphical_proxy::ClientEvent {
            event: Some(proto_def::graphical_proxy::client_event::Event::Scroll(
                proto_def::graphical_proxy::ScrollInput {
                    delta_y: 50, // Scroll server down by 50px
                }
            )),
        };
        
        if let Err(e) = event_tx.send(server_scroll_event) {
            eprintln!("[Client] Failed to send server scroll event: {:?}", e);
        } else {
            println!("[Client] ✓ Server scroll DOWN event sent");
        }
    }
    
    // Update key states for next frame
    *last_w_key_state = current_w_key_state;
    *last_s_key_state = current_s_key_state;
    *last_pageup_key_state = current_pageup_key_state;
    *last_pagedown_key_state = current_pagedown_key_state;
}
