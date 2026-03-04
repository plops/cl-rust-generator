use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use log::{trace, debug};
use crate::state::ClientState;

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
