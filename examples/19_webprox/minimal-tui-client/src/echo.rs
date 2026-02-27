use std::time::Instant;

/// Local echo state for latency mitigation
pub struct LocalEcho {
    loading_links: Vec<(u32, Instant)>,
    input_buffer: String,
}

impl LocalEcho {
    pub fn new() -> Self {
        Self {
            loading_links: Vec::new(),
            input_buffer: String::new(),
        }
    }

    /// Mark a link as loading (local feedback before server responds)
    pub fn mark_loading(&mut self, link_id: u32) {
        self.loading_links.push((link_id, Instant::now()));
    }

    /// Clear loading state (server responded)
    pub fn clear_loading(&mut self) {
        self.loading_links.clear();
    }

    /// Check if a link is in loading state
    pub fn is_loading(&self, link_id: u32) -> bool {
        self.loading_links.iter().any(|(id, _)| *id == link_id)
    }

    /// Echo a character locally before server confirmation
    pub fn echo_char(&mut self, c: char) {
        self.input_buffer.push(c);
    }

    /// Clear the local input echo
    pub fn clear_input(&mut self) {
        self.input_buffer.clear();
    }

    /// Get the current local input echo text
    pub fn input_text(&self) -> &str {
        &self.input_buffer
    }
}

/// Status bar state
pub struct StatusBar {
    pub message: String,
    pub connection_status: String,
    pub mode_indicator: String,
}

impl StatusBar {
    pub fn new() -> Self {
        Self {
            message: "Press 'g' to enter URL, 'q' to quit".to_string(),
            connection_status: "Offline".to_string(),
            mode_indicator: "NORMAL".to_string(),
        }
    }

    pub fn set_message(&mut self, msg: impl Into<String>) {
        self.message = msg.into();
    }

    pub fn set_connection(&mut self, status: impl Into<String>) {
        self.connection_status = status.into();
    }

    pub fn set_mode(&mut self, mode: impl Into<String>) {
        self.mode_indicator = mode.into();
    }
}
