use crate::input::HitBoxMap;
use crate::state::PageState;
use std::io::{self, Write as IoWrite};

/// Renders Markdown content to the terminal using ANSI escape sequences
pub struct TerminalRenderer {
    use_osc8: bool,
}

/// Tracks the visible column position while rendering a line (ignoring ANSI escapes)
struct ColumnTracker {
    col: u16,
    row: u16,
    hits: Vec<(u16, u16, u16, u32)>, // (x, y, width, link_id)
}

impl ColumnTracker {
    fn new(row: u16) -> Self {
        Self {
            col: 0,
            row,
            hits: Vec::new(),
        }
    }

    fn advance(&mut self, visible_chars: u16) {
        self.col += visible_chars;
    }

    fn record_link(&mut self, text_len: u16, link_id: u32) {
        self.hits.push((self.col, self.row, text_len, link_id));
    }
}

impl TerminalRenderer {
    pub fn new() -> Self {
        // Detect OSC 8 support from environment
        let term_program = std::env::var("TERM_PROGRAM").unwrap_or_default();
        let use_osc8 = matches!(
            term_program.as_str(),
            "ghostty" | "WezTerm" | "iTerm.app" | "iTerm2"
        );
        Self { use_osc8 }
    }

    /// Render the full page state to stdout, populating the hit box map for mouse clicks
    pub fn render(
        &self,
        state: &PageState,
        scroll_offset: usize,
        term_height: u16,
        hitbox_map: &mut HitBoxMap,
    ) {
        let mut stdout = io::stdout();
        let lines: Vec<&str> = state.markdown.lines().collect();
        let visible_lines = term_height.saturating_sub(2) as usize; // Reserve 2 for status bar

        // Clear screen and hit boxes
        let _ = write!(stdout, "\x1b[2J\x1b[H");
        hitbox_map.clear();

        // Title bar (row 0)
        let _ = write!(
            stdout,
            "\x1b[7m {:<width$}\x1b[0m\r\n",
            format!(" {} — {}", state.title, state.url),
            width = 80
        );

        // Render visible lines (starting at row 1)
        let start = scroll_offset.min(lines.len());
        let end = (start + visible_lines).min(lines.len());

        for (i, line) in lines[start..end].iter().enumerate() {
            let screen_row = (i + 1) as u16; // +1 for title bar
            let mut tracker = ColumnTracker::new(screen_row);
            let rendered = self.render_line_tracked(line, &state.link_map, &mut tracker);
            let _ = write!(stdout, "{}\r\n", rendered);

            // Register hit boxes from this line
            for (x, y, w, id) in tracker.hits {
                hitbox_map.add(x, y, w, id);
            }
        }

        // Pad remaining lines
        for _ in (end - start)..visible_lines {
            let _ = write!(stdout, "~\r\n");
        }

        let _ = stdout.flush();
    }

    /// Render a single Markdown line with ANSI styling, tracking link positions
    fn render_line_tracked(
        &self,
        line: &str,
        link_map: &crate::state::LinkMap,
        tracker: &mut ColumnTracker,
    ) -> String {
        let trimmed = line.trim();

        // Headers
        if trimmed.starts_with("### ") {
            let text = &trimmed[4..];
            tracker.advance(text.len() as u16);
            return format!("\x1b[1;33m{}\x1b[0m", text);
        }
        if trimmed.starts_with("## ") {
            let text = &trimmed[3..];
            tracker.advance(text.len() as u16);
            return format!("\x1b[1;36m{}\x1b[0m", text);
        }
        if trimmed.starts_with("# ") {
            let text = &trimmed[2..];
            tracker.advance(text.len() as u16);
            return format!("\x1b[1;35m{}\x1b[0m", text);
        }

        // List items
        if trimmed.starts_with("- ") || trimmed.starts_with("* ") {
            tracker.advance(2); // "  • " prefix
            let rest = &trimmed[2..];
            tracker.advance(rest.len() as u16);
            return format!("  • {}", rest);
        }

        // Process inline links: [text](id)
        self.render_links_tracked(line, link_map, tracker)
    }

    /// Replace [text](numeric_id) with OSC 8 links or styled text, tracking positions
    fn render_links_tracked(
        &self,
        line: &str,
        link_map: &crate::state::LinkMap,
        tracker: &mut ColumnTracker,
    ) -> String {
        let mut result = String::new();
        let mut chars = line.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '[' {
                // Try to parse [text](id)
                let mut text = String::new();
                let mut found_link = false;

                for c in chars.by_ref() {
                    if c == ']' {
                        if chars.peek() == Some(&'(') {
                            chars.next(); // consume '('
                            let mut id_str = String::new();
                            for c2 in chars.by_ref() {
                                if c2 == ')' {
                                    break;
                                }
                                id_str.push(c2);
                            }
                            if let Ok(id) = id_str.parse::<u32>() {
                                if let Some(url) = link_map.get(&id) {
                                    let text_len = text.len() as u16;
                                    // Record hit box before advancing
                                    tracker.record_link(text_len, id);

                                    if self.use_osc8 {
                                        result.push_str(&format!(
                                            "\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\",
                                            url, text
                                        ));
                                    } else {
                                        result.push_str(&format!(
                                            "\x1b[4;34m{}\x1b[0m[{}]",
                                            text, id
                                        ));
                                    }
                                    tracker.advance(text_len);
                                    if !self.use_osc8 {
                                        // Account for [id] suffix
                                        tracker.advance(id_str.len() as u16 + 2);
                                    }
                                    found_link = true;
                                }
                            }
                            if !found_link {
                                result.push('[');
                                result.push_str(&text);
                                result.push_str("](");
                                result.push_str(&id_str);
                                result.push(')');
                                tracker.advance(text.len() as u16 + id_str.len() as u16 + 4);
                            }
                        } else {
                            result.push('[');
                            result.push_str(&text);
                            result.push(']');
                            tracker.advance(text.len() as u16 + 2);
                        }
                        found_link = true;
                        break;
                    }
                    text.push(c);
                }
                if !found_link {
                    result.push('[');
                    result.push_str(&text);
                    tracker.advance(text.len() as u16 + 1);
                }
            } else {
                result.push(ch);
                tracker.advance(1);
            }
        }

        result
    }

    /// Render the status bar at the bottom
    pub fn render_status_bar(&self, message: &str, connection_status: &str, term_height: u16) {
        let mut stdout = io::stdout();
        let _ = write!(
            stdout,
            "\x1b[{};1H\x1b[7m {:<60} {:>18} \x1b[0m",
            term_height, message, connection_status
        );
        let _ = stdout.flush();
    }
}
