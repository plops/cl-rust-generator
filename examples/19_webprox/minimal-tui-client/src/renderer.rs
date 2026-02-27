use crate::state::PageState;
use std::io::{self, Write as IoWrite};

/// Renders Markdown content to the terminal using ANSI escape sequences
pub struct TerminalRenderer {
    use_osc8: bool,
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

    /// Render the full page state to stdout
    pub fn render(&self, state: &PageState, scroll_offset: usize, term_height: u16) {
        let mut stdout = io::stdout();
        let lines: Vec<&str> = state.markdown.lines().collect();
        let visible_lines = term_height.saturating_sub(2) as usize; // Reserve 2 for status bar

        // Clear screen
        let _ = write!(stdout, "\x1b[2J\x1b[H");

        // Title bar
        let _ = write!(
            stdout,
            "\x1b[7m {:<width$}\x1b[0m\r\n",
            format!(" {} — {}", state.title, state.url),
            width = 80
        );

        // Render visible lines
        let start = scroll_offset.min(lines.len());
        let end = (start + visible_lines).min(lines.len());

        for line in &lines[start..end] {
            let rendered = self.render_line(line, &state.link_map);
            let _ = write!(stdout, "{}\r\n", rendered);
        }

        // Pad remaining lines
        for _ in (end - start)..visible_lines {
            let _ = write!(stdout, "~\r\n");
        }

        let _ = stdout.flush();
    }

    /// Render a single Markdown line with ANSI styling
    fn render_line(&self, line: &str, link_map: &crate::state::LinkMap) -> String {
        let trimmed = line.trim();

        // Headers
        if trimmed.starts_with("### ") {
            return format!(
                "\x1b[1;33m{}\x1b[0m",
                &trimmed[4..]
            );
        }
        if trimmed.starts_with("## ") {
            return format!(
                "\x1b[1;36m{}\x1b[0m",
                &trimmed[3..]
            );
        }
        if trimmed.starts_with("# ") {
            return format!(
                "\x1b[1;35m{}\x1b[0m",
                &trimmed[2..]
            );
        }

        // List items
        if trimmed.starts_with("- ") || trimmed.starts_with("* ") {
            return format!("  • {}", &trimmed[2..]);
        }

        // Process inline links: [text](id)
        self.render_links(line, link_map)
    }

    /// Replace [text](numeric_id) with OSC 8 links or styled text
    fn render_links(&self, line: &str, link_map: &crate::state::LinkMap) -> String {
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
                                    if self.use_osc8 {
                                        // OSC 8 hyperlink
                                        result.push_str(&format!(
                                            "\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\",
                                            url, text
                                        ));
                                    } else {
                                        // Fallback: colored text with ID
                                        result.push_str(&format!(
                                            "\x1b[4;34m{}\x1b[0m[{}]",
                                            text, id
                                        ));
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
                            }
                        } else {
                            result.push('[');
                            result.push_str(&text);
                            result.push(']');
                        }
                        found_link = true;
                        break;
                    }
                    text.push(c);
                }
                if !found_link {
                    result.push('[');
                    result.push_str(&text);
                }
            } else {
                result.push(ch);
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
