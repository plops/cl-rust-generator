use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent, MouseEventKind};
use std::time::Duration;

/// Input mode state machine
#[derive(Debug, Clone, PartialEq)]
pub enum InputMode {
    Normal,    // Scrolling, clicking
    TextInput, // Keystrokes forwarded to server
    UrlBar,    // Typing a URL
}

/// A rectangular hit region on screen mapped to a link ID
#[derive(Debug, Clone)]
pub struct HitBox {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub link_id: u32,
}

/// Maps screen regions to link IDs for mouse click resolution
pub struct HitBoxMap {
    boxes: Vec<HitBox>,
}

impl HitBoxMap {
    pub fn new() -> Self {
        Self { boxes: Vec::new() }
    }

    pub fn clear(&mut self) {
        self.boxes.clear();
    }

    pub fn add(&mut self, x: u16, y: u16, width: u16, link_id: u32) {
        self.boxes.push(HitBox {
            x,
            y,
            width,
            link_id,
        });
    }

    /// Find which link was clicked at the given coordinates
    pub fn hit_test(&self, x: u16, y: u16) -> Option<u32> {
        for hb in &self.boxes {
            if y == hb.y && x >= hb.x && x < hb.x + hb.width {
                return Some(hb.link_id);
            }
        }
        None
    }
}

/// Actions produced by the input handler
#[derive(Debug)]
pub enum InputAction {
    Navigate(String),
    Click(u32),
    TypeText(String),
    Submit,
    ScrollUp,
    ScrollDown,
    Quit,
    ToggleInputMode,
    None,
}

/// Handles terminal input events and maps them to actions
pub struct InputHandler {
    pub mode: InputMode,
    pub url_buffer: String,
    pub text_buffer: String,
}

impl InputHandler {
    pub fn new() -> Self {
        Self {
            mode: InputMode::Normal,
            url_buffer: String::new(),
            text_buffer: String::new(),
        }
    }

    /// Poll for input events with a timeout
    pub fn poll(&mut self, timeout: Duration, hitbox_map: &HitBoxMap) -> InputAction {
        if event::poll(timeout).unwrap_or(false) {
            if let Ok(ev) = event::read() {
                return self.handle_event(ev, hitbox_map);
            }
        }
        InputAction::None
    }

    fn handle_event(&mut self, ev: Event, hitbox_map: &HitBoxMap) -> InputAction {
        match ev {
            Event::Key(key) => self.handle_key(key),
            Event::Mouse(mouse) => Self::handle_mouse(mouse, hitbox_map),
            Event::Resize(_, _) => InputAction::None,
            _ => InputAction::None,
        }
    }

    fn handle_key(&mut self, key: KeyEvent) -> InputAction {
        // Ctrl+C or Ctrl+Q always quits
        if key.modifiers.contains(KeyModifiers::CONTROL)
            && (key.code == KeyCode::Char('c') || key.code == KeyCode::Char('q'))
        {
            return InputAction::Quit;
        }

        match self.mode {
            InputMode::Normal => match key.code {
                KeyCode::Char('g') => {
                    self.mode = InputMode::UrlBar;
                    self.url_buffer.clear();
                    InputAction::None
                }
                KeyCode::Char('i') => {
                    self.mode = InputMode::TextInput;
                    self.text_buffer.clear();
                    InputAction::ToggleInputMode
                }
                KeyCode::Up | KeyCode::Char('k') => InputAction::ScrollUp,
                KeyCode::Down | KeyCode::Char('j') => InputAction::ScrollDown,
                KeyCode::Char('q') => InputAction::Quit,
                _ => InputAction::None,
            },
            InputMode::UrlBar => match key.code {
                KeyCode::Enter => {
                    let url = self.url_buffer.clone();
                    self.url_buffer.clear();
                    self.mode = InputMode::Normal;
                    if !url.is_empty() {
                        let url = if !url.starts_with("http://") && !url.starts_with("https://") {
                            format!("https://{}", url)
                        } else {
                            url
                        };
                        InputAction::Navigate(url)
                    } else {
                        InputAction::None
                    }
                }
                KeyCode::Esc => {
                    self.url_buffer.clear();
                    self.mode = InputMode::Normal;
                    InputAction::None
                }
                KeyCode::Backspace => {
                    self.url_buffer.pop();
                    InputAction::None
                }
                KeyCode::Char(c) => {
                    self.url_buffer.push(c);
                    InputAction::None
                }
                _ => InputAction::None,
            },
            InputMode::TextInput => match key.code {
                KeyCode::Esc => {
                    self.mode = InputMode::Normal;
                    InputAction::ToggleInputMode
                }
                KeyCode::Enter => {
                    let _text = self.text_buffer.clone();
                    self.text_buffer.clear();
                    InputAction::Submit
                }
                KeyCode::Backspace => {
                    self.text_buffer.pop();
                    InputAction::None
                }
                KeyCode::Char(c) => {
                    self.text_buffer.push(c);
                    InputAction::TypeText(c.to_string())
                }
                _ => InputAction::None,
            },
        }
    }

    fn handle_mouse(mouse: MouseEvent, hitbox_map: &HitBoxMap) -> InputAction {
        match mouse.kind {
            MouseEventKind::Down(_) => {
                if let Some(link_id) = hitbox_map.hit_test(mouse.column, mouse.row) {
                    InputAction::Click(link_id)
                } else {
                    InputAction::None
                }
            }
            MouseEventKind::ScrollUp => InputAction::ScrollUp,
            MouseEventKind::ScrollDown => InputAction::ScrollDown,
            _ => InputAction::None,
        }
    }
}
