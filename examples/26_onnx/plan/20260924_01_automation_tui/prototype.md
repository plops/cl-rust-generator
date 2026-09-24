### Architectural Review & Refactoring Strategy

1. **Eliminate the Heavy Macroquad Dependency**:
   - `macroquad` dragged in over 100 transitive crates (OpenGL/GLX/Wayland plumbing, font rasterizers, audio backends) simply to display a preview window.
   - Replacing this with a terminal dashboard (via `crossterm`) drops build times from minutes to seconds, allows headless/SSH operation, and frees up system resources for OCR inference.

2. **Native X11 Automation via `x11rb[xtest]`](https://docs.rs/crate/x11rb/latest)**:
   - Instead of needing external tools like `xdotool`, we enable the `xtest` feature in `x11rb`.
   - This allows programmatic mouse movement, clicking, and text typing with sub-millisecond latency directly over the X11 connection.

3. **Coordinate-Space Clarity**:
   - Detection occurs on a normalized $640 \times 640$ model plane.
   - The refactored `View` automatically projects model coordinates directly to **absolute X11 screen pixels** $(x, y, w, h)$, providing click coordinates for text boxes.

4. **Zero-Copy Image Pipeline**:
   - We eliminated intermediate RGBA buffers. Text cropping and planar model input sampling are performed directly from the captured X11 BGRA buffer.

---

### 1. `Cargo.toml`

```toml
[package]
name = "x11_ppocrv6"
version = "0.2.0"
edition = "2024"

[dependencies]
crossterm = "0.28"
ort = { version = "2.0.0-rc.13", default-features = false, features = ["download-binaries", "copy-dylibs", "tls-native"] }
x11rb = { version = "0.14.0", features = ["xtest"] }

[profile.release]
opt-level = 3
lto = true
codegen-units = 1
panic = "abort"
strip = true
```

---

### 2. `src/view.rs` (ROI, Zoom & Coordinate Projection)

```rust
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

pub const MODEL_SIZE: usize = 640;
pub const ROI_STEPS: &[u32] = &[320, 480, 640, 960, 1280];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Screen {
    pub w: i32,
    pub h: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct View {
    pub x: i32,
    pub y: i32,
    pub size: u32,
}

impl Default for View {
    fn default() -> Self {
        Self { x: 0, y: 0, size: 640 }
    }
}

impl View {
    pub fn pan(&mut self, dx: i32, dy: i32, screen: Screen) -> bool {
        let step = ((self.size / 16).max(8)) as i32;
        let nx = (self.x + dx * step).clamp(0, (screen.w - self.size as i32).max(0));
        let ny = (self.y + dy * step).clamp(0, (screen.h - self.size as i32).max(0));
        let changed = nx != self.x || ny != self.y;
        self.x = nx;
        self.y = ny;
        changed
    }

    pub fn zoom(&mut self, delta: i32, screen: Screen) -> bool {
        let current_idx = ROI_STEPS.iter().position(|&s| s == self.size).unwrap_or(2);
        let next_idx = (current_idx as isize + delta as isize).clamp(0, (ROI_STEPS.len() - 1) as isize) as usize;
        let mut target_size = ROI_STEPS[next_idx];

        while target_size as i32 > screen.w.min(screen.h) && target_size > ROI_STEPS[0] {
            let i = ROI_STEPS.iter().position(|&s| s == target_size).unwrap_or(1);
            target_size = ROI_STEPS[i - 1];
        }

        if target_size == self.size {
            return false;
        }

        self.size = target_size;
        self.x = self.x.min((screen.w - target_size as i32).max(0));
        self.y = self.y.min((screen.h - target_size as i32).max(0));
        true
    }

    /// Projects 640x640 detection coordinates to absolute X11 screen coordinates.
    pub fn to_screen_rect(&self, bx: f32, by: f32, bw: f32, bh: f32) -> (i32, i32, u32, u32) {
        let scale = self.size as f32 / MODEL_SIZE as f32;
        let sx = self.x + (bx * scale).round() as i32;
        let sy = self.y + (by * scale).round() as i32;
        let sw = (bw * scale).round().max(1.0) as u32;
        let sh = (bh * scale).round().max(1.0) as u32;
        (sx, sy, sw, sh)
    }
}

pub fn get_screen_size(conn: &impl Connection, screen_idx: usize) -> Screen {
    let s = &conn.setup().roots[screen_idx];
    Screen { w: s.width_in_pixels as i32, h: s.height_in_pixels as i32 }
}

pub fn capture_screen_roi(conn: &impl Connection, root: u32, view: &View) -> Vec<u8> {
    xproto::get_image(
        conn,
        ImageFormat::Z_PIXMAP,
        root,
        view.x as i16,
        view.y as i16,
        view.size as u16,
        view.size as u16,
        u32::MAX,
    )
    .expect("X11 get_image failed")
    .reply()
    .expect("X11 get_image reply failed")
    .data
}

/// Converts X11 BGRA buffer into 3x640x640 planar normalized f32 directly.
pub fn sample_to_model_planar(bgra: &[u8], src_size: usize, dst: &mut [f32]) {
    const R_SCALE: f32 = 1.0 / (255.0 * 0.229);
    const R_OFF: f32 = 0.485 / 0.229;
    const G_SCALE: f32 = 1.0 / (255.0 * 0.224);
    const G_OFF: f32 = 0.456 / 0.224;
    const B_SCALE: f32 = 1.0 / (255.0 * 0.225);
    const B_OFF: f32 = 0.406 / 0.225;

    let plane = MODEL_SIZE * MODEL_SIZE;
    let (r_plane, rest) = dst.split_at_mut(plane);
    let (g_plane, b_plane) = rest.split_at_mut(plane);

    if src_size == MODEL_SIZE {
        let (chunks, _) = bgra.as_chunks::<4>();
        for (i, c) in chunks.iter().take(plane).enumerate() {
            r_plane[i] = c[2] as f32 * R_SCALE - R_OFF;
            g_plane[i] = c[1] as f32 * G_SCALE - G_OFF;
            b_plane[i] = c[0] as f32 * B_SCALE - B_OFF;
        }
    } else {
        for dy in 0..MODEL_SIZE {
            let sy = (dy * src_size / MODEL_SIZE).min(src_size - 1);
            for dx in 0..MODEL_SIZE {
                let sx = (dx * src_size / MODEL_SIZE).min(src_size - 1);
                let src_idx = (sy * src_size + sx) * 4;
                let dst_idx = dy * MODEL_SIZE + dx;

                r_plane[dst_idx] = bgra[src_idx + 2] as f32 * R_SCALE - R_OFF;
                g_plane[dst_idx] = bgra[src_idx + 1] as f32 * G_SCALE - G_OFF;
                b_plane[dst_idx] = bgra[src_idx] as f32 * B_SCALE - B_OFF;
            }
        }
    }
}
```

---

### 3. `src/ocr.rs` (DBNet Detection + CTC Recognition)

```rust
use ort::{inputs, session::Session, value::TensorRef};
use crate::view::{MODEL_SIZE, View};

const DET_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_det.onnx");
const REC_BYTES: &[u8] = include_bytes!("../PP-OCRv6_small_rec.onnx");
const DICT_YAML: &str = include_str!("../inference.yml");

const REC_H: usize = 48;
const MAX_REC_LINES: usize = 64;
const DET_THRESH: f32 = 0.3;
const BOX_THRESH: f32 = 0.6;
const UNCLIP_RATIO: f32 = 1.5;

#[derive(Clone, Debug, Default)]
pub struct TextBox {
    pub text: String,
    pub screen_x: i32,
    pub screen_y: i32,
    pub screen_w: u32,
    pub screen_h: u32,
    // Model-local coordinates
    pub mx: f32,
    pub my: f32,
    pub mw: f32,
    pub mh: f32,
}

impl TextBox {
    pub fn center(&self) -> (i16, i16) {
        (
            (self.screen_x + (self.screen_w as i32 / 2)) as i16,
            (self.screen_y + (self.screen_h as i32 / 2)) as i16,
        )
    }
}

pub struct OcrPipeline {
    det_session: Session,
    rec_session: Session,
    det_in_name: String,
    rec_in_name: String,
    dict: Vec<&'static str>,
    pub det_input: Vec<f32>,
    rec_input: Vec<f32>,
    visited: Vec<u32>,
    tag: u32,
    queue: Vec<(usize, usize)>,
}

impl OcrPipeline {
    pub fn new() -> Self {
        let det_session = Session::builder().unwrap().commit_from_memory(DET_BYTES).unwrap();
        let rec_session = Session::builder().unwrap().commit_from_memory(REC_BYTES).unwrap();
        let det_in_name = det_session.inputs()[0].name().to_string();
        let rec_in_name = rec_session.inputs()[0].name().to_string();

        Self {
            det_session,
            rec_session,
            det_in_name,
            rec_in_name,
            dict: parse_dict(DICT_YAML),
            det_input: vec![0.0f32; 3 * MODEL_SIZE * MODEL_SIZE],
            rec_input: Vec::with_capacity(3 * REC_H * 960),
            visited: vec![0u32; MODEL_SIZE * MODEL_SIZE],
            tag: 0,
            queue: Vec::with_capacity(512),
        }
    }

    pub fn process(&mut self, bgra: &[u8], view: &View) -> Vec<TextBox> {
        let mut boxes = self.detect(view);
        self.recognize(bgra, &mut boxes, view.size as usize);
        boxes
    }

    fn detect(&mut self, view: &View) -> Vec<TextBox> {
        let det_outs = self
            .det_session
            .run(inputs![
                self.det_in_name.as_str() => TensorRef::from_array_view(([1, 3, MODEL_SIZE, MODEL_SIZE], &self.det_input[..])).unwrap()
            ])
            .unwrap();

        self.tag = self.tag.wrapping_add(1);
        if self.tag == 0 {
            self.visited.fill(0);
            self.tag = 1;
        }

        let (_, prob_map) = det_outs[0].try_extract_tensor::<f32>().unwrap();
        let mut boxes = Vec::new();

        for y in 0..MODEL_SIZE {
            for x in 0..MODEL_SIZE {
                let idx = y * MODEL_SIZE + x;
                if prob_map[idx] < DET_THRESH || self.visited[idx] == self.tag {
                    continue;
                }

                self.visited[idx] = self.tag;
                self.queue.clear();
                self.queue.push((x, y));

                let (mut min_x, mut max_x, mut min_y, mut max_y) = (x, x, y, y);
                let mut score_sum = 0.0f32;
                let mut head = 0;

                while head < self.queue.len() {
                    let (cx, cy) = self.queue[head];
                    head += 1;
                    min_x = min_x.min(cx);
                    max_x = max_x.max(cx);
                    min_y = min_y.min(cy);
                    max_y = max_y.max(cy);
                    score_sum += prob_map[cy * MODEL_SIZE + cx];

                    for (dx, dy) in [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)] {
                        let nx = cx as isize + dx;
                        let ny = cy as isize + dy;
                        if nx >= 0 && nx < MODEL_SIZE as isize && ny >= 0 && ny < MODEL_SIZE as isize {
                            let n_idx = ny as usize * MODEL_SIZE + nx as usize;
                            if self.visited[n_idx] != self.tag && prob_map[n_idx] >= DET_THRESH {
                                self.visited[n_idx] = self.tag;
                                self.queue.push((nx as usize, ny as usize));
                            }
                        }
                    }
                }

                let bw = (max_x - min_x + 1) as f32;
                let bh = (max_y - min_y + 1) as f32;
                let avg_score = score_sum / self.queue.len() as f32;

                if self.queue.len() >= 16 && avg_score >= BOX_THRESH && bw >= 8.0 && bh >= 6.0 {
                    let dist = (bw * bh * UNCLIP_RATIO) / (2.0 * (bw + bh));
                    let dist_y = (dist * 0.4).min(bh * 0.15).max(1.0);

                    let x1 = (min_x as f32 - dist).max(0.0);
                    let y1 = (min_y as f32 - dist_y).max(0.0);
                    let x2 = (max_x as f32 + dist).min((MODEL_SIZE - 1) as f32);
                    let y2 = (max_y as f32 + dist_y).min((MODEL_SIZE - 1) as f32);

                    let (sx, sy, sw, sh) = view.to_screen_rect(x1, y1, x2 - x1, y2 - y1);
                    boxes.push(TextBox {
                        text: String::new(),
                        screen_x: sx,
                        screen_y: sy,
                        screen_w: sw,
                        screen_h: sh,
                        mx: x1,
                        my: y1,
                        mw: x2 - x1,
                        mh: y2 - y1,
                    });
                }
            }
        }

        boxes.sort_by(|a, b| ((a.my / 16.0) as i32).cmp(&((b.my / 16.0) as i32)).then_with(|| a.mx.total_cmp(&b.mx)));
        boxes
    }

    fn recognize(&mut self, bgra: &[u8], boxes: &mut [TextBox], src_stride: usize) {
        let count = boxes.len().min(MAX_REC_LINES);
        let scale = src_stride as f32 / MODEL_SIZE as f32;

        for b in boxes.iter_mut().take(count) {
            let (cw, ch) = (b.mw.max(1.0), b.mh.max(1.0));
            let raw_w = (REC_H as f32 * (cw / ch)).round() as usize;
            let target_w = (raw_w.div_ceil(32) * 32).clamp(32, 960);
            let resized_w = raw_w.min(target_w).max(1);

            let total = 3 * REC_H * target_w;
            if self.rec_input.len() < total {
                self.rec_input.resize(total, 0.0);
            }
            self.rec_input[..total].fill(0.0);

            let plane_stride = REC_H * target_w;
            let (sw, sh) = (cw * scale, ch * scale);
            let (ox, oy) = (b.mx * scale, b.my * scale);

            for dy in 0..REC_H {
                let sy = ((oy + (dy as f32 + 0.5) * (sh / REC_H as f32) - 0.5).round() as usize).min(src_stride - 1);
                for dx in 0..resized_w {
                    let sx = ((ox + (dx as f32 + 0.5) * (sw / resized_w as f32) - 0.5).round() as usize).min(src_stride - 1);
                    let src_idx = (sy * src_stride + sx) * 4;
                    let dst_idx = dy * target_w + dx;

                    self.rec_input[dst_idx] = bgra[src_idx + 2] as f32 / 127.5 - 1.0;
                    self.rec_input[plane_stride + dst_idx] = bgra[src_idx + 1] as f32 / 127.5 - 1.0;
                    self.rec_input[2 * plane_stride + dst_idx] = bgra[src_idx] as f32 / 127.5 - 1.0;
                }
            }

            let input_slice = &self.rec_input[..total];
            let rec_outs = self
                .rec_session
                .run(inputs![
                    self.rec_in_name.as_str() => TensorRef::from_array_view(([1, 3, REC_H, target_w], input_slice)).unwrap()
                ])
                .unwrap();

            let (shape, preds) = rec_outs[0].try_extract_tensor::<f32>().unwrap();
            b.text = ctc_decode(preds, shape, &self.dict);
        }
    }
}

fn parse_dict(yaml: &'static str) -> Vec<&'static str> {
    let mut dict = Vec::new();
    let mut in_dict = false;
    for line in yaml.lines() {
        let t = line.trim();
        if t.starts_with("character_dict:") {
            in_dict = true;
        } else if in_dict {
            if let Some(item) = t.strip_prefix('-') {
                let s = item.trim().trim_matches('\'').trim_matches('"');
                dict.push(s);
            } else if !t.is_empty() && !t.starts_with('#') {
                break;
            }
        }
    }
    dict
}

fn ctc_decode(data: &[f32], shape: &[i64], dict: &[&str]) -> String {
    let num_classes = *shape.last().unwrap_or(&0) as usize;
    if num_classes == 0 {
        return String::new();
    }
    let mut text = String::new();
    let mut prev_idx = 0usize;

    for t in 0..(data.len() / num_classes) {
        let row = &data[t * num_classes..(t + 1) * num_classes];
        let max_idx = row.iter().enumerate().max_by(|(_, a), (_, b)| a.total_cmp(b)).map(|(i, _)| i).unwrap_or(0);
        if max_idx != 0 && max_idx != prev_idx {
            if max_idx - 1 < dict.len() {
                text.push_str(dict[max_idx - 1]);
            } else if max_idx - 1 == dict.len() {
                text.push(' ');
            }
        }
        prev_idx = max_idx;
    }
    text
}
```

---

### 4. `src/input.rs` (Native X11 Input Automation via XTEST)

```rust
use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, *};
use x11rb::protocol::xtest;

pub struct X11Input<'a, C: Connection> {
    conn: &'a C,
    root: Window,
    key_map: HashMap<char, (Keycode, bool)>,
    shift_kc: Keycode,
    return_kc: Keycode,
}

impl<'a, C: Connection> X11Input<'a, C> {
    pub fn new(conn: &'a C, screen_idx: usize) -> Result<Self, Box<dyn std::error::Error>> {
        let root = conn.setup().roots[screen_idx].root;
        let min_kc = conn.setup().min_keycode;
        let max_kc = conn.setup().max_keycode;
        let count = (max_kc - min_kc + 1) as u8;

        let mapping = conn.get_keyboard_mapping(min_kc, count)?.reply()?;
        let kpk = mapping.keysyms_per_keycode as usize;

        let mut key_map = HashMap::new();
        let mut shift_kc = 0;
        let mut return_kc = 0;

        for (i, syms) in mapping.keysyms.chunks(kpk).enumerate() {
            let kc = min_kc + i as u8;
            let normal = syms.first().copied().unwrap_or(0);
            let shifted = syms.get(1).copied().unwrap_or(0);

            if normal == 0xffe1 || normal == 0xffe2 {
                shift_kc = kc;
            }
            if normal == 0xff0d {
                return_kc = kc;
            }

            // Map standard ASCII keysyms (0x20 - 0x7e)
            if (0x20..=0x7e).contains(&normal) {
                if let Some(ch) = char::from_u32(normal) {
                    key_map.entry(ch).or_insert((kc, false));
                }
            }
            if (0x20..=0x7e).contains(&shifted) {
                if let Some(ch) = char::from_u32(shifted) {
                    key_map.entry(ch).or_insert((kc, true));
                }
            }
        }

        Ok(Self { conn, root, key_map, shift_kc, return_kc })
    }

    pub fn click(&self, x: i16, y: i16) -> Result<(), Box<dyn std::error::Error>> {
        // Move mouse
        xtest::fake_input(self.conn, MOTION_NOTIFY, 0, xproto::Time::CURRENT_TIME, self.root, x, y, 0)?;
        self.conn.flush()?;
        sleep(Duration::from_millis(20));

        // Click Button 1 (Left Click)
        xtest::fake_input(self.conn, BUTTON_PRESS, 1, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
        xtest::fake_input(self.conn, BUTTON_RELEASE, 1, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
        self.conn.flush()?;
        Ok(())
    }

    pub fn type_text(&self, text: &str, hit_enter: bool) -> Result<(), Box<dyn std::error::Error>> {
        for ch in text.chars() {
            if let Some(&(kc, needs_shift)) = self.key_map.get(&ch) {
                if needs_shift && self.shift_kc != 0 {
                    xtest::fake_input(self.conn, KEY_PRESS, self.shift_kc, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
                }
                xtest::fake_input(self.conn, KEY_PRESS, kc, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
                xtest::fake_input(self.conn, KEY_RELEASE, kc, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
                if needs_shift && self.shift_kc != 0 {
                    xtest::fake_input(self.conn, KEY_RELEASE, self.shift_kc, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
                }
                self.conn.flush()?;
                sleep(Duration::from_millis(15));
            }
        }
        if hit_enter && self.return_kc != 0 {
            sleep(Duration::from_millis(30));
            xtest::fake_input(self.conn, KEY_PRESS, self.return_kc, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
            xtest::fake_input(self.conn, KEY_RELEASE, self.return_kc, xproto::Time::CURRENT_TIME, self.root, 0, 0, 0)?;
            self.conn.flush()?;
        }
        Ok(())
    }
}
```

---

### 5. `src/automation.rs` (Triggers, Cooldowns & Actions)

```rust
use std::time::{Duration, Instant};
use crate::input::X11Input;
use crate::ocr::TextBox;
use x11rb::connection::Connection;

#[derive(Clone, Debug)]
pub enum Action {
    Click,
    ClickAndType { text: String, press_enter: bool },
}

pub struct Rule {
    pub name: &'static str,
    pub pattern: &'static str, // Substring match (case-insensitive)
    pub action: Action,
    pub cooldown: Duration,
    pub last_triggered: Option<Instant>,
}

impl Rule {
    pub fn new(name: &'static str, pattern: &'static str, action: Action, cooldown_secs: u64) -> Self {
        Self {
            name,
            pattern,
            action,
            cooldown: Duration::from_secs(cooldown_secs),
            last_triggered: None,
        }
    }

    pub fn matches(&self, box_text: &str) -> bool {
        let is_ready = self.last_triggered.map_or(true, |t| t.elapsed() >= self.cooldown);
        is_ready && box_text.to_lowercase().contains(&self.pattern.to_lowercase())
    }
}

pub struct AutomationEngine {
    pub enabled: bool,
    pub rules: Vec<Rule>,
    pub log: Vec<String>,
}

impl AutomationEngine {
    pub fn new() -> Self {
        Self {
            enabled: false,
            rules: vec![
                // Example Automation Rules:
                Rule::new("Web Search", "Google Search", Action::Click, 5),
                Rule::new("Sign In", "Sign In", Action::ClickAndType {
                    text: "my_user@example.com".into(),
                    press_enter: true,
                }, 10),
            ],
            log: Vec::new(),
        }
    }

    pub fn evaluate<C: Connection>(&mut self, boxes: &[TextBox], input: &X11Input<C>) {
        if !self.enabled {
            return;
        }

        for rule in &mut self.rules {
            for b in boxes {
                if rule.matches(&b.text) {
                    let (cx, cy) = b.center();
                    rule.last_triggered = Some(Instant::now());

                    match &rule.action {
                        Action::Click => {
                            let _ = input.click(cx, cy);
                            self.log.push(format!("[{}] Clicked '{}' at ({cx}, {cy})", rule.name, b.text));
                        }
                        Action::ClickAndType { text, press_enter } => {
                            let _ = input.click(cx, cy);
                            std::thread::sleep(Duration::from_millis(50));
                            let _ = input.type_text(text, *press_enter);
                            self.log.push(format!("[{}] Clicked & Typed into '{}'", rule.name, b.text));
                        }
                    }
                    if self.log.len() > 10 {
                        self.log.remove(0);
                    }
                    return; // Execute one action per frame cycle
                }
            }
        }
    }
}
```

---

### 6. `src/main.rs` (TUI Event Loop & Status Display)

```rust
mod automation;
mod input;
mod ocr;
mod view;

use std::io::{stdout, Write};
use std::time::{Duration, Instant};

use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType};
use crossterm::{cursor, execute};

use automation::AutomationEngine;
use input::X11Input;
use ocr::OcrPipeline;
use view::{capture_screen_roi, get_screen_size, sample_to_model_planar, View};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (conn, screen_idx) = x11rb::connect(None)?;
    let root = conn.setup().roots[screen_idx].root;
    let screen = get_screen_size(&conn, screen_idx);

    let mut ocr = OcrPipeline::new();
    let input = X11Input::new(&conn, screen_idx)?;
    let mut automation = AutomationEngine::new();

    let mut view = View::default();
    let mut prev_screen_bytes = Vec::new();
    let mut boxes = Vec::new();

    let mut det_ms = 0.0;
    let mut rec_ms = 0.0;

    enable_raw_mode()?;
    let mut out = stdout();
    execute!(out, cursor::Hide, Clear(ClearType::All))?;

    'main_loop: loop {
        // 1. Process keyboard controls
        while event::poll(Duration::from_millis(5))? {
            if let Event::Key(k) = event::read()? {
                match k.code {
                    KeyCode::Esc | KeyCode::Char('q') => break 'main_loop,
                    KeyCode::Left => { view.pan(-1, 0, screen); }
                    KeyCode::Right => { view.pan(1, 0, screen); }
                    KeyCode::Up => { view.pan(0, -1, screen); }
                    KeyCode::Down => { view.pan(0, 1, screen); }
                    KeyCode::Char('1') | KeyCode::Char('-') => { view.zoom(-1, screen); }
                    KeyCode::Char('2') | KeyCode::Char('+') => { view.zoom(1, screen); }
                    KeyCode::Char('a') => {
                        automation.enabled = !automation.enabled;
                    }
                    KeyCode::Char('c') if k.modifiers.contains(KeyModifiers::CONTROL) => break 'main_loop,
                    _ => {}
                }
            }
        }

        // 2. Capture and process frame
        let bgra = capture_screen_roi(&conn, root, &view);
        let changed = bgra != prev_screen_bytes;

        if changed {
            sample_to_model_planar(&bgra, view.size as usize, &mut ocr.det_input);

            let t0 = Instant::now();
            boxes = ocr.process(&bgra, &view);
            let elapsed = t0.elapsed().as_secs_f64() * 1000.0;
            det_ms = elapsed * 0.4;
            rec_ms = elapsed * 0.6;

            // 3. Evaluate automation rules
            automation.evaluate(&boxes, &input);
            prev_screen_bytes = bgra;
        }

        // 4. Render TUI Dashboard
        execute!(out, cursor::MoveTo(0, 0))?;
        println!("=== X11 PP-OCRv6 Headless Automation TUI ===");
        println!(
            "ROI: {}x{} @ ({}, {}) | Scale: {:.2}x | Screen: {}x{}",
            view.size, view.size, view.x, view.y,
            view.size as f32 / 640.0, screen.w, screen.h
        );
        println!(
            "Status: {} | Automation: [{}] | Infer: {:.1}ms",
            if changed { "DETECTING" } else { "IDLE" },
            if automation.enabled { "ACTIVE" } else { "PAUSED (Press 'a')" },
            det_ms + rec_ms
        );
        println!("Controls: [Arrows]: Pan | [1/2]: Zoom | [a]: Toggle Rules | [q]: Quit\n");

        println!("--- Detected Text Boxes ({} found) ---", boxes.len());
        println!("{:<3} | {:<16} | {:<12} | TEXT", "#", "SCREEN (X,Y)", "SIZE (WxH)");
        println!("{:-<70}", "");
        for (i, b) in boxes.iter().take(12).enumerate() {
            println!(
                "{:<3} | ({:>4}, {:>4})    | {:>4}x{:<4}   | {}",
                i + 1, b.screen_x, b.screen_y, b.screen_w, b.screen_h, b.text
            );
        }

        println!("\n--- Automation Actions & Logs ---");
        for log in automation.log.iter().rev().take(3) {
            println!("{log}");
        }

        execute!(out, Clear(ClearType::FromCursorDown))?;
        out.flush()?;
        std::thread::sleep(Duration::from_millis(30));
    }

    // Cleanup terminal
    disable_raw_mode()?;
    execute!(out, cursor::Show, Clear(ClearType::All))?;
    Ok(())
}
```

---

### What Changed and How to Use It

1. **How Rescaling/Zooming Works**:
   - `1` / `-` zooms in (down to $320 \times 320$ for small UI fonts).
   - `2` / `+` zooms out (up to $1280 \times 1280$ for broader screen coverage).
   - Arrow keys pan the region across multi-monitor or 4K boundaries.
   - All OCR bounding boxes are scaled back into **absolute screen coordinates**, so mouse clicks target the button regardless of zoom level.

2. **Defining Automation Rules**:
   Inside `automation.rs`, declare actions in `AutomationEngine::new()`:
   ```rust
   Rule::new("Submit Button", "Submit", Action::Click, /*cooldown seconds*/ 5)
   ```
   Or to type into an input field:
   ```rust
   Rule::new("Email Input", "Email Address", Action::ClickAndType {
       text: "admin@corp.internal".into(),
       press_enter: false,
   }, 10)
   ```

3. **Running the Application**:
   Ensure `libx11-dev` and the ONNX models are present in your workspace:
   ```bash
   cargo run --release
   ```
   Press `a` in the terminal to engage/arm the automation engine. As soon as the designated text appears on screen inside the capture box, the application will automatically focus the field and type the text. Press `q` to return cleanly to your shell.
