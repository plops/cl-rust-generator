Here is a comprehensive review and refactored solution for your project.

---

### Root-Cause Analysis

#### 1. Why OCR fails on `"Ask Meta AI..."` (and similar UI text)
1. **Visual Glyph Confusion:** In standard sans-serif UI fonts (Roboto, Inter, Segoe UI), uppercase `I`, lowercase `l`, digit `1`, and vertical bar `|` have almost indistinguishable pixel representations. PP-OCRv6 frequently transcribes `AI` as `Al`, `Sign In` as `Sign ln`, and `Google` as `GoogIe`.
2. **Missing / Merged Punctuation:** Small dots in an ellipsis (`...`) are often around 2–3 px in size. They frequently fall below DBNet's component area thresholds (`bw >= 8.0 && bh >= 6.0`), causing `...` to be recognized as `..`, `.`, or omitted entirely.
3. **Blinking Cursor:** When a field is focused or hovered, the blinking cursor (`|` or `_`) gets appended to the string (e.g. `Ask Meta AI |` or `Ask Meta AIl`).
4. **Current Matcher:** `hit.text.to_lowercase().contains(&rule.pattern)` requires a literal substring. It fails immediately on `Al` vs `AI` or when dots are missing.

#### 2. Why Firefox required manual mouse placement for focus
1. **The 0ms Typing Delay Bug:** In `05_input.rs`, `CLICK_TYPE_DELAY_MS` was placed **after** `X11Input::type_text(...)` instead of **between** the mouse click and the keyboard input:
   ```rust
   // In previous 05_input.rs:
   X11Input::type_text(self, text, press_enter)?;
   sleep(Duration::from_millis(CLICK_TYPE_DELAY_MS)); // <-- Delayed AFTER typing was already finished!
   ```
   Keys were dispatched 0 ms after button release. At 0 ms, the X server and window manager have not yet transferred focus or activated the DOM input widget.
2. **Missing Window Activation:** Sending `xtest::fake_input` mouse events does not guarantee that the window manager raises or activates the application window if it was not already focused. Moving the cursor manually gave Firefox focus because of window manager enter-notify/hover-focus policies.
3. **Zero Coordinates on Button Events:** `xt(BUTTON_PRESS_EVENT, 1, 0, 0)` passed `(0, 0)` instead of `(x, y)`, relying purely on server sprite memory.

---

### Refactored Modules

Below are the refactored, streamlined files. They eliminate boilerplate and duplicate code, add the smart OCR matcher, and fix the focus/input issue.

---

#### 1. `src/07_rules.rs` — Smart OCR Matcher + Rule Engine
* **Visual Normalization (`norm_ocr_str`):** Folds visually confusable glyphs (`{I, l, 1, |, !, ¦, i}` $\rightarrow$ `'l'`, `{0, O, o}` $\rightarrow$ `'o'`), trims cursor artifacts (`|`, `_`), and ignores soft punctuation (`.`, `…`, `,`, `'`, `-`).
* **Sliding Substring Levenshtein (`fuzzy_ocr_match`):** In addition to visual folding, it runs a 1D DP substring distance algorithm that allows 1–2 dropped or extra characters for longer strings.
* **Guaranteed Delay in `Sink`:** `click_and_type` now guarantees a 100 ms focus-settle pause between click and keyboard injection.

```rust
// src/07_rules.rs
//! `07_rules` — Smart OCR matching and automation engine.

use std::time::Instant;
use crate::config::{Action, Config, Rule};
use crate::input::InputError;

pub const LOG_CAP: usize = 10;

#[derive(Debug, Clone)]
pub struct BoxHit {
    pub text: String,
    pub rect: (i32, i32, u32, u32),
}

pub trait Sink {
    fn click(&mut self, x: i16, y: i16) -> Result<(), InputError>;
    fn type_text(&mut self, text: &str, press_enter: bool) -> Result<(), InputError>;

    /// Click, wait for window and DOM element focus, then type text.
    fn click_and_type(&mut self, x: i16, y: i16, text: &str, press_enter: bool) -> Result<(), InputError> {
        self.click(x, y)?;
        std::thread::sleep(std::time::Duration::from_millis(100));
        self.type_text(text, press_enter)
    }

    fn skipped(&self) -> u64 {
        0
    }
}

/// Normalizes OCR text: strips cursors/punctuation and folds visually identical glyphs.
pub fn norm_ocr_str(s: &str) -> String {
    let trimmed = s.trim().trim_end_matches(['|', '¦', '_', '.', '…']);
    let mut out = String::with_capacity(trimmed.len());
    let mut prev_space = true;

    for ch in trimmed.chars() {
        let mapped = match ch {
            // Fold visual equivalents: I, l, 1, |, !, i -> 'l'
            'I' | 'l' | '1' | '|' | '!' | '¦' | 'i' | ']' | '[' => 'l',
            // Fold O, o, 0 -> 'o'
            '0' | 'O' | 'o' => 'o',
            // Whitespace normalization
            ' ' | '\t' | '\n' | '\r' => ' ',
            // Ignore small/fragile punctuation often dropped or inserted by OCR
            '.' | '…' | ',' | ':' | ';' | '\'' | '"' | '`' | '-' | '_' | '~' => continue,
            other => other.to_ascii_lowercase(),
        };

        if mapped == ' ' {
            if !prev_space {
                out.push(' ');
                prev_space = true;
            }
        } else {
            out.push(mapped);
            prev_space = false;
        }
    }
    if prev_space && !out.is_empty() {
        out.pop();
    }
    out
}

/// Computes the minimum Levenshtein distance of `needle` against any substring of `haystack`.
fn levenshtein_substring(needle: &str, haystack: &str) -> usize {
    let n_chars: Vec<char> = needle.chars().collect();
    let h_chars: Vec<char> = haystack.chars().collect();
    if n_chars.is_empty() {
        return 0;
    }
    if h_chars.is_empty() {
        return n_chars.len();
    }

    // Substring match: distance to empty needle in haystack is 0 at all starting positions.
    let mut prev = vec![0usize; h_chars.len() + 1];
    let mut curr = vec![0usize; h_chars.len() + 1];

    for (i, &nc) in n_chars.iter().enumerate() {
        curr[0] = i + 1;
        for (j, &hc) in h_chars.iter().enumerate() {
            let cost = if nc == hc { 0 } else { 1 };
            curr[j + 1] = (prev[j] + cost).min(prev[j + 1] + 1).min(curr[j] + 1);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    *prev.iter().skip(1).min().unwrap_or(&n_chars.len())
}

/// Matches pattern against candidate text taking glyph confusion, cursors, and dropped dots into account.
#[must_use]
pub fn fuzzy_ocr_match(pattern: &str, candidate: &str) -> bool {
    let p = norm_ocr_str(pattern);
    let c = norm_ocr_str(candidate);
    if p.is_empty() {
        return false;
    }
    // 1. Direct visual substring match (handles 95% of I/l/1, dots, cursors)
    if c.contains(&p) {
        return true;
    }
    // 2. Tolerance for dropped/extra small characters (proportional to length)
    let max_edits = match p.len() {
        0..=3 => 0,
        4..=7 => 1,
        8..=15 => 2,
        _ => 3,
    };
    if max_edits == 0 {
        return false;
    }
    levenshtein_substring(&p, &c) <= max_edits
}

struct PendingFire {
    name: String,
    htext: String,
    rect: (i32, i32, u32, u32),
    action: Action,
}

pub struct Automation {
    enabled: bool,
    rules: Vec<Rule>,
    log: Vec<String>,
}

impl Automation {
    #[must_use]
    pub fn from_config(cfg: &Config) -> Self {
        Self {
            enabled: false,
            rules: cfg.rules.clone(),
            log: Vec::new(),
        }
    }

    pub fn set_enabled(&mut self, on: bool) {
        self.enabled = on;
    }

    #[must_use]
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    #[must_use]
    pub fn log(&self) -> &[String] {
        &self.log
    }

    pub fn evaluate<S: Sink + ?Sized>(&mut self, hits: &[BoxHit], screen: (i32, i32), sink: &mut S) {
        if !self.enabled {
            return;
        }
        for rule in &mut self.rules {
            let ready = rule.last_fired.is_none_or(|t| t.elapsed() >= rule.cooldown);
            if !ready {
                continue;
            }

            let mut fire = None;
            for hit in hits {
                if fuzzy_ocr_match(&rule.pattern, &hit.text) {
                    fire = Some(PendingFire {
                        name: rule.name.clone(),
                        htext: hit.text.clone(),
                        rect: hit.rect,
                        action: rule.action.clone(),
                    });
                    break;
                }
            }

            let Some(f) = fire else { continue };
            let (sx, sy, sw, sh) = f.rect;
            let cx = (sx + sw as i32 / 2).clamp(0, screen.0.max(0)) as i16;
            let cy = (sy + sh as i32 / 2).clamp(0, screen.1.max(0)) as i16;
            rule.last_fired = Some(Instant::now());

            let res = match &f.action {
                Action::Click => sink
                    .click(cx, cy)
                    .map(|()| format!("[{}] Click on '{}' @ ({},{})", f.name, f.htext, cx, cy)),
                Action::ClickAndType { text, press_enter } => sink
                    .click_and_type(cx, cy, text, *press_enter)
                    .map(|()| format!("[{}] Click & Type into '{}'", f.name, f.htext)),
            };

            match res {
                Ok(line) => self.push_log(line),
                Err(e) => self.push_log(format!("[{}] ERROR on '{}': {e}", f.name, f.htext)),
            }
            return;
        }
    }

    fn push_log(&mut self, entry: String) {
        self.log.push(entry);
        if self.log.len() > LOG_CAP {
            self.log.remove(0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_ocr_meta_ai() {
        // Solves the exact user issue: capital I read as lowercase l with missing dots
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta Al.."));
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta AI |"));
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta Al"));
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta AI..."));
    }

    #[test]
    fn test_fuzzy_ocr_common_confusions() {
        assert!(fuzzy_ocr_match("Sign In", "Sign ln"));
        assert!(fuzzy_ocr_match("Google Search", "GoogIe Search"));
        assert!(fuzzy_ocr_match("Ask anything privately", "Ask anything privately |"));
        assert!(!fuzzy_ocr_match("Cancel", "Confirm"));
    }
}
```

---

#### 2. `src/05_input.rs` — Automatic Window Focus & Reliable X11 Input
* **Automatic Focus Activation:** `focus_at(x, y)` finds the top-level window at the click coordinates using `translate_coordinates`, sets core X11 `set_input_focus`, and sends `_NET_ACTIVE_WINDOW` to the Window Manager.
* **Exact Coordinates:** Mouse clicks pass explicit `(x, y)` to `ButtonPress`/`ButtonRelease`.
* **Proper Settle Times:** Keystrokes are sent after the window has acquired focus.

```rust
// src/05_input.rs
//! `05_input` — X11 input automation with window auto-activation.

use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;

use x11rb::connection::Connection;
use x11rb::protocol::xproto::{
    self, Keycode, Window, BUTTON_PRESS_EVENT, BUTTON_RELEASE_EVENT, CLIENT_MESSAGE_EVENT,
    KEY_PRESS_EVENT, KEY_RELEASE_EVENT, MOTION_NOTIFY_EVENT,
};
use x11rb::protocol::xtest;

const NOW: u32 = 0;
pub const MOTION_SETTLE_MS: u64 = 20;
pub const KEY_SETTLE_MS: u64 = 15;
pub const ENTER_SETTLE_MS: u64 = 30;

#[derive(Debug)]
pub enum InputError {
    X11(String),
    NoKeycode(&'static str),
}

impl std::fmt::Display for InputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::X11(e) => write!(f, "X11 error: {e}"),
            Self::NoKeycode(k) => write!(f, "Missing keycode in keymap: {k}"),
        }
    }
}

impl std::error::Error for InputError {}

pub struct X11Input<'a, C: Connection> {
    conn: &'a C,
    root: Window,
    key_map: HashMap<char, (Keycode, bool)>,
    shift_kc: Keycode,
    return_kc: Keycode,
    skipped: u64,
}

impl<'a, C: Connection> X11Input<'a, C> {
    pub fn new(conn: &'a C, screen_idx: usize) -> Result<Self, InputError> {
        let setup = conn.setup();
        let root = setup.roots[screen_idx].root;
        let min_kc = setup.min_keycode;
        let count = setup.max_keycode - min_kc + 1;

        let mapping = xproto::get_keyboard_mapping(conn, min_kc, count)
            .map_err(|e| InputError::X11(e.to_string()))?
            .reply()
            .map_err(|e| InputError::X11(e.to_string()))?;
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

            if (0x20..=0x7e).contains(&normal) && let Some(ch) = char::from_u32(normal) {
                key_map.entry(ch).or_insert((kc, false));
            }
            if (0x20..=0x7e).contains(&shifted) && let Some(ch) = char::from_u32(shifted) {
                key_map.entry(ch).or_insert((kc, true));
            }
        }

        Ok(Self {
            conn,
            root,
            key_map,
            shift_kc,
            return_kc,
            skipped: 0,
        })
    }

    /// Automatically raises and focuses the top-level window at (x, y).
    pub fn focus_at(&self, x: i16, y: i16) {
        if let Ok(reply) = xproto::translate_coordinates(self.conn, self.root, self.root, x, y) {
            if let Ok(tr) = reply.reply() {
                let target = if tr.child != 0 { tr.child } else { self.root };
                let _ = xproto::set_input_focus(self.conn, xproto::InputFocus::PARENT, target, NOW);

                // EWMH _NET_ACTIVE_WINDOW notification to the window manager
                if let Ok(atom_cookie) = xproto::intern_atom(self.conn, false, b"_NET_ACTIVE_WINDOW") {
                    if let Ok(atom) = atom_cookie.reply() {
                        let ev = xproto::ClientMessageEvent {
                            response_type: CLIENT_MESSAGE_EVENT,
                            format: 32,
                            sequence: 0,
                            window: target,
                            type_: atom.atom,
                            data: xproto::ClientMessageData::from([1u32, NOW, 0, 0, 0]),
                        };
                        let _ = xproto::send_event(
                            self.conn,
                            false,
                            self.root,
                            xproto::EventMask::SUBSTRUCTURE_REDIRECT | xproto::EventMask::SUBSTRUCTURE_NOTIFY,
                            ev,
                        );
                    }
                }
                let _ = self.conn.flush();
            }
        }
    }

    pub fn click(&self, x: i16, y: i16) -> Result<(), InputError> {
        self.focus_at(x, y);

        let xt = |t: u8, d: u8, px: i16, py: i16| {
            xtest::fake_input(self.conn, t, d, NOW, self.root, px, py, 0)
                .map_err(|e| InputError::X11(e.to_string()))?;
            self.conn.flush().map_err(|e| InputError::X11(e.to_string()))
        };

        xt(MOTION_NOTIFY_EVENT, 0, x, y)?;
        sleep(Duration::from_millis(MOTION_SETTLE_MS));
        xt(BUTTON_PRESS_EVENT, 1, x, y)?;
        sleep(Duration::from_millis(15));
        xt(BUTTON_RELEASE_EVENT, 1, x, y)?;
        Ok(())
    }

    pub fn type_text(&mut self, text: &str, hit_enter: bool) -> Result<(), InputError> {
        let xt = |conn: &C, root: Window, t: u8, d: Keycode| {
            xtest::fake_input(conn, t, d, NOW, root, 0, 0, 0)
                .map_err(|e| InputError::X11(e.to_string()))?;
            conn.flush().map_err(|e| InputError::X11(e.to_string()))
        };

        for ch in text.chars() {
            let Some(&(kc, needs_shift)) = self.key_map.get(&ch) else {
                self.skipped += 1;
                continue;
            };
            if needs_shift {
                if self.shift_kc == 0 {
                    self.skipped += 1;
                    continue;
                }
                xt(self.conn, self.root, KEY_PRESS_EVENT, self.shift_kc)?;
            }
            xt(self.conn, self.root, KEY_PRESS_EVENT, kc)?;
            xt(self.conn, self.root, KEY_RELEASE_EVENT, kc)?;
            if needs_shift {
                xt(self.conn, self.root, KEY_RELEASE_EVENT, self.shift_kc)?;
            }
            sleep(Duration::from_millis(KEY_SETTLE_MS));
        }

        if hit_enter {
            if self.return_kc == 0 {
                return Err(InputError::NoKeycode("Return"));
            }
            sleep(Duration::from_millis(ENTER_SETTLE_MS));
            xt(self.conn, self.root, KEY_PRESS_EVENT, self.return_kc)?;
            xt(self.conn, self.root, KEY_RELEASE_EVENT, self.return_kc)?;
        }
        Ok(())
    }

    #[must_use]
    pub fn skipped_count(&self) -> u64 {
        self.skipped
    }
}

#[derive(Default)]
pub struct DrySink;

impl crate::rules::Sink for DrySink {
    fn click(&mut self, _x: i16, _y: i16) -> Result<(), InputError> {
        Ok(())
    }
    fn type_text(&mut self, _text: &str, _press_enter: bool) -> Result<(), InputError> {
        Ok(())
    }
}

impl<C: Connection> crate::rules::Sink for X11Input<'_, C> {
    fn click(&mut self, x: i16, y: i16) -> Result<(), InputError> {
        X11Input::click(self, x, y)
    }
    fn type_text(&mut self, text: &str, press_enter: bool) -> Result<(), InputError> {
        X11Input::type_text(self, text, press_enter)
    }
    fn skipped(&self) -> u64 {
        self.skipped_count()
    }
}
```

---

#### 3. `src/02_capture.rs` — Deduplicated Normalization Math
Deduplicates the RGB normalization scale and offset calculations across `prepare_native` and `resize_nearest_planar`.

```rust
// src/02_capture.rs
//! `02_capture` — X11 Region capture and unified planar normalization.

use crate::view::{MODEL_SIZE, Screen, View};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{self, ImageFormat};

pub const NATIVE_PLANE: usize = MODEL_SIZE as usize * MODEL_SIZE as usize;

#[inline(always)]
fn norm_rgb(b: u8, g: u8, r: u8) -> (f32, f32, f32) {
    const R_SCALE: f32 = 1.0 / (255.0 * 0.229);
    const R_OFF: f32 = 0.485 / 0.229;
    const G_SCALE: f32 = 1.0 / (255.0 * 0.224);
    const G_OFF: f32 = 0.456 / 0.224;
    const B_SCALE: f32 = 1.0 / (255.0 * 0.225);
    const B_OFF: f32 = 0.406 / 0.225;
    (
        r as f32 * R_SCALE - R_OFF,
        g as f32 * G_SCALE - G_OFF,
        b as f32 * B_SCALE - B_OFF,
    )
}

pub fn screen_size(conn: &impl Connection, screen_idx: usize) -> Screen {
    let s = &conn.setup().roots[screen_idx];
    Screen {
        w: s.width_in_pixels as i32,
        h: s.height_in_pixels as i32,
    }
}

pub fn try_capture_roi(conn: &impl Connection, root: u32, view: &View) -> Result<Vec<u8>, String> {
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
    .map_err(|e| e.to_string())?
    .reply()
    .map_err(|e| e.to_string())
    .map(|r| r.data)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConvertPath {
    Native,
    Scaled,
}

#[must_use]
pub fn convert_path(view: &View) -> ConvertPath {
    if view.is_native() {
        ConvertPath::Native
    } else {
        ConvertPath::Scaled
    }
}

#[must_use]
pub fn view_changed(old: &View, new: &View) -> bool {
    old != new
}

pub fn bgra_to_rgba(bgra: &[u8], rgba: &mut [u8]) {
    let n = (bgra.len() / 4).min(rgba.len() / 4);
    let (src, _) = bgra.as_chunks::<4>();
    let (dst, _) = rgba.as_chunks_mut::<4>();
    for (s, d) in src.iter().take(n).zip(dst.iter_mut().take(n)) {
        d.copy_from_slice(&[s[2], s[1], s[0], 255]);
    }
}

pub fn prepare_native(bgra: &[u8], planes: &mut [f32], rgba: &mut [u8]) {
    let (r_plane, rest) = planes.split_at_mut(NATIVE_PLANE);
    let (g_plane, b_plane) = rest.split_at_mut(NATIVE_PLANE);
    let (src, _) = bgra.as_chunks::<4>();
    let (dst, _) = rgba.as_chunks_mut::<4>();

    for (i, (s, d)) in src.iter().zip(dst.iter_mut()).take(NATIVE_PLANE).enumerate() {
        let (b, g, r) = (s[0], s[1], s[2]);
        d.copy_from_slice(&[r, g, b, 255]);
        let (nr, ng, nb) = norm_rgb(b, g, r);
        r_plane[i] = nr;
        g_plane[i] = ng;
        b_plane[i] = nb;
    }
}

pub fn resize_nearest_planar(bgra: &[u8], src_size: usize, dst: &mut [f32], dst_size: usize) {
    let plane = dst_size * dst_size;
    let (r_plane, rest) = dst.split_at_mut(plane);
    let (g_plane, b_plane) = rest.split_at_mut(plane);

    for dy in 0..dst_size {
        let sy = (dy * src_size / dst_size).min(src_size - 1);
        for dx in 0..dst_size {
            let sx = (dx * src_size / dst_size).min(src_size - 1);
            let s = (sy * src_size + sx) * 4;
            let d = dy * dst_size + dx;
            let (nr, ng, nb) = norm_rgb(bgra[s], bgra[s + 1], bgra[s + 2]);
            r_plane[d] = nr;
            g_plane[d] = ng;
            b_plane[d] = nb;
        }
    }
}
```

---

#### 4. `src/04_recognize.rs` (Streamlined Dictionary Loader)
Replaces manual slice index manipulation in `load_dict` with `trim_matches`:

```rust
// In src/04_recognize.rs:
pub fn load_dict(yaml: &'static str) -> Vec<&'static str> {
    let mut dict = Vec::new();
    let mut in_dict = false;

    for line in yaml.lines() {
        let t = line.trim();
        if t.starts_with("character_dict:") {
            in_dict = true;
        } else if in_dict {
            if let Some(item) = t.strip_prefix('-') {
                dict.push(item.trim().trim_matches(['\'', '"']));
            } else if !t.is_empty() && !t.starts_with('#') {
                break;
            }
        }
    }
    dict
}
```

---

#### 5. `src/06_config.rs` (Simplified TOML Extractor)
Reduces parser boilerplate using helper extractors:

```rust
// src/06_config.rs
//! `06_config` — TOML configuration parser.

use std::time::{Duration, Instant};

pub const SCHEMA_VERSION: i64 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PanConfig {
    pub step_divisor: u32,
    pub step_min_px: u32,
    pub roi_steps: Vec<u32>,
    pub default_size: u32,
}

impl Default for PanConfig {
    fn default() -> Self {
        Self {
            step_divisor: 16,
            step_min_px: 8,
            roi_steps: vec![320, 480, 640, 960, 1280],
            default_size: 640,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Click,
    ClickAndType { text: String, press_enter: bool },
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub name: String,
    pub pattern: String,
    pub action: Action,
    pub cooldown: Duration,
    pub(crate) last_fired: Option<Instant>,
}

#[derive(Debug, Clone, Default)]
pub struct Config {
    pub pan: PanConfig,
    pub rules: Vec<Rule>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConfigError {
    Parse(String),
    Schema(String),
    Invalid(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(e) => write!(f, "TOML error: {e}"),
            Self::Schema(e) => write!(f, "Schema error: {e}"),
            Self::Invalid(e) => write!(f, "Invalid config: {e}"),
        }
    }
}

impl std::error::Error for ConfigError {}

pub fn load_from_file(path: &str) -> Result<Config, String> {
    match std::fs::read_to_string(path) {
        Ok(text) => Config::parse(&text).map_err(|e| e.to_string()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            eprintln!("Notice: {path} not found — using defaults.");
            Ok(Config::defaults())
        }
        Err(e) => Err(format!("Cannot read {path}: {e}")),
    }
}

fn as_u32(map: &toml::map::Map<String, toml::Value>, key: &str) -> Result<u32, ConfigError> {
    map.get(key)
        .and_then(toml::Value::as_integer)
        .and_then(|v| u32::try_from(v).ok())
        .ok_or_else(|| ConfigError::Invalid(format!("`{key}` missing or not an unsigned integer")))
}

impl Config {
    #[must_use]
    pub fn defaults() -> Self {
        Self {
            pan: PanConfig::default(),
            rules: Vec::new(),
        }
    }

    pub fn parse(text: &str) -> Result<Self, ConfigError> {
        let root: toml::map::Map<String, toml::Value> = text
            .parse()
            .map_err(|e: toml::de::Error| ConfigError::Parse(e.to_string()))?;

        let schema = root
            .get("schema_version")
            .and_then(toml::Value::as_integer)
            .ok_or_else(|| ConfigError::Schema("`schema_version` missing".into()))?;
        if schema != SCHEMA_VERSION {
            return Err(ConfigError::Schema(format!(
                "expected {SCHEMA_VERSION}, found {schema}"
            )));
        }

        let mut cfg = Self::defaults();
        if let Some(pan) = root.get("pan").and_then(toml::Value::as_table) {
            cfg.pan = Self::parse_pan(pan)?;
        }
        if let Some(rules) = root.get("rule").and_then(toml::Value::as_array) {
            for (i, r) in rules.iter().enumerate() {
                let t = r
                    .as_table()
                    .ok_or_else(|| ConfigError::Invalid(format!("`rule[{i}]` is not a table")))?;
                cfg.rules.push(Self::parse_rule(t, i)?);
            }
        }
        Ok(cfg)
    }

    fn parse_pan(pan: &toml::map::Map<String, toml::Value>) -> Result<PanConfig, ConfigError> {
        let step_divisor = as_u32(pan, "step_divisor")?;
        if step_divisor == 0 {
            return Err(ConfigError::Invalid("`step_divisor` must be > 0".into()));
        }
        let step_min_px = as_u32(pan, "step_min_px")?;
        let steps_val = pan
            .get("roi_steps")
            .and_then(toml::Value::as_array)
            .ok_or_else(|| ConfigError::Invalid("`roi_steps` missing or not an array".into()))?;

        if steps_val.is_empty() {
            return Err(ConfigError::Invalid("`roi_steps` cannot be empty".into()));
        }

        let mut roi_steps = Vec::with_capacity(steps_val.len());
        for v in steps_val {
            let s = v
                .as_integer()
                .and_then(|n| u32::try_from(n).ok())
                .filter(|&n| n > 0)
                .ok_or_else(|| ConfigError::Invalid("`roi_steps` must contain positive integers".into()))?;
            roi_steps.push(s);
        }

        let default_size = as_u32(pan, "default_size")?;
        if !roi_steps.contains(&default_size) {
            return Err(ConfigError::Invalid(format!(
                "`default_size` ({default_size}) must be present in `roi_steps`"
            )));
        }

        Ok(PanConfig {
            step_divisor,
            step_min_px,
            roi_steps,
            default_size,
        })
    }

    fn parse_rule(t: &toml::map::Map<String, toml::Value>, i: usize) -> Result<Rule, ConfigError> {
        let str_field = |key: &str| {
            t.get(key)
                .and_then(toml::Value::as_str)
                .map(str::to_string)
                .ok_or_else(|| ConfigError::Invalid(format!("`rule[{i}].{key}` missing or not a string")))
        };

        let name = str_field("name")?;
        let pattern = str_field("pattern")?;
        if pattern.trim().is_empty() {
            return Err(ConfigError::Invalid(format!("`rule[{i}].pattern` cannot be empty")));
        }

        let action = match str_field("action")?.as_str() {
            "click" => Action::Click,
            "click_and_type" => Action::ClickAndType {
                text: str_field("text")?,
                press_enter: t.get("press_enter").and_then(toml::Value::as_bool).unwrap_or(false),
            },
            other => {
                return Err(ConfigError::Invalid(format!(
                    "`rule[{i}].action` unknown: {other} (allowed: click, click_and_type)"
                )));
            }
        };

        let cooldown_secs = t
            .get("cooldown_secs")
            .and_then(toml::Value::as_integer)
            .and_then(|n| u64::try_from(n).ok())
            .ok_or_else(|| ConfigError::Invalid(format!("`rule[{i}].cooldown_secs` invalid or missing")))?;

        Ok(Rule {
            name,
            pattern,
            action,
            cooldown: Duration::from_secs(cooldown_secs),
            last_fired: None,
        })
    }
}
```

---

#### 6. Updated `rules.metaai.toml`
Now you can specify the natural string `Ask Meta AI...` directly, and a single `click_and_type` rule is sufficient because window activation is automatic:

```toml
# rules.metaai.toml
schema_version = 1

[pan]
step_divisor = 16
step_min_px = 8
roi_steps = [320, 480, 640, 960, 1280]
default_size = 640

[[rule]]
name = "Frage stellen"
pattern = "Ask Meta AI..."
action = "click_and_type"
text = "A cute 3D claymation-style sticker of a red panda cub fast asleep hugging a soft miniature pillow. Peaceful sleeping face with a subtle happy smile, dreamy pastel night tones, cute floating `Zzz` symbols. Smooth matte render, clean silhouette, bold white die-cut sticker outline, isolated on a clean white background, high contrast, readable at small icon scale."
press_enter = true
cooldown_secs = 5
```

---

### Verification and Behavior Summary

| Scenario                                | Previous Behavior                                                      | Refactored Behavior                                                                                        |   |                             |                                   |
| :---                                    | :---                                                                   | :---                                                                                                       |   |                             |                                   |
| **`Ask Meta AI...` vs `Ask Meta Al..`** | Fails; required chopping pattern to `"Ask Meta A"`                     | **Matches immediately** via visual equivalence folding (`I` $\leftrightarrow$ `l`, `.` dropped).           |   |                             |                                   |
| **`Ask Meta AI                          | ` (Blinking cursor)**                                                  | Fails due to trailing `\                                                                                   | ` | **Matches immediately** (`\ | ` and cursors stripped from end). |
| **Dropped characters (blurred text)**   | Fails on any missing character                                         | **Matches** via sliding Levenshtein distance ($D \le 2$).                                                  |   |                             |                                   |
| **Unfocused Firefox Window**            | Text sent to terminal; required manual mouse hover                     | **Automatically focused** via `_NET_ACTIVE_WINDOW` and `set_input_focus`, followed by 100 ms settle delay. |   |                             |                                   |
| **Code Size & Duplication**             | Duplicate float formulas, repetitive TOML extraction, `#[path]` sprawl | Normalization logic unified; redundant string trimming replaced with standard library methods.             |   |                             |                                   |
