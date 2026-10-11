//! Interaktiver X11-Viewer via `minifb` (nur Feature `show`).
//!
//! Rad = Zeit-Zoom um Cursor, Links-Drag = Pan, `+`/`-`/Pfeile ebenso,
//! `R` = Reset, `ESC` = Ende. Hover ueber dem Wasserfall zeigt
//! Fadenkreuz plus Frame-Details.

use minifb::{Key, KeyRepeat, MouseButton, MouseMode, Window, WindowOptions};

use crate::canvas::{FG, GRID, Image};
use crate::render::{freq_of_bin, layout, render_range};
use crate::spectrum::{BINS, HOP_MS};
use crate::types::{Analysis, Error, FrameMeta};

/// Plot-Breite im Fenster (unabhaengig von `--width`).
pub const VIEW_PLOT_W: u32 = 1200;
/// Kleinste Zoom-Spanne in ms (10 Hops).
pub const MIN_SPAN_MS: f64 = 200.0;

/// Sichtbarer Zeitbereich `[t0, t1)` in ms (rein/testbar).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct View {
    /// Anfang in ms.
    pub t0: f64,
    /// Ende in ms.
    pub t1: f64,
    /// Gesamt-Dauer in ms (Klemmgrenze).
    pub dur: f64,
}

impl View {
    /// Gesamtansicht.
    #[must_use]
    pub fn full(dur_ms: f64) -> Self {
        let dur = dur_ms.max(1.0);
        Self { t0: 0.0, t1: dur, dur }
    }

    /// Aktuelle Spanne in ms.
    #[must_use]
    pub fn span(&self) -> f64 {
        self.t1 - self.t0
    }

    /// Zoom um `anchor_ms` (Faktor < 1 = hinein).
    pub fn zoom_around(&mut self, anchor_ms: f64, factor: f64) {
        let min = MIN_SPAN_MS.min(self.dur);
        let span = (self.span() * factor).clamp(min, self.dur);
        let f = if self.span() > 0.0 {
            (anchor_ms - self.t0) / self.span()
        } else {
            0.5
        };
        let f = f.clamp(0.0, 1.0);
        self.t0 = anchor_ms - f * span;
        self.t1 = self.t0 + span;
        self.clamp_span(span);
    }

    /// Verschieben um `delta_ms` (positiv = spaeter).
    pub fn pan_by(&mut self, delta_ms: f64) {
        let span = self.span();
        self.t0 += delta_ms;
        self.t1 = self.t0 + span;
        self.clamp_span(span);
    }

    /// Zurueck zur Gesamtansicht.
    pub fn reset(&mut self) {
        self.t0 = 0.0;
        self.t1 = self.dur;
    }

    /// In `[0, dur]` schieben (Spanne bleibt, ausser bei winziger Datei).
    fn clamp_span(&mut self, span: f64) {
        let span = span.clamp(MIN_SPAN_MS.min(self.dur), self.dur);
        if self.t0 < 0.0 {
            self.t0 = 0.0;
            self.t1 = span;
        }
        if self.t1 > self.dur {
            self.t1 = self.dur;
            self.t0 = self.dur - span;
        }
        if self.t0 < 0.0 {
            self.t0 = 0.0;
            self.t1 = self.dur;
        }
    }

    /// Plot-Pixel (0-basiert) → Zeit in ms.
    #[must_use]
    pub fn pixel_to_ms(&self, x_px: f32, plot_w: u32) -> f64 {
        let f = (x_px / plot_w as f32).clamp(0.0, 1.0);
        self.t0 + f64::from(f) * self.span()
    }
}

/// Hover-Punkt: Zeit, Bin, Pegel, Frame (fuer Panel-Text).
#[derive(Debug, Clone, Copy)]
pub struct Hover<'a> {
    /// Zeit in ms.
    pub t_ms: f64,
    /// Log-Bin (0 = tief).
    pub bin: usize,
    /// Pegel in dB.
    pub db: f32,
    /// Frame an dieser Zeit (kann fehlen).
    pub frame: Option<&'a FrameMeta>,
}

/// Zwei Panel-Zeilen aus Hover-Punkt (rein/testbar).
#[must_use]
pub fn hover_lines(h: Hover<'_>) -> (String, String) {
    let l1 = format!(
        "T={:.3}S F={:.0}HZ PEGEL={:.0}DB",
        h.t_ms / 1000.0,
        freq_of_bin(h.bin),
        h.db,
    );
    let l2 = h.frame.map_or_else(
        || "KEIN FRAME".to_string(),
        |f| {
            format!(
                "{} {} {:.0}MS {}B {:.1}KBPS",
                f.mode, f.bandwidth.short(), f.duration_ms, f.bytes, f.kbps,
            )
        },
    );
    (l1, l2)
}

/// Fadenkreuz ueber den Wasserfall malen (Overlay, keine Basis).
fn draw_hover(img: &mut Image, lay: &crate::render::Layout, x: u32, y: u32) {
    for xx in 0..lay.plot_w {
        img.blend(lay.plot_x + xx, y, [255; 3], 0.3);
    }
    for yy in 0..BINS as u32 {
        img.blend(x, lay.plot_y + yy, [255; 3], 0.3);
    }
}

/// Infotext oben links in den Plot legen (Box + zwei Zeilen).
fn draw_panel(img: &mut Image, lay: &crate::render::Layout, l1: &str, l2: &str) {
    let w = l1.len().max(l2.len()) as u32 * 6 + 10;
    let (x, y) = (lay.plot_x + 4, lay.plot_y + 4);
    img.rect(x, y, w, 24, [10, 12, 18]);
    img.rect(x, y, w, 1, GRID);
    img.rect(x, y + 23, w, 1, GRID);
    img.rect(x, y, 1, 24, GRID);
    img.rect(x + w - 1, y, 1, 24, GRID);
    img.text(x + 5, y + 4, l1, FG);
    img.text(x + 5, y + 13, l2, FG);
}

/// Bedienhinweis in die Legendenzeile (nur Viewer, nicht PNG).
fn draw_hint(img: &mut Image, lay: &crate::render::Layout) {
    let hint = "RAD:ZOOM DRAG:PAN R:RESET ESC:ENDE";
    let x = lay.width.saturating_sub(8 + hint.len() as u32 * 6);
    img.text(x, lay.legend_y, hint, FG);
}

/// Viewer-Schleife: rendert nur bei Sicht-Aenderung neu.
///
/// `max_frames` begrenzt die Schleife (Test-Haken; normal `u64::MAX`).
pub fn show_interactive(
    analysis: &Analysis,
    cols: &[[f32; BINS]],
    title: &str,
    max_frames: u64,
) -> Result<(), Error> {
    let lay = layout(VIEW_PLOT_W);
    let (w, h) = (lay.width as usize, lay.height as usize);
    let mut window = Window::new(title, w, h, WindowOptions::default())
        .map_err(|e| Error::Show(format!("oeffnen: {e}")))?;
    window.set_target_fps(30);
    let mut view = View::full(analysis.summary.duration_ms);
    let mut base = render_range(analysis, cols, view.t0, view.t1, lay.plot_w);
    draw_hint(&mut base, &lay);
    let mut prev_mouse: Option<(f32, f32)> = None;
    let mut frames = 0;
    while window.is_open() && !window.is_key_down(Key::Escape) && frames < max_frames
    {
        let mouse = window.get_mouse_pos(MouseMode::Clamp);
        let pressed = window.get_keys_pressed(KeyRepeat::No);
        if pressed.contains(&Key::Escape) {
            break; // Kurzer Tastendruck (kein Halten) beendet auch.
        }
        let mut dirty = handle_keys(&window, &mut view, &pressed);
        if let Some((_, dy)) = window.get_scroll_wheel() {
            if dy != 0.0 {
                let anchor = mouse.map_or_else(
                    || view.pixel_to_ms(lay.plot_w as f32 / 2.0, lay.plot_w),
                    |(mx, _)| {
                        view.pixel_to_ms(mx - lay.plot_x as f32, lay.plot_w)
                    },
                );
                view.zoom_around(anchor, if dy > 0.0 { 0.8 } else { 1.25 });
                dirty = true;
            }
        }
        // Drag relativ zur Vorframe-Position (faengt auch Spruenge).
        if window.get_mouse_down(MouseButton::Left) {
            if let (Some((mx, _)), Some((px, _))) = (mouse, prev_mouse) {
                let dx_ms =
                    f64::from((px - mx) / lay.plot_w as f32) * view.span();
                if dx_ms != 0.0 {
                    view.pan_by(dx_ms);
                    dirty = true;
                }
            }
        }
        prev_mouse = mouse;
        if dirty {
            base = render_range(analysis, cols, view.t0, view.t1, lay.plot_w);
            draw_hint(&mut base, &lay);
        }
        let mut frame = base.clone();
        if let Some((mx, my)) = mouse {
            draw_hover_state(&mut frame, &lay, analysis, cols, &view, mx, my);
        }
        window
            .update_with_buffer(&pack(&frame), w, h)
            .map_err(|e| Error::Show(format!("anzeige: {e}")))?;
        frames += 1;
    }
    Ok(())
}

/// Tasten: `+`/`-` Zoom, Pfeile Pan, `R` Reset. Rueckgabe = geaendert.
fn handle_keys(window: &Window, view: &mut View, pressed: &[Key]) -> bool {
    let mid = view.pixel_to_ms(600.0, VIEW_PLOT_W);
    if window.is_key_down(Key::Equal) || window.is_key_down(Key::NumPadPlus) {
        view.zoom_around(mid, 0.95);
        true
    } else if window.is_key_down(Key::Minus) || window.is_key_down(Key::NumPadMinus)
    {
        view.zoom_around(mid, 1.05);
        true
    } else if window.is_key_down(Key::Left) {
        view.pan_by(-0.03 * view.span());
        true
    } else if window.is_key_down(Key::Right) {
        view.pan_by(0.03 * view.span());
        true
    } else if pressed.contains(&Key::R) {
        view.reset();
        true
    } else {
        false
    }
}

/// Hover-Overlay, falls die Maus im Wasserfall steht.
fn draw_hover_state(
    img: &mut Image,
    lay: &crate::render::Layout,
    analysis: &Analysis,
    cols: &[[f32; BINS]],
    view: &View,
    mx: f32,
    my: f32,
) {
    let (px, py) = (mx as i32, my as i32);
    let in_x = px >= lay.plot_x as i32 && px < (lay.plot_x + lay.plot_w) as i32;
    let in_y = py >= lay.plot_y as i32 && py < (lay.plot_y + BINS as u32) as i32;
    if !in_x || !in_y || cols.is_empty() {
        return;
    }
    let (x, y) = (px as u32, py as u32);
    draw_hover(img, lay, x, y);
    let t_ms = view.pixel_to_ms(mx - lay.plot_x as f32, lay.plot_w);
    let bin = BINS - 1 - (y - lay.plot_y) as usize;
    let hop = (t_ms / HOP_MS).floor() as usize;
    let db = cols[hop.min(cols.len() - 1)][bin.min(BINS - 1)];
    let (l1, l2) = hover_lines(Hover {
        t_ms,
        bin: bin.min(BINS - 1),
        db,
        frame: analysis.frame_at(t_ms),
    });
    draw_panel(img, lay, &l1, &l2);
}

/// RGB-Bytes → `0x00RRGGBB`-Puffer (minifb-Format, rein/testbar).
fn pack(img: &Image) -> Vec<u32> {
    img.rgb
        .as_chunks::<3>()
        .0
        .iter()
        .map(|p| (u32::from(p[0]) << 16) | (u32::from(p[1]) << 8) | u32::from(p[2]))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Bandwidth, Mode};

    #[test]
    fn zoom_keeps_anchor_stable() {
        let mut v = View::full(10_000.0);
        v.zoom_around(2500.0, 0.5); // Halbieren um 2,5 s.
        assert_eq!((v.t0, v.t1), (1250.0, 6250.0));
        // Anker-Anteil 25 % bleibt: (2500-1250)/5000 = 0,25.
        assert!(((2500.0 - v.t0) / v.span() - 0.25).abs() < 1e-9);
        // Herauszoomen ueber Vollbild klemmt.
        v.zoom_around(2500.0, 10.0);
        assert_eq!((v.t0, v.t1), (0.0, 10_000.0));
    }

    #[test]
    fn zoom_respects_min_span_and_pan_clamps() {
        let mut v = View::full(10_000.0);
        for _ in 0..30 {
            v.zoom_around(5000.0, 0.8);
        }
        assert!(v.span() >= MIN_SPAN_MS - 1e-9);
        v.pan_by(-99_999.0);
        assert_eq!(v.t0, 0.0);
        assert!((v.span() - MIN_SPAN_MS).abs() < 1e-6);
        v.pan_by(99_999.0);
        assert_eq!(v.t1, 10_000.0);
        v.reset();
        assert_eq!((v.t0, v.t1), (0.0, 10_000.0));
    }

    #[test]
    fn tiny_file_stays_full_view() {
        let mut v = View::full(100.0); // Kuerzer als MIN_SPAN.
        v.zoom_around(50.0, 0.5);
        v.pan_by(30.0);
        assert_eq!((v.t0, v.t1), (0.0, 100.0));
    }

    #[test]
    fn pixel_mapping_roundtrips() {
        let v = View { t0: 1000.0, t1: 3000.0, dur: 10_000.0 };
        assert_eq!(v.pixel_to_ms(0.0, 1200), 1000.0);
        assert_eq!(v.pixel_to_ms(1200.0, 1200), 3000.0);
        assert_eq!(v.pixel_to_ms(600.0, 1200), 2000.0);
        assert_eq!(v.pixel_to_ms(-50.0, 1200), 1000.0); // Klemmung.
    }

    #[test]
    fn hover_lines_format() {
        let f = FrameMeta {
            index: 3,
            t_start_ms: 60.0,
            duration_ms: 20.0,
            mode: Mode::Silk,
            bandwidth: Bandwidth::Wide,
            channels: 1,
            bytes: 61,
            kbps: 24.4,
        };
        let (l1, l2) = hover_lines(Hover {
            t_ms: 1234.567,
            bin: 93, // ≈ 440 Hz.
            db: -12.3,
            frame: Some(&f),
        });
        assert_eq!(l1, "T=1.235S F=441HZ PEGEL=-12DB");
        assert_eq!(l2, "SILK WB 20MS 61B 24.4KBPS");
        let (_, l2) = hover_lines(Hover {
            t_ms: 0.0,
            bin: 0,
            db: -72.0,
            frame: None,
        });
        assert_eq!(l2, "KEIN FRAME");
    }

    #[test]
    fn pack_orders_rgb_channels() {
        let img = Image { width: 2, height: 1, rgb: vec![255, 0, 0, 0, 17, 34] };
        assert_eq!(pack(&img), vec![0x00ff_0000, 0x0000_1122]);
    }
}
