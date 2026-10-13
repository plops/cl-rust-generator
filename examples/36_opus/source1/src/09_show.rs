//! Interaktiver X11-Viewer via `minifb` (nur Feature `show`).
//!
//! Rad = Zeit-Zoom um Cursor, Links-Drag = Pan, `+`/`-`/Pfeile ebenso,
//! `1`-`4` = Ansicht, `R` = Reset, `ESC` = Ende. Hover ueber dem
//! Wasserfall zeigt Fadenkreuz plus Frame-/CELT-Details.

use minifb::{Key, KeyRepeat, MouseButton, MouseMode, Window, WindowOptions};

use crate::bitstream::{Source, Sources};
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
        Self {
            t0: 0.0,
            t1: dur,
            dur,
        }
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

/// Hover-Punkt: Zeit, Bin, Wert, Frame, CELT-Info (fuer Panel-Text).
#[derive(Debug, Clone, Copy)]
pub struct Hover<'a> {
    /// Zeit in ms.
    pub t_ms: f64,
    /// Log-Bin (0 = tief).
    pub bin: usize,
    /// Wert an der Stelle (dB oder Bits, je Quelle).
    pub value: f32,
    /// Aktive Quelle (bestimmt Wert-Label).
    pub source: Source,
    /// Frame an dieser Zeit (kann fehlen).
    pub frame: Option<&'a FrameMeta>,
    /// Transient-Flag des Hops (`None` = kein CELT).
    pub transient: Option<bool>,
    /// Signal-Bits des Hops (`None` = kein CELT).
    pub frame_bits: Option<f32>,
    /// SILK-Werte des Hops: (Signal, Pitch-Hz, Gain-dB), `None` = kein SILK.
    pub silk: Option<(i8, f32, f32)>,
}

/// Drei Panel-Zeilen aus Hover-Punkt (rein/testbar).
#[must_use]
pub fn hover_lines(h: Hover<'_>) -> (String, String, String) {
    let val = match h.source {
        Source::Spectrum | Source::Bitstream => format!("PEGEL={:.0}DB", h.value),
        Source::Divergence if h.value.is_nan() => "DIV=---".to_string(),
        Source::Divergence => format!("DIV={:.0}DB", h.value),
        Source::Density => format!("DICHTE={:.1}B", h.value),
    };
    let l1 = format!(
        "T={:.3}S F={:.0}HZ {val}",
        h.t_ms / 1000.0,
        freq_of_bin(h.bin),
    );
    let l2 = h.frame.map_or_else(
        || "KEIN FRAME".to_string(),
        |f| {
            format!(
                "{} {} {:.0}MS {}B {:.1}KBPS C{} P{}",
                f.mode,
                f.bandwidth.short(),
                f.duration_ms,
                f.bytes,
                f.kbps,
                f.packing,
                f.pad_bytes,
            )
        },
    );
    let l3 = match (h.silk, h.transient, h.frame_bits) {
        (Some((sig, pitch, gain)), _, _) if sig >= 2 => {
            format!("STIMMHAFT F0={:.0}HZ GAIN={:.0}DB", pitch, gain)
        }
        (Some((1, _, gain)), _, _) => format!("STIMMLOS GAIN={:.0}DB", gain),
        (Some(_), _, _) => "INAKTIV".to_string(),
        (None, Some(t), Some(b)) => format!("TRANS {} SIG {:.0}B", u8::from(t), b),
        _ => "KEIN CELT".to_string(),
    };
    (l1, l2, l3)
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

/// Infotext oben links in den Plot legen (Box + drei Zeilen).
fn draw_panel(img: &mut Image, lay: &crate::render::Layout, l1: &str, l2: &str, l3: &str) {
    let w = l1.len().max(l2.len()).max(l3.len()) as u32 * 6 + 10;
    let (x, y) = (lay.plot_x + 4, lay.plot_y + 4);
    img.rect(x, y, w, 33, [10, 12, 18]);
    img.rect(x, y, w, 1, GRID);
    img.rect(x, y + 32, w, 1, GRID);
    img.rect(x, y, 1, 33, GRID);
    img.rect(x + w - 1, y, 1, 33, GRID);
    img.text(x + 5, y + 4, l1, FG);
    img.text(x + 5, y + 13, l2, FG);
    img.text(x + 5, y + 22, l3, FG);
}

/// Bedienhinweis in die Legendenzeile (nur Viewer, nicht PNG).
fn draw_hint(img: &mut Image, lay: &crate::render::Layout) {
    let hint = "RAD:ZOOM DRAG:PAN 1-4:ANSICHT R:RESET ESC:ENDE";
    let x = lay.width.saturating_sub(8 + hint.len() as u32 * 6);
    img.text(x, lay.legend_y, hint, FG);
}

/// Viewer-Schleife: rendert nur bei Sicht-Aenderung neu.
///
/// `max_frames` begrenzt die Schleife (Test-Haken; normal `u64::MAX`).
pub fn show_interactive(
    analysis: &Analysis,
    src: &Sources,
    initial: Source,
    title: &str,
    max_frames: u64,
) -> Result<(), Error> {
    let lay = layout(VIEW_PLOT_W);
    let (w, h) = (lay.width as usize, lay.height as usize);
    let mut window = Window::new(title, w, h, WindowOptions::default())
        .map_err(|e| Error::Show(format!("oeffnen: {e}")))?;
    window.set_target_fps(30);
    let mut view = View::full(analysis.summary.duration_ms);
    let mut source = initial;
    let mut base = render_range(analysis, src, source, view.t0, view.t1, lay.plot_w);
    draw_hint(&mut base, &lay);
    let mut prev_mouse: Option<(f32, f32)> = None;
    let mut frames = 0;
    while window.is_open() && !window.is_key_down(Key::Escape) && frames < max_frames {
        let mouse = window.get_mouse_pos(MouseMode::Clamp);
        let pressed = window.get_keys_pressed(KeyRepeat::No);
        if pressed.contains(&Key::Escape) {
            break; // Kurzer Tastendruck (kein Halten) beendet auch.
        }
        let mut dirty = handle_keys(&window, &mut view, &mut source, &pressed);
        if let Some((_, dy)) = window.get_scroll_wheel()
            && dy != 0.0
        {
            let anchor = mouse.map_or_else(
                || view.pixel_to_ms(lay.plot_w as f32 / 2.0, lay.plot_w),
                |(mx, _)| view.pixel_to_ms(mx - lay.plot_x as f32, lay.plot_w),
            );
            view.zoom_around(anchor, if dy > 0.0 { 0.8 } else { 1.25 });
            dirty = true;
        }
        // Drag relativ zur Vorframe-Position (faengt auch Spruenge).
        if window.get_mouse_down(MouseButton::Left)
            && let (Some((mx, _)), Some((px, _))) = (mouse, prev_mouse)
        {
            let dx_ms = f64::from((px - mx) / lay.plot_w as f32) * view.span();
            if dx_ms != 0.0 {
                view.pan_by(dx_ms);
                dirty = true;
            }
        }
        prev_mouse = mouse;
        if dirty {
            base = render_range(analysis, src, source, view.t0, view.t1, lay.plot_w);
            draw_hint(&mut base, &lay);
        }
        let mut frame = base.clone();
        if let Some(mouse) = mouse {
            draw_hover_state(&mut frame, &lay, analysis, src, source, &view, mouse);
        }
        window
            .update_with_buffer(&pack(&frame), w, h)
            .map_err(|e| Error::Show(format!("anzeige: {e}")))?;
        frames += 1;
    }
    Ok(())
}

/// Tasten: `+`/`-` Zoom, Pfeile Pan, `1`-`4` Ansicht, `R` Reset.
/// Rueckgabe = geaendert.
fn handle_keys(window: &Window, view: &mut View, source: &mut Source, pressed: &[Key]) -> bool {
    for (key, src) in [
        (Key::Key1, Source::Spectrum),
        (Key::Key2, Source::Bitstream),
        (Key::Key3, Source::Divergence),
        (Key::Key4, Source::Density),
    ] {
        if pressed.contains(&key) && *source != src {
            *source = src;
            return true;
        }
    }
    let mid = view.pixel_to_ms(600.0, VIEW_PLOT_W);
    if window.is_key_down(Key::Equal) || window.is_key_down(Key::NumPadPlus) {
        view.zoom_around(mid, 0.95);
        true
    } else if window.is_key_down(Key::Minus) || window.is_key_down(Key::NumPadMinus) {
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
    src: &Sources,
    source: Source,
    view: &View,
    mouse: (f32, f32),
) {
    let (mx, my) = mouse;
    let (px, py) = (mx as i32, my as i32);
    let in_x = px >= lay.plot_x as i32 && px < (lay.plot_x + lay.plot_w) as i32;
    let in_y = py >= lay.plot_y as i32 && py < (lay.plot_y + BINS as u32) as i32;
    let cols = src.cols(source);
    if !in_x || !in_y || cols.is_empty() {
        return;
    }
    let (x, y) = (px as u32, py as u32);
    draw_hover(img, lay, x, y);
    let t_ms = view.pixel_to_ms(mx - lay.plot_x as f32, lay.plot_w);
    let bin = (BINS - 1 - (y - lay.plot_y) as usize).min(BINS - 1);
    let hop = ((t_ms / HOP_MS).floor() as usize).min(cols.len() - 1);
    let bs = &src.bitstream;
    let celt = bs.available.get(hop).copied().unwrap_or(false);
    let silk = bs.silk_available.get(hop).copied().unwrap_or(false);
    let (l1, l2, l3) = hover_lines(Hover {
        t_ms,
        bin,
        value: cols[hop][bin],
        source,
        frame: analysis.frame_at(t_ms),
        transient: celt.then(|| bs.transient.get(hop).copied().unwrap_or(false)),
        frame_bits: celt.then(|| bs.frame_bits.get(hop).copied().unwrap_or(0.0)),
        silk: silk.then(|| {
            (
                bs.silk_signal.get(hop).copied().unwrap_or(0),
                bs.silk_pitch.get(hop).copied().unwrap_or(0.0),
                bs.silk_gain.get(hop).copied().unwrap_or(0.0),
            )
        }),
    });
    draw_panel(img, lay, &l1, &l2, &l3);
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
        let v = View {
            t0: 1000.0,
            t1: 3000.0,
            dur: 10_000.0,
        };
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
            config: 9,
            packing: 2,
            pad_bytes: 5,
        };
        let (l1, l2, l3) = hover_lines(Hover {
            t_ms: 1234.567,
            bin: 93, // ≈ 440 Hz.
            value: -12.3,
            source: Source::Spectrum,
            frame: Some(&f),
            transient: Some(true),
            frame_bits: Some(1822.0),
            silk: None,
        });
        assert_eq!(l1, "T=1.235S F=441HZ PEGEL=-12DB");
        assert_eq!(l2, "SILK WB 20MS 61B 24.4KBPS C2 P5");
        assert_eq!(l3, "TRANS 1 SIG 1822B");
        let (l1, _, l3) = hover_lines(Hover {
            t_ms: 0.0,
            bin: 0,
            value: f32::NAN,
            source: Source::Divergence,
            frame: None,
            transient: None,
            frame_bits: None,
            silk: None,
        });
        assert_eq!(l1, "T=0.000S F=50HZ DIV=---");
        assert_eq!(l3, "KEIN CELT");
        let (l1, _, _) = hover_lines(Hover {
            t_ms: 0.0,
            bin: 0,
            value: 2.34,
            source: Source::Density,
            frame: None,
            transient: None,
            frame_bits: None,
            silk: None,
        });
        assert_eq!(l1, "T=0.000S F=50HZ DICHTE=2.3B");
    }

    #[test]
    fn hover_lines_silk() {
        let base = Hover {
            t_ms: 0.0,
            bin: 0,
            value: 0.0,
            source: Source::Spectrum,
            frame: None,
            transient: None,
            frame_bits: None,
            silk: Some((2, 142.4, 34.6)),
        };
        assert_eq!(hover_lines(base).2, "STIMMHAFT F0=142HZ GAIN=35DB");
        let unvoiced = Hover {
            silk: Some((1, 0.0, 28.1)),
            ..base
        };
        assert_eq!(hover_lines(unvoiced).2, "STIMMLOS GAIN=28DB");
        let inactive = Hover {
            silk: Some((0, 0.0, 0.0)),
            ..base
        };
        assert_eq!(hover_lines(inactive).2, "INAKTIV");
    }

    #[test]
    fn pack_orders_rgb_channels() {
        let img = Image {
            width: 2,
            height: 1,
            rgb: vec![255, 0, 0, 0, 17, 34],
        };
        assert_eq!(pack(&img), vec![0x00ff_0000, 0x0000_1122]);
    }
}
