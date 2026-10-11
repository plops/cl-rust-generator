//! Renderer: Layout plus Opus-Ebenen (Wasserfall, Streifen, Lane).
//!
//! Kopfzeilen, Wasserfall (Modus-Toenung, Bandgrenze),
//! Modus-Streifen, Bitraten-Lane, Legende, Achsen.

use crate::canvas::{BG, FG, GRID, Image, colormap};
use crate::spectrum::{BINS, DB_FLOOR, HOP_MS};
use crate::types::{Analysis, Mode};

/// Modus-Farbe: SILK blau, Hybrid violett, CELT amber.
#[must_use]
pub fn mode_color(mode: Mode) -> [u8; 3] {
    match mode {
        Mode::Silk => [70, 170, 255],
        Mode::Hybrid => [185, 120, 255],
        Mode::Celt => [255, 178, 70],
    }
}

/// Layout-Masse (alle px).
#[derive(Debug, Clone, Copy)]
pub struct Layout {
    /// Gesamtbreite/-hoehe.
    pub width: u32,
    /// Siehe `width`.
    pub height: u32,
    /// Wasserfall links/oben.
    pub plot_x: u32,
    /// Siehe `plot_x`.
    pub plot_y: u32,
    /// Wasserfall-Breite (Hoehe = `BINS`).
    pub plot_w: u32,
    /// Modus-Streifen oben.
    pub strip_y: u32,
    /// Bitraten-Lane oben/Hoehe.
    pub lane_y: u32,
    /// Siehe `lane_y`.
    pub lane_h: u32,
    /// Legende oben.
    pub legend_y: u32,
}

/// Layout aus Plot-Breite berechnen.
#[must_use]
pub fn layout(plot_w: u32) -> Layout {
    let plot_w = plot_w.max(64);
    let plot_x = 56;
    let plot_y = 30;
    let strip_y = plot_y + BINS as u32 + 4;
    let lane_y = strip_y + 14 + 4;
    let lane_h = 56;
    let legend_y = lane_y + lane_h + 16;
    Layout {
        width: plot_x + plot_w + 8,
        height: legend_y + 22,
        plot_x,
        plot_y,
        plot_w,
        strip_y,
        lane_y,
        lane_h,
        legend_y,
    }
}

/// Log-Bin-Index zu Frequenz in Hz (Umkehrung der Achse).
fn bin_of(hz: f32) -> usize {
    let b = BINS as f32 * (hz / 50.0).ln() / 400.0f32.ln();
    (b.round() as usize).min(BINS - 1)
}

/// Frequenz in Hz zu Log-Bin-Index `b` (fuer Hover-Angaben).
#[must_use]
pub fn freq_of_bin(b: usize) -> f32 {
    50.0 * 400.0f32.powf(b.min(BINS - 1) as f32 / BINS as f32)
}

/// Rendert Analyse + Spektrum-Spalten (ein Hop = 20 ms).
#[must_use]
pub fn render(analysis: &Analysis, cols: &[[f32; BINS]], plot_w: u32) -> Image {
    render_range(analysis, cols, 0.0, analysis.summary.duration_ms, plot_w)
}

/// Rendert den Zeitbereich `[t0_ms, t1_ms)` (Viewer-Aussicht).
#[must_use]
pub fn render_range(
    analysis: &Analysis,
    cols: &[[f32; BINS]],
    t0_ms: f64,
    t1_ms: f64,
    plot_w: u32,
) -> Image {
    let lay = layout(plot_w);
    let mut img = Image::new(lay.width, lay.height, BG);
    header(&mut img, analysis, t0_ms, t1_ms);
    let h0 = (t0_ms / HOP_MS).floor().max(0.0) as usize;
    let h1 = (t1_ms / HOP_MS).ceil().max(h0 as f64 + 1.0) as usize;
    let h1 = h1.min(cols.len());
    if h1 <= h0 || analysis.frames.is_empty() {
        img.text(lay.plot_x + 8, lay.plot_y + 8, "KEINE FRAMES", FG);
        return img;
    }
    let hops = &cols[h0..h1];
    waterfall(&mut img, &lay, analysis, hops, h0);
    strip(&mut img, &lay, analysis, hops.len(), h0);
    lane(&mut img, &lay, analysis, t0_ms, t1_ms);
    axes(&mut img, &lay, t0_ms, t1_ms);
    legend(&mut img, &lay);
    img
}

/// Kopfzeilen: Kennzahlen der Summary (+ Sicht bei Zoom).
fn header(img: &mut Image, a: &Analysis, t0_ms: f64, t1_ms: f64) {
    let s = &a.summary;
    let mut l1 = format!(
        "FRAMES {} DAUER {:.1}S AVG {:.1}KBPS ROH {:.1}X CH {}",
        s.frames,
        s.duration_ms / 1000.0,
        s.avg_kbps,
        s.ratio,
        s.channels,
    );
    if let Some(note) = range_note(t0_ms, t1_ms, s.duration_ms) {
        l1.push_str(&note);
    }
    let l2 = format!(
        "SILK {:.0}% HYBRID {:.0}% CELT {:.0}% OVERHEAD {:.1}%",
        s.frac_silk * 100.0,
        s.frac_hybrid * 100.0,
        s.frac_celt * 100.0,
        s.overhead_pct,
    );
    img.text(8, 5, &l1, FG);
    img.text(8, 15, &l2, FG);
}

/// Sicht-Hinweis fuer die Kopfzeile (`None` = Gesamtansicht).
fn range_note(t0_ms: f64, t1_ms: f64, dur_ms: f64) -> Option<String> {
    if t0_ms <= 0.5 && t1_ms >= dur_ms - 0.5 {
        None
    } else {
        Some(format!(
            " SICHT {:.1}-{:.1}S",
            t0_ms / 1000.0,
            t1_ms / 1000.0
        ))
    }
}

/// Wasserfall: dB → Farbe, Modus-Toenung, Bandgrenze weiss.
fn waterfall(img: &mut Image, lay: &Layout, a: &Analysis, cols: &[[f32; BINS]], hop_base: usize) {
    let n = cols.len() as u32;
    for x in 0..lay.plot_w {
        // Hops dieses Pixels (Max-Pooling = spitzenerhaltend).
        let h0 = (u64::from(x) * u64::from(n) / u64::from(lay.plot_w)) as usize;
        let h1 =
            (u64::from(x + 1) * u64::from(n) / u64::from(lay.plot_w)).max(h0 as u64 + 1) as usize;
        let mut db = [DB_FLOOR; BINS];
        for col in &cols[h0..h1.min(cols.len())] {
            for (d, v) in db.iter_mut().zip(col) {
                *d = d.max(*v);
            }
        }
        let t_ms = (hop_base as f64 + (h0 + h1) as f64 / 2.0) * HOP_MS;
        let frame = a.frame_at(t_ms);
        let tint = frame.map(|f| mode_color(f.mode));
        for (b, d) in db.iter().enumerate() {
            let c = colormap((d - DB_FLOOR) / -DB_FLOOR);
            let y = lay.plot_y + BINS as u32 - 1 - b as u32;
            img.set(lay.plot_x as i32 + x as i32, y as i32, c);
            if let Some(t) = tint {
                img.blend(lay.plot_x + x, y, t, 0.22);
            }
        }
        // Bandgrenze des Frames als weisser Punkt.
        if let Some(f) = frame {
            let bw = f.bandwidth.hz() as f32;
            let y = lay.plot_y + BINS as u32 - 1 - bin_of(bw) as u32;
            img.set(lay.plot_x as i32 + x as i32, y as i32, [255; 3]);
        }
    }
}

/// Modus-Streifen: Vollfarbe pro Pixel (Mitte des hops entscheidet).
fn strip(img: &mut Image, lay: &Layout, a: &Analysis, hops: usize, hop_base: usize) {
    for x in 0..lay.plot_w {
        let h = (u64::from(x) * hops as u64 / u64::from(lay.plot_w)) as usize;
        if let Some(f) = a.frame_at((hop_base + h) as f64 * HOP_MS) {
            img.rect(lay.plot_x + x, lay.strip_y, 1, 12, mode_color(f.mode));
        }
    }
}

/// Bitraten-Lane: Balken pro Frame + AVG-Linie (Skala global).
fn lane(img: &mut Image, lay: &Layout, a: &Analysis, t0_ms: f64, t1_ms: f64) {
    let s = &a.summary;
    let max = a.frames.iter().map(|f| f.kbps).fold(0.0f64, f64::max);
    let scale = (2.0 * s.avg_kbps).max(max).max(1.0);
    let span = (t1_ms - t0_ms).max(1.0);
    img.rect(lay.plot_x, lay.lane_y, lay.plot_w, lay.lane_h, [20, 24, 32]);
    for f in &a.frames {
        let x0f = (f.t_start_ms - t0_ms) / span * lay.plot_w as f64;
        let x1f = (f.t_start_ms + f.duration_ms - t0_ms) / span * lay.plot_w as f64;
        if x1f <= 0.0 || x0f >= lay.plot_w as f64 {
            continue; // Ausserhalb der Sicht.
        }
        let x0 = x0f.max(0.0) as u32;
        let x1 = x1f.ceil().max(0.0) as u32;
        let h = (f.kbps / scale * lay.lane_h as f64).ceil() as u32;
        let h = h.clamp(1, lay.lane_h);
        img.rect(
            lay.plot_x + x0.min(lay.plot_w - 1),
            lay.lane_y + lay.lane_h - h,
            (x1 - x0).clamp(1, lay.plot_w - x0.min(lay.plot_w - 1)),
            h,
            [255, 190, 90],
        );
    }
    // AVG-Linie gestrichelt weiss.
    let y = lay.lane_y + lay.lane_h - (s.avg_kbps / scale * lay.lane_h as f64) as u32;
    for x in (0..lay.plot_w).step_by(4) {
        img.set(lay.plot_x as i32 + x as i32, y as i32, [255; 3]);
    }
    img.text(2, lay.lane_y, &format!("{:.0}K", scale), FG);
}

/// Achsen: Frequenz-Ticks links, Zeit-Ticks unter der Lane.
fn axes(img: &mut Image, lay: &Layout, t0_ms: f64, t1_ms: f64) {
    for (hz, label) in [
        (100.0, "100"),
        (500.0, "500"),
        (1000.0, "1K"),
        (5000.0, "5K"),
        (10000.0, "10K"),
        (20000.0, "20K"),
    ] {
        let y = lay.plot_y + BINS as u32 - 1 - bin_of(hz) as u32;
        img.rect(0, y, lay.plot_x - 2, 1, GRID);
        img.text(2, y.saturating_sub(3), label, FG);
    }
    let (t0_s, t1_s) = (t0_ms / 1000.0, t1_ms / 1000.0);
    let span = (t1_s - t0_s).max(0.001);
    let step = nice_step(span, lay.plot_w);
    let mut t = (t0_s / step).ceil() * step;
    if t < t0_s {
        t = t0_s; // Rundungsschutz.
    }
    while t <= t1_s + 1e-9 {
        let x = lay.plot_x + ((t - t0_s) / span * lay.plot_w as f64) as u32;
        img.rect(x, lay.lane_y + lay.lane_h + 2, 1, 4, GRID);
        img.text(x + 2, lay.lane_y + lay.lane_h + 3, &fmt_time(t, step), FG);
        t += step;
    }
}

/// Zeitschritt, sodass hoechstens ~1 Tick pro 100 px steht.
fn nice_step(span_s: f64, plot_w: u32) -> f64 {
    for s in [
        0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 15.0, 30.0, 60.0, 120.0, 300.0,
    ] {
        if span_s / s <= f64::from(plot_w) / 100.0 {
            return s;
        }
    }
    600.0
}

/// Sekunden → "NS" bzw. "M:SS" (bei Fein-Step mit Dezimale).
fn fmt_time(t: f64, step: f64) -> String {
    if t < 60.0 {
        if step < 1.0 {
            format!("{t:.1}S")
        } else {
            format!("{t:.0}S")
        }
    } else {
        format!("{}:{:02.0}", (t / 60.0) as u32, t % 60.0)
    }
}

/// Legende: Modus-Farben + dB-Verlauf.
fn legend(img: &mut Image, lay: &Layout) {
    let mut x = 8;
    for (mode, label) in [
        (Mode::Silk, "SILK SPRACHE"),
        (Mode::Hybrid, "HYBRID"),
        (Mode::Celt, "CELT MUSIK"),
    ] {
        img.rect(x, lay.legend_y, 10, 7, mode_color(mode));
        x += 12;
        x += img.text(x, lay.legend_y, label, FG);
        x += 10;
    }
    img.text(x, lay.legend_y, "DB", FG);
    x += 18;
    for i in 0..60 {
        let c = colormap(i as f32 / 59.0);
        img.rect(x + i, lay.legend_y, 1, 7, c);
    }
    x += 62;
    img.text(x, lay.legend_y, "-72/0", FG);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Bandwidth, FrameMeta, Summary};

    fn two_frames() -> Analysis {
        let mk = |index: usize, t: f64, mode: Mode| FrameMeta {
            index,
            t_start_ms: t,
            duration_ms: 20.0,
            mode,
            bandwidth: Bandwidth::Full,
            channels: 1,
            bytes: 80,
            kbps: 32.0,
        };
        Analysis {
            frames: vec![mk(0, 0.0, Mode::Silk), mk(1, 20.0, Mode::Celt)],
            summary: Summary {
                frames: 2,
                duration_ms: 40.0,
                audio_bytes: 160,
                total_bytes: 200,
                avg_kbps: 32.0,
                raw_kbps: 768.0,
                ratio: 24.0,
                overhead_pct: 20.0,
                frac_silk: 0.5,
                frac_hybrid: 0.0,
                frac_celt: 0.5,
                channels: 1,
            },
        }
    }

    #[test]
    fn render_places_modes_and_bars() {
        let a = two_frames();
        let cols = vec![[DB_FLOOR; BINS], [-6.0; BINS]];
        let lay = layout(64);
        let img = render(&a, &cols, 64);
        assert_eq!((img.width, img.height), (lay.width, lay.height));
        // Streifen: links SILK-blau, rechts CELT-amber.
        assert_eq!(img.get(lay.plot_x, lay.strip_y), mode_color(Mode::Silk));
        assert_eq!(
            img.get(lay.plot_x + lay.plot_w - 1, lay.strip_y),
            mode_color(Mode::Celt)
        );
        // Lane enthaelt Balken (nicht nur Hintergrund).
        let mut bars = 0;
        for x in 0..lay.plot_w {
            let p = img.get(lay.plot_x + x, lay.lane_y + lay.lane_h - 1);
            if p != [20, 24, 32] {
                bars += 1;
            }
        }
        assert!(bars > lay.plot_w / 2);
        // Kopfzeile enthaelt helle Textpixel.
        assert!(
            img.rgb[..lay.width as usize * 30 * 3]
                .iter()
                .any(|&v| v > 200)
        );
    }

    #[test]
    fn full_range_matches_render_bytes() {
        let a = two_frames();
        let cols = vec![[DB_FLOOR; BINS], [-6.0; BINS]];
        let full = render(&a, &cols, 64);
        let range = render_range(&a, &cols, 0.0, 40.0, 64);
        assert_eq!(full.rgb, range.rgb);
        assert_eq!((full.width, full.height), (range.width, range.height));
    }

    #[test]
    fn zoomed_range_shows_only_visible_modes() {
        let a = two_frames(); // 0-20 ms SILK, 20-40 ms CELT.
        let cols = vec![[DB_FLOOR; BINS], [-6.0; BINS]];
        let lay = layout(64);
        let silk = render_range(&a, &cols, 0.0, 20.0, 64);
        let celt = render_range(&a, &cols, 20.0, 40.0, 64);
        for x in 0..lay.plot_w {
            assert_eq!(
                silk.get(lay.plot_x + x, lay.strip_y),
                mode_color(Mode::Silk),
                "x={x}"
            );
            assert_eq!(
                celt.get(lay.plot_x + x, lay.strip_y),
                mode_color(Mode::Celt),
                "x={x}"
            );
        }
    }

    #[test]
    fn range_note_and_time_format() {
        assert_eq!(range_note(0.0, 6000.0, 6000.0), None);
        assert_eq!(
            range_note(12000.0, 18000.0, 30000.0).as_deref(),
            Some(" SICHT 12.0-18.0S")
        );
        assert_eq!(fmt_time(5.0, 1.0), "5S");
        assert_eq!(fmt_time(5.25, 0.5), "5.2S");
        assert_eq!(fmt_time(90.0, 30.0), "1:30");
        assert_eq!(nice_step(6.0, 800), 1.0);
        assert_eq!(nice_step(0.4, 800), 0.1);
    }

    #[test]
    fn freq_of_bin_spans_audio_band() {
        assert!((freq_of_bin(0) - 50.0).abs() < 1.0);
        assert!(freq_of_bin(255) > 19000.0);
        assert!(freq_of_bin(100) < freq_of_bin(200));
    }
}
