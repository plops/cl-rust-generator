//! Renderer: Layout plus Opus-Ebenen (Wasserfall, Streifen, Lane).
//!
//! Kopfzeilen, Wasserfall (Modus-Toenung, Bandgrenze, CELT-Bandgitter),
//! Modus-Streifen, Bitraten-Lane, Legende, Achsen. Vier Quellen
//! ([`Source`]) teilen sich ein Layout; nur Farbskala und Pooling
//! unterscheiden sich.

use crate::alloc::EBANDS;
use crate::bitstream::{Source, Sources};
use crate::canvas::{BG, FG, GRID, Image, NAN_COLOR, colormap, diverging};
use crate::spectrum::{BINS, DB_FLOOR, HOP_MS};
use crate::types::{Analysis, Mode};

/// Divergenz-Skala in dB (±, geklemmt).
pub const DIV_RANGE_DB: f32 = 12.0;

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
    /// Tonhoehen-Lane oben/Hoehe (SILK-Stimmverlauf).
    pub pitch_y: u32,
    /// Siehe `pitch_y`.
    pub pitch_h: u32,
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
    let pitch_y = lane_y + lane_h + 4;
    let pitch_h = 30;
    let legend_y = pitch_y + pitch_h + 16;
    Layout {
        width: plot_x + plot_w + 8,
        height: legend_y + 22,
        plot_x,
        plot_y,
        plot_w,
        strip_y,
        lane_y,
        lane_h,
        pitch_y,
        pitch_h,
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

/// Rendert Analyse + Quellen (ein Hop = 20 ms).
#[must_use]
pub fn render(analysis: &Analysis, src: &Sources, view: Source, plot_w: u32) -> Image {
    render_range(
        analysis,
        src,
        view,
        0.0,
        analysis.summary.duration_ms,
        plot_w,
    )
}

/// Rendert den Zeitbereich `[t0_ms, t1_ms)` (Viewer-Aussicht).
#[must_use]
pub fn render_range(
    analysis: &Analysis,
    src: &Sources,
    view: Source,
    t0_ms: f64,
    t1_ms: f64,
    plot_w: u32,
) -> Image {
    let lay = layout(plot_w);
    let mut img = Image::new(lay.width, lay.height, BG);
    header(&mut img, analysis, view, t0_ms, t1_ms);
    let cols = src.cols(view);
    let h0 = (t0_ms / HOP_MS).floor().max(0.0) as usize;
    let h1 = (t1_ms / HOP_MS).ceil().max(h0 as f64 + 1.0) as usize;
    let h1 = h1.min(cols.len());
    if h1 <= h0 || analysis.frames.is_empty() {
        img.text(lay.plot_x + 8, lay.plot_y + 8, "KEINE FRAMES", FG);
        return img;
    }
    let hops = &cols[h0..h1];
    waterfall(&mut img, &lay, analysis, src, view, hops, h0);
    strip(&mut img, &lay, analysis, hops.len(), h0);
    lane(&mut img, &lay, analysis, t0_ms, t1_ms);
    pitch_lane(&mut img, &lay, src, t0_ms, t1_ms);
    axes(&mut img, &lay, t0_ms, t1_ms);
    legend(&mut img, &lay, view, src.density_max);
    img
}

/// Kopfzeilen: Kennzahlen der Summary (+ Ansicht, + Sicht bei Zoom).
fn header(img: &mut Image, a: &Analysis, view: Source, t0_ms: f64, t1_ms: f64) {
    let s = &a.summary;
    let mut l1 = format!(
        "FRAMES {} DAUER {:.1}S AVG {:.1}KBPS ROH {:.1}X CH {} {}",
        s.frames,
        s.duration_ms / 1000.0,
        s.avg_kbps,
        s.ratio,
        s.channels,
        view.name(),
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

/// Wasserfall: Wert → Farbe (je Quelle), Modus-Toenung, Bandgrenze.
fn waterfall(
    img: &mut Image,
    lay: &Layout,
    a: &Analysis,
    src: &Sources,
    view: Source,
    cols: &[[f32; BINS]],
    hop_base: usize,
) {
    let n = cols.len() as u32;
    for x in 0..lay.plot_w {
        // Hops dieses Pixels (Pooling = spitzenerhaltend).
        let h0 = (u64::from(x) * u64::from(n) / u64::from(lay.plot_w)) as usize;
        let h1 =
            (u64::from(x + 1) * u64::from(n) / u64::from(lay.plot_w)).max(h0 as u64 + 1) as usize;
        let mut db = [pool_init(view); BINS];
        for col in &cols[h0..h1.min(cols.len())] {
            for (d, v) in db.iter_mut().zip(col) {
                *d = pool(*d, *v, view);
            }
        }
        let t_ms = (hop_base as f64 + (h0 + h1) as f64 / 2.0) * HOP_MS;
        let frame = a.frame_at(t_ms);
        let tint = frame.map(|f| mode_color(f.mode));
        for (b, d) in db.iter().enumerate() {
            let c = pixel_color(*d, view, src.density_max);
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
    // CELT-Bandgitter in den Bitstrom-Ansichten (Struktur sichtbar machen).
    if view != Source::Spectrum {
        for e in EBANDS.iter().skip(1) {
            let y = lay.plot_y + BINS as u32 - 1 - bin_of(*e as f32 * 200.0) as u32;
            img.rect(lay.plot_x, y, lay.plot_w, 1, GRID);
        }
    }
}

/// Pool-Startwert je Quelle (Divergenz: NaN = noch nichts gesehen).
fn pool_init(view: Source) -> f32 {
    match view {
        Source::Divergence => f32::NAN,
        _ => DB_FLOOR,
    }
}

/// Pooling ueber Hops: Maximum, bei Divergenz betrags-groesster Wert.
fn pool(acc: f32, v: f32, view: Source) -> f32 {
    if view == Source::Divergence {
        if v.is_nan() {
            acc
        } else if acc.is_nan() || v.abs() > acc.abs() {
            v
        } else {
            acc
        }
    } else if v.is_nan() {
        acc
    } else {
        acc.max(v)
    }
}

/// Pixel-Farbe je Quelle (NaN = neutral, ausserhalb = geklemmt).
fn pixel_color(v: f32, view: Source, density_max: f32) -> [u8; 3] {
    if v.is_nan() {
        return NAN_COLOR;
    }
    match view {
        Source::Spectrum | Source::Bitstream => colormap((v - DB_FLOOR) / -DB_FLOOR),
        Source::Divergence => diverging(v / DIV_RANGE_DB),
        // Linear: Helligkeit proportional zu Bits (Log wuerde Dynamik fressen).
        Source::Density => colormap((v.max(0.0) / density_max).clamp(0.0, 1.0)),
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

/// Tonhoehen-Lane: Stimmverlauf je Hop (SILK), sonst leer.
///
/// Stimmhaft = amberner Punkt auf Log-Hoehe (50–500 Hz), stimmlos =
/// blauer Strich unten, inaktiv = grauer Strich unten.
fn pitch_lane(img: &mut Image, lay: &Layout, src: &Sources, t0_ms: f64, t1_ms: f64) {
    img.rect(
        lay.plot_x,
        lay.pitch_y,
        lay.plot_w,
        lay.pitch_h,
        [20, 24, 32],
    );
    img.text(2, lay.pitch_y, "F0", FG);
    let n = src.spectrum.len().min(src.bitstream.silk_available.len());
    if n == 0 {
        return;
    }
    let span = (t1_ms - t0_ms).max(1.0);
    for x in 0..lay.plot_w {
        let t_ms = t0_ms + (f64::from(x) + 0.5) / f64::from(lay.plot_w) * span;
        let hop = ((t_ms / HOP_MS).floor() as usize).min(n - 1);
        if !src.bitstream.silk_available[hop] {
            continue;
        }
        let (sig, pitch) = (
            src.bitstream.silk_signal[hop],
            src.bitstream.silk_pitch[hop],
        );
        let (color, y) = if sig >= 2 && pitch > 0.0 {
            let f = (pitch.clamp(50.0, 500.0) / 50.0).ln() / 10.0f32.ln();
            let y = lay.pitch_y + lay.pitch_h - 1 - (f * (lay.pitch_h - 1) as f32) as u32;
            ([255, 190, 90], y)
        } else if sig == 1 {
            ([70, 170, 255], lay.pitch_y + lay.pitch_h - 1)
        } else {
            (GRID, lay.pitch_y + lay.pitch_h - 1)
        };
        img.rect(
            lay.plot_x + x,
            y.saturating_sub(1),
            1,
            2.min(lay.pitch_h),
            color,
        );
    }
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
        img.rect(x, lay.pitch_y + lay.pitch_h + 2, 1, 4, GRID);
        img.text(x + 2, lay.pitch_y + lay.pitch_h + 3, &fmt_time(t, step), FG);
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

/// Legende: Modus-Farben + quellen-abhaengiger Verlauf.
fn legend(img: &mut Image, lay: &Layout, view: Source, density_max: f32) {
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
    let hi_owned;
    let (tag, lo, hi) = match view {
        Source::Spectrum | Source::Bitstream => ("DB", "-72", "0"),
        Source::Divergence => ("DIV", "-12", "+12"),
        Source::Density => {
            hi_owned = format!("{density_max:.0}B");
            ("BIT", "0", hi_owned.as_str())
        }
    };
    img.text(x, lay.legend_y, tag, FG);
    x += 30;
    for i in 0..60 {
        let c = match view {
            Source::Divergence => diverging(i as f32 / 59.0 * 2.0 - 1.0),
            _ => colormap(i as f32 / 59.0),
        };
        img.rect(x + i, lay.legend_y, 1, 7, c);
    }
    x += 62;
    img.text(x, lay.legend_y, &format!("{lo}/{hi}"), FG);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitstream::Bitstream;
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
            config: 31,
            packing: 0,
            pad_bytes: 0,
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
            split_fallbacks: 0,
        }
    }

    fn two_sources() -> Sources {
        let cols = vec![[DB_FLOOR; BINS], [-6.0; BINS]];
        let bs = Bitstream {
            spectrum: cols.clone(),
            density: vec![[1.0; BINS]; 2],
            available: vec![true, true],
            transient: vec![false, false],
            frame_bits: vec![256.0, 256.0],
            packets: vec![None, None],
            silk_packets: vec![None, None],
            silk_available: vec![false, false],
            silk_signal: vec![0, 0],
            silk_pitch: vec![0.0, 0.0],
            silk_gain: vec![0.0, 0.0],
        };
        Sources::build(cols, bs)
    }

    #[test]
    fn render_places_modes_and_bars() {
        let a = two_frames();
        let src = two_sources();
        let lay = layout(64);
        let img = render(&a, &src, Source::Spectrum, 64);
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
        let src = two_sources();
        let full = render(&a, &src, Source::Spectrum, 64);
        let range = render_range(&a, &src, Source::Spectrum, 0.0, 40.0, 64);
        assert_eq!(full.rgb, range.rgb);
        assert_eq!((full.width, full.height), (range.width, range.height));
    }

    #[test]
    fn zoomed_range_shows_only_visible_modes() {
        let a = two_frames(); // 0-20 ms SILK, 20-40 ms CELT.
        let src = two_sources();
        let lay = layout(64);
        let silk = render_range(&a, &src, Source::Spectrum, 0.0, 20.0, 64);
        let celt = render_range(&a, &src, Source::Spectrum, 20.0, 40.0, 64);
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
    fn divergence_pooling_keeps_sign_and_nan() {
        assert!(pool(f32::NAN, f32::NAN, Source::Divergence).is_nan());
        assert_eq!(pool(f32::NAN, -5.0, Source::Divergence), -5.0);
        assert_eq!(pool(-5.0, 3.0, Source::Divergence), -5.0);
        assert_eq!(pool(-5.0, 8.0, Source::Divergence), 8.0);
        assert_eq!(pixel_color(f32::NAN, Source::Divergence, 1.0), NAN_COLOR);
        assert_eq!(pixel_color(0.0, Source::Divergence, 1.0), diverging(0.0));
        assert_eq!(pool(DB_FLOOR, -6.0, Source::Spectrum), -6.0);
    }

    #[test]
    fn freq_of_bin_spans_audio_band() {
        assert!((freq_of_bin(0) - 50.0).abs() < 1.0);
        assert!(freq_of_bin(255) > 19000.0);
        assert!(freq_of_bin(100) < freq_of_bin(200));
    }
}
