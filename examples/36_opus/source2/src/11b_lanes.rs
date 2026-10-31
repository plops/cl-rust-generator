//! 11b — Layout und Lanes unter dem Wasserfall.
//!
//! Modus-Streifen, Bitraten-Lane, F0-Lane (SILK-Stimme) und die neue
//! **Budget-Lane**: je Hop ein gestapelter Balken, wohin die Bits des
//! Pakets gingen (SILK-Seiteninfo, SILK-Pulse, FEC, CELT-Kopf,
//! CELT-Energie, PVQ, Redundanz, ungenutzt). Aus `11c_render.rs`
//! ausgelagert (600er-Regel).

use crate::canvas::{FG, GRID, Image};
use crate::model::Sources;
use crate::render::mode_color;
use crate::spectrum::{BINS, HOP_MS};
use crate::types::Analysis;
use crate::walk::Budget;

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
    /// Budget-Lane oben/Hoehe (gestapelte Bit-Anteile je Hop).
    pub budget_y: u32,
    /// Siehe `budget_y`.
    pub budget_h: u32,
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
    let budget_y = pitch_y + pitch_h + 4;
    let budget_h = 24;
    let legend_y = budget_y + budget_h + 16;
    Layout {
        width: plot_x + plot_w + 8,
        height: legend_y + 33,
        plot_x,
        plot_y,
        plot_w,
        strip_y,
        lane_y,
        lane_h,
        pitch_y,
        pitch_h,
        budget_y,
        budget_h,
        legend_y,
    }
}

/// Budget-Posten in Stapel-Reihenfolge: (Name, Farbe, Anteil-Funktion).
pub const BUDGET_PARTS: [(&str, [u8; 3]); 7] = [
    ("SILK-KOPF", [40, 110, 200]),
    ("SILK-PULS", [110, 190, 255]),
    ("FEC", [60, 200, 140]),
    ("CELT-KOPF", [150, 90, 40]),
    ("ENERGIE", [255, 150, 40]),
    ("PVQ", [255, 225, 120]),
    ("REDUNDANZ", [220, 70, 160]),
];

/// Budget in die sieben Stapel-Posten (1/8 Bit) zerlegen; Rest = ungenutzt.
#[must_use]
pub fn budget_parts(b: &Budget) -> [i32; 7] {
    [
        b.init + b.silk_flags + b.silk_stereo + b.silk_side + b.red_header,
        b.silk_pulses,
        b.silk_lbrr,
        b.celt_header,
        b.celt_energy,
        b.celt_pvq,
        b.redundancy,
    ]
}

/// Budget-Lane: gestapelte Anteile am Paket (volle Hoehe = Paketgroesse).
pub(crate) fn budget_lane(img: &mut Image, lay: &Layout, src: &Sources, t0_ms: f64, t1_ms: f64) {
    img.rect(
        lay.plot_x,
        lay.budget_y,
        lay.plot_w,
        lay.budget_h,
        [20, 24, 32],
    );
    img.text(2, lay.budget_y, "BITS", FG);
    let n = src.model.hops.len();
    if n == 0 {
        return;
    }
    let span = (t1_ms - t0_ms).max(1.0);
    for x in 0..lay.plot_w {
        let t_ms = t0_ms + (f64::from(x) + 0.5) / f64::from(lay.plot_w) * span;
        let hop = ((t_ms / HOP_MS).floor() as usize).min(n - 1);
        let p = &src.model.packets[src.model.hops[hop].packet];
        let total = p.budget.total().max(1) as f32;
        let mut y = lay.budget_y + lay.budget_h;
        for (part, (_, color)) in budget_parts(&p.budget).iter().zip(BUDGET_PARTS.iter()) {
            let h = (*part as f32 / total * lay.budget_h as f32).round() as u32;
            let h = h.min(y - lay.budget_y);
            if h > 0 {
                y -= h;
                img.rect(lay.plot_x + x, y, 1, h, *color);
            }
        }
    }
}

/// Modus-Streifen: Vollfarbe pro Pixel (Mitte des hops entscheidet).
pub(crate) fn strip(img: &mut Image, lay: &Layout, a: &Analysis, hops: usize, hop_base: usize) {
    for x in 0..lay.plot_w {
        let h = (u64::from(x) * hops as u64 / u64::from(lay.plot_w)) as usize;
        if let Some(f) = a.frame_at((hop_base + h) as f64 * HOP_MS) {
            img.rect(lay.plot_x + x, lay.strip_y, 1, 12, mode_color(f.mode));
        }
    }
}

/// Bitraten-Lane: Balken pro Frame + AVG-Linie (Skala global).
pub(crate) fn lane(img: &mut Image, lay: &Layout, a: &Analysis, t0_ms: f64, t1_ms: f64) {
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
pub(crate) fn pitch_lane(img: &mut Image, lay: &Layout, src: &Sources, t0_ms: f64, t1_ms: f64) {
    img.rect(
        lay.plot_x,
        lay.pitch_y,
        lay.plot_w,
        lay.pitch_h,
        [20, 24, 32],
    );
    img.text(2, lay.pitch_y, "F0", FG);
    let n = src.model.hops.len();
    if n == 0 {
        return;
    }
    let span = (t1_ms - t0_ms).max(1.0);
    for x in 0..lay.plot_w {
        let t_ms = t0_ms + (f64::from(x) + 0.5) / f64::from(lay.plot_w) * span;
        let hop = ((t_ms / HOP_MS).floor() as usize).min(n - 1);
        let h = &src.model.hops[hop];
        if !h.silk {
            continue;
        }
        let (sig, pitch) = (h.silk_signal, h.silk_pitch);
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
