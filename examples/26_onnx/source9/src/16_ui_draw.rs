//! `16_ui_draw` — Bild, Boxen, HUD und Statistik-Panel.
//!
//! Die `draw_*`-Funktionen brauchen den macroquad-Kontext (nur unter
//! Xvfb getestet); `status_line`/`panel_lines` sind rein und haben
//! Host-Tests.

use macroquad::prelude::*;

use crate::engine::Sample;
use crate::render::CANVAS;
use crate::stats::Stats;
use crate::ui_state::{UiState, ViewMode};

/// HUD-Schriftgröße.
const FONT_SIZE: u16 = 16;
/// Höhe der Status-/Hilfe-Balken.
const BAR: f32 = 24.0;

/// Zeichnet Bild (außer `Hidden`) + Boxen (nur `Boxes`).
pub fn draw_sample(tex: &Texture2D, sample: &Sample, view: ViewMode, font: &Font) {
    if view != ViewMode::Hidden {
        draw_texture(tex, 0.0, 0.0, WHITE);
    }
    if view == ViewMode::Boxes {
        for (i, b) in sample.boxes.iter().enumerate() {
            let matched = sample.eval.box_matched.get(i).copied().unwrap_or(false);
            let color = if matched { GREEN } else { RED };
            let r = &b.rect;
            draw_rectangle_lines(r.x, r.y, r.w, r.h, 2.0, color);
            if !b.text.is_empty() {
                let dims = measure_text(&b.text, Some(font), FONT_SIZE, 1.0);
                let (bw, bh) = (dims.width + 6.0, dims.height + 6.0);
                let bx = r.x.clamp(0.0, (CANVAS as f32 - bw).max(0.0));
                let by = if r.y >= bh + 2.0 {
                    r.y - bh - 2.0
                } else {
                    r.y + r.h + 2.0
                };
                draw_rectangle(bx, by, bw, bh, Color::new(0.0, 0.0, 0.0, 0.85));
                draw_text_ex(
                    &b.text,
                    bx + 3.0,
                    by + bh - 5.0,
                    TextParams {
                        font: Some(font),
                        font_size: FONT_SIZE,
                        color: WHITE,
                        ..Default::default()
                    },
                );
            }
        }
    }
}

/// „Warte auf erstes Sample …“ (Worker rechnet noch).
pub fn draw_waiting(font: &Font) {
    draw_text_ex(
        "warming up …",
        CANVAS as f32 / 2.0 - 60.0,
        CANVAS as f32 / 2.0,
        TextParams {
            font: Some(font),
            font_size: 24,
            color: WHITE,
            ..Default::default()
        },
    );
}

/// Zeichnet Status-Balken, Statistik-Panel und Hilfe-Balken.
pub fn draw_hud(sample: &Sample, stats: &Stats, state: &UiState, font: &Font) {
    let out = CANVAS as f32;
    draw_rectangle(0.0, 0.0, out, BAR, Color::new(0.0, 0.0, 0.0, 0.75));
    draw_text_ex(
        status_line(sample, stats, state),
        10.0,
        17.0,
        TextParams {
            font: Some(font),
            font_size: FONT_SIZE,
            color: WHITE,
            ..Default::default()
        },
    );
    let panel = panel_lines(stats, &sample.lang);
    let (pw, ph) = (300.0, panel.len() as f32 * 18.0 + 10.0);
    let (px, py) = (out - pw - 8.0, out - BAR - ph - 8.0);
    draw_rectangle(px, py, pw, ph, Color::new(0.0, 0.0, 0.0, 0.75));
    for (i, line) in panel.iter().enumerate() {
        draw_text_ex(
            line,
            px + 8.0,
            py + 20.0 + i as f32 * 18.0,
            TextParams {
                font: Some(font),
                font_size: FONT_SIZE,
                color: WHITE,
                ..Default::default()
            },
        );
    }
    draw_rectangle(0.0, out - BAR, out, BAR, Color::new(0.0, 0.0, 0.0, 0.75));
    draw_text_ex(
        help_line(),
        10.0,
        out - 7.0,
        TextParams {
            font: Some(font),
            font_size: FONT_SIZE,
            color: WHITE,
            ..Default::default()
        },
    );
}

/// Statuszeile: Sprache, Generator, Modell, Kennzahlen.
#[must_use]
pub fn status_line(sample: &Sample, stats: &Stats, state: &UiState) -> String {
    let pause = if state.paused { "PAUSE " } else { "" };
    let rnd = if state.random_lang { " R" } else { "" };
    format!(
        "{pause}[{} {}{} {} {}px] CER {:.3} rec {:.2} det {:.0}ms rec {:.0}ms #{}",
        sample.lang,
        sample.mode.name(),
        rnd,
        short_model(&sample.model),
        sample.px,
        sample.eval.mean_cer(),
        sample.eval.recall(),
        sample.times.det_ms,
        sample.times.rec_ms,
        stats.samples(),
    )
}

/// Statistik-Panel: Samples, Top-Verwechslung, schlechtestes Zeichen.
#[must_use]
pub fn panel_lines(stats: &Stats, lang: &str) -> Vec<String> {
    let mut v = vec![format!("samples: {}", stats.samples())];
    match stats.top_confusions(lang, 1).first() {
        Some(((g, o), n)) => v.push(format!("top: {}→{} {n}×", show(*g), show(*o))),
        None => v.push("top: -".to_string()),
    }
    match stats.worst_chars(lang, 1).first() {
        Some((c, seen, rate)) => v.push(format!("bad: `{c}` {seen}× {:.0}%", rate * 100.0)),
        None => v.push("bad: -".to_string()),
    }
    v
}

fn show(c: Option<char>) -> String {
    c.map_or_else(|| "∅".to_string(), |ch| format!("`{ch}`"))
}

/// Modell-Ordnername kürzen (`PP-OCRv6_small_rec_onnx` → `v6`).
fn short_model(model: &str) -> &str {
    if model.starts_with("PP-OCRv6") {
        "v6"
    } else {
        model.split('_').next().unwrap_or(model)
    }
}

/// Tasten-Hilfe (unterer Balken).
#[must_use]
pub fn help_line() -> &'static str {
    "L/R: lang | R: rand | G: gen | V: view | Up/Dn: size | M: model | Spc: pause | N: step | C: clear | Q: quit"
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::detect::TextBox;
    use crate::generate::GenMode;
    use crate::metrics::{LineEval, SampleEval};
    use crate::render::Rect;
    use crate::stats::Times;

    fn sample() -> Sample {
        Sample {
            lang: "de".to_string(),
            mode: GenMode::Pangram,
            px: 32,
            rgba: Vec::new(),
            model: "PP-OCRv6_small_rec_onnx".to_string(),
            gt: vec!["ab".to_string()],
            boxes: vec![TextBox {
                rect: Rect {
                    x: 0.0,
                    y: 0.0,
                    w: 10.0,
                    h: 10.0,
                },
                text: "ab".to_string(),
            }],
            eval: SampleEval {
                lines: vec![LineEval {
                    ocr: "ab".to_string(),
                    errors: 0,
                    cer: 0.0,
                    exact: true,
                    matched: 1,
                    iou: 0.7,
                    conf: 0.9,
                    ops: Vec::new(),
                }],
                fp_boxes: 0,
                box_matched: vec![true],
            },
            times: Times {
                render_ms: 1.0,
                det_ms: 50.0,
                rec_ms: 100.0,
            },
        }
    }

    #[test]
    fn status_line_shows_key_numbers() {
        let s = sample();
        let stats = Stats::new();
        let line = status_line(&s, &stats, &UiState::default());
        assert!(line.contains("[de pangram v6 32px]"), "{line}");
        assert!(line.contains("CER 0.000"), "{line}");
        assert!(line.contains("det 50ms rec 100ms"), "{line}");
    }

    #[test]
    fn panel_shows_confusion_and_bad_char() {
        let s = sample();
        let mut stats = Stats::new();
        stats.add("de", &s.model, &s.eval, s.times);
        let p = panel_lines(&stats, "de");
        assert_eq!(p[0], "samples: 1");
        assert_eq!(p[1], "top: -");
        // Fehlerfrei → kein schlechtestes Zeichen.
        assert_eq!(p[2], "bad: -");
        assert_eq!(short_model("eslav_PP-OCRv5_mobile_rec_onnx"), "eslav");
    }

    #[test]
    fn help_mentions_all_keys() {
        for k in ["L/R", "G:", "V:", "M:", "Spc", "N:", "C:", "Q:"] {
            assert!(help_line().contains(k), "{k}");
        }
    }
}
