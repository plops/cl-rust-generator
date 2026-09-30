//! `10_metrics` — CER, Levenshtein-Alignment, Box-Zuordnung.
//!
//! Vergleich auf Codepoints nach Normalisierung: Leerzeichen raus
//! (Modelle setzen sie bei CJK/Thai uneinheitlich), Vollbreiten-ASCII
//! gefaltet. Box-Zuordnung per Mittelpunkt in GT-Box (±4 px Toleranz).

use crate::detect::TextBox;
use crate::render::GtLine;

/// Toleranz um die GT-Box bei der Zuordnung (Pixel).
const MATCH_TOL: f32 = 4.0;

/// Normalisiert zum Vergleich: ohne Whitespace, Vollbreite gefaltet.
#[must_use]
pub fn normalize(s: &str) -> String {
    s.chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| {
            if ('\u{FF01}'..='\u{FF5E}').contains(&c) {
                char::from_u32(c as u32 - 0xFEE0).unwrap_or(c)
            } else {
                c
            }
        })
        .collect()
}

/// Levenshtein-Distanz auf Zeichen (Einfügen/Löschen/Ersetzen je 1).
#[must_use]
pub fn levenshtein(a: &str, b: &str) -> usize {
    let (a, b): (Vec<char>, Vec<char>) = (a.chars().collect(), b.chars().collect());
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0usize; b.len() + 1];
    for (i, &ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, &cb) in b.iter().enumerate() {
            let sub = prev[j] + usize::from(ca != cb);
            cur[j + 1] = (prev[j + 1] + 1).min(cur[j] + 1).min(sub);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}

/// Character Error Rate = Distanz / |GT| (normalisiert).
#[must_use]
pub fn cer(gt: &str, ocr: &str) -> f32 {
    let (g, o) = (normalize(gt), normalize(ocr));
    let n = g.chars().count();
    if n == 0 {
        return f32::from(!o.is_empty());
    }
    levenshtein(&g, &o) as f32 / n as f32
}

/// Ein Schritt im Levenshtein-Alignment (normalisierte Zeichen).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AlignOp {
    /// Gleich.
    Hit(char),
    /// Ersetzt: GT → OCR.
    Sub { gt: char, ocr: char },
    /// Gelöscht (fehlt im OCR-Text).
    Del(char),
    /// Eingefügt (überzählig im OCR-Text).
    Ins(char),
}

/// Alignment GT → OCR (beide normalisiert).
#[must_use]
pub fn align(gt: &str, ocr: &str) -> Vec<AlignOp> {
    let (g, o): (Vec<char>, Vec<char>) = (
        normalize(gt).chars().collect(),
        normalize(ocr).chars().collect(),
    );
    let (n, m) = (g.len(), o.len());
    let mut d = vec![vec![0usize; m + 1]; n + 1];
    for (i, row) in d.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in d[0].iter_mut().enumerate() {
        *cell = j;
    }
    for i in 1..=n {
        for j in 1..=m {
            let sub = d[i - 1][j - 1] + usize::from(g[i - 1] != o[j - 1]);
            d[i][j] = (d[i - 1][j] + 1).min(d[i][j - 1] + 1).min(sub);
        }
    }
    let mut ops = Vec::new();
    let (mut i, mut j) = (n, m);
    while i > 0 || j > 0 {
        if i > 0 && j > 0 && d[i][j] == d[i - 1][j - 1] + usize::from(g[i - 1] != o[j - 1]) {
            if g[i - 1] == o[j - 1] {
                ops.push(AlignOp::Hit(g[i - 1]));
            } else {
                ops.push(AlignOp::Sub {
                    gt: g[i - 1],
                    ocr: o[j - 1],
                });
            }
            i -= 1;
            j -= 1;
        } else if i > 0 && d[i][j] == d[i - 1][j] + 1 {
            ops.push(AlignOp::Del(g[i - 1]));
            i -= 1;
        } else {
            ops.push(AlignOp::Ins(o[j - 1]));
            j -= 1;
        }
    }
    ops.reverse();
    ops
}

/// Auswertung einer GT-Zeile.
#[derive(Clone, Debug)]
pub struct LineEval {
    /// Erkannter Text der zugeordneten Boxen (links→rechts, verkettet).
    pub ocr: String,
    /// Fehler (Levenshtein) und GT-Länge (normalisiert).
    pub errors: usize,
    /// CER dieser Zeile.
    pub cer: f32,
    /// CER == 0.
    pub exact: bool,
    /// Zugeordnete Detektions-Boxen.
    pub matched: usize,
    /// IoU (Vereinigung der Boxen vs. GT-Box).
    pub iou: f32,
    /// Mittlere Konfidenz der zugeordneten Boxen.
    pub conf: f32,
    /// Alignment für die Zeichen-Statistik.
    pub ops: Vec<AlignOp>,
}

/// Auswertung eines Samples (alle Zeilen + FP-Boxen).
#[derive(Clone, Debug)]
pub struct SampleEval {
    /// Eine Auswertung je GT-Zeile.
    pub lines: Vec<LineEval>,
    /// Detektions-Boxen ohne GT-Zuordnung.
    pub fp_boxes: usize,
}

impl SampleEval {
    /// Anteil GT-Zeilen mit ≥1 Box.
    #[must_use]
    pub fn recall(&self) -> f32 {
        if self.lines.is_empty() {
            return 1.0;
        }
        self.lines.iter().filter(|l| l.matched > 0).count() as f32 / self.lines.len() as f32
    }

    /// Mittlere CER über Zeilen.
    #[must_use]
    pub fn mean_cer(&self) -> f32 {
        if self.lines.is_empty() {
            return 0.0;
        }
        self.lines.iter().map(|l| l.cer).sum::<f32>() / self.lines.len() as f32
    }
}

/// Ordnet Detektions-Boxen den GT-Zeilen zu und wertet aus.
///
/// `confs[i]` gehört zu `det[i]` (fehlende → 0).
#[must_use]
pub fn evaluate(gt: &[GtLine], det: &[TextBox], confs: &[f32]) -> SampleEval {
    let mut used = vec![false; det.len()];
    let mut lines = Vec::with_capacity(gt.len());
    for g in gt {
        let mut idx: Vec<usize> = det
            .iter()
            .enumerate()
            .filter(|(i, b)| {
                !used[*i] && {
                    let (cx, cy) = b.rect.center();
                    cx >= g.rect.x - MATCH_TOL
                        && cx <= g.rect.x + g.rect.w + MATCH_TOL
                        && cy >= g.rect.y - MATCH_TOL
                        && cy <= g.rect.y + g.rect.h + MATCH_TOL
                }
            })
            .map(|(i, _)| i)
            .collect();
        idx.sort_by(|&a, &b| det[a].rect.x.total_cmp(&det[b].rect.x));
        for &i in &idx {
            used[i] = true;
        }
        let ocr: String = idx.iter().map(|&i| det[i].text.as_str()).collect();
        let norm_gt = normalize(&g.text);
        let errors = levenshtein(&norm_gt, &normalize(&ocr));
        let n = norm_gt.chars().count();
        let cer = if n == 0 {
            f32::from(!ocr.is_empty())
        } else {
            errors as f32 / n as f32
        };
        let mut uni = idx.first().map_or(g.rect, |&i| det[i].rect);
        for &i in &idx {
            uni = uni.union(&det[i].rect);
        }
        let conf = if idx.is_empty() {
            0.0
        } else {
            idx.iter()
                .map(|&i| confs.get(i).copied().unwrap_or(0.0))
                .sum::<f32>()
                / idx.len() as f32
        };
        lines.push(LineEval {
            ocr: ocr.clone(),
            errors,
            cer,
            exact: errors == 0,
            matched: idx.len(),
            iou: if idx.is_empty() {
                0.0
            } else {
                uni.iou(&g.rect)
            },
            conf,
            ops: align(&g.text, &ocr),
        });
    }
    SampleEval {
        lines,
        fp_boxes: used.iter().filter(|u| !**u).count(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::render::Rect;

    #[test]
    fn levenshtein_counts_insert_delete_substitute() {
        assert_eq!(levenshtein("abc", "abc"), 0);
        assert_eq!(levenshtein("abc", "ab"), 1); // Löschen
        assert_eq!(levenshtein("ab", "abc"), 1); // Einfügen
        assert_eq!(levenshtein("abc", "axc"), 1); // Ersetzen
        assert_eq!(levenshtein("", "abc"), 3);
    }

    #[test]
    fn alignment_reports_confusion() {
        assert_eq!(align("ß", "B"), vec![AlignOp::Sub { gt: 'ß', ocr: 'B' }]);
        assert_eq!(
            align("ab", "ab"),
            vec![AlignOp::Hit('a'), AlignOp::Hit('b')]
        );
        assert_eq!(align("ab", "a"), vec![AlignOp::Hit('a'), AlignOp::Del('b')]);
    }

    #[test]
    fn normalization_ignores_spaces_and_fullwidth() {
        assert_eq!(normalize("a b\tc"), "abc");
        assert_eq!(normalize("ＡＢｃ"), "ABc"); // U+FF21…
        assert_eq!(cer("Hallo Welt", "HalloWelt"), 0.0);
        assert_eq!(cer("abc", "axc"), 1.0 / 3.0);
    }

    fn gt(text: &str, x: f32, y: f32) -> GtLine {
        GtLine {
            text: text.to_string(),
            rect: Rect {
                x,
                y,
                w: 100.0,
                h: 20.0,
            },
        }
    }

    fn det(text: &str, x: f32, y: f32) -> TextBox {
        TextBox {
            rect: Rect {
                x,
                y,
                w: 40.0,
                h: 20.0,
            },
            text: text.to_string(),
        }
    }

    #[test]
    fn box_matching_joins_split_boxes_and_counts_fp() {
        let gt = vec![gt("HalloWelt", 10.0, 10.0)];
        let det = vec![
            det("Hallo", 12.0, 10.0), // Mittelpunkt in GT
            det("Welt", 60.0, 10.0),  // Mittelpunkt in GT
            det("x", 500.0, 500.0),   // FP
        ];
        let e = evaluate(&gt, &det, &[0.9, 0.8, 0.1]);
        assert_eq!(e.fp_boxes, 1);
        assert_eq!(e.lines.len(), 1);
        let l = &e.lines[0];
        assert_eq!(l.ocr, "HalloWelt");
        assert_eq!(l.errors, 0);
        assert!(l.exact);
        assert_eq!(l.matched, 2);
        assert!((l.conf - 0.85).abs() < 1e-6);
        assert!(l.iou > 0.0);
        assert_eq!(e.recall(), 1.0);
    }

    #[test]
    fn unmatched_gt_line_has_zero_iou() {
        let gt = vec![gt("ab", 10.0, 10.0)];
        let e = evaluate(&gt, &[], &[]);
        assert_eq!(e.lines[0].matched, 0);
        assert_eq!(e.lines[0].iou, 0.0);
        assert_eq!(e.recall(), 0.0);
        assert_eq!(e.mean_cer(), 1.0);
    }
}
