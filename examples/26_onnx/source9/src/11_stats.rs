//! `11_stats` — Aggregation je (Sprache, Modell) + Zeichen-Statistik.
//!
//! Korpus-CER = Fehler/Zeichen über alle Zeilen; dazu Exakt-Anteil,
//! Detektions-Recall, FP-Boxen, IoU, Konfidenz und Zeiten. Die Zeichen-
//! Statistik zählt aus dem Alignment je GT-Zeichen gesehen/korrekt und
//! die häufigsten Verwechslungen (`∅` = gelöscht/eingefügt).

use std::collections::{BTreeMap, HashMap};

use crate::metrics::{AlignOp, SampleEval};

/// Verwechslung: (GT, OCR), `None` = ∅ (gelöscht bzw. eingefügt).
pub type Confusion = (Option<char>, Option<char>);

/// Stufen-Zeiten eines Samples in Millisekunden.
#[derive(Clone, Copy, Debug, Default)]
pub struct Times {
    /// Rendern.
    pub render_ms: f64,
    /// Detektion.
    pub det_ms: f64,
    /// Erkennung.
    pub rec_ms: f64,
}

impl Times {
    /// Summe aller Stufen.
    #[must_use]
    pub fn total_ms(&self) -> f64 {
        self.render_ms + self.det_ms + self.rec_ms
    }
}

#[derive(Debug, Default)]
struct Agg {
    samples: u64,
    lines: u64,
    exact_lines: u64,
    matched_lines: u64,
    errors: u64,
    chars: u64,
    fp: u64,
    iou_sum: f64,
    conf_sum: f64,
    conf_n: u64,
    ms: f64,
}

#[derive(Debug, Default)]
struct LangChars {
    seen: HashMap<char, u64>,
    correct: HashMap<char, u64>,
    confusions: HashMap<Confusion, u64>,
}

/// Kumulierte Benchmark-Statistik.
#[derive(Debug, Default)]
pub struct Stats {
    agg: BTreeMap<(String, String), Agg>,
    chars: BTreeMap<String, LangChars>,
}

impl Stats {
    /// Leere Statistik.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Bucht ein Sample (`model`: Ordnername des Erkennungsmodells).
    pub fn add(&mut self, lang: &str, model: &str, eval: &SampleEval, times: Times) {
        let a = self
            .agg
            .entry((lang.to_string(), model.to_string()))
            .or_default();
        a.samples += 1;
        a.fp += eval.fp_boxes as u64;
        a.ms += times.total_ms();
        for l in &eval.lines {
            a.lines += 1;
            a.exact_lines += u64::from(l.exact);
            a.matched_lines += u64::from(l.matched > 0);
            a.errors += l.errors as u64;
            a.chars += l
                .ops
                .iter()
                .filter(|o| !matches!(o, AlignOp::Ins(_)))
                .count() as u64;
            a.iou_sum += f64::from(l.iou);
            if l.matched > 0 {
                a.conf_sum += f64::from(l.conf);
                a.conf_n += 1;
            }
        }
        let c = self.chars.entry(lang.to_string()).or_default();
        for l in &eval.lines {
            for op in &l.ops {
                match *op {
                    AlignOp::Hit(g) => {
                        *c.seen.entry(g).or_default() += 1;
                        *c.correct.entry(g).or_default() += 1;
                    }
                    AlignOp::Sub { gt, ocr } => {
                        *c.seen.entry(gt).or_default() += 1;
                        *c.confusions.entry((Some(gt), Some(ocr))).or_default() += 1;
                    }
                    AlignOp::Del(g) => {
                        *c.seen.entry(g).or_default() += 1;
                        *c.confusions.entry((Some(g), None)).or_default() += 1;
                    }
                    AlignOp::Ins(o) => {
                        *c.confusions.entry((None, Some(o))).or_default() += 1;
                    }
                }
            }
        }
    }

    /// Anzahl Samples insgesamt.
    #[must_use]
    pub fn samples(&self) -> u64 {
        self.agg.values().map(|a| a.samples).sum()
    }

    /// Korpus-CER je (Sprache, Modell).
    #[must_use]
    pub fn cer(&self, lang: &str, model: &str) -> f64 {
        self.agg
            .get(&(lang.to_string(), model.to_string()))
            .map_or(0.0, |a| {
                if a.chars > 0 {
                    a.errors as f64 / a.chars as f64
                } else {
                    0.0
                }
            })
    }

    /// Schlechteste Zeichen: (Zeichen, gesehen, Fehlerrate), nach Fehlern absteigend.
    #[must_use]
    pub fn worst_chars(&self, lang: &str, n: usize) -> Vec<(char, u64, f64)> {
        self.chars.get(lang).map_or(Vec::new(), |c| {
            let mut v: Vec<(char, u64, f64)> = c
                .seen
                .iter()
                .map(|(&ch, &seen)| {
                    let ok = c.correct.get(&ch).copied().unwrap_or(0);
                    (ch, seen, 1.0 - ok as f64 / seen as f64)
                })
                .filter(|&(_, _, rate)| rate > 0.0)
                .collect();
            v.sort_by(|a, b| {
                let ea = a.1 as f64 * a.2;
                let eb = b.1 as f64 * b.2;
                eb.total_cmp(&ea).then_with(|| b.2.total_cmp(&a.2))
            });
            v.truncate(n);
            v
        })
    }

    /// Häufigste Verwechslungen ((GT, OCR), Anzahl).
    #[must_use]
    pub fn top_confusions(&self, lang: &str, n: usize) -> Vec<(Confusion, u64)> {
        self.chars.get(lang).map_or(Vec::new(), |c| {
            let mut v: Vec<(Confusion, u64)> = c.confusions.iter().map(|(&k, &v)| (k, v)).collect();
            v.sort_by_key(|a| std::cmp::Reverse(a.1));
            v.truncate(n);
            v
        })
    }

    /// Markdown-Report: Tabelle je (Sprache, Modell) + Zeichen-Statistik.
    #[must_use]
    pub fn markdown(&self) -> String {
        let mut s = String::from("# Unicode-OCR-Benchmark\n\n");
        s.push_str("| Lang | Modell | Samples | Zeilen | CER | Exakt | Recall | FP | IoU | Konf | ms/Sample | Z./s |\n");
        s.push_str("|---|---|---|---|---|---|---|---|---|---|---|---|\n");
        for ((lang, model), a) in &self.agg {
            let cer = if a.chars > 0 {
                a.errors as f64 / a.chars as f64
            } else {
                0.0
            };
            let exact = pct(a.exact_lines, a.lines);
            let recall = pct(a.matched_lines, a.lines);
            let iou = if a.lines > 0 {
                a.iou_sum / a.lines as f64
            } else {
                0.0
            };
            let conf = if a.conf_n > 0 {
                a.conf_sum / a.conf_n as f64
            } else {
                0.0
            };
            let ms = if a.samples > 0 {
                a.ms / a.samples as f64
            } else {
                0.0
            };
            let cps = if a.ms > 0.0 {
                a.chars as f64 / (a.ms / 1000.0)
            } else {
                0.0
            };
            s.push_str(&format!(
                "| {lang} | {model} | {} | {} | {cer:.3} | {exact:.1}% | {recall:.1}% | {} | {iou:.2} | {conf:.2} | {ms:.0} | {cps:.0} |\n",
                a.samples, a.lines, a.fp
            ));
        }
        for lang in self.chars.keys() {
            s.push_str(&format!("\n## Zeichen ({lang})\n\n"));
            let worst = self.worst_chars(lang, 8);
            if worst.is_empty() {
                s.push_str("keine Fehler.\n");
            } else {
                s.push_str("schlechteste Zeichen (Zeichen, gesehen, Fehlerrate):\n");
                for (ch, seen, rate) in worst {
                    s.push_str(&format!("- `{ch}`: {seen}×, {:.0}%\n", rate * 100.0));
                }
            }
            let conf = self.top_confusions(lang, 8);
            if !conf.is_empty() {
                s.push_str("häufigste Verwechslungen (GT→OCR):\n");
                for ((g, o), n) in conf {
                    s.push_str(&format!("- {}→{}: {n}×\n", show(g), show(o)));
                }
            }
        }
        s
    }
}

fn pct(n: u64, d: u64) -> f64 {
    if d > 0 {
        100.0 * n as f64 / d as f64
    } else {
        0.0
    }
}

fn show(c: Option<char>) -> String {
    c.map_or_else(|| "∅".to_string(), |ch| format!("`{ch}`"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::detect::TextBox;
    use crate::metrics::evaluate;
    use crate::render::{GtLine, Rect};

    fn gt(text: &str) -> GtLine {
        GtLine {
            text: text.to_string(),
            rect: Rect {
                x: 0.0,
                y: 0.0,
                w: 100.0,
                h: 20.0,
            },
        }
    }

    fn det(text: &str) -> TextBox {
        TextBox {
            rect: Rect {
                x: 10.0,
                y: 2.0,
                w: 40.0,
                h: 16.0,
            },
            text: text.to_string(),
        }
    }

    fn times() -> Times {
        Times {
            render_ms: 10.0,
            det_ms: 100.0,
            rec_ms: 50.0,
        }
    }

    #[test]
    fn aggregation_sums_errors_and_chars() {
        let mut st = Stats::new();
        // "Größe" als "GröBe" gelesen: 1 Fehler auf 5 Zeichen.
        let e = evaluate(&[gt("Größe")], &[det("GröBe")], &[0.7]);
        st.add("de", "m", &e, times());
        // Perfekte Zeile.
        let e2 = evaluate(&[gt("ab")], &[det("ab")], &[0.9]);
        st.add("de", "m", &e2, times());
        assert_eq!(st.samples(), 2);
        assert!((st.cer("de", "m") - 1.0 / 7.0).abs() < 1e-9);
        // Verwechslung ß→B gezählt.
        assert_eq!(
            st.top_confusions("de", 1),
            vec![((Some('ß'), Some('B')), 1)]
        );
        let worst = st.worst_chars("de", 2);
        assert_eq!(worst[0].0, 'ß');
        assert_eq!(worst[0].1, 1);
    }

    #[test]
    fn markdown_report_has_table_and_chars() {
        let mut st = Stats::new();
        let e = evaluate(&[gt("Größe")], &[det("GröBe")], &[0.7]);
        st.add("de", "m", &e, times());
        let md = st.markdown();
        assert!(md.contains("| Lang | Modell |"), "{md}");
        assert!(md.contains("| de | m | 1 | 1 | 0.200 |"), "{md}");
        assert!(md.contains("## Zeichen (de)"), "{md}");
        assert!(md.contains("`ß`→`B`: 1×"), "{md}");
        assert!(md.contains("- `ß`: 1×, 100%"), "{md}");
    }
}
