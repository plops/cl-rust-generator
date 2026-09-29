//! `08_bench` — Benchmark-Schleife: Warmup, N Iterationen, Median/p90 je
//! Stufe (Grab optional, Pre, Inferenz, Post) und Tabellenzeile.

use crate::capture::Screen;
use crate::detector::{Detector, Timings};
use crate::image::Rgb;
use std::time::Instant;

/// Ergebnis eines Modells.
#[derive(Debug)]
pub struct Row {
    pub name: String,
    pub provider: &'static str,
    pub threads: usize,
    pub mbytes: f64,
    pub grab: f64,
    pub pre: f64,
    pub infer: f64,
    pub post: f64,
    pub total: f64,
    pub total_p90: f64,
    pub boxes: usize,
}

pub const HEADER: &str = "model\tprovider\tthreads\tMB\tgrab_ms\tpre_ms\tinfer_ms\tpost_ms\ttotal_ms\ttotal_p90\tfps\tboxes";

impl Row {
    #[must_use]
    pub fn tsv(&self) -> String {
        format!(
            "{}\t{}\t{}\t{:.1}\t{:.2}\t{:.2}\t{:.2}\t{:.2}\t{:.2}\t{:.2}\t{:.1}\t{}",
            self.name,
            self.provider,
            self.threads,
            self.mbytes,
            self.grab,
            self.pre,
            self.infer,
            self.post,
            self.total,
            self.total_p90,
            1e3 / self.total,
            self.boxes
        )
    }
}

/// Median und 90-%-Quantil (nearest rank) einer Stichprobe.
#[must_use]
pub fn median_p90(v: &[f64]) -> (f64, f64) {
    if v.is_empty() {
        return (0.0, 0.0);
    }
    let mut s = v.to_vec();
    s.sort_by(f64::total_cmp);
    let q = |p: f64| s[((p * s.len() as f64).ceil() as usize).clamp(1, s.len()) - 1];
    (q(0.5), q(0.9))
}

/// Misst `iters` Detektionen nach `warmup` Läufen. Mit `screen` wird pro
/// Iteration neu gegrabbt (End-to-end), sonst `img` wiederverwendet.
pub fn run(
    det: &mut Detector,
    img: &Rgb,
    screen: Option<&Screen>,
    warmup: usize,
    iters: usize,
) -> Result<Row, String> {
    for _ in 0..warmup {
        det.detect(img)?;
    }
    let mut t: Vec<(f64, Timings)> = Vec::with_capacity(iters);
    let mut boxes = 0;
    for _ in 0..iters {
        let (grab_ms, frame) = match screen {
            Some(s) => {
                let t0 = Instant::now();
                let f = s.grab()?;
                (t0.elapsed().as_secs_f64() * 1e3, Some(f))
            }
            None => (0.0, None),
        };
        let (d, tm) = det.detect(frame.as_ref().unwrap_or(img))?;
        boxes = d.len();
        t.push((grab_ms, tm));
    }
    let col = |f: fn(&(f64, Timings)) -> f64| median_p90(&t.iter().map(f).collect::<Vec<_>>());
    let (total, total_p90) = col(|x| x.0 + x.1.total());
    Ok(Row {
        name: String::new(),
        provider: det.model.provider,
        threads: 0,
        mbytes: 0.0,
        grab: col(|x| x.0).0,
        pre: col(|x| x.1.pre).0,
        infer: col(|x| x.1.infer).0,
        post: col(|x| x.1.post).0,
        total,
        total_p90,
        boxes,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn median_and_p90() {
        let v: Vec<f64> = (1..=10).map(f64::from).collect();
        assert_eq!(median_p90(&v), (5.0, 9.0));
        assert_eq!(median_p90(&[3.0]), (3.0, 3.0));
        assert_eq!(median_p90(&[]), (0.0, 0.0));
    }

    #[test]
    fn tsv_has_header_arity() {
        let r = Row {
            name: "m".into(),
            provider: "CPU",
            threads: 4,
            mbytes: 1.0,
            grab: 0.0,
            pre: 1.0,
            infer: 2.0,
            post: 1.0,
            total: 4.0,
            total_p90: 5.0,
            boxes: 7,
        };
        assert_eq!(r.tsv().split('\t').count(), HEADER.split('\t').count());
        assert!(r.tsv().contains("\t250.0\t"));
    }
}
