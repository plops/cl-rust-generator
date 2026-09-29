//! `05_decode` — YOLO11-Rohausgabe `[1, 4+nc, N]` → Boxen, plus greedy NMS
//! (Semantik wie Ultralytics/torchvision: Score `> conf`, Unterdrückung bei
//! IoU `> thr`, klassenweise, höchstens `max_det` Boxen).

/// Eine Detektion (`x1,y1,x2,y2` im jeweiligen Koordinatenraum).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Det {
    pub b: [f32; 4],
    pub score: f32,
    pub class: usize,
}

/// Dekodiert kanal-major `out` (`rows = 4+nc` Zeilen à `n` Spalten):
/// `cx,cy,w,h,score_0..score_nc` → Kandidaten mit Score `> conf`.
#[must_use]
pub fn decode(out: &[f32], rows: usize, n: usize, conf: f32) -> Vec<Det> {
    assert!(rows > 4 && out.len() >= rows * n);
    let at = |r: usize, i: usize| out[r * n + i];
    let mut dets = Vec::new();
    for i in 0..n {
        let (class, score) = (4..rows)
            .map(|r| (r - 4, at(r, i)))
            .fold((0, f32::MIN), |a, c| if c.1 > a.1 { c } else { a });
        if score > conf {
            let (cx, cy, w, h) = (at(0, i), at(1, i), at(2, i) / 2.0, at(3, i) / 2.0);
            dets.push(Det {
                b: [cx - w, cy - h, cx + w, cy + h],
                score,
                class,
            });
        }
    }
    dets
}

/// Intersection over Union zweier `xyxy`-Boxen.
#[must_use]
pub fn iou(a: &[f32; 4], b: &[f32; 4]) -> f32 {
    let iw = (a[2].min(b[2]) - a[0].max(b[0])).max(0.0);
    let ih = (a[3].min(b[3]) - a[1].max(b[1])).max(0.0);
    let inter = iw * ih;
    let area = |r: &[f32; 4]| (r[2] - r[0]) * (r[3] - r[1]);
    let union = area(a) + area(b) - inter;
    if union > 0.0 { inter / union } else { 0.0 }
}

/// Greedy NMS: nach Score absteigend, Boxen gleicher Klasse mit
/// IoU `> thr` zu einer behaltenen Box fallen weg.
#[must_use]
pub fn nms(mut dets: Vec<Det>, thr: f32, max_det: usize) -> Vec<Det> {
    dets.sort_by(|a, b| b.score.total_cmp(&a.score));
    let mut keep: Vec<Det> = Vec::new();
    for d in dets {
        if keep.len() == max_det {
            break;
        }
        if keep
            .iter()
            .all(|k| k.class != d.class || iou(&k.b, &d.b) <= thr)
        {
            keep.push(d);
        }
    }
    keep
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Baut einen `[5, n]`-Tensor aus (cx,cy,w,h,score)-Spalten.
    fn tensor(cols: &[[f32; 5]]) -> Vec<f32> {
        let n = cols.len();
        let mut t = vec![0.0; 5 * n];
        for (i, c) in cols.iter().enumerate() {
            for r in 0..5 {
                t[r * n + i] = c[r];
            }
        }
        t
    }

    #[test]
    fn decode_converts_center_to_corners_and_filters() {
        let t = tensor(&[[100.0, 50.0, 20.0, 10.0, 0.9], [1.0, 1.0, 1.0, 1.0, 0.05]]);
        let d = decode(&t, 5, 2, 0.05);
        assert_eq!(d.len(), 1); // 0.05 ist nicht > 0.05
        assert_eq!(d[0].b, [90.0, 45.0, 110.0, 55.0]);
        assert_eq!(d[0].class, 0);
    }

    #[test]
    fn decode_picks_best_class() {
        // 2 Klassen: rows = 6.
        let t = vec![10.0, 10.0, 4.0, 4.0, 0.2, 0.7];
        let d = decode(&t, 6, 1, 0.1);
        assert_eq!((d[0].class, d[0].score), (1, 0.7));
    }

    #[test]
    fn iou_basics() {
        let a = [0.0, 0.0, 10.0, 10.0];
        assert_eq!(iou(&a, &a), 1.0);
        assert_eq!(iou(&a, &[20.0, 20.0, 30.0, 30.0]), 0.0);
        assert!((iou(&a, &[5.0, 0.0, 15.0, 10.0]) - 1.0 / 3.0).abs() < 1e-6);
    }

    #[test]
    fn nms_keeps_strongest_and_disjoint() {
        let mk = |x: f32, s: f32| Det {
            b: [x, 0.0, x + 10.0, 10.0],
            score: s,
            class: 0,
        };
        let out = nms(vec![mk(1.0, 0.5), mk(0.0, 0.9), mk(50.0, 0.3)], 0.7, 300);
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].score, 0.9);
        assert_eq!(out[1].score, 0.3);
        // max_det begrenzt.
        assert_eq!(nms(vec![mk(0.0, 0.9), mk(50.0, 0.3)], 0.7, 1).len(), 1);
    }

    #[test]
    fn nms_is_class_aware() {
        let a = Det {
            b: [0.0, 0.0, 10.0, 10.0],
            score: 0.9,
            class: 0,
        };
        let b = Det { class: 1, ..a };
        assert_eq!(nms(vec![a, b], 0.5, 300).len(), 2);
    }
}
