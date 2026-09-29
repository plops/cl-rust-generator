//! `08_dirty` — geänderte Bildbereiche als wenige, block-ausgerichtete
//! Rechtecke (Grundlage der AV1-Kacheln).
//!
//! Vorgehen: 16×16-Blockraster vergleichen → Zusammenhangskomponenten →
//! Bounding-Boxes → nahe/überlappende zusammenlegen (eine Kachel kostet
//! ~100 Byte Header, daher lohnt Zusammenlegen bei wenig Mehrfläche) →
//! höchstens `max` Rechtecke.

use lbw_common::Rect;

use crate::image::Rgb;

/// Blockkante in Pixeln (auch Mindestkachelgröße, gerade).
pub const BLOCK: usize = 16;
/// Zusätzliche Fläche (px), die ein Zusammenlegen kosten darf.
pub const MERGE_SLACK: u32 = 4096;

/// Blockraster: `true` = Block unterscheidet sich (`None` = alles neu).
#[must_use]
pub fn changed_blocks(cur: &Rgb, reference: Option<&Rgb>) -> (usize, usize, Vec<bool>) {
    let (gw, gh) = (cur.w.div_ceil(BLOCK), cur.h.div_ceil(BLOCK));
    let Some(r) = reference else {
        return (gw, gh, vec![true; gw * gh]);
    };
    assert_eq!((r.w, r.h), (cur.w, cur.h));
    let mut g = vec![false; gw * gh];
    for y in 0..cur.h {
        let s = y * cur.w * 3;
        let (a, b) = (&cur.data[s..s + cur.w * 3], &r.data[s..s + cur.w * 3]);
        for bx in 0..gw {
            let (x0, x1) = (bx * BLOCK * 3, ((bx + 1) * BLOCK).min(cur.w) * 3);
            if a[x0..x1] != b[x0..x1] {
                g[(y / BLOCK) * gw + bx] = true;
            }
        }
    }
    (gw, gh, g)
}

/// Rechteck in Blockeinheiten (`x0,y0` inklusiv, `x1,y1` exklusiv).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct B {
    x0: usize,
    y0: usize,
    x1: usize,
    y1: usize,
}

impl B {
    fn area(&self) -> u32 {
        ((self.x1 - self.x0) * (self.y1 - self.y0) * BLOCK * BLOCK) as u32
    }
    fn union(&self, o: &B) -> B {
        B {
            x0: self.x0.min(o.x0),
            y0: self.y0.min(o.y0),
            x1: self.x1.max(o.x1),
            y1: self.y1.max(o.y1),
        }
    }
    /// Zusatzfläche beim Zusammenlegen (negativ bei Überlappung → 0).
    fn cost(&self, o: &B) -> u32 {
        self.union(o).area().saturating_sub(self.area() + o.area())
    }
}

fn components(gw: usize, gh: usize, g: &[bool]) -> Vec<B> {
    let mut seen = vec![false; g.len()];
    let mut out = Vec::new();
    let mut q = Vec::new();
    for s in 0..g.len() {
        if !g[s] || seen[s] {
            continue;
        }
        seen[s] = true;
        q.clear();
        q.push(s);
        let mut b = B {
            x0: s % gw,
            y0: s / gw,
            x1: s % gw + 1,
            y1: s / gw + 1,
        };
        while let Some(i) = q.pop() {
            let (x, y) = (i % gw, i / gw);
            b = b.union(&B {
                x0: x,
                y0: y,
                x1: x + 1,
                y1: y + 1,
            });
            for dy in -1isize..=1 {
                for dx in -1isize..=1 {
                    let (nx, ny) = (x as isize + dx, y as isize + dy);
                    if nx < 0 || ny < 0 || nx as usize >= gw || ny as usize >= gh {
                        continue;
                    }
                    let n = ny as usize * gw + nx as usize;
                    if g[n] && !seen[n] {
                        seen[n] = true;
                        q.push(n);
                    }
                }
            }
        }
        out.push(b);
    }
    out
}

/// Legt Rechtecke zusammen: erst alle „billigen“, dann bis `max` erreicht ist.
fn merge(mut v: Vec<B>, max: usize) -> Vec<B> {
    loop {
        let mut best: Option<(usize, usize, u32)> = None;
        for i in 0..v.len() {
            for j in i + 1..v.len() {
                let c = v[i].cost(&v[j]);
                if best.is_none_or(|b| c < b.2) {
                    best = Some((i, j, c));
                }
            }
        }
        match best {
            Some((i, j, c)) if c <= MERGE_SLACK || v.len() > max.max(1) => {
                v[i] = v[i].union(&v[j]);
                v.swap_remove(j);
            }
            _ => return v,
        }
    }
}

/// Geänderte Bereiche von `cur` gegenüber `reference` in Pixeln (auf das
/// Bild begrenzt, sortiert nach y, x).
#[must_use]
pub fn dirty_rects(cur: &Rgb, reference: Option<&Rgb>, max: usize) -> Vec<Rect> {
    let (gw, gh, g) = changed_blocks(cur, reference);
    let mut rects: Vec<Rect> = merge(components(gw, gh, &g), max)
        .into_iter()
        .map(|b| {
            cur.clamp(Rect::new(
                (b.x0 * BLOCK) as u16,
                (b.y0 * BLOCK) as u16,
                ((b.x1 - b.x0) * BLOCK) as u16,
                ((b.y1 - b.y0) * BLOCK) as u16,
            ))
        })
        .collect();
    rects.sort_by_key(|r| (r.y, r.x));
    rects
}

/// Erweitert `r` auf Blockraster (für Icon-Kacheln: gerade, ≥ 16 px).
#[must_use]
pub fn align(r: Rect, w: usize, h: usize) -> Rect {
    let b = BLOCK as u16;
    let x0 = r.x / b * b;
    let y0 = r.y / b * b;
    let x1 = (r.x2().div_ceil(b) * b).min(w as u16);
    let y1 = (r.y2().div_ceil(b) * b).min(h as u16);
    Rect::new(x0, y0, x1 - x0, y1 - y0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base() -> Rgb {
        Rgb::filled(160, 160, [255; 3])
    }

    #[test]
    fn identical_frames_have_no_dirt_and_first_frame_is_full() {
        let a = base();
        assert!(dirty_rects(&a, Some(&a), 4).is_empty());
        assert_eq!(dirty_rects(&a, None, 4), vec![Rect::new(0, 0, 160, 160)]);
    }

    #[test]
    fn single_pixel_change_gives_one_block() {
        let a = base();
        let mut b = a.clone();
        b.put(37, 70, [0; 3]);
        assert_eq!(
            dirty_rects(&b, Some(&a), 4),
            vec![Rect::new(32, 64, 16, 16)]
        );
    }

    #[test]
    fn far_changes_stay_separate_near_ones_merge() {
        let a = base();
        let mut b = a.clone();
        b.put(1, 1, [0; 3]);
        b.put(150, 150, [0; 3]);
        assert_eq!(dirty_rects(&b, Some(&a), 4).len(), 2);
        b.put(40, 1, [0; 3]); // zwei Blöcke Lücke zu (1,1) → billig
        let r = dirty_rects(&b, Some(&a), 4);
        assert_eq!(r.len(), 2);
        assert_eq!(r[0], Rect::new(0, 0, 48, 16));
    }

    #[test]
    fn max_rects_is_enforced() {
        let a = base();
        let mut b = a.clone();
        for i in 0..5 {
            b.put(i * 32 + 1, (i % 2) * 140 + 1, [0; 3]);
        }
        let r = dirty_rects(&b, Some(&a), 2);
        assert!(r.len() <= 2);
        // Alle Änderungen bleiben abgedeckt.
        for i in 0..5 {
            let p = Rect::new((i * 32 + 1) as u16, ((i % 2) * 140 + 1) as u16, 1, 1);
            assert!(r.iter().any(|q| q.intersection(&p) == 1));
        }
    }

    #[test]
    fn align_snaps_to_blocks_and_image() {
        assert_eq!(
            align(Rect::new(5, 17, 20, 3), 640, 640),
            Rect::new(0, 16, 32, 16)
        );
        assert_eq!(
            align(Rect::new(630, 630, 10, 10), 640, 640),
            Rect::new(624, 624, 16, 16)
        );
    }
}
