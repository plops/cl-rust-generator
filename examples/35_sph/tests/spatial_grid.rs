//! Integrationstests: Spatial-Hash und Nachbarschaftssuche.

use sph::spatial_grid::{CpuGrid, cell_coords, hash_cell};
use sph::sph_math::dist;
use sph::types::GridMeta;

/// Deterministischer LCG (kein rand-Crate nötig).
struct Lcg(u64);

impl Lcg {
    fn next_f32(&mut self) -> f32 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        ((self.0 >> 33) as f32) / (u32::MAX as f32)
    }
}

#[test]
fn hash_ist_konsistent_mit_zellkoordinaten() {
    let g = GridMeta::new(1.6, 1.0, 0.04);
    let mut rng = Lcg(12345);
    for _ in 0..2000 {
        let x = rng.next_f32() * 1.6;
        let y = rng.next_f32() * 1.0;
        let h = hash_cell(x, y, g.inv_cell, g.w, g.h);
        let (cx, cy) = cell_coords(x, y, &g);
        assert_eq!(h, cy * g.w + cx);
        assert!((h as usize) < g.num_cells());
    }
}

#[test]
fn rebuild_ist_vollstaendig_fuer_mehrere_groessen() {
    let g = GridMeta::new(1.6, 1.0, 0.04);
    for n in [1, 17, 1000, 16_384] {
        let mut rng = Lcg(n as u64 + 7);
        let pos: Vec<[f32; 2]> = (0..n)
            .map(|_| [rng.next_f32() * 1.59 + 0.005, rng.next_f32() * 0.99 + 0.005])
            .collect();
        let mut grid = CpuGrid::new(g.num_cells(), n);
        grid.rebuild(&pos, &g);
        // Jeder Index genau einmal, Präfix-Sum schließt mit N.
        let mut seen = vec![0u32; n];
        for &idx in &grid.order {
            seen[idx as usize] += 1;
        }
        assert!(seen.iter().all(|&c| c == 1), "n={n}");
        assert_eq!(*grid.cell_start.last().unwrap(), n as u32);
        // Zellstarts monoton wachsend.
        assert!(grid.cell_start.windows(2).all(|w| w[0] <= w[1]));
    }
}

#[test]
fn nachbarschaft_entspricht_brute_force() {
    let g = GridMeta::new(1.6, 1.0, 0.04);
    let mut rng = Lcg(999);
    let pos: Vec<[f32; 2]> = (0..2000)
        .map(|_| [rng.next_f32() * 1.59 + 0.005, rng.next_f32() * 0.99 + 0.005])
        .collect();
    let mut grid = CpuGrid::new(g.num_cells(), pos.len());
    grid.rebuild(&pos, &g);
    // 40 zufällige Sonden gegen Brute-Force-Referenz.
    for _ in 0..40 {
        let probe = [rng.next_f32() * 1.6, rng.next_f32() * 1.0];
        let expected: Vec<usize> = pos
            .iter()
            .enumerate()
            .filter(|(_, p)| dist(probe, **p) < g.cell)
            .map(|(i, _)| i)
            .collect();
        let (cx, cy) = cell_coords(probe[0], probe[1], &g);
        let mut found = Vec::new();
        grid.for_each_neighbor(cx, cy, &g, |i| {
            if dist(probe, pos[i]) < g.cell {
                found.push(i);
            }
        });
        found.sort();
        assert_eq!(found, expected, "Sonde {probe:?}");
    }
}
