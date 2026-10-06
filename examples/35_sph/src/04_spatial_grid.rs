//! Uniform-Grid-Spatial-Hashing: Zell-Hash + CPU-Counting-Sort.
//!
//! `hash_cell` ist gerätekompatibel (läuft auch im GPU-Kernel), `CpuGrid`
//! ist die CPU-Referenz für Tests und CPU-Backend: Zählen → exklusiver
//! Präfix-Sum → stabiler Scatter, also O(N + Zellen).

use crate::types::GridMeta;

/// Zellindex für (x, y), auf das Grid geklemmt (kein Panikpfad).
pub fn hash_cell(x: f32, y: f32, inv_cell: f32, grid_w: u32, grid_h: u32) -> u32 {
    let cx = (x * inv_cell).floor() as i32;
    let cy = (y * inv_cell).floor() as i32;
    let cx = cx.clamp(0, grid_w as i32 - 1) as u32;
    let cy = cy.clamp(0, grid_h as i32 - 1) as u32;
    cy * grid_w + cx
}

/// Zellkoordinaten für (x, y), auf das Grid geklemmt.
pub fn cell_coords(x: f32, y: f32, grid: &GridMeta) -> (u32, u32) {
    let h = hash_cell(x, y, grid.inv_cell, grid.w, grid.h);
    (h % grid.w, h / grid.w)
}

/// CPU-Uniform-Grid mit sortierter Indexliste (Counting-Sort).
#[derive(Clone, Debug, Default)]
pub struct CpuGrid {
    /// Exklusiver Präfix-Sum der Zellbelegungen, Länge Zellen+1.
    pub cell_start: Vec<u32>,
    /// Partikelindizes, nach Zellen sortiert, Länge N.
    pub order: Vec<u32>,
    /// Arbeitszähler, Länge Zellen.
    counts: Vec<u32>,
}

impl CpuGrid {
    /// Leeres Grid für `num_cells` Zellen und `num_particles` Partikel.
    pub fn new(num_cells: usize, num_particles: usize) -> Self {
        Self {
            cell_start: vec![0; num_cells + 1],
            order: vec![0; num_particles],
            counts: vec![0; num_cells],
        }
    }

    /// Baut Zählung, Präfix-Sum und sortierte Ordnung aus Positionen neu auf.
    pub fn rebuild(&mut self, positions: &[[f32; 2]], grid: &GridMeta) {
        assert_eq!(self.order.len(), positions.len());
        assert_eq!(self.counts.len(), grid.num_cells());
        self.counts.fill(0);
        // 1. Zählen.
        for p in positions {
            let h = hash_cell(p[0], p[1], grid.inv_cell, grid.w, grid.h) as usize;
            self.counts[h] += 1;
        }
        // 2. Exklusiver Präfix-Sum → Zellstarts.
        let mut sum = 0u32;
        for (c, start) in self
            .counts
            .iter()
            .zip(self.cell_start.iter_mut())
            .take(self.counts.len())
        {
            *start = sum;
            sum += c;
        }
        self.cell_start[self.counts.len()] = sum;
        // 3. Stabiler Scatter über laufende Cursor.
        let mut cursor = self.cell_start.clone();
        for (i, p) in positions.iter().enumerate() {
            let h = hash_cell(p[0], p[1], grid.inv_cell, grid.w, grid.h) as usize;
            let slot = cursor[h] as usize;
            self.order[slot] = i as u32;
            cursor[h] += 1;
        }
    }

    /// Ruft `f` für jeden Partikelindex in Zelle `cell` auf.
    pub fn for_each_in_cell(&self, cell: usize, mut f: impl FnMut(usize)) {
        let begin = self.cell_start[cell] as usize;
        let end = self.cell_start[cell + 1] as usize;
        for &o in &self.order[begin..end] {
            f(o as usize);
        }
    }

    /// Ruft `f` für jeden Partikel in der 3×3-Nachbarschaft von (cx, cy) auf.
    pub fn for_each_neighbor(&self, cx: u32, cy: u32, grid: &GridMeta, mut f: impl FnMut(usize)) {
        let x0 = cx.saturating_sub(1);
        let y0 = cy.saturating_sub(1);
        let x1 = (cx + 1).min(grid.w - 1);
        let y1 = (cy + 1).min(grid.h - 1);
        for y in y0..=y1 {
            for x in x0..=x1 {
                let cell = (y * grid.w + x) as usize;
                self.for_each_in_cell(cell, &mut f);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_klemmt_raender_und_ecken() {
        let g = GridMeta::new(1.6, 1.0, 0.04);
        assert_eq!(hash_cell(0.0, 0.0, g.inv_cell, g.w, g.h), 0);
        assert_eq!(hash_cell(0.039, 0.039, g.inv_cell, g.w, g.h), 0);
        assert_eq!(hash_cell(0.04, 0.0, g.inv_cell, g.w, g.h), 1);
        assert_eq!(hash_cell(1.59, 0.99, g.inv_cell, g.w, g.h), 999);
        // Außerhalb → geklemmt, nie Panik.
        assert_eq!(hash_cell(-5.0, -5.0, g.inv_cell, g.w, g.h), 0);
        assert_eq!(hash_cell(99.0, 99.0, g.inv_cell, g.w, g.h), 999);
    }

    #[test]
    fn rebuild_sortiert_jeden_index_genau_einmal() {
        let g = GridMeta::new(1.6, 1.0, 0.04);
        let pos: Vec<[f32; 2]> = (0..1000)
            .map(|i| {
                let x = (i as f32 * 0.0013) % 1.59;
                let y = (i as f32 * 0.0007) % 0.99;
                [x, y]
            })
            .collect();
        let mut grid = CpuGrid::new(g.num_cells(), pos.len());
        grid.rebuild(&pos, &g);
        let mut seen = vec![false; pos.len()];
        for &idx in &grid.order {
            assert!(!seen[idx as usize], "Index doppelt: {idx}");
            seen[idx as usize] = true;
        }
        assert!(seen.iter().all(|&s| s));
        assert_eq!(*grid.cell_start.last().unwrap(), pos.len() as u32);
    }

    #[test]
    fn nachbarschaft_findet_alle_partikel_im_support() {
        let g = GridMeta::new(1.6, 1.0, 0.04);
        // 5×5-Gitter mit Abstand h/2 um die Mitte.
        let mut pos = Vec::new();
        for iy in 0..5 {
            for ix in 0..5 {
                pos.push([
                    0.8 + (ix as f32 - 2.0) * 0.02,
                    0.5 + (iy as f32 - 2.0) * 0.02,
                ]);
            }
        }
        let mut grid = CpuGrid::new(g.num_cells(), pos.len());
        grid.rebuild(&pos, &g);
        // Brute-Force-Referenz: alle mit Abstand < h zum Zentrum.
        let center = [0.8, 0.5];
        let expected: Vec<usize> = pos
            .iter()
            .enumerate()
            .filter(|(_, p)| crate::sph_math::dist(center, **p) < g.cell)
            .map(|(i, _)| i)
            .collect();
        let (cx, cy) = cell_coords(center[0], center[1], &g);
        let mut found = Vec::new();
        grid.for_each_neighbor(cx, cy, &g, |i| {
            if crate::sph_math::dist(center, pos[i]) < g.cell {
                found.push(i);
            }
        });
        found.sort();
        assert_eq!(found, expected);
        assert!(!expected.is_empty());
    }
}
