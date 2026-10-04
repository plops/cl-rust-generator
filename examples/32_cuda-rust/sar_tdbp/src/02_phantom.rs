//! Phantome: diskrete Punktstreuer als Ground Truth.
//!
//! Drei Szenarien: einzelner Punkt (PSF-Nachweis), 5×5-Gitter (Geometrie)
//! und der Schriftzug „RUST“ aus Einzelpunkten (Anschauung).

use crate::types::SceneGeometry;

/// Wählbares Streuer-Szenario (CLI: `--phantom`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum PhantomKind {
    Single,
    #[default]
    Grid,
    Rust,
}

/// Baut das gewählte Phantom für `geo`.
pub fn build(kind: PhantomKind, geo: SceneGeometry) -> Vec<PointTarget> {
    match kind {
        PhantomKind::Single => single_point(geo),
        PhantomKind::Grid => grid_5x5(geo),
        PhantomKind::Rust => rust_text(geo),
    }
}

/// Punktstreuer im Boden-Koordinatensystem (Meter).
#[derive(Clone, Copy, Debug)]
pub struct PointTarget {
    /// x-Position in Metern.
    pub x: f32,
    /// y-Position in Metern.
    pub y: f32,
    /// z-Position in Metern (Boden: 0).
    pub z: f32,
    /// Radarquerschnitt / Amplitude.
    pub sigma: f32,
}

/// Ein einzelner Streuer exakt in der Szenenmitte — zum Vermessen der
/// Punktspreizfunktion (PSF).
pub fn single_point(geo: SceneGeometry) -> Vec<PointTarget> {
    vec![PointTarget {
        x: geo.x0 + geo.width as f32 * geo.dx / 2.0,
        y: geo.y0 + geo.height as f32 * geo.dy / 2.0,
        z: 0.0,
        sigma: 1.0,
    }]
}

/// 5×5-Gitter isolierter Punktstreuer über die mittleren 50 % der Szene —
/// zum Prüfen von PSF und geometrischen Verzerrungen.
pub fn grid_5x5(geo: SceneGeometry) -> Vec<PointTarget> {
    let mut out = Vec::with_capacity(25);
    let cx = geo.x0 + geo.width as f32 * geo.dx / 2.0;
    let cy = geo.y0 + geo.height as f32 * geo.dy / 2.0;
    let span_x = geo.width as f32 * geo.dx * 0.5;
    let span_y = geo.height as f32 * geo.dy * 0.5;
    for iy in 0..5 {
        for ix in 0..5 {
            out.push(PointTarget {
                x: cx - span_x / 2.0 + span_x * ix as f32 / 4.0,
                y: cy - span_y / 2.0 + span_y * iy as f32 / 4.0,
                z: 0.0,
                sigma: 1.0,
            });
        }
    }
    out
}

/// 5×7-Pixel-Font für „RUST“, Zeile 0 = oben. 1 = Streuer.
const GLYPH_R: [[u8; 5]; 7] = [
    [0, 1, 1, 1, 0],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [1, 1, 1, 1, 0],
    [1, 0, 1, 0, 0],
    [1, 0, 0, 1, 0],
    [1, 0, 0, 0, 1],
];
const GLYPH_U: [[u8; 5]; 7] = [
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [0, 1, 1, 1, 0],
];
const GLYPH_S: [[u8; 5]; 7] = [
    [0, 1, 1, 1, 1],
    [1, 0, 0, 0, 0],
    [1, 0, 0, 0, 0],
    [0, 1, 1, 1, 0],
    [0, 0, 0, 0, 1],
    [0, 0, 0, 0, 1],
    [1, 1, 1, 1, 0],
];
const GLYPH_T: [[u8; 5]; 7] = [
    [1, 1, 1, 1, 1],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
];

/// Schriftzug „RUST“ aus Einzelpunkten, zentriert, über ~60 % der
/// Szenenbreite. y wächst nach oben (Zeile 0 = oben).
pub fn rust_text(geo: SceneGeometry) -> Vec<PointTarget> {
    const GLYPHS: [[[u8; 5]; 7]; 4] = [GLYPH_R, GLYPH_U, GLYPH_S, GLYPH_T];
    const COLS: f32 = 4.0 * 5.0 + 3.0; // 4 Glyphen + 3 Spalten Abstand
    const ROWS: f32 = 7.0;
    let step_x = geo.width as f32 * geo.dx * 0.6 / COLS;
    let step = step_x.min(geo.height as f32 * geo.dy * 0.6 / ROWS);
    let cx = geo.x0 + geo.width as f32 * geo.dx / 2.0;
    let cy = geo.y0 + geo.height as f32 * geo.dy / 2.0;
    let mut out = Vec::new();
    for (gi, glyph) in GLYPHS.iter().enumerate() {
        for (row, line) in glyph.iter().enumerate() {
            for (col, &bit) in line.iter().enumerate() {
                if bit == 0 {
                    continue;
                }
                let gx = gi as f32 * 6.0 + col as f32;
                out.push(PointTarget {
                    x: cx + (gx - (COLS - 1.0) / 2.0) * step,
                    y: cy + ((ROWS - 1.0) / 2.0 - row as f32) * step,
                    z: 0.0,
                    sigma: 1.0,
                });
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::SceneGeometry;

    fn geo() -> SceneGeometry {
        SceneGeometry::default_scene(256, 256, 256)
    }

    fn in_scene(g: SceneGeometry, p: &PointTarget) -> bool {
        p.x >= g.x0
            && p.x <= g.x0 + g.width as f32 * g.dx
            && p.y >= g.y0
            && p.y <= g.y0 + g.height as f32 * g.dy
    }

    #[test]
    fn single_point_zentriert() {
        let pts = single_point(geo());
        assert_eq!(pts.len(), 1);
        assert!(pts[0].x.abs() < 1e-4 && (pts[0].y - 60.0).abs() < 1e-4);
        assert_eq!(pts[0].sigma, 1.0);
    }

    #[test]
    fn grid_zaehlung_und_lage() {
        let g = geo();
        let pts = grid_5x5(g);
        assert_eq!(pts.len(), 25);
        assert!(pts.iter().all(|p| in_scene(g, p)));
        // Mitte des Gitters = Szenenmitte.
        let mid = &pts[12];
        assert!(mid.x.abs() < 1e-3 && (mid.y - 60.0).abs() < 1e-3);
        // Ecken bei ±10 m (50 % von 40 m).
        assert!((pts[0].x + 10.0).abs() < 1e-3);
        assert!((pts[24].x - 10.0).abs() < 1e-3);
    }

    #[test]
    fn rust_text_zaehlung_und_lage() {
        let g = geo();
        let pts = rust_text(g);
        // R:17 + U:15 + S:15 + T:11 = 58.
        assert_eq!(pts.len(), 58);
        assert!(pts.iter().all(|p| in_scene(g, p)));
        // Zentriert: Bounding-Box-Mitte ≈ Szenenmitte (der Massenmittelpunkt
        // liegt bauartbedingt ~1 m links, da „R“ mehr Pixel hat als „T“).
        let (mut x0, mut x1, mut y0, mut y1) = (f32::MAX, f32::MIN, f32::MAX, f32::MIN);
        for p in &pts {
            x0 = x0.min(p.x);
            x1 = x1.max(p.x);
            y0 = y0.min(p.y);
            y1 = y1.max(p.y);
        }
        assert!(((x0 + x1) / 2.0).abs() < 1e-3);
        assert!(((y0 + y1) / 2.0 - 60.0).abs() < 1e-3);
    }
}
