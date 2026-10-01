//! Squarified-Treemap-Layout: teilt ein Rechteck flächenproportional auf.
//!
//! Port des macroquad-MVP: Kinder werden absteigend nach Größe sortiert,
//! dann zeilenweise so gruppiert, dass die Rechtecke möglichst quadratisch
//! bleiben (Bruls et al., „Squarified Treemaps“).

use std::cmp::Reverse;

use crate::types::{Node, Rect};

/// Mindestgröße für die Rekursion in Unterverzeichnisse (MVP-Regel).
const MIN_RECURSE_PX: f32 = 4.0;

/// Seitenverhältnis-Maß einer Zeile: Maximum aus größtem und Kehrwert des
/// kleinsten normierten Seitenverhältnisses (kleiner = quadratischer).
fn worst(areas: &[f64], row: &[usize], sum: f64, side: f32) -> f64 {
    let side_sq = f64::from(side) * f64::from(side);
    let sum_sq = sum * sum;
    row.iter()
        .map(|&i| {
            let scaled = side_sq * areas[i] / sum_sq;
            scaled.max(1.0 / scaled)
        })
        .fold(0.0, f64::max)
}

/// Schreibt die Rechtecke einer fertigen Zeile und schrumpft den Restbereich.
fn layout_row(nodes: &mut [Node], areas: &[f64], row: &[usize], sum: f64, rest: &mut Rect) {
    let side = rest.w.min(rest.h);
    let thickness = (sum / f64::from(side)) as f32;
    // Schmale Restfläche -> Zeile liegt horizontal (volle Breite, wenig Höhe).
    let horizontal = rest.w < rest.h;
    let mut offset = if horizontal { rest.x } else { rest.y };
    for &i in row {
        let len = (areas[i] / sum * f64::from(side)) as f32;
        nodes[i].rect = if horizontal {
            Rect::new(offset, rest.y, len, thickness)
        } else {
            Rect::new(rest.x, offset, thickness, len)
        };
        offset += len;
    }
    if horizontal {
        rest.y += thickness;
        rest.h -= thickness;
    } else {
        rest.x += thickness;
        rest.w -= thickness;
    }
}

/// Weist `nodes` Rechtecke innerhalb von `rect` zu (flächenproportional).
/// Leere Eingabe, Gesamtgröße 0 oder entartete Fläche sind No-Ops.
pub fn squarify(nodes: &mut [Node], mut rect: Rect) {
    let total: u64 = nodes.iter().map(|n| n.size).sum();
    if total == 0 || rect.w <= 0.0 || rect.h <= 0.0 {
        return;
    }
    nodes.sort_unstable_by_key(|n| Reverse(n.size));
    let area_per_byte = f64::from(rect.w * rect.h) / total as f64;
    let areas: Vec<f64> = nodes
        .iter()
        .map(|n| n.size as f64 * area_per_byte)
        .collect();

    let mut row: Vec<usize> = Vec::new();
    let mut row_sum = 0.0;
    for (i, &area) in areas.iter().enumerate() {
        let side = rect.w.min(rect.h);
        let mut candidate = row.clone();
        candidate.push(i);
        if row.is_empty()
            || worst(&areas, &candidate, row_sum + area, side) <= worst(&areas, &row, row_sum, side)
        {
            row.push(i);
            row_sum += area;
        } else {
            layout_row(nodes, &areas, &row, row_sum, &mut rect);
            row = vec![i];
            row_sum = area;
        }
    }
    if !row.is_empty() {
        layout_row(nodes, &areas, &row, row_sum, &mut rect);
    }
    for node in nodes.iter_mut() {
        if node.is_dir && node.rect.w > MIN_RECURSE_PX && node.rect.h > MIN_RECURSE_PX {
            squarify(&mut node.children, node.rect);
        }
    }
}

/// Tiefster Knoten unter dem Punkt `(x, y)`: unter allen Treffern gewinnt
/// das kleinste Rechteck, danach wird in dessen Kinder abgestiegen.
pub fn pick(nodes: &[Node], x: f32, y: f32) -> Option<&Node> {
    let hit = nodes
        .iter()
        .filter(|n| n.rect.contains(x, y))
        .min_by(|a, b| a.rect.area().total_cmp(&b.rect.area()))?;
    Some(pick(&hit.children, x, y).unwrap_or(hit))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn file(name: &str, size: u64) -> Node {
        Node {
            path: PathBuf::from(name),
            size,
            is_dir: false,
            children: Vec::new(),
            rect: Rect::default(),
            color: crate::types::Rgb::default(),
        }
    }

    fn total_rect_area(nodes: &[Node]) -> f32 {
        nodes.iter().map(|n| n.rect.area()).sum()
    }

    #[test]
    fn areas_sum_to_canvas() {
        let mut nodes = vec![
            file("a", 600),
            file("b", 300),
            file("c", 100),
            file("d", 50),
            file("e", 25),
        ];
        let canvas = Rect::new(0.0, 0.0, 800.0, 600.0);
        squarify(&mut nodes, canvas);
        let got = total_rect_area(&nodes);
        assert!(
            (got - canvas.area()).abs() / canvas.area() < 0.01,
            "area {got}"
        );
        // Größte Datei zuerst (Sortierung), alle Rechtecke im Canvas.
        assert_eq!(nodes[0].size, 600);
        for n in &nodes {
            assert!(n.rect.x >= 0.0 && n.rect.y >= 0.0);
            assert!(n.rect.x + n.rect.w <= canvas.w + 1.0);
            assert!(n.rect.y + n.rect.h <= canvas.h + 1.0);
        }
    }

    #[test]
    fn degenerate_inputs_are_noops() {
        let mut empty: Vec<Node> = Vec::new();
        squarify(&mut empty, Rect::new(0.0, 0.0, 100.0, 100.0));
        let mut zero = vec![file("z", 0)];
        squarify(&mut zero, Rect::new(0.0, 0.0, 100.0, 100.0));
        assert_eq!(zero[0].rect, Rect::default());
        let mut nodes = vec![file("a", 10)];
        squarify(&mut nodes, Rect::new(0.0, 0.0, 0.0, 100.0));
        assert_eq!(nodes[0].rect, Rect::default());
    }

    #[test]
    fn nested_dirs_recurse() {
        let mut inner = vec![file("x", 400), file("y", 100)];
        squarify(&mut inner, Rect::new(0.0, 0.0, 500.0, 100.0));
        assert!(inner.iter().all(|n| n.rect.area() > 0.0));
    }

    #[test]
    fn pick_finds_deepest_and_misses_outside() {
        let mut nodes = vec![file("a", 900), file("b", 100)];
        let canvas = Rect::new(0.0, 0.0, 100.0, 100.0);
        squarify(&mut nodes, canvas);
        // Canvas-Mitte trifft einen der beiden Knoten.
        let hit = pick(&nodes, 50.0, 50.0).expect("center must hit");
        assert!(hit.rect.contains(50.0, 50.0));
        // Außerhalb trifft nichts.
        assert!(pick(&nodes, -1.0, 50.0).is_none());
        assert!(pick(&nodes, 150.0, 150.0).is_none());
    }
}
