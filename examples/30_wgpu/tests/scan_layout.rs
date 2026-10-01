//! Integrationstest: Scan → Layout → Picking auf einem Fixture-Baum.

use std::fs;
use std::path::PathBuf;

use treemap::layout::{pick, squarify};
use treemap::scan::scan_tree;
use treemap::types::{Rect, format_bytes};

fn fixture_tree() -> PathBuf {
    let root = std::env::temp_dir().join(format!("treemap-itest-{}", std::process::id()));
    let _ = fs::remove_dir_all(&root);
    fs::create_dir_all(root.join("docs")).unwrap();
    fs::write(root.join("docs").join("a.md"), vec![b'a'; 400]).unwrap();
    fs::write(root.join("docs").join("b.md"), vec![b'b'; 100]).unwrap();
    fs::write(root.join("video.mkv"), vec![b'v'; 500]).unwrap();
    root
}

#[test]
fn scan_layout_pick_pipeline() {
    let root = fixture_tree();
    let mut tree = scan_tree(&root);
    assert_eq!(tree.size, 1000);
    assert_eq!(format_bytes(tree.size), "1000.0 B");

    let canvas = Rect::new(0.0, 36.0, 1000.0, 600.0);
    tree.rect = canvas;
    squarify(&mut tree.children, canvas);

    // Flächen-Erhaltung auf Top-Level.
    let area: f32 = tree.children.iter().map(|n| n.rect.area()).sum();
    assert!((area - canvas.area()).abs() / canvas.area() < 0.01);

    // Picking: Canvas-Mitte trifft, daneben nicht.
    let hit = pick(&tree.children, 500.0, 336.0).expect("center must hit");
    assert!(hit.size == 500 || hit.size == 400 || hit.size == 100);
    assert!(pick(&tree.children, 500.0, 10.0).is_none());
    assert!(pick(&tree.children, -5.0, 400.0).is_none());

    fs::remove_dir_all(&root).unwrap();
}
