//! Treemap-Disk-Visualisierer (wgpu + winit, Vulkan-only).
//!
//! Nur Verdrahtung: Modul-Deklarationen, CLI-Einstiegspunkt, Exit-Codes.

use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

#[path = "04_color.rs"]
mod color;
#[path = "03_layout.rs"]
mod layout;
#[path = "02_scan.rs"]
mod scan;
#[path = "01_types.rs"]
mod types;

use types::{Rect, format_bytes};

/// Größenklassen für die Scan-Zusammenfassung (Phase 1: Headless-Überblick).
const SUMMARY_TOP_N: usize = 10;
const SUMMARY_CANVAS_W: f32 = 1920.0;
const SUMMARY_CANVAS_H: f32 = 1044.0;

fn main() -> ExitCode {
    let raw = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));
    let target = fs::canonicalize(&raw).unwrap_or(raw);
    if !target.is_dir() {
        eprintln!("error: not a directory: {}", target.display());
        return ExitCode::from(1);
    }
    let mut root = scan::scan_tree(&target);
    let canvas = Rect::new(0.0, 0.0, SUMMARY_CANVAS_W, SUMMARY_CANVAS_H);
    root.rect = canvas;
    layout::squarify(&mut root.children, canvas);
    println!(
        "{}: {} in {} nodes",
        target.display(),
        format_bytes(root.size),
        scan::count_nodes(&root)
    );
    for child in root.children.iter().take(SUMMARY_TOP_N) {
        println!(
            "  {:>10}  {:>4.0}x{:<4.0}  #{:02X}{:02X}{:02X}  {}",
            format_bytes(child.size),
            child.rect.w,
            child.rect.h,
            (child.color.r * 255.0) as u8,
            (child.color.g * 255.0) as u8,
            (child.color.b * 255.0) as u8,
            child.path.display()
        );
    }
    // Center-Probe: Vorwegnahme des Hover-Pickings (tiefster Treffer).
    match layout::pick(
        &root.children,
        SUMMARY_CANVAS_W / 2.0,
        SUMMARY_CANVAS_H / 2.0,
    ) {
        Some(hit) => println!(
            "center: {} ({})",
            hit.path.display(),
            format_bytes(hit.size)
        ),
        None => println!("center: (no hit)"),
    }
    ExitCode::SUCCESS
}
