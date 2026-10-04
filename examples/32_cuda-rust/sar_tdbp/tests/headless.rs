//! Headless- und Fokus-Tests (GPU).
//!
//! Laufen unter `cargo oxide test`. Der Gitter-Test vermisst die
//! End-zu-End-Fokussierung: Energie­konzentration an den Streuern.

use sar_tdbp::kernel::peak_power;
use sar_tdbp::phantom::{PhantomKind, PointTarget, build as build_phantom};
use sar_tdbp::pipeline::{HeadlessJob, SarPipeline, run_headless};
use sar_tdbp::simulator::simulate;
use sar_tdbp::types::{Complex32, RadarParams, SceneGeometry};

/// Anteil der Bildleistung im Radius `rad_px` (Quadrat) um einen Streuer.
fn focus_fraction(
    img: &[Complex32],
    geo: SceneGeometry,
    targets: &[PointTarget],
    rad_px: i32,
) -> f32 {
    let w = geo.width as i32;
    let h = geo.height as i32;
    let total: f32 = img.iter().map(|c| c.norm_sqr()).sum();
    let mut near = 0.0f32;
    for y in 0..h {
        for x in 0..w {
            let px = geo.x0 + (x as f32 + 0.5) * geo.dx;
            let py = geo.y0 + (y as f32 + 0.5) * geo.dy;
            let close = targets.iter().any(|t| {
                ((t.x - px) / geo.dx).abs() <= rad_px as f32 + 0.5
                    && ((t.y - py) / geo.dy).abs() <= rad_px as f32 + 0.5
            });
            if close {
                near += img[(y * w + x) as usize].norm_sqr();
            }
        }
    }
    near / total
}

fn gpu_image(kind: PhantomKind, size: u32, pulses: u32) -> (SceneGeometry, Vec<Complex32>) {
    let geo = SceneGeometry::default_scene(size, size, pulses);
    let radar = RadarParams::x_band();
    let targets = build_phantom(kind, geo);
    let raw = simulate(geo, radar, &targets);
    let mut pipe = SarPipeline::new(geo, radar, &raw).expect("Pipeline auf GPU");
    let img = pipe.run(u32::MAX).expect("Kernel-Lauf");
    (geo, img)
}

#[test]
fn headless_png_und_report() {
    let out = std::env::temp_dir().join(format!("sar_headless_{}.png", std::process::id()));
    let job = HeadlessJob {
        phantom: PhantomKind::Single,
        size: 33,
        num_pulses: 16,
        limit: None,
        output: out.clone(),
    };
    let rep = run_headless(&job).expect("Headless-Lauf");
    assert_eq!((rep.width, rep.height), (33, 33));
    assert_eq!(rep.pulses_used, 16);
    // Single-Point exakt auf Pixelmitte (16, 16); kohärent ≈ 16.
    assert_eq!((rep.peak_px, rep.peak_py), (16, 16));
    assert!((rep.peak_mag - 16.0).abs() < 1.0, "Peak {}", rep.peak_mag);
    assert!(rep.ascii.contains('@'));
    let meta = std::fs::metadata(&out).expect("PNG existiert");
    assert!(meta.len() > 500, "PNG zu klein: {} B", meta.len());
    std::fs::remove_file(&out).ok();
}

#[test]
fn gitter_fokussiert() {
    let (geo, img) = gpu_image(PhantomKind::Grid, 65, 512);
    let targets = build_phantom(PhantomKind::Grid, geo);
    let frac = focus_fraction(&img, geo, &targets, 2);
    eprintln!("Gitter-Fokus (512 Pulse): {frac:.3}");
    assert!(frac > 0.5, "Energieanteil {frac}");
    // CPU-Gegenprobe auf kleiner Rechnung aus gpu_tdbp ist abgedeckt;
    // hier nur GPU-Konsistenz: Peak muss an einem Streuer liegen.
    let (peak, _) = peak_power(&img);
    let px = geo.x0 + ((peak as u32 % geo.width) as f32 + 0.5) * geo.dx;
    let py = geo.y0 + ((peak as u32 / geo.width) as f32 + 0.5) * geo.dy;
    let at_target = targets
        .iter()
        .any(|t| (t.x - px).abs() < geo.dx && (t.y - py).abs() < geo.dy);
    assert!(at_target, "Peak bei ({px}, {py}) abseits der Streuer");
}

/// Streuer-Pixel-Maske: je Target das nächste Pixel.
fn glyph_mask(geo: SceneGeometry, targets: &[PointTarget]) -> Vec<bool> {
    let w = geo.width as i32;
    let mut m = vec![false; (geo.width * geo.height) as usize];
    for t in targets {
        let x = ((t.x - geo.x0) / geo.dx - 0.5).round() as i32;
        let y = ((t.y - geo.y0) / geo.dy - 0.5).round() as i32;
        if x >= 0 && x < w && y >= 0 && y < geo.height as i32 {
            m[(y * w + x) as usize] = true;
        }
    }
    m
}

/// Mittlere Leistung auf vs. abseits der Maske.
fn contrast(img: &[Complex32], mask: &[bool]) -> f32 {
    let (mut s_on, mut n_on, mut s_off, mut n_off) = (0.0, 0, 0.0, 0);
    for (c, &on) in img.iter().zip(mask.iter()) {
        if on {
            s_on += c.norm_sqr();
            n_on += 1;
        } else {
            s_off += c.norm_sqr();
            n_off += 1;
        }
    }
    (s_on / n_on as f32) / (s_off / n_off as f32)
}

#[test]
fn rust_schriftzug_fokussiert() {
    // 1024 Pulse: Gitterkeulen liegen außerhalb der Szene (bei 512 wären
    // sie sichtbar — Abtasttheorem, siehe walkthrough.md).
    let (geo, img) = gpu_image(PhantomKind::Rust, 65, 1024);
    let targets = build_phantom(PhantomKind::Rust, geo);
    let frac = focus_fraction(&img, geo, &targets, 1);
    assert!(frac > 0.6, "RUST-Energieanteil {frac}");
    let mask = glyph_mask(geo, &targets);
    assert_eq!(mask.iter().filter(|&&b| b).count(), 58);
    let con = contrast(&img, &mask);
    assert!(con > 20.0, "RUST-Kontrast {con}");
}
