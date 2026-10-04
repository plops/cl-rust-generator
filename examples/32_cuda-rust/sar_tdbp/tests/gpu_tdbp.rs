//! GPU-Integrationstests: Kernel gegen CPU-Referenz.
//!
//! Laufen unter `cargo oxide test` (baut zusätzlich den Device-Code).
//! Die Tests brauchen eine NVIDIA-GPU.

use sar_tdbp::kernel::{peak_power, tdbp_cpu};
use sar_tdbp::phantom::single_point;
use sar_tdbp::pipeline::SarPipeline;
use sar_tdbp::simulator::simulate;
use sar_tdbp::types::{RadarParams, SceneGeometry};

fn setup() -> (SceneGeometry, RadarParams) {
    // Ungerades Grid: Single-Point liegt exakt auf einer Pixelmitte,
    // damit Peak-Lage und -Höhe eindeutig vergleichbar sind.
    (
        SceneGeometry::default_scene(33, 33, 16),
        RadarParams::x_band(),
    )
}

#[test]
fn kernel_gleicht_cpu_referenz() {
    let (geo, radar) = setup();
    let targets = single_point(geo);
    let raw = simulate(geo, radar, &targets);
    let cpu = tdbp_cpu(&raw, geo, radar, u32::MAX);
    let mut pipe = SarPipeline::new(geo, radar, &raw).expect("Pipeline auf GPU");
    let gpu = pipe.run(u32::MAX).expect("Kernel-Lauf");
    assert_eq!(gpu.len(), cpu.len());
    let (_, peak) = peak_power(&cpu);
    assert!(peak > 0.0);
    let mut max_rel = 0.0f32;
    for (g, c) in gpu.iter().zip(cpu.iter()) {
        let d = (g.re - c.re).hypot(g.im - c.im) / peak.sqrt();
        max_rel = max_rel.max(d);
    }
    assert!(
        max_rel < 1e-3,
        "max. relative Abweichung {max_rel} (Schranke 1e-3)"
    );
    // Gleiche Peak-Lage.
    assert_eq!(peak_power(&gpu).0, peak_power(&cpu).0);
}

#[test]
fn puls_limit_waechst_monoton() {
    let (geo, radar) = setup();
    let targets = single_point(geo);
    let raw = simulate(geo, radar, &targets);
    let mut pipe = SarPipeline::new(geo, radar, &raw).expect("Pipeline auf GPU");
    let mut prev = 0.0f32;
    for limit in [1, 4, 8, 16] {
        let img = pipe.run(limit).expect("Kernel-Lauf");
        let (_, peak) = peak_power(&img);
        assert!(
            peak > prev,
            "Peak-Leistung muss mit Limit wachsen ({limit}: {peak} <= {prev})"
        );
        prev = peak;
    }
}
