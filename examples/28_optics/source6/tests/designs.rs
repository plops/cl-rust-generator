//! Benchmark designs from `integration_tests.md`: landscape, Cooke triplet,
//! double Gauss, plus distance/material/wavelength optimization.
//!
//! Provenance: assets transcribe the doc's prescriptions literally. The
//! doc's nominal values (EFL 100, listed back focuses) do not match its own
//! prescriptions under exact tracing; the tracer is verified independently
//! (Gullstrand unit test + hand-checked landscape BFL 107.34 from the last
//! vertex). Paraxial goldens below pin measured values as regressions.

use optics::*;
use std::fs;

fn load(name: &str) -> OpticalSetup {
    let text = fs::read_to_string(format!("assets/{name}")).expect("asset");
    load_toml(&text).expect("parse")
}

fn axial_ray_at(x: f64, y: f64, z: f64) -> Ray {
    Ray {
        origin: Point3::constant(x, y, z),
        direction: Vec3::constant(0.0, 0.0, 1.0),
    }
}

#[test]
fn landscape_stop_transfer_and_focus() {
    let setup = load("landscape.toml");
    assert_eq!(setup.surfaces.len(), 3);
    assert!(setup.surfaces[0].stop);
    assert!((pupil_radius(&setup.source) - 5.0).abs() < 1e-12);

    // Hand-checked meniscus: EFL 102.86, BFL 107.34 from last vertex.
    let f = efl(&setup).expect("EFL");
    assert!((f - 102.8639).abs() < 0.01, "EFL = {f}");
    let b = back_focal_z(&setup).expect("BFL");
    assert!((b - 19.5 - 107.34).abs() < 0.05, "BFL z = {b}");

    // Doc case: parallel ray at Y = 5 must arrive. The transcribed image
    // plane sits 14.8 mm inside focus, so the height is small but nonzero
    // (measured golden, pinned as regression).
    let (surfs, image) = layout_for(&setup.surfaces, 0.5876);
    let p = trace_ray(&surfs, &axial_ray_at(0.0, 5.0, -10.0), image, 5.0, 0.5876);
    assert_eq!(p.end, RayEnd::Image);
    let img = p.image().unwrap();
    assert!((img.y.v - 0.426891).abs() < 1e-4, "image y = {}", img.y.v);

    // Beyond the stop aperture the ray is blocked, not silently kept.
    let p = trace_ray(&surfs, &axial_ray_at(0.0, 6.0, -10.0), image, 5.0, 0.5876);
    assert_eq!(p.end, RayEnd::Vignetted);
}

#[test]
fn cooke_triplet_paraxial_and_stability() {
    let setup = load("cooke.toml");
    assert_eq!(setup.surfaces.len(), 6);
    // Omitted materials default to air (patent-table style).
    assert!((setup.surfaces[1].material - 1.0).abs() < 1e-12);
    assert!((setup.surfaces[3].material - 1.0).abs() < 1e-12);
    assert_eq!(setup.source.wavelengths.len(), 3);

    let f = efl(&setup).expect("EFL");
    assert!((f - 89.1073).abs() < 0.01, "EFL = {f}");
    let b = back_focal_z(&setup).expect("BFL");
    assert!((b - 104.1349).abs() < 0.01, "BFL z = {b}");

    let paths = trace_system(&setup.surfaces, &setup);
    assert_eq!(paths.len(), 30);
    let img = paths.iter().filter(|p| p.end == RayEnd::Image).count();
    let vig = paths.iter().filter(|p| p.end == RayEnd::Vignetted).count();
    let lost = paths
        .iter()
        .filter(|p| matches!(p.end, RayEnd::Missed | RayEnd::Tir))
        .count();
    // 4x4 grid over +-10 at pupil r = 10: 2 corner rays per
    // wavelength vignette at the L2 stop, the rest arrive.
    assert_eq!(img, 24);
    assert_eq!(vig, 6);
    assert_eq!(lost, 0);
    assert!(loss_for(&setup).is_finite());
}

#[test]
fn double_gauss_stress() {
    let setup = load("double_gauss.toml");
    assert_eq!(setup.surfaces.len(), 11);
    // Strong curves are really present (26.1, -28.2).
    let min_r = setup
        .surfaces
        .iter()
        .map(|s| s.radius.abs())
        .fold(f64::INFINITY, f64::min);
    assert!(min_r < 30.0);

    let f = efl(&setup).expect("EFL");
    assert!((f - 111.3997).abs() < 0.05, "EFL = {f}");
    let b = back_focal_z(&setup).expect("BFL");
    assert!((b - 145.7156).abs() < 0.05, "BFL z = {b}");

    let paths = trace_system(&setup.surfaces, &setup);
    for p in &paths {
        for pt in &p.points {
            let v = pt.values();
            assert!(v.iter().all(|c| c.is_finite()), "NaN in path");
        }
    }
    let ends: Vec<RayEnd> = paths.iter().map(|p| p.end).collect();
    assert_eq!(
        ends,
        vec![
            RayEnd::Vignetted,
            RayEnd::Image,
            RayEnd::Vignetted,
            RayEnd::Image,
            RayEnd::Image
        ]
    );
    assert!(loss_for(&setup).is_finite());
}

#[test]
fn thickness_optimization_lowers_loss() {
    // Changing distances: refocus by optimizing the air gap.
    let mut setup = load("sample.toml");
    setup.surfaces[0].optimize.clear();
    setup.surfaces[1].optimize = vec!["thickness".to_string()];
    let vars = variables(&setup).expect("vars");
    assert_eq!(vars.len(), 1);
    let analytic = gradient(&setup, &vars)[0];
    let e = 1e-7;
    let mut plus = setup.clone();
    let mut minus = setup.clone();
    let base = get_var(&setup.surfaces, vars[0]);
    set_var(&mut plus.surfaces, vars[0], base + e);
    set_var(&mut minus.surfaces, vars[0], base - e);
    let numeric = (loss_for(&plus) - loss_for(&minus)) / (2.0 * e);
    assert!(
        (analytic - numeric).abs() < 1e-4 * numeric.abs().max(1.0),
        "analytic = {analytic}, numeric = {numeric}"
    );
    let (_opt, h) = descend(&setup).expect("descent");
    assert!(h.last().unwrap() < h.first().unwrap());
}

#[test]
fn material_optimization_lowers_loss() {
    // Changing materials: re-optimize the front index for the spot.
    let mut setup = load("sample.toml");
    setup.surfaces[0].optimize = vec!["material".to_string()];
    let (opt, h) = descend(&setup).expect("descent");
    assert!(h.last().unwrap() < h.first().unwrap());
    let n = opt.surfaces[0].material;
    assert!(n.is_finite() && n > 1.0, "index = {n}");
}

#[test]
fn combined_radius_thickness_material_descent() {
    let mut setup = load("sample.toml");
    setup.surfaces[0].optimize = vec!["radius".to_string(), "material".to_string()];
    setup.surfaces[1].optimize = vec!["thickness".to_string()];
    let vars = variables(&setup).expect("vars");
    assert_eq!(vars.len(), 3);
    let (_opt, h) = descend(&setup).expect("descent");
    assert!(h.last().unwrap() < h.first().unwrap());
}

#[test]
fn dispersion_shifts_off_axis_focus() {
    // Wavelengths: BK7-like Cauchy glass separates F and C laterally.
    let mut setup = load("sample.toml");
    setup.surfaces[0].cauchy_b = 0.0042;
    let mut heights = Vec::new();
    for lam in [0.4861, 0.6563] {
        let (surfs, image) = layout_for(&setup.surfaces, lam);
        let p = trace_ray(&surfs, &axial_ray_at(0.0, 5.0, -10.0), image, 5.0, lam);
        heights.push(p.image().unwrap().y.v);
    }
    // Measured lateral color is ~0.05; assert a clear fraction of it.
    assert!(
        (heights[0] - heights[1]).abs() > 0.01,
        "heights = {heights:?}"
    );
}
