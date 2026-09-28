//! Verifies the extracted Canon four-group zoom (US 5,146,366 A, Numerical
//! Example 1) parses and behaves as a zoom: EFL grows monotonically from the
//! wide to the tele config, tracking the patent's F = 1.00 / 2.50 / 5.70.
//!
//! The three configs share an identical spherical prescription and differ
//! only in the three variable air spaces D5 / D10 / D12 (patent zoom data).

use optics::*;
use std::fs;

fn load(name: &str) -> OpticalSetup {
    let text = fs::read_to_string(format!("assets/{name}")).expect("asset");
    load_toml(&text).expect("parse")
}

#[test]
fn three_configs_parse_with_expected_shape() {
    for name in [
        "zoom_us5146366_wide.toml",
        "zoom_us5146366_mid.toml",
        "zoom_us5146366_tele.toml",
    ] {
        let setup = load(name);
        // 27 surfaces: R1..R14 (14) + aperture R15 (1) + R16..R25 (10) +
        // cover-glass planes R26/R27 (2). The last surface's thickness is
        // the gap to the image plane.
        assert_eq!(setup.surfaces.len(), 27, "{name} surface count");
        // Exactly one aperture stop.
        assert_eq!(setup.surfaces.iter().filter(|s| s.stop).count(), 1);
    }
}

#[test]
fn variable_air_spaces_match_patent_zoom_data() {
    // D5 is surface index 4 (G1 L3 back), D10 is index 9 (G2 L6 back),
    // D12 is index 11 (G3 L7 back). Patent zoom table values per position.
    let wide = load("zoom_us5146366_wide.toml");
    let mid = load("zoom_us5146366_mid.toml");
    let tele = load("zoom_us5146366_tele.toml");

    assert!((wide.surfaces[4].thickness - 0.16).abs() < 1e-9);
    assert!((wide.surfaces[9].thickness - 2.03).abs() < 1e-9);
    assert!((wide.surfaces[11].thickness - 0.18).abs() < 1e-9);

    assert!((mid.surfaces[4].thickness - 1.32).abs() < 1e-9);
    assert!((mid.surfaces[9].thickness - 0.63).abs() < 1e-9);
    assert!((mid.surfaces[11].thickness - 0.42).abs() < 1e-9);

    assert!((tele.surfaces[4].thickness - 1.94).abs() < 1e-9);
    assert!((tele.surfaces[9].thickness - 0.29).abs() < 1e-9);
    assert!((tele.surfaces[11].thickness - 0.14).abs() < 1e-9);
}

#[test]
fn efl_increases_from_wide_to_tele() {
    let fw = efl(&load("zoom_us5146366_wide.toml")).expect("wide EFL");
    let fm = efl(&load("zoom_us5146366_mid.toml")).expect("mid EFL");
    let ft = efl(&load("zoom_us5146366_tele.toml")).expect("tele EFL");

    // Monotonic zoom: focal length grows wide -> mid -> tele.
    assert!(fm > fw, "mid {fm} should exceed wide {fw}");
    assert!(ft > fm, "tele {ft} should exceed mid {fm}");

    // Patent nominal F = 1.00 / 2.50 / 5.70 in the normalized patent scale.
    // After pupil-aiming and a solved back focus each position focuses, so the
    // paraxial EFL tracks the patent values closely (within a few percent).
    assert!((fw - 1.00).abs() < 0.05, "wide EFL {fw} ~ 1.00");
    assert!((fm - 2.50).abs() < 0.10, "mid EFL {fm} ~ 2.50");
    assert!((ft - 5.70).abs() < 0.20, "tele EFL {ft} ~ 5.70");

    // Full-range zoom ratio ~5.7x (well above the ~1.17x toy design).
    let ratio = ft / fw;
    assert!(ratio > 5.0, "zoom ratio {ratio} should be ~5.7x");
}

#[test]
fn design_is_parfocal_across_zoom() {
    // Parfocal: the image-plane focus (back-focal z of the marginal ray)
    // stays nearly constant across the three zoom positions, because each
    // position's back focus was solved to hold the image plane fixed.
    let bw = back_focal_z(&load("zoom_us5146366_wide.toml")).expect("wide bf");
    let bm = back_focal_z(&load("zoom_us5146366_mid.toml")).expect("mid bf");
    let bt = back_focal_z(&load("zoom_us5146366_tele.toml")).expect("tele bf");
    let spread = [bw, bm, bt]
        .iter()
        .fold(f64::MIN, |a, &b| a.max(b))
        - [bw, bm, bt].iter().fold(f64::MAX, |a, &b| a.min(b));
    // Back-focal positions agree to well under 0.1 patent units.
    assert!(spread < 0.1, "back focus spread {spread} (bw={bw} bm={bm} bt={bt})");
}

#[test]
fn all_field_and_pupil_rays_reach_image() {
    // With pupil-aiming, every (field, pupil, wavelength) ray traced for each
    // position reaches the image plane -- the wide-angle end no longer
    // vignettes almost entirely as it did without aiming (3/75 -> full).
    for name in [
        "zoom_us5146366_wide.toml",
        "zoom_us5146366_mid.toml",
        "zoom_us5146366_tele.toml",
    ] {
        let setup = load(name);
        let paths = trace_system(&setup.surfaces, &setup);
        let arrived = paths.iter().filter(|p| p.end == RayEnd::Image).count();
        assert!(
            arrived == paths.len(),
            "{name}: {arrived}/{} rays reached image",
            paths.len()
        );
        // Multiple field angles were actually traced.
        assert!(setup.source.field_angles_deg.len() >= 2, "{name} fields");
    }
}
