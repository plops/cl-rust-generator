//! End-to-end pipeline on the prompt's sample system:
//! TOML -> trace -> optimize -> JSON export round-trip.

use optics::*;
use std::fs;

fn sample() -> OpticalSetup {
    let text = fs::read_to_string("assets/sample.toml").expect("asset");
    load_toml(&text).expect("parse")
}

#[test]
fn pipeline_trace_optimize_export() {
    let setup = sample();
    let paths = trace_system(&setup.surfaces, &setup);
    assert_eq!(paths.len(), 10);
    assert!(paths.iter().all(|p| p.end == RayEnd::Image));

    let (opt, history) = descend(&setup).expect("descent");
    assert!(history.last().unwrap() < history.first().unwrap());
    assert_ne!(opt.surfaces[0].radius, setup.surfaces[0].radius);

    let opt_paths = trace_system(&opt.surfaces, &opt);
    let text = to_json(&opt, &opt_paths);
    let back: SystemJson = serde_json::from_str(&text).expect("reparse");
    assert_eq!(back.version, 1);
    assert_eq!(back.surfaces.len(), 2);
    assert!(!back.segments.is_empty());

    let out = std::env::temp_dir().join("optics_system.json");
    fs::write(&out, &text).expect("write");
    let reread = fs::read_to_string(&out).expect("read");
    assert_eq!(reread, text);
}


/// The `sensitivity` CLI path re-exports `gradient`; here we exercise that
/// exact library entry on a multi-group, polychromatic system flagging all
/// three tolerance kinds (radius / thickness / material) and confirm every
/// component matches central finite differences of the scalar loss. This is
/// the same guarantee the Python tolerance driver relies on end-to-end.
#[test]
fn sensitivity_gradient_matches_finite_differences_all_kinds() {
    let setup = load_toml(
        "[source]\n\
         aperture_diameter = 12.0\n\
         wavelengths = [0.4861, 0.5876, 0.6563]\n\
         [[surfaces]]\n\
         name = \"A\"\nradius = 62.0\nthickness = 6.0\nmaterial = 1.617\n\
         cauchy_b = 0.0042\noptimize = [\"radius\", \"thickness\", \"material\"]\n\
         [[surfaces]]\n\
         name = \"B\"\nradius = -90.0\nthickness = 20.0\nmaterial = 1.62\n\
         cauchy_b = 0.0045\noptimize = [\"radius\", \"material\"]\n\
         [[surfaces]]\n\
         name = \"C\"\nradius = 48.0\nthickness = 40.0\nmaterial = 1.617\n\
         cauchy_b = 0.0042\noptimize = [\"thickness\"]\n",
    )
    .expect("multi-group config parses");

    let vars = variables(&setup).expect("vars");
    assert_eq!(vars.len(), 6);
    let analytic = gradient(&setup, &vars);

    for (i, v) in vars.iter().enumerate() {
        // Step scaled to the parameter kind (indices need a finer step).
        let e = match v.key {
            VarKey::Material => 1e-6,
            _ => 1e-4,
        };
        let base = get_var(&setup.surfaces, *v);
        let mut plus = setup.clone();
        let mut minus = setup.clone();
        set_var(&mut plus.surfaces, *v, base + e);
        set_var(&mut minus.surfaces, *v, base - e);
        let numeric = (loss_for(&plus) - loss_for(&minus)) / (2.0 * e);
        let tol = 1e-3 * numeric.abs().max(1.0);
        assert!(
            (analytic[i] - numeric).abs() < tol,
            "var {i} ({:?}): analytic = {}, numeric = {}",
            v.key,
            analytic[i],
            numeric
        );
    }
}
