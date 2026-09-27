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
