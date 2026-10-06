//! Stabilitätstest: 500 CPU-Schritte ohne Tunneln/Explosion.
//!
//! Nutzt dieselbe Validierung wie der Headless-Runner (kein NaN/Inf, alle
//! Partikel in der Domäne, Dichte in (0, 5ρ₀]).

use sph::backend::{Backend, CpuBackend};
use sph::headless::validate;
use sph::params::SimConfig;
use sph::types::InteractParams;

#[test]
fn cpu_500_schritte_bleiben_stabil() {
    let cfg = SimConfig {
        particles: 2048,
        ..SimConfig::default()
    };
    let mut backend = CpuBackend::new(&cfg);
    backend.reset(&cfg.dam_break());
    backend.set_interact(InteractParams::neutral(cfg.domain_w, cfg.domain_h));
    for _ in 0..500 {
        backend.step();
    }
    backend.sync_host();
    let problems = validate(&backend, &cfg);
    assert!(problems.is_empty(), "Probleme: {problems:?}");
}

#[test]
fn cpu_reset_ist_reproduzierbar() {
    let cfg = SimConfig {
        particles: 2048,
        ..SimConfig::default()
    };
    let init = cfg.dam_break();
    let mut a = CpuBackend::new(&cfg);
    let mut b = CpuBackend::new(&cfg);
    a.reset(&init);
    b.reset(&init);
    for _ in 0..50 {
        a.step();
        b.step();
    }
    a.sync_host();
    b.sync_host();
    assert_eq!(a.positions(), b.positions());
    assert_eq!(a.velocities(), b.velocities());
    assert_eq!(a.densities(), b.densities());
}
