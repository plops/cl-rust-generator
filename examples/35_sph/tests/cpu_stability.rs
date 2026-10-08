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
fn unterdruck_kohäsion_zieht_isoliertes_paar_zusammen() {
    // Zwei Partikel im Abstand h/2, sonst leere Domäne (Cluster weit weg),
    // ohne Gravitation: Unterdichte → negativer Druck → Anziehung.
    // Mit rein abstoßendem EOS (P auf 0 geklemmt) wirkt keine Kraft und der
    // Abstand bleibt exakt konstant — dieser Test fällt dort.
    let cfg = SimConfig {
        particles: 2048,
        gravity: 0.0,
        ..SimConfig::default()
    };
    let h = cfg.h;
    let d0 = 0.5 * h;
    let mut init = Vec::with_capacity(cfg.particles);
    // Messpaar rechts, fern von Cluster, Wänden und Hindernis.
    init.push(sph::types::Particle::at_rest([1.4, 0.5], cfg.rest_density));
    init.push(sph::types::Particle::at_rest(
        [1.4 + d0, 0.5],
        cfg.rest_density,
    ));
    // Rest als kompaktes Cluster links (Abstand zum Paar >> h).
    for i in 2..cfg.particles {
        let k = i - 2;
        init.push(sph::types::Particle::at_rest(
            [
                0.02 + (k % 10) as f32 * 0.004,
                0.02 + (k / 10) as f32 * 0.004,
            ],
            cfg.rest_density,
        ));
    }
    let mut backend = CpuBackend::new(&cfg);
    backend.reset(&init);
    backend.set_interact(InteractParams::neutral(cfg.domain_w, cfg.domain_h));
    for _ in 0..3 {
        backend.step();
    }
    backend.sync_host();
    let pos = backend.positions();
    let dx = pos[0][0] - pos[1][0];
    let dy = pos[0][1] - pos[1][1];
    let dist = (dx * dx + dy * dy).sqrt();
    assert!(
        dist < 0.95 * d0,
        "Paarabstand schrumpft nicht: {dist} vs. {d0}"
    );
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
