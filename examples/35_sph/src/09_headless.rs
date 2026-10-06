//! Headless-Modus: ohne Fenster rechnen, validieren, Benchmark drucken.
//!
//! `cargo oxide run -- --headless --steps 500` führt N Physikschritte auf
//! der GPU aus (oder `--cpu`), prüft numerische Stabilität (kein NaN/Inf,
//! kein Wand-Tunneln, Dichte in plausiblen Schranken) und meldet ms, FPS
//! und Partikel-Durchsatz. Exit-Code 1 bei Validierungsfehlern.

use std::time::Instant;

use crate::backend::{Backend, CpuBackend};
use crate::params::{Cli, SimConfig};
use crate::types::InteractParams;

#[cfg(feature = "gpu")]
use crate::backend::GpuBackend;

/// Wählt das Backend (GPU außer bei `--cpu`), exit(1) ohne GPU.
fn make_backend(
    cfg: &SimConfig,
    #[cfg_attr(not(feature = "gpu"), allow(unused_variables))] cli: &Cli,
) -> Box<dyn Backend> {
    #[cfg(feature = "gpu")]
    if !cli.cpu {
        match GpuBackend::new(cfg) {
            Ok(gpu) => return Box::new(gpu),
            Err(e) => {
                eprintln!("GPU-Backend fehlgeschlagen: {e}");
                std::process::exit(1);
            }
        }
    }
    Box::new(CpuBackend::new(cfg))
}

/// Validiert einen Schnappschuss; leere Rückgabe = bestanden.
pub fn validate(backend: &dyn Backend, cfg: &SimConfig) -> Vec<String> {
    let mut problems = Vec::new();
    let pos = backend.positions();
    let vel = backend.velocities();
    let dens = backend.densities();
    assert_eq!(pos.len(), cfg.particles);
    let mut bad_pos = 0usize;
    let mut tunneled = 0usize;
    let mut bad_dens = 0usize;
    let mut rho_min = f32::INFINITY;
    let mut rho_max = f32::NEG_INFINITY;
    for i in 0..pos.len() {
        let p = pos[i];
        let v = vel[i];
        let r = dens[i];
        if !(p[0].is_finite() && p[1].is_finite() && v[0].is_finite() && v[1].is_finite()) {
            bad_pos += 1;
        }
        let eps = 1e-4;
        if p[0].is_finite()
            && (p[0] < -eps
                || p[0] > cfg.domain_w + eps
                || p[1] < -eps
                || p[1] > cfg.domain_h + eps)
        {
            tunneled += 1;
        }
        if !r.is_finite() || r <= 0.0 || r > 5.0 * cfg.rest_density {
            bad_dens += 1;
        }
        rho_min = rho_min.min(r);
        rho_max = rho_max.max(r);
    }
    if bad_pos > 0 {
        problems.push(format!("{bad_pos} Partikel mit NaN/Inf in pos/vel"));
    }
    if tunneled > 0 {
        problems.push(format!("{tunneled} Partikel durch Wände getunnelt"));
    }
    if bad_dens > 0 {
        problems.push(format!(
            "{bad_dens} Dichten außerhalb (0, 5ρ₀], ρ∈[{rho_min:.1}, {rho_max:.1}]"
        ));
    }
    problems
}

/// Führt den Headless-Lauf aus und druckt Bericht (+ exit 1 bei Fehlern).
pub fn run(cli: Cli) {
    let cfg = cli.sim_config();
    let backend_name = if cli.cpu { "CPU" } else { "GPU" };
    println!(
        "sph headless: backend={backend_name} N={} steps={}",
        cfg.particles, cli.steps
    );
    let mut backend = make_backend(&cfg, &cli);
    let init = cfg.dam_break();
    backend.reset(&init);
    backend.set_interact(InteractParams::neutral(cfg.domain_w, cfg.domain_h));

    let t0 = Instant::now();
    for _ in 0..cli.steps {
        backend.step();
    }
    // Ein Sync fürs Timing; Downloads zählen nicht zur Physikzeit.
    backend.sync_host();
    let phys = t0.elapsed();

    let problems = validate(backend.as_ref(), &cfg);
    // Dichte-/Geschwindigkeitsstatistik als Stabilitätssignal.
    let dens = backend.densities();
    let (mut rmin, mut rmax, mut rsum) = (f32::INFINITY, f32::NEG_INFINITY, 0.0f64);
    for &r in dens {
        rmin = rmin.min(r);
        rmax = rmax.max(r);
        rsum += r as f64;
    }
    let mut vmax = 0.0f32;
    for v in backend.velocities() {
        vmax = vmax.max((v[0] * v[0] + v[1] * v[1]).sqrt());
    }
    println!(
        "Statistik: ρ∈[{rmin:.1}, {rmax:.1}] ρ̄={:.1} (ρ₀={}), |v|max={vmax:.2} m/s",
        rsum / dens.len() as f64,
        cfg.rest_density,
    );
    let steps = cli.steps as f64;
    let n = cfg.particles as f64;
    let secs = phys.as_secs_f64();
    let ms_step = phys.as_secs_f64() * 1000.0 / steps;
    let steps_s = steps / secs;
    let parts_s = n * steps / secs;
    if cli.bench {
        println!("backend,particles,steps,total_ms,ms_per_step,steps_per_s,particles_per_s");
        println!(
            "{backend_name},{},{},{:.2},{ms_step:.4},{steps_s:.1},{parts_s:.0}",
            cfg.particles,
            cli.steps,
            phys.as_secs_f64() * 1000.0,
        );
    } else {
        println!(
            "Physik: {steps} Schritte in {:.2} ms ({ms_step:.4} ms/Schritt, {steps_s:.1} Schritte/s, {parts_s:.0} Partikel/s)",
            phys.as_secs_f64() * 1000.0,
        );
    }
    if problems.is_empty() {
        println!("Validierung: PASS (kein NaN/Inf, kein Tunneln, Dichte ok)");
    } else {
        println!("Validierung: FAIL");
        for p in &problems {
            println!("  - {p}");
        }
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validierung_akzeptiert_ruhezustand() {
        let cfg = SimConfig::default();
        let mut backend = CpuBackend::new(&cfg);
        backend.reset(&cfg.dam_break());
        assert!(validate(&backend, &cfg).is_empty());
    }
}
