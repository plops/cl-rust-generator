//! CPU-Durchsatzbank (harness=false): `cargo bench` druckt steps/s.
//!
//! GPU-Zahlen liefert der Headless-Modus:
//! `cargo oxide run -- --headless --steps 2000 --bench`.

use std::time::Instant;

use sph::backend::{Backend, CpuBackend};
use sph::params::SimConfig;
use sph::types::InteractParams;

fn bench_n(n: usize, steps: usize) {
    let cfg = SimConfig {
        particles: n,
        ..SimConfig::default()
    };
    let mut backend = CpuBackend::new(&cfg);
    backend.reset(&cfg.dam_break());
    backend.set_interact(InteractParams::neutral(cfg.domain_w, cfg.domain_h));
    let t0 = Instant::now();
    for _ in 0..steps {
        backend.step();
    }
    backend.sync_host();
    let secs = t0.elapsed().as_secs_f64();
    println!(
        "CPU N={n}: {steps} Schritte in {:.2} ms ({:.4} ms/Schritt, {:.0} Partikel/s)",
        secs * 1000.0,
        secs * 1000.0 / steps as f64,
        n as f64 * steps as f64 / secs,
    );
}

fn main() {
    bench_n(2_048, 200);
    bench_n(16_384, 50);
}
