//! `lbw-throttle` — nur Verdrahtung: Proxy starten, Zeitplan abarbeiten, loggen.

use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

use lbw_throttle::config::Config;
use lbw_throttle::pipe::start;

fn main() {
    let cfg = Config::parse(std::env::args().skip(1)).unwrap_or_else(|m| {
        eprintln!("{m}");
        std::process::exit(2)
    });
    let p = start(cfg.pipe.clone()).unwrap_or_else(|e| {
        eprintln!("lbw-throttle: {}: {e}", cfg.pipe.listen);
        std::process::exit(1)
    });
    eprintln!(
        "[throttle] {} → {} ({} B/s ↓, {} B/s ↑, {:?})",
        p.addr, cfg.pipe.to, cfg.pipe.down_rate, cfg.pipe.up_rate, cfg.pipe.delay
    );
    let t0 = Instant::now();
    let (mut last_d, mut last_u, mut sec) = (0, 0, 0u64);
    loop {
        std::thread::sleep(Duration::from_millis(50));
        let t = t0.elapsed();
        let black = cfg.blackouts.iter().any(|(s, d)| t >= *s && t < *s + *d);
        p.set_blackout(black);
        if cfg
            .cuts
            .iter()
            .any(|c| t >= *c && t < *c + Duration::from_millis(50))
        {
            eprintln!(
                "[throttle] {:.1}s: Verbindungen abgerissen",
                t.as_secs_f64()
            );
            p.cut();
        }
        if cfg.log && t.as_secs() > sec {
            sec = t.as_secs();
            let (d, u) = (
                p.stats.down.load(Ordering::Relaxed),
                p.stats.up.load(Ordering::Relaxed),
            );
            eprintln!(
                "[throttle] t={sec}s ↓ {} B/s ↑ {} B/s{}",
                d - last_d,
                u - last_u,
                if black { " BLACKOUT" } else { "" }
            );
            (last_d, last_u) = (d, u);
        }
    }
}
