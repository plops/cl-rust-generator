//! Drossel-Proxy für HIL-Tests: `throttle <listen> <ziel> <B/s>`.
//!
//! Leitet TCP von `listen` nach `ziel` weiter; die Richtung Ziel→Client
//! (Server-Downstream) wird per Token-Bucket auf `B/s` begrenzt, der
//! Upstream ebenfalls (Eingaben). Was schneller kommt, staut sich in den
//! Kernel-Puffern vor dem Proxy — wie im 6-kB/s-SSH-Tunnel. Ein Client zur
//! Zeit; jede neue Verbindung bekommt eine frische Ziel-Verbindung.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::time::{Duration, Instant};

/// Kopiert `from` → `to` mit höchstens `bps` Byte/s (Burst ≤ 1/10 s).
fn pump(mut from: TcpStream, mut to: TcpStream, bps: f64) {
    let cap = (bps / 10.0).max(64.0);
    let (mut tokens, mut last) = (cap, Instant::now());
    let mut buf = vec![0u8; 4096];
    loop {
        let now = Instant::now();
        tokens = (tokens + now.duration_since(last).as_secs_f64() * bps).min(cap);
        last = now;
        if tokens < 1.0 {
            std::thread::sleep(Duration::from_millis(2));
            continue;
        }
        let want = (tokens as usize).clamp(1, buf.len());
        match from.read(&mut buf[..want]) {
            Ok(0) | Err(_) => break,
            Ok(n) => {
                tokens -= n as f64;
                if to.write_all(&buf[..n]).is_err() {
                    break;
                }
            }
        }
    }
    let _ = to.shutdown(std::net::Shutdown::Both);
    let _ = from.shutdown(std::net::Shutdown::Both);
}

fn main() {
    let a: Vec<String> = std::env::args().skip(1).collect();
    let [listen, target, bps] = a.as_slice() else {
        eprintln!("Aufruf: throttle <listen> <ziel> <B/s>");
        std::process::exit(2);
    };
    let bps: f64 = bps.parse().expect("B/s");
    let l = TcpListener::bind(listen).expect("bind");
    eprintln!("[throttle] {listen} → {target} mit {bps} B/s");
    for c in l.incoming().flatten() {
        let Ok(s) = TcpStream::connect(target) else {
            eprintln!("[throttle] Ziel {target} nicht erreichbar");
            continue;
        };
        let _ = c.set_nodelay(true);
        let _ = s.set_nodelay(true);
        let (c2, s2) = (c.try_clone().unwrap(), s.try_clone().unwrap());
        std::thread::spawn(move || pump(s, c, bps)); // Downstream
        std::thread::spawn(move || pump(c2, s2, bps)); // Upstream
    }
}
