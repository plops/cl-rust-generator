//! `02_pipe` — TCP-Proxy mit Ratenbegrenzung, Latenz, Blackout und Abriss.
//!
//! Je Verbindung und Richtung ein Leser- und ein Schreiber-Thread. Der
//! Leser stempelt Stücke (≤ 256 B) mit der Ankunftszeit; der Schreiber
//! wartet Latenz, Blackout und Token-Bucket ab. Während eines Blackouts
//! bleibt die TCP-Verbindung offen, es fließt nur nichts — wie bei einem
//! Funkloch unter TCP/SSH.

use std::io::{Read, Write};
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use lbw_common::rate::TokenBucket;

/// Proxy-Parameter (Rate 0 = unbegrenzt).
#[derive(Clone, Debug)]
pub struct Cfg {
    pub listen: String,
    pub to: String,
    /// Server → Client (Byte/s).
    pub down_rate: u32,
    /// Client → Server (Byte/s).
    pub up_rate: u32,
    /// Einweg-Latenz.
    pub delay: Duration,
}

/// Zähler (für Tests und Log).
#[derive(Default, Debug)]
pub struct Stats {
    pub down: AtomicU64,
    pub up: AtomicU64,
    pub conns: AtomicU64,
}

/// Laufender Proxy.
pub struct Proxy {
    pub addr: SocketAddr,
    pub stats: Arc<Stats>,
    blackout: Arc<AtomicBool>,
    active: Arc<Mutex<Vec<TcpStream>>>,
}

impl Proxy {
    /// Blackout an/aus (nichts wird weitergeleitet, Verbindungen bleiben).
    pub fn set_blackout(&self, on: bool) {
        self.blackout.store(on, Ordering::SeqCst);
    }

    /// Reißt alle aktiven Verbindungen ab (simuliert Tunnel-Abbruch).
    pub fn cut(&self) {
        for s in self.active.lock().unwrap().drain(..) {
            let _ = s.shutdown(Shutdown::Both);
        }
    }
}

const PIECE: usize = 256;

/// Eine Richtung: `from` → `to`.
fn pump(
    from: TcpStream,
    mut to: TcpStream,
    rate: u32,
    delay: Duration,
    blackout: Arc<AtomicBool>,
    count: Arc<Stats>,
    down: bool,
) {
    let (tx, rx) = channel::<(Instant, Vec<u8>)>();
    let mut reader = from.try_clone().expect("clone");
    std::thread::spawn(move || {
        let mut buf = [0u8; PIECE];
        loop {
            match reader.read(&mut buf) {
                Ok(0) | Err(_) => break,
                Ok(n) => {
                    if tx.send((Instant::now(), buf[..n].to_vec())).is_err() {
                        break;
                    }
                }
            }
        }
    });
    let mut bucket = (rate > 0).then(|| TokenBucket::new(rate, PIECE as u32 * 2, Instant::now()));
    for (t, piece) in rx {
        let due = t + delay;
        let now = Instant::now();
        if due > now {
            std::thread::sleep(due - now);
        }
        while blackout.load(Ordering::SeqCst) {
            std::thread::sleep(Duration::from_millis(20));
        }
        if let Some(b) = bucket.as_mut() {
            let w = b.wait(piece.len(), Instant::now());
            std::thread::sleep(w);
            b.take(piece.len(), Instant::now());
        }
        if to.write_all(&piece).is_err() {
            break;
        }
        let c = if down { &count.down } else { &count.up };
        c.fetch_add(piece.len() as u64, Ordering::Relaxed);
    }
    let _ = to.shutdown(Shutdown::Both);
    let _ = from.shutdown(Shutdown::Both);
}

/// Startet den Proxy im Hintergrund.
pub fn start(cfg: Cfg) -> std::io::Result<Proxy> {
    let l = TcpListener::bind(&cfg.listen)?;
    let addr = l.local_addr()?;
    let stats = Arc::new(Stats::default());
    let blackout = Arc::new(AtomicBool::new(false));
    let active = Arc::new(Mutex::new(Vec::new()));
    let (st, bo, ac) = (stats.clone(), blackout.clone(), active.clone());
    std::thread::spawn(move || {
        for c in l.incoming().flatten() {
            let Ok(u) = TcpStream::connect(&cfg.to) else {
                let _ = c.shutdown(Shutdown::Both);
                continue;
            };
            let _ = (c.set_nodelay(true), u.set_nodelay(true));
            st.conns.fetch_add(1, Ordering::Relaxed);
            if let (Ok(a), Ok(b)) = (c.try_clone(), u.try_clone()) {
                ac.lock().unwrap().extend([a, b]);
            }
            let (c2, u2) = (c.try_clone().unwrap(), u.try_clone().unwrap());
            let (b1, b2, s1, s2) = (bo.clone(), bo.clone(), st.clone(), st.clone());
            let (dr, ur, d) = (cfg.down_rate, cfg.up_rate, cfg.delay);
            std::thread::spawn(move || pump(u, c, dr, d, b1, s1, true));
            std::thread::spawn(move || pump(c2, u2, ur, d, b2, s2, false));
        }
    });
    Ok(Proxy {
        addr,
        stats,
        blackout,
        active,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Echo-Server, der `n` Byte sendet und dann schließt.
    fn source(n: usize) -> SocketAddr {
        let l = TcpListener::bind("127.0.0.1:0").unwrap();
        let a = l.local_addr().unwrap();
        std::thread::spawn(move || {
            let (mut s, _) = l.accept().unwrap();
            let _ = s.write_all(&vec![7u8; n]);
        });
        a
    }

    fn proxy(to: SocketAddr, rate: u32, delay_ms: u64) -> Proxy {
        start(Cfg {
            listen: "127.0.0.1:0".into(),
            to: to.to_string(),
            down_rate: rate,
            up_rate: rate,
            delay: Duration::from_millis(delay_ms),
        })
        .unwrap()
    }

    #[test]
    fn rate_limit_is_enforced() {
        let p = proxy(source(12_000), 6000, 0);
        let mut c = TcpStream::connect(p.addr).unwrap();
        let t0 = Instant::now();
        let mut buf = Vec::new();
        c.read_to_end(&mut buf).unwrap();
        let dt = t0.elapsed().as_secs_f64();
        assert_eq!(buf.len(), 12_000);
        // 12 kB bei 6 kB/s mit 512-B-Burst: ~1,9 s.
        assert!((1.7..2.6).contains(&dt), "{dt}");
        assert_eq!(p.stats.down.load(Ordering::Relaxed), 12_000);
    }

    #[test]
    fn delay_blackout_and_cut() {
        let p = proxy(source(10), 0, 150);
        p.set_blackout(true);
        let mut c = TcpStream::connect(p.addr).unwrap();
        c.set_read_timeout(Some(Duration::from_millis(400)))
            .unwrap();
        let mut b = [0u8; 10];
        assert!(c.read(&mut b).is_err(), "Blackout: nichts kommt an");
        let t = Instant::now();
        p.set_blackout(false);
        c.set_read_timeout(Some(Duration::from_secs(2))).unwrap();
        c.read_exact(&mut b).unwrap();
        assert!(t.elapsed() < Duration::from_millis(300));
        p.cut();
        assert_eq!(c.read(&mut b).unwrap_or(0), 0, "abgerissen");
    }
}
