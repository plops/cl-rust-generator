//! `03_net` — Verbindung zum Server mit automatischem Reconnect.
//!
//! Ein Netz-Thread verbindet (Backoff 0,5 → 5 s), sendet `Hello` mit dem
//! zuletzt angewandten Zustand (Resume), liest Frames, setzt Kacheln
//! zusammen, dekodiert AV1 und reicht fertige Ereignisse an die UI.
//! Acks gehen alle ≥ 512 Byte bzw. 100 ms zurück (Flusskontrolle des
//! Servers). Stille wird bis `dead_after` (Default 90 s) toleriert.
//! `drop(Net)` beendet Thread und Verbindung binnen ~100 ms.

use std::net::{TcpStream, ToSocketAddrs};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, Sender, channel};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use lbw_common::frame::{FrameReader, Read1, write_frame};
use lbw_common::{ClientMsg, Input, PROTO_VERSION, Rect, ServerMsg, TextItem};

use crate::av1::Decoder;

/// Ack spätestens nach so vielen empfangenen Bytes.
pub const ACK_BYTES: u64 = 512;

/// Ereignisse an die UI.
#[derive(Debug)]
pub enum Event {
    Connected {
        w: u16,
        h: u16,
        resumed: bool,
    },
    Disconnected(String),
    Clear,
    Text {
        remove: Vec<u32>,
        add: Vec<TextItem>,
    },
    /// Dekodierte Kachel (RGBA8, `rect.w × rect.h`).
    Tile {
        rect: Rect,
        rgba: Vec<u8>,
        bytes: usize,
    },
    Stats {
        rate: u32,
        backlog: u32,
        tiles: u32,
    },
}

/// Netz-Parameter.
#[derive(Clone, Debug)]
pub struct NetCfg {
    pub addr: String,
    pub dead_after: Duration,
    pub verbose: bool,
}

/// Griff der UI auf den Netz-Thread.
pub struct Net {
    pub events: Receiver<Event>,
    out: Sender<ClientMsg>,
    stop: Arc<AtomicBool>,
}

impl Drop for Net {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
    }
}

impl Net {
    /// Sendet eine Eingabe (geht verloren, wenn gerade keine Verbindung besteht).
    pub fn send(&self, i: Input) {
        let _ = self.out.send(ClientMsg::Input(i));
    }
}

/// Setzt `TileStart` + `TileData` zu vollständigen AV1-Daten zusammen.
#[derive(Default)]
pub struct Assembler {
    cur: Option<(u32, u32, Rect, Vec<u8>, usize)>,
}

impl Assembler {
    pub fn start(&mut self, id: u32, seq: u32, rect: Rect, len: u32) {
        self.cur = Some((
            id,
            seq,
            rect,
            Vec::with_capacity(len as usize),
            len as usize,
        ));
    }

    /// Hängt Daten an; liefert `(seq, rect, daten)` sobald komplett.
    pub fn data(&mut self, id: u32, offset: u32, d: &[u8]) -> Option<(u32, Rect, Vec<u8>)> {
        let (cid, _, _, buf, len) = self.cur.as_mut()?;
        if *cid != id || offset as usize != buf.len() {
            self.cur = None; // Lücke → Kachel verwerfen
            return None;
        }
        buf.extend_from_slice(d);
        if buf.len() < *len {
            return None;
        }
        let (_, seq, rect, buf, _) = self.cur.take()?;
        Some((seq, rect, buf))
    }
}

/// Zustand über Verbindungen hinweg (Resume).
#[derive(Default, Clone, Copy)]
struct Resume {
    server_id: u64,
    seq: u32,
}

/// Startet den Netz-Thread.
#[must_use]
pub fn spawn(cfg: NetCfg) -> Net {
    let (ev_tx, events) = channel();
    let (out, out_rx) = channel();
    let out_rx = Arc::new(Mutex::new(out_rx));
    let ack_tx = out.clone();
    let stop = Arc::new(AtomicBool::new(false));
    let stop_t = stop.clone();
    std::thread::spawn(move || {
        let mut dec = Decoder::new(1).expect("rav1d");
        let mut resume = Resume::default();
        let mut backoff = Duration::from_millis(500);
        loop {
            let r = session(
                &cfg,
                &mut resume,
                &mut dec,
                &ev_tx,
                &ack_tx,
                &out_rx,
                &stop_t,
            );
            if stop_t.load(Ordering::Relaxed) {
                return;
            }
            match r {
                Ok(()) => return, // UI beendet
                Err((e, was_up)) => {
                    if ev_tx.send(Event::Disconnected(e.clone())).is_err() {
                        return;
                    }
                    if cfg.verbose {
                        eprintln!("[client] getrennt: {e}");
                    }
                    if was_up {
                        backoff = Duration::from_millis(500);
                    }
                    let until = Instant::now() + backoff;
                    while Instant::now() < until && !stop_t.load(Ordering::Relaxed) {
                        std::thread::sleep(Duration::from_millis(50));
                    }
                    backoff = (backoff * 2).min(Duration::from_secs(5));
                }
            }
        }
    });
    Net { events, out, stop }
}

type SessErr = (String, bool);

fn connect(addr: &str) -> Result<TcpStream, String> {
    let a = addr
        .to_socket_addrs()
        .map_err(|e| format!("{addr}: {e}"))?
        .next()
        .ok_or("keine Adresse")?;
    TcpStream::connect_timeout(&a, Duration::from_secs(5)).map_err(|e| format!("{addr}: {e}"))
}

/// Eine Verbindung bis zu ihrem Ende. `Ok` nur, wenn die UI weg ist.
fn session(
    cfg: &NetCfg,
    resume: &mut Resume,
    dec: &mut Decoder,
    ev: &Sender<Event>,
    ack_tx: &Sender<ClientMsg>,
    out_rx: &Arc<Mutex<Receiver<ClientMsg>>>,
    stop: &Arc<AtomicBool>,
) -> Result<(), SessErr> {
    let down = |e: String| (e, false);
    let mut s = connect(&cfg.addr).map_err(down)?;
    let _ = s.set_nodelay(true);
    let hello = ClientMsg::Hello {
        version: PROTO_VERSION,
        server_id: resume.server_id,
        seq: resume.seq,
    };
    write_frame(&mut s, &hello.encode()).map_err(|e| down(e.to_string()))?;
    s.set_read_timeout(Some(Duration::from_millis(100)))
        .map_err(|e| down(e.to_string()))?;

    // Alte Eingaben (während der Trennung) verwerfen, dann Writer starten.
    while out_rx.lock().unwrap().try_recv().is_ok() {}
    let alive = Arc::new(AtomicBool::new(true));
    let mut ws = s.try_clone().map_err(|e| down(e.to_string()))?;
    let (alive_w, rx_w, stop_w) = (alive.clone(), out_rx.clone(), stop.clone());
    let writer = std::thread::spawn(move || {
        while alive_w.load(Ordering::Relaxed) && !stop_w.load(Ordering::Relaxed) {
            let m = rx_w
                .lock()
                .unwrap()
                .recv_timeout(Duration::from_millis(100));
            match m {
                Ok(m) => {
                    if write_frame(&mut ws, &m.encode()).is_err() {
                        break;
                    }
                }
                Err(RecvTimeoutError::Timeout) => {}
                Err(RecvTimeoutError::Disconnected) => break,
            }
        }
        alive_w.store(false, Ordering::Relaxed);
    });

    let r = read_loop(cfg, &mut s, resume, dec, ev, ack_tx, &alive);
    alive.store(false, Ordering::Relaxed);
    let _ = s.shutdown(std::net::Shutdown::Both);
    let _ = writer.join();
    r
}

#[allow(clippy::too_many_arguments)]
fn read_loop(
    cfg: &NetCfg,
    s: &mut TcpStream,
    resume: &mut Resume,
    dec: &mut Decoder,
    ev: &Sender<Event>,
    ack_tx: &Sender<ClientMsg>,
    alive: &AtomicBool,
) -> Result<(), SessErr> {
    let mut fr = FrameReader::new();
    let mut asm = Assembler::default();
    let (mut last_rx, mut acked, mut last_ack) = (Instant::now(), 0u64, Instant::now());
    let mut up = false;
    let send = |e: Event| ev.send(e).map_err(|_| ());
    loop {
        if !alive.load(Ordering::Relaxed) {
            return Err(("Writer beendet".into(), up));
        }
        let b = match fr.read(s) {
            Ok(Read1::Frame(b)) => b,
            Ok(Read1::Idle) => {
                if last_rx.elapsed() > cfg.dead_after {
                    return Err((format!("{} s ohne Daten", cfg.dead_after.as_secs()), up));
                }
                Vec::new()
            }
            Err(e) => return Err((e.to_string(), up)),
        };
        if !b.is_empty() {
            last_rx = Instant::now();
            let m = ServerMsg::decode(&b).map_err(|e| (e.to_string(), up))?;
            let r = match m {
                ServerMsg::Hello {
                    server_id,
                    w,
                    h,
                    resumed,
                } => {
                    up = true;
                    if !resumed {
                        resume.seq = 0;
                    }
                    resume.server_id = server_id;
                    send(Event::Connected { w, h, resumed })
                }
                ServerMsg::Clear => {
                    resume.seq = 0;
                    send(Event::Clear)
                }
                ServerMsg::Text { seq, remove, add } => {
                    resume.seq = seq;
                    send(Event::Text { remove, add })
                }
                ServerMsg::TileStart {
                    tile_id,
                    seq,
                    rect,
                    len,
                } => {
                    asm.start(tile_id, seq, rect, len);
                    Ok(())
                }
                ServerMsg::TileData {
                    tile_id,
                    offset,
                    data,
                } => match asm.data(tile_id, offset, &data) {
                    Some((seq, rect, obu)) => {
                        resume.seq = seq;
                        match dec.decode(&obu) {
                            Ok(img) if (img.w, img.h) == (rect.w.into(), rect.h.into()) => {
                                send(Event::Tile {
                                    rect,
                                    rgba: img.data,
                                    bytes: obu.len(),
                                })
                            }
                            Ok(img) => {
                                eprintln!("[client] Kachelgröße {}x{} ≠ {rect:?}", img.w, img.h);
                                Ok(())
                            }
                            Err(e) => {
                                eprintln!("[client] AV1: {e}");
                                Ok(())
                            }
                        }
                    }
                    None => Ok(()),
                },
                ServerMsg::Ping { t } => {
                    let _ = ack_tx.send(ClientMsg::Pong { t });
                    Ok(())
                }
                ServerMsg::Pong { .. } => Ok(()),
                ServerMsg::Stats {
                    rate,
                    backlog,
                    tiles,
                } => send(Event::Stats {
                    rate,
                    backlog,
                    tiles,
                }),
            };
            if r.is_err() {
                return Ok(()); // UI geschlossen
            }
        }
        let rx = fr.rx_bytes;
        if rx >= acked + ACK_BYTES
            || (rx > acked && last_ack.elapsed() >= Duration::from_millis(100))
        {
            acked = rx;
            last_ack = Instant::now();
            let _ = ack_tx.send(ClientMsg::Ack {
                rx_bytes: rx,
                seq: resume.seq,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn drop_closes_the_connection_promptly() {
        use std::io::Read;
        let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let net = spawn(NetCfg {
            addr: l.local_addr().unwrap().to_string(),
            dead_after: Duration::from_secs(90),
            verbose: false,
        });
        let (mut c, _) = l.accept().unwrap();
        c.set_read_timeout(Some(Duration::from_secs(3))).unwrap();
        let mut hello = [0u8; 64];
        assert!(c.read(&mut hello).unwrap() > 0, "Hello kommt an");
        let t = Instant::now();
        drop(net);
        let mut rest = Vec::new();
        let n = c.read_to_end(&mut rest).expect("EOF statt Timeout");
        assert_eq!(n, 0);
        assert!(t.elapsed() < Duration::from_secs(1), "{:?}", t.elapsed());
    }

    #[test]
    fn assembler_joins_chunks_and_drops_gaps() {
        let mut a = Assembler::default();
        let r = Rect::new(0, 0, 16, 16);
        a.start(1, 9, r, 5);
        assert!(a.data(1, 0, &[1, 2]).is_none());
        assert_eq!(a.data(1, 2, &[3, 4, 5]), Some((9, r, vec![1, 2, 3, 4, 5])));
        a.start(2, 10, r, 4);
        assert!(a.data(2, 1, &[0]).is_none()); // Lücke
        assert!(a.data(2, 0, &[0, 0, 0, 0]).is_none()); // verworfen
        a.start(3, 11, r, 2);
        assert!(a.data(4, 0, &[0, 0]).is_none()); // falsche ID
    }
}
