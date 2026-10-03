//! `03_net` — Verbindung zum Server mit automatischem Reconnect.
//!
//! Ein Netz-Thread verbindet (Backoff 0,5 → 5 s), sendet `Hello`, liest
//! Frames, dekodiert AV1-Kacheln und reicht fertige Ereignisse an die UI.
//! Ausgehende Eingaben fließen über einen Kanal (max. ~50 ms Verzögerung).
//! `drop(Net)` beendet Thread und Verbindung. Kein Heartbeat im MVP:
//! Neuaufbau nur bei TCP-Fehler (Stille ist bei statischem Bild normal).

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{Receiver, Sender, channel};
use std::time::Duration;

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, TextItem};

use crate::av1::Decoder;

/// Ereignisse an die UI.
#[derive(Debug)]
pub enum Event {
    Connected,
    Disconnected(String),
    ClearText,
    AddText(TextItem),
    /// Dekodierte AV1-Box (RGBA8, `w`×`h`).
    Tile {
        x: u16,
        y: u16,
        w: usize,
        h: usize,
        rgba: Vec<u8>,
        bytes: usize,
    },
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
    /// Verbindet mit `addr` (Reconnect läuft im Hintergrund).
    pub fn connect(addr: &str) -> Self {
        let (ev_tx, events) = channel();
        let (out, out_rx) = channel();
        let stop = Arc::new(AtomicBool::new(false));
        std::thread::spawn({
            let addr = addr.to_owned();
            let stop = stop.clone();
            move || run(&addr, ev_tx, out_rx, &stop)
        });
        Self { events, out, stop }
    }

    /// Sendet eine Nachricht (geht verloren, wenn gerade keine Verbindung besteht).
    pub fn send(&self, m: ClientMsg) {
        let _ = self.out.send(m);
    }
}

fn run(addr: &str, ev: Sender<Event>, out: Receiver<ClientMsg>, stop: &AtomicBool) {
    let mut decoder = match Decoder::new() {
        Ok(d) => d,
        Err(e) => {
            let _ = ev.send(Event::Disconnected(format!("rav1d: {e}")));
            return;
        }
    };
    let mut backoff = Duration::from_millis(500);
    while !stop.load(Ordering::Relaxed) {
        match TcpStream::connect(addr) {
            Ok(s) => {
                backoff = Duration::from_millis(500);
                session(s, &ev, &out, stop, &mut decoder);
                if stop.load(Ordering::Relaxed) {
                    break;
                }
                let _ = ev.send(Event::Disconnected("getrennt".into()));
            }
            Err(e) => {
                let _ = ev.send(Event::Disconnected(format!("kein Server ({e})")));
            }
        }
        // Backoff in Scheiben, damit `drop` schnell wirkt.
        let steps = backoff.as_millis().div_ceil(100);
        for _ in 0..steps {
            if stop.load(Ordering::Relaxed) {
                return;
            }
            std::thread::sleep(Duration::from_millis(100));
        }
        backoff = (backoff * 2).min(Duration::from_secs(5));
    }
}

fn session(
    s: TcpStream,
    ev: &Sender<Event>,
    out: &Receiver<ClientMsg>,
    stop: &AtomicBool,
    dec: &mut Decoder,
) {
    if s.set_read_timeout(Some(Duration::from_millis(50))).is_err() {
        return;
    }
    let mut rd = match s.try_clone() {
        Ok(r) => r,
        Err(_) => return,
    };
    let mut wr = s;
    if write_msg(
        &mut wr,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .is_err()
    {
        return;
    }
    let mut fr = FrameReader::new();
    loop {
        if stop.load(Ordering::Relaxed) {
            return;
        }
        while let Ok(m) = out.try_recv() {
            if write_msg(&mut wr, &m).is_err() {
                return;
            }
        }
        match fr.read(&mut rd) {
            Ok(Read1::Frame(b)) => match decode_msg::<ServerMsg>(&b) {
                Ok(ServerMsg::Hello) => {
                    let _ = ev.send(Event::Connected);
                }
                Ok(ServerMsg::ClearText) => {
                    let _ = ev.send(Event::ClearText);
                }
                Ok(ServerMsg::AddText(t)) => {
                    let _ = ev.send(Event::AddText(t));
                }
                Ok(ServerMsg::Tile { x, y, data }) => match dec.decode(&data) {
                    Ok(rgba) => {
                        let _ = ev.send(Event::Tile {
                            x,
                            y,
                            w: rgba.w,
                            h: rgba.h,
                            rgba: rgba.data,
                            bytes: data.len(),
                        });
                    }
                    Err(e) => eprintln!("[net] AV1: {e}"),
                },
                Err(e) => {
                    eprintln!("[net] Protokoll: {e}");
                    return;
                }
            },
            Ok(Read1::Idle) => {}
            Err(_) => return, // EOF/Verbindung weg → Reconnect
        }
    }
}
