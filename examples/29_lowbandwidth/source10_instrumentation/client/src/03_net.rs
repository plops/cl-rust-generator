//! `03_net` — Verbindung zum Server mit automatischem Reconnect.
//!
//! Ein Netz-Thread verbindet (Backoff 0,5 → 5 s), sendet `Hello`, liest
//! Frames, dekodiert AV1-Kacheln und reicht fertige Ereignisse an die UI.
//! Ausgehende Eingaben fließen über einen Kanal (max. ~50 ms Verzögerung).
//! `drop(Net)` beendet Thread und Verbindung. Kein Heartbeat nötig:
//! Neuaufbau nur bei TCP-Fehler (Stille ist bei statischem Bild normal).
//!
//! Instrumentierung: Mit `Recorder` loggt der Thread Connects/Disconnects
//! (mit Down-Dauer), jede empfangene Nachricht (mit AV1-Decode-ms) und jede
//! gesendete Eingabe (mit Sendezeit) — daraus misst `logstat` offline die
//! Input→Photon-Latenz auf der Client-Uhr.

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{Receiver, Sender, channel};
use std::time::{Duration, Instant};

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, TextItem};
use lbw_log::Recorder;

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
        Self::connect_recorder(addr, Recorder::none())
    }

    /// Wie [`Net::connect`], aber mit Aufnahme in `rec`.
    pub fn connect_recorder(addr: &str, rec: Recorder) -> Self {
        let (ev_tx, events) = channel();
        let (out, out_rx) = channel();
        let stop = Arc::new(AtomicBool::new(false));
        std::thread::spawn({
            let addr = addr.to_owned();
            let stop = stop.clone();
            move || run(&addr, ev_tx, out_rx, &stop, &rec)
        });
        Self { events, out, stop }
    }

    /// Sendet eine Nachricht (geht verloren, wenn gerade keine Verbindung besteht).
    pub fn send(&self, m: ClientMsg) {
        let _ = self.out.send(m);
    }
}

fn run(addr: &str, ev: Sender<Event>, out: Receiver<ClientMsg>, stop: &AtomicBool, rec: &Recorder) {
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
                let why = session(s, &ev, &out, stop, &mut decoder, rec, addr);
                if stop.load(Ordering::Relaxed) {
                    break;
                }
                rec.gap_down(&why);
                let _ = ev.send(Event::Disconnected("getrennt".into()));
            }
            Err(e) => {
                rec.gap_down(&format!("kein Server ({e})"));
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
    rec: &Recorder,
    addr: &str,
) -> String {
    if s.set_read_timeout(Some(Duration::from_millis(50))).is_err() {
        return "timeout-Flag".into();
    }
    let mut rd = match s.try_clone() {
        Ok(r) => r,
        Err(_) => return "clone".into(),
    };
    let mut wr = s;
    let hello = ClientMsg::Hello {
        version: PROTO_VERSION,
    };
    match write_msg(&mut wr, &hello) {
        Ok(n) => rec.msg_client(&hello, n),
        Err(_) => return "hello-sendefehler".into(),
    }
    let mut fr = FrameReader::new();
    loop {
        if stop.load(Ordering::Relaxed) {
            return "stop".into();
        }
        while let Ok(m) = out.try_recv() {
            match write_msg(&mut wr, &m) {
                Ok(n) => rec.msg_client(&m, n),
                Err(_) => return "sendefehler".into(),
            }
        }
        match fr.read(&mut rd) {
            Ok(Read1::Frame(b)) => match decode_msg::<ServerMsg>(&b) {
                Ok(m) => {
                    rec.msg_server_raw(&m, &b);
                    match m {
                        ServerMsg::Hello => {
                            rec.gap_up(addr);
                            let _ = ev.send(Event::Connected);
                        }
                        ServerMsg::ClearText => {
                            let _ = ev.send(Event::ClearText);
                        }
                        ServerMsg::AddText(t) => {
                            let _ = ev.send(Event::AddText(t));
                        }
                        ServerMsg::Tile { x, y, data } => {
                            let t = Instant::now();
                            let r = dec.decode(&data);
                            let dms = t.elapsed().as_secs_f32() * 1000.0;
                            match r {
                                Ok(rgba) => {
                                    rec.decode(data.len(), dms, true);
                                    let _ = ev.send(Event::Tile {
                                        x,
                                        y,
                                        w: rgba.w,
                                        h: rgba.h,
                                        rgba: rgba.data,
                                        bytes: data.len(),
                                    });
                                }
                                Err(e) => {
                                    rec.decode(data.len(), dms, false);
                                    eprintln!("[net] AV1: {e}");
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("[net] Protokoll: {e}");
                    return "protokollfehler".into();
                }
            },
            Ok(Read1::Idle) => {}
            Err(_) => return "eof".into(), // EOF/Verbindung weg → Reconnect
        }
    }
}
