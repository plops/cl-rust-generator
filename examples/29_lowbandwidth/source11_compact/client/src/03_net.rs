//! `03_net` — Verbindung zum Server mit automatischem Reconnect.
//!
//! Ein Netz-Thread verbindet (Backoff 0,5 → 5 s), beweist das LBW-Token
//! (Challenge → Auth → Hello), liest Frames, dekodiert AV1-Kacheln und
//! reicht fertige Ereignisse an die UI. Ausgehende Eingaben fließen über
//! einen Kanal (max. ~10 ms Verzögerung). `drop(Net)` beendet Thread und
//! Verbindung. Stale Inputs aus der Offline-Phase werden beim Reconnect
//! verworfen. Debug-`--tap` spiegelt den Klartext-Strom auf localhost.

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{Receiver, Sender, channel};
use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, TextItem, auth_mac};

use crate::av1::Decoder;

/// Ereignisse an die UI.
#[derive(Debug)]
pub enum Event {
    Connected,
    Disconnected(String),
    ClearText,
    NewText(TextItem),
    /// Bekannter Inhalt an neuer Position (nur ID + Box).
    MoveText {
        id: u64,
        rect: lbw_common::Rect,
    },
    /// Text mit `id` entfernen (v4-Delta).
    RemoveText(u64),
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

/// Live-Metriken (eine Uhr: Input→sichtbar ohne Sync-Probleme).
#[derive(Clone, Debug, Default)]
pub struct Metrics {
    /// Empfangene Bytes (Leitung, inkl. Framing).
    pub rx_bytes: u64,
    /// Gesendete Bytes (Eingaben).
    pub tx_bytes: u64,
    /// Empfangene Text-Nachrichten.
    pub rx_texts: u64,
    /// Letzte diskrete Eingabe (Taste/Text/Button).
    pub last_input: Option<Instant>,
    /// Letzte sichtbare Text-Änderung.
    pub last_text_visible: Option<Instant>,
    /// Letzte sichtbare Kachel.
    pub last_tile_visible: Option<Instant>,
}

impl Metrics {
    /// Input→Text-Latenz (None = noch kein Paar).
    #[must_use]
    pub fn input_to_text_ms(&self) -> Option<f32> {
        match (self.last_input, self.last_text_visible) {
            (Some(a), Some(b)) if b >= a => Some((b - a).as_secs_f32() * 1000.0),
            _ => None,
        }
    }

    /// Input→Kachel-Latenz (None = noch kein Paar).
    #[must_use]
    pub fn input_to_tile_ms(&self) -> Option<f32> {
        match (self.last_input, self.last_tile_visible) {
            (Some(a), Some(b)) if b >= a => Some((b - a).as_secs_f32() * 1000.0),
            _ => None,
        }
    }
}

#[derive(Default)]
struct Shared {
    metrics: Mutex<Metrics>,
}

/// Griff der UI auf den Netz-Thread.
pub struct Net {
    pub events: Receiver<Event>,
    out: Sender<ClientMsg>,
    stop: Arc<AtomicBool>,
    shared: Arc<Shared>,
}

impl Drop for Net {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
    }
}

impl Net {
    /// Verbindet mit `addr` (Reconnect läuft im Hintergrund). `token` ist
    /// das LBW-Token, `tap` optionaler Debug-Spiegel (`127.0.0.1:<port>`).
    pub fn connect(addr: &str, token: &[u8], tap: Option<u16>) -> Self {
        let (ev_tx, events) = channel();
        let (out, out_rx) = channel();
        let stop = Arc::new(AtomicBool::new(false));
        let shared = Arc::new(Shared::default());
        std::thread::spawn({
            let addr = addr.to_owned();
            let token = token.to_vec();
            let stop = stop.clone();
            let shared = shared.clone();
            move || run(&addr, &token, tap, ev_tx, out_rx, &stop, &shared)
        });
        Self {
            events,
            out,
            stop,
            shared,
        }
    }

    /// Sendet eine Nachricht (geht verloren, wenn gerade keine Verbindung besteht).
    pub fn send(&self, m: ClientMsg) {
        if matches!(
            m,
            ClientMsg::Text(_) | ClientMsg::Key { .. } | ClientMsg::Button { .. }
        ) {
            self.shared.metrics.lock().unwrap().last_input = Some(Instant::now());
        }
        let _ = self.out.send(m);
    }

    /// Aktuelle Metrik-Momentaufnahme (HUD).
    #[must_use]
    pub fn metrics(&self) -> Metrics {
        self.shared.metrics.lock().unwrap().clone()
    }
}

fn metric(shared: &Shared) -> MutexGuard<'_, Metrics> {
    shared.metrics.lock().unwrap()
}

fn run(
    addr: &str,
    token: &[u8],
    tap: Option<u16>,
    ev: Sender<Event>,
    out: Receiver<ClientMsg>,
    stop: &AtomicBool,
    shared: &Shared,
) {
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
                // Stale Inputs aus der Offline-Phase verwerfen (kämen sonst
                // Minuten zu spät an).
                while out.try_recv().is_ok() {}
                let why = session(
                    s,
                    Ctx {
                        token,
                        tap,
                        ev: &ev,
                        out: &out,
                        stop,
                        dec: &mut decoder,
                        shared,
                    },
                );
                if stop.load(Ordering::Relaxed) {
                    break;
                }
                let _ = ev.send(Event::Disconnected(why));
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

/// Alles, was eine Session zum Laufen braucht (gegen 7-Argument-Lint).
struct Ctx<'a> {
    token: &'a [u8],
    tap: Option<u16>,
    ev: &'a Sender<Event>,
    out: &'a Receiver<ClientMsg>,
    stop: &'a AtomicBool,
    dec: &'a mut Decoder,
    shared: &'a Shared,
}

fn session(s: TcpStream, ctx: Ctx<'_>) -> String {
    let Ctx {
        token,
        tap,
        ev,
        out,
        stop,
        dec,
        shared,
    } = ctx;
    // Nagle aus: 10-Byte-Eingaben warten nicht auf Paketfüllung.
    // (Fehler nur melden — die Verbindung ist wichtiger als das Flag.)
    if let Err(e) = s.set_nodelay(true) {
        eprintln!("[net] nodelay: {e}");
    }
    if s.set_read_timeout(Some(Duration::from_millis(10))).is_err() {
        return "timeout-Flag".into();
    }
    let mut rd = match s.try_clone() {
        Ok(r) => r,
        Err(_) => return "clone".into(),
    };
    let mut wr = s;
    // Debug-Tap (Default aus): Klartext-Spiegel für Wireshark.
    if let Some(port) = tap {
        eprintln!("[net] WARNUNG: Klartext-Tap auf 127.0.0.1:{port} (nur Debug!)");
    }
    let mut fr = FrameReader::new();
    // Auth: Challenge lesen → Auth + Hello senden → Server-Hello lesen.
    let nonce = loop {
        match fr.read(&mut rd) {
            Ok(Read1::Frame(b)) => match decode_msg::<ServerMsg>(&b) {
                Ok(ServerMsg::Challenge { version, nonce }) => {
                    if version != PROTO_VERSION {
                        return "falsche Version".into();
                    }
                    metric(shared).rx_bytes += (b.len() + 4) as u64;
                    break nonce;
                }
                Ok(_) => return "kein Challenge".into(),
                Err(_) => return "protokollfehler".into(),
            },
            Ok(Read1::Idle) => {
                if stop.load(Ordering::Relaxed) {
                    return "stop".into();
                }
            }
            Err(_) => return "eof".into(),
        }
    };
    let auth = ClientMsg::Auth {
        mac: auth_mac(token, &nonce),
    };
    let hello = ClientMsg::Hello {
        version: PROTO_VERSION,
    };
    match write_msg(&mut wr, &auth).and_then(|a| write_msg(&mut wr, &hello).map(|b| (a, b))) {
        Ok((a, b)) => metric(shared).tx_bytes += (a + b) as u64,
        Err(_) => return "auth-sendefehler".into(),
    }
    if let Some(port) = tap {
        tap_send(port, &auth);
        tap_send(port, &hello);
    }
    loop {
        if stop.load(Ordering::Relaxed) {
            return "stop".into();
        }
        while let Ok(m) = out.try_recv() {
            match write_msg(&mut wr, &m) {
                Ok(n) => {
                    metric(shared).tx_bytes += n as u64;
                    if let Some(port) = tap {
                        tap_send(port, &m);
                    }
                }
                Err(_) => return "sendefehler".into(),
            }
        }
        match fr.read(&mut rd) {
            Ok(Read1::Frame(b)) => match decode_msg::<ServerMsg>(&b) {
                Ok(m) => {
                    metric(shared).rx_bytes += (b.len() + 4) as u64;
                    if let Some(port) = tap {
                        tap_send(port, &m);
                    }
                    match m {
                        ServerMsg::Challenge { .. } => {
                            return "doppelte Challenge".into();
                        }
                        ServerMsg::Hello => {
                            let _ = ev.send(Event::Connected);
                        }
                        ServerMsg::ClearText => {
                            let _ = ev.send(Event::ClearText);
                        }
                        ServerMsg::NewText(t) => {
                            metric(shared).rx_texts += 1;
                            metric(shared).last_text_visible = Some(Instant::now());
                            let _ = ev.send(Event::NewText(t));
                        }
                        ServerMsg::MoveText { id, rect } => {
                            metric(shared).last_text_visible = Some(Instant::now());
                            let _ = ev.send(Event::MoveText { id, rect });
                        }
                        ServerMsg::RemoveText(id) => {
                            let _ = ev.send(Event::RemoveText(id));
                        }
                        ServerMsg::Tile { x, y, data } => {
                            let t = Instant::now();
                            let r = dec.decode(&data);
                            let _dms = t.elapsed().as_secs_f32() * 1000.0;
                            match r {
                                Ok(rgba) => {
                                    metric(shared).last_tile_visible = Some(Instant::now());
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

/// Best-Effort-Spiegel einer Nachricht an den lokalen Tap-Port
/// (beide Richtungen — der Tap zeigt den kompletten Klartext-Strom).
fn tap_send(port: u16, m: &impl serde::Serialize) {
    if let Ok(mut s) = TcpStream::connect(("127.0.0.1", port)) {
        let _ = write_msg(&mut s, m);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metrics_pair_input_with_visibility() {
        let mut m = Metrics::default();
        assert_eq!(m.input_to_text_ms(), None);
        let t0 = Instant::now();
        m.last_input = Some(t0);
        m.last_text_visible = Some(t0 + Duration::from_millis(150));
        m.last_tile_visible = Some(t0 + Duration::from_millis(600));
        assert!((m.input_to_text_ms().unwrap() - 150.0).abs() < 1.0);
        assert!((m.input_to_tile_ms().unwrap() - 600.0).abs() < 1.0);
    }

    #[test]
    fn metrics_ignores_reordered_stamps() {
        let t0 = Instant::now();
        let m = Metrics {
            last_input: Some(t0 + Duration::from_millis(10)),
            last_text_visible: Some(t0),
            ..Default::default()
        };
        assert_eq!(m.input_to_text_ms(), None);
    }
}
