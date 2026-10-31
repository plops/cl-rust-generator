//! `12_send` — Sender-Thread mit zwei Spuren (Express + Kacheln).
//!
//! Review B2: In source10 schlief die Session-Schleife selbst im Pacer —
//! während eine große Kachel auf Kanalzeit wartete (bis 6 s), gab es keine
//! OCR und keine Text-Deltas. Hier besitzt ein eigener Thread Socket und
//! Pacer. Regeln:
//!
//! 1. **Express** (Hello/ClearText/AddText/RemoveText) geht sofort raus und
//!    bucht nur (`Pacer::note`).
//! 2. **Kacheln** gehen erst, wenn der Pacer sie ohne Wartezeit erlaubt.
//!    Während der Wartezeit schaut der Thread alle ≤ 10 ms nach Express —
//!    ein Text überholt also auch eine *wartende* Kachel.
//! 3. Die Session fragt [`Sender::tile_lane_free`], bevor sie überhaupt
//!    kodiert (Review B3: nichts kodieren, was ohnehin warten müsste).
//!
//! Recorder-`Msg`-Records entstehen hier, zum **echten** Schreibzeitpunkt.

use std::collections::VecDeque;
use std::io::Write;
use std::net::{Shutdown, TcpStream};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use lbw_common::framing::{HEADER, encode_msg};
use lbw_common::{MAX_MSG, ServerMsg};
use lbw_log::Recorder;

use crate::pace::Pacer;

/// Max. Schlafscheibe, während eine Kachel auf Guthaben wartet.
const POLL: Duration = Duration::from_millis(10);

/// Eine kodierte, sendefertige Nachricht.
struct Out {
    msg: ServerMsg,
    frame: Vec<u8>,
}

#[derive(Default)]
struct Lanes {
    express: VecDeque<Out>,
    tiles: VecDeque<Out>,
    closed: bool,
    dead: bool,
}

struct Shared {
    lanes: Mutex<(Lanes, Pacer)>,
    cv: Condvar,
}

/// Griff der Session auf den Sender-Thread. `Drop` beendet ihn (offene
/// Nachrichten verfallen — die Verbindung ist dann ohnehin vorbei).
pub struct Sender {
    shared: Arc<Shared>,
    sock: TcpStream,
    handle: Option<JoinHandle<()>>,
}

/// Rahmt eine Nachricht (`[u32 LE][bincode]`) — wie `framing::write_msg`,
/// aber vorab, damit der Thread die Leitungsbytes vor dem Senden kennt.
fn frame(msg: ServerMsg) -> Result<Out, String> {
    let body = encode_msg(&msg)?;
    if body.len() > MAX_MSG {
        return Err("Nachricht zu groß".into());
    }
    let mut frame = Vec::with_capacity(HEADER + body.len());
    frame.extend_from_slice(&(body.len() as u32).to_le_bytes());
    frame.extend_from_slice(&body);
    Ok(Out { msg, frame })
}

impl Sender {
    /// Startet den Thread auf einem Klon von `sock`; `pacer` ist der
    /// Pacer der Verbindung (Hello ist schon gebucht).
    pub fn spawn(sock: &TcpStream, pacer: Pacer, rec: Recorder) -> Result<Self, String> {
        let mut wr = sock.try_clone().map_err(|e| e.to_string())?;
        let shared = Arc::new(Shared {
            lanes: Mutex::new((Lanes::default(), pacer)),
            cv: Condvar::new(),
        });
        let s = shared.clone();
        let handle = std::thread::spawn(move || run(&s, &mut wr, &rec));
        Ok(Self {
            shared,
            sock: sock.try_clone().map_err(|e| e.to_string())?,
            handle: Some(handle),
        })
    }

    fn push(&self, out: Out, express: bool) -> usize {
        let n = out.frame.len();
        let mut g = self.shared.lanes.lock().unwrap();
        if express {
            g.0.express.push_back(out);
        } else {
            g.0.tiles.push_back(out);
        }
        drop(g);
        self.shared.cv.notify_all();
        n
    }

    /// Reiht eine Text-/Steuernachricht in die Express-Spur; liefert die
    /// Leitungsbytes (0 bei Kodierfehler — der zählt nicht als Abriss).
    pub fn express(&self, msg: ServerMsg) -> usize {
        frame(msg).map_or(0, |o| self.push(o, true))
    }

    /// Reiht eine Kachel in die gepacete Spur; liefert die Leitungsbytes.
    pub fn tile(&self, msg: ServerMsg) -> usize {
        frame(msg).map_or(0, |o| self.push(o, false))
    }

    /// Kachel-Spur leer und Restschuld des Pacers ≤ `lead`? Dann darf die
    /// Session die nächste Kachel kodieren (Encode läuft im Vorlauf).
    #[must_use]
    pub fn tile_lane_free(&self, lead: Duration) -> bool {
        let g = self.shared.lanes.lock().unwrap();
        g.0.tiles.is_empty() && g.1.wait_for_at(0, Instant::now()) <= lead
    }

    /// Schreibfehler aufgetreten (Client weg)?
    #[must_use]
    pub fn is_dead(&self) -> bool {
        self.shared.lanes.lock().unwrap().0.dead
    }
}

impl Drop for Sender {
    fn drop(&mut self) {
        self.shared.lanes.lock().unwrap().0.closed = true;
        self.shared.cv.notify_all();
        // Ein `write` kann bei vollem Kernel-Puffer (stehender Tunnel)
        // ewig blockieren — Shutdown löst ihn, danach ist Join sicher.
        let _ = self.sock.shutdown(Shutdown::Write);
        if let Some(h) = self.handle.take() {
            let _ = h.join();
        }
    }
}

/// Thread-Schleife: Express zuerst, Kachel bei Guthaben, sonst warten.
fn run(s: &Shared, wr: &mut TcpStream, rec: &Recorder) {
    let mut g = s.lanes.lock().unwrap();
    loop {
        if g.0.closed {
            return;
        }
        let out = if let Some(o) = g.0.express.pop_front() {
            o
        } else if let Some(o) = g.0.tiles.front() {
            let wait = g.1.wait_for_at(o.frame.len(), Instant::now());
            if !wait.is_zero() {
                g = s.cv.wait_timeout(g, wait.min(POLL)).unwrap().0;
                continue;
            }
            g.0.tiles.pop_front().unwrap()
        } else {
            g = s.cv.wait(g).unwrap();
            continue;
        };
        let n = out.frame.len();
        g.1.note(n);
        drop(g); // Schreiben ohne Lock: Session reiht derweil weiter ein
        let ok = wr.write_all(&out.frame).is_ok();
        if ok {
            rec.msg_server(&out.msg, n);
        }
        g = s.lanes.lock().unwrap();
        if !ok {
            g.0.dead = true;
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lbw_common::framing::FrameReader;
    use std::net::TcpListener;

    fn pair() -> (TcpStream, TcpStream) {
        let l = TcpListener::bind("127.0.0.1:0").unwrap();
        let c = TcpStream::connect(l.local_addr().unwrap()).unwrap();
        (l.accept().unwrap().0, c)
    }

    fn tile(n: usize) -> ServerMsg {
        ServerMsg::Tile {
            x: 0,
            y: 0,
            data: vec![7; n],
        }
    }

    #[test]
    fn express_overtakes_a_waiting_tile() {
        let (srv, mut cli) = pair();
        // 1000 B/s, voller Eimer 1000 B: Kachel 1 (900 B) sofort, Kachel 2
        // (900 B) wartet ~0,8 s — der Text kommt trotzdem dazwischen.
        let tx = Sender::spawn(&srv, Pacer::new(1000), Recorder::none()).unwrap();
        tx.tile(tile(900));
        tx.tile(tile(900));
        std::thread::sleep(Duration::from_millis(50));
        assert!(!tx.tile_lane_free(Duration::ZERO));
        tx.express(ServerMsg::RemoveText(5));
        let mut fr = FrameReader::new();
        cli.set_read_timeout(Some(Duration::from_secs(3))).unwrap();
        let mut kinds = Vec::new();
        let t0 = Instant::now();
        while kinds.len() < 3 {
            if let Some(m) = fr.read_msg::<ServerMsg>(&mut cli).unwrap() {
                kinds.push((matches!(m, ServerMsg::Tile { .. }), t0.elapsed()));
            }
        }
        assert_eq!(
            kinds.iter().map(|k| k.0).collect::<Vec<_>>(),
            vec![true, false, true],
            "Text muss vor der wartenden Kachel kommen"
        );
        assert!(kinds[1].1 < Duration::from_millis(300), "{:?}", kinds[1]);
        assert!(kinds[2].1 >= Duration::from_millis(600), "{:?}", kinds[2]);
    }

    #[test]
    fn tile_bigger_than_bucket_is_not_starved() {
        let (srv, mut cli) = pair();
        // 1000 B/s, 3000-B-Kachel: früher nie fällig (Eimer max 1000 B).
        let tx = Sender::spawn(&srv, Pacer::new(1000), Recorder::none()).unwrap();
        tx.tile(tile(3000));
        cli.set_read_timeout(Some(Duration::from_secs(3))).unwrap();
        let m = FrameReader::new().read_msg::<ServerMsg>(&mut cli).unwrap();
        assert!(matches!(m, Some(ServerMsg::Tile { .. })));
        // Danach ~2 s Schuld: Spur gilt als belegt.
        assert!(!tx.tile_lane_free(Duration::from_millis(250)));
    }

    #[test]
    fn lane_frees_after_drain_and_write_errors_mark_dead() {
        let (srv, cli) = pair();
        let tx = Sender::spawn(&srv, Pacer::new(100_000), Recorder::none()).unwrap();
        tx.tile(tile(100));
        let t0 = Instant::now();
        while !tx.tile_lane_free(Duration::ZERO) {
            assert!(t0.elapsed() < Duration::from_secs(2));
            std::thread::sleep(Duration::from_millis(5));
        }
        drop(cli);
        // Nach dem Abriss scheitert spätestens ein Folge-Write (RST).
        let t0 = Instant::now();
        while !tx.is_dead() {
            assert!(t0.elapsed() < Duration::from_secs(5), "nie tot");
            tx.express(ServerMsg::RemoveText(1));
            std::thread::sleep(Duration::from_millis(20));
        }
    }
}
