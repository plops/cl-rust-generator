//! `12_scheduler` — Sendereihenfolge und Drosselung für die schmale Leitung.
//!
//! Priorität: Kontrolle (Hello/Ping/Pong/Stats) > Text > Bildstücke.
//! Kacheln gehen als `TileStart` + `TileData`-Stücke (≤ 512 B), sodass Text
//! zwischen zwei Stücken überholt. Zwei Bremsen gegen volle Puffer:
//! - Token-Bucket: höchstens `rate` Byte/s (Default 6000).
//! - Ack-Fenster: `gesendet − quittiert ≤ Fenster`. Ist die echte Leitung
//!   langsamer als `rate`, stauen sich Daten sonst in TCP/SSH und Text
//!   wartet Sekunden (Bufferbloat).

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use lbw_common::frame::HEADER;
use lbw_common::rate::{RateMeter, TokenBucket};
use lbw_common::{CHUNK, Rect, ServerMsg};

/// Kleinstes Fenster (Byte) — muss größer als die Ack-Granularität sein.
pub const WINDOW_MIN: usize = 2048;

/// Eine AV1-Kachel in der Warteschlange.
#[derive(Debug)]
struct Tile {
    id: u32,
    seq: u32,
    rect: Rect,
    data: Vec<u8>,
    offset: usize,
    started: bool,
}

/// Entscheidung des Schedulers.
#[derive(Debug, PartialEq, Eq)]
pub enum Next {
    /// Diesen Body jetzt senden.
    Send(Vec<u8>),
    /// Frühestens nach dieser Zeit erneut fragen (oder bei neuem Ereignis).
    Wait(Duration),
}

/// Reine Scheduler-Logik (Zeit wird übergeben → deterministisch testbar).
pub struct Scheduler {
    control: VecDeque<ServerMsg>,
    text: VecDeque<(u32, ServerMsg)>,
    tiles: VecDeque<Tile>,
    bucket: TokenBucket,
    next_tile: u32,
    tx: u64,
    acked: u64,
    rtt_min: Option<Duration>,
    /// `seq` der zuletzt vollständig gesendeten Nachricht.
    pub sent_seq: u32,
    pub tiles_sent: u32,
    /// Generation der Verbindung, für die gerade gesendet wird.
    pub conn: u64,
    meter: RateMeter,
}

impl Scheduler {
    #[must_use]
    pub fn new(rate: u32, now: Instant) -> Self {
        Self {
            control: VecDeque::new(),
            text: VecDeque::new(),
            tiles: VecDeque::new(),
            bucket: TokenBucket::new(rate, (rate / 6).max(CHUNK as u32 * 2), now),
            next_tile: 1,
            tx: 0,
            acked: 0,
            rtt_min: None,
            sent_seq: 0,
            tiles_sent: 0,
            conn: 0,
            meter: RateMeter::new(Duration::from_secs(5)),
        }
    }

    pub fn push_control(&mut self, m: ServerMsg) {
        self.control.push_back(m);
    }

    /// Text-Delta mit Sequenznummer einreihen.
    pub fn push_text(&mut self, seq: u32, m: ServerMsg) {
        self.text.push_back((seq, m));
    }

    /// AV1-Kachel einreihen; liefert die Kachel-ID.
    pub fn push_tile(&mut self, seq: u32, rect: Rect, data: Vec<u8>) -> u32 {
        let id = self.next_tile;
        self.next_tile = self.next_tile.wrapping_add(1).max(1);
        self.tiles.push_back(Tile {
            id,
            seq,
            rect,
            data,
            offset: 0,
            started: false,
        });
        id
    }

    /// Noch nicht gesendete Bildbytes (Grundlage der adaptiven Bildrate).
    #[must_use]
    pub fn image_backlog(&self) -> usize {
        self.tiles.iter().map(|t| t.data.len() - t.offset).sum()
    }

    /// Text- oder Bilddaten in der Warteschlange?
    #[must_use]
    pub fn is_idle(&self) -> bool {
        self.text.is_empty() && self.tiles.is_empty()
    }

    /// Neue Verbindung: Zähler zurücksetzen. `keep` = Warteschlangen behalten
    /// (Resume), sonst verwerfen (Voll-Refresh folgt).
    pub fn reset(&mut self, keep: bool, conn: u64) {
        self.conn = conn;
        self.control.clear();
        self.tx = 0;
        self.acked = 0;
        if keep {
            for t in &mut self.tiles {
                t.offset = 0;
                t.started = false;
            }
        } else {
            self.text.clear();
            self.tiles.clear();
        }
    }

    pub fn ack(&mut self, rx_bytes: u64) {
        self.acked = rx_bytes.clamp(self.acked, self.tx);
    }

    /// RTT-Messung (Ping/Pong). Das Minimum ist robust gegen Warteschlangen.
    pub fn rtt(&mut self, rtt: Duration) {
        self.rtt_min = Some(self.rtt_min.map_or(rtt, |m| m.min(rtt)));
    }

    /// Aktuelles Fenster: `rate × (rtt_min + 200 ms)`, begrenzt auf 1 s Daten.
    #[must_use]
    pub fn window(&self) -> usize {
        let rate = f64::from(self.bucket.rate());
        let rtt = self
            .rtt_min
            .unwrap_or(Duration::from_millis(100))
            .as_secs_f64();
        ((rate * (rtt + 0.2)) as usize).clamp(WINDOW_MIN, (rate as usize).max(WINDOW_MIN))
    }

    #[must_use]
    pub fn in_flight(&self) -> usize {
        (self.tx - self.acked) as usize
    }

    /// Gemessene Senderate (Byte/s, 5-s-Fenster).
    #[must_use]
    pub fn tx_rate(&self, now: Instant) -> u32 {
        self.meter.rate(now) as u32
    }

    fn account(&mut self, body: &[u8], now: Instant, limited: bool) {
        let n = body.len() + HEADER;
        self.tx += n as u64;
        self.meter.add(n, now);
        if limited {
            self.bucket.take(n, now);
        }
    }

    /// Nächster Kandidat (ohne Entnahme) als Body.
    fn peek(&self) -> Option<Vec<u8>> {
        if let Some((_, m)) = self.text.front() {
            return Some(m.encode());
        }
        let t = self.tiles.front()?;
        Some(if t.started {
            let end = (t.offset + CHUNK).min(t.data.len());
            ServerMsg::TileData {
                tile_id: t.id,
                offset: t.offset as u32,
                data: t.data[t.offset..end].to_vec(),
            }
            .encode()
        } else {
            ServerMsg::TileStart {
                tile_id: t.id,
                seq: t.seq,
                rect: t.rect,
                len: t.data.len() as u32,
            }
            .encode()
        })
    }

    /// Entnimmt den gepeekten Kandidaten.
    fn commit(&mut self) {
        if let Some((seq, _)) = self.text.pop_front() {
            self.sent_seq = seq;
            return;
        }
        let Some(t) = self.tiles.front_mut() else {
            return;
        };
        if t.started {
            t.offset = (t.offset + CHUNK).min(t.data.len());
        } else {
            t.started = true;
        }
        if t.started && t.offset == t.data.len() {
            self.sent_seq = t.seq;
            self.tiles_sent += 1;
            self.tiles.pop_front();
        }
    }

    /// Was jetzt gesendet werden darf.
    pub fn next(&mut self, now: Instant) -> Next {
        if let Some(m) = self.control.pop_front() {
            let b = m.encode();
            self.account(&b, now, false);
            return Next::Send(b);
        }
        let Some(body) = self.peek() else {
            return Next::Wait(Duration::from_secs(3600));
        };
        let n = body.len() + HEADER;
        let flight = self.in_flight();
        if flight > 0 && flight + n > self.window() {
            // Erst Acks abwarten (Ereignis weckt), Poll als Rückfall.
            return Next::Wait(Duration::from_millis(50));
        }
        let w = self.bucket.wait(n, now);
        if w > Duration::ZERO {
            return Next::Wait(w);
        }
        self.commit();
        self.account(&body, now, true);
        Next::Send(body)
    }
}

/// Thread-sicherer Postausgang: Pipeline/Reader schieben, Writer entnimmt.
#[derive(Clone)]
pub struct Outbox(Arc<(Mutex<Scheduler>, Condvar)>);

impl Outbox {
    #[must_use]
    pub fn new(rate: u32) -> Self {
        Self(Arc::new((
            Mutex::new(Scheduler::new(rate, Instant::now())),
            Condvar::new(),
        )))
    }

    /// Zugriff auf den Scheduler; weckt danach den Writer.
    pub fn with<T>(&self, f: impl FnOnce(&mut Scheduler) -> T) -> T {
        let r = f(&mut self.0.0.lock().unwrap());
        self.0.1.notify_all();
        r
    }

    /// Blockiert bis ein Body für Verbindung `conn` gesendet werden darf
    /// oder `max` verstrichen ist. Eine abgelöste Verbindung bekommt nichts
    /// mehr (sonst könnte ihr Writer Nachrichten der neuen verschlucken).
    pub fn next_blocking(&self, max: Duration, conn: u64) -> Option<Vec<u8>> {
        let deadline = Instant::now() + max;
        let mut s = self.0.0.lock().unwrap();
        loop {
            let now = Instant::now();
            if s.conn != conn {
                return None;
            }
            match s.next(now) {
                Next::Send(b) => return Some(b),
                Next::Wait(_) if now >= deadline => return None,
                Next::Wait(d) => {
                    let d = d.min(deadline - now);
                    s = self.0.1.wait_timeout(s, d).unwrap().0;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn text(seq: u32) -> ServerMsg {
        ServerMsg::Text {
            seq,
            remove: vec![],
            add: vec![],
        }
    }

    /// Lässt den Scheduler `secs` Sekunden lang laufen; Acks sofort.
    fn drain(s: &mut Scheduler, t0: Instant, secs: f64) -> (Vec<ServerMsg>, usize) {
        let (mut now, mut out, mut bytes) = (t0, Vec::new(), 0);
        while now < t0 + Duration::from_secs_f64(secs) {
            match s.next(now) {
                Next::Send(b) => {
                    bytes += b.len() + HEADER;
                    out.push(ServerMsg::decode(&b).unwrap());
                    s.ack(s.tx);
                }
                Next::Wait(d) => now += d.min(Duration::from_millis(10)),
            }
        }
        (out, bytes)
    }

    #[test]
    fn tile_is_chunked_and_text_overtakes() {
        let t0 = Instant::now();
        let mut s = Scheduler::new(6000, t0);
        s.push_tile(1, Rect::new(0, 0, 64, 64), vec![7; 3000]);
        // Zwei Stücke senden lassen, dann kommt Text.
        let mut now = t0;
        let mut first = Vec::new();
        while first.len() < 3 {
            match s.next(now) {
                Next::Send(b) => first.push(ServerMsg::decode(&b).unwrap()),
                Next::Wait(d) => now += d,
            }
            s.ack(s.tx);
        }
        assert!(matches!(first[0], ServerMsg::TileStart { len: 3000, .. }));
        s.push_text(2, text(2));
        let (rest, _) = drain(&mut s, now, 2.0);
        assert!(matches!(rest[0], ServerMsg::Text { seq: 2, .. }));
        let data: usize = first
            .iter()
            .chain(&rest)
            .map(|m| match m {
                ServerMsg::TileData { data, .. } => data.len(),
                _ => 0,
            })
            .sum();
        assert_eq!(data, 3000);
        assert_eq!((s.sent_seq, s.tiles_sent, s.image_backlog()), (1, 1, 0));
    }

    #[test]
    fn rate_is_respected() {
        let t0 = Instant::now();
        let mut s = Scheduler::new(6000, t0);
        for i in 0..20 {
            s.push_tile(i, Rect::new(0, 0, 16, 16), vec![0; 5000]);
        }
        let (_, bytes) = drain(&mut s, t0, 10.0);
        let rate = bytes as f64 / 10.0;
        assert!((5500.0..=6600.0).contains(&rate), "{rate}");
    }

    #[test]
    fn window_blocks_without_acks_but_control_passes() {
        let t0 = Instant::now();
        let mut s = Scheduler::new(100_000, t0);
        s.push_tile(1, Rect::new(0, 0, 16, 16), vec![0; 50_000]);
        let mut now = t0;
        for _ in 0..1000 {
            if let Next::Wait(d) = s.next(now) {
                now += d;
            }
        }
        assert!(
            s.in_flight() <= s.window() + CHUNK + 16,
            "{}",
            s.in_flight()
        );
        s.push_control(ServerMsg::Ping { t: 1 });
        assert!(matches!(s.next(now), Next::Send(_)));
        s.ack(s.tx);
        assert!(matches!(s.next(now), Next::Send(_)));
    }

    #[test]
    fn window_uses_min_rtt() {
        let mut s = Scheduler::new(6000, Instant::now());
        assert_eq!(s.window(), 2048);
        s.rtt(Duration::from_millis(900));
        assert_eq!(s.window(), 6000); // gedeckelt auf 1 s
        s.rtt(Duration::from_millis(300));
        assert_eq!(s.window(), 3000);
    }

    #[test]
    fn reset_keeps_or_drops_queues() {
        let t0 = Instant::now();
        let mut s = Scheduler::new(6000, t0);
        s.push_tile(1, Rect::new(0, 0, 16, 16), vec![0; 2000]);
        s.push_text(2, text(2));
        let _ = s.next(t0);
        s.reset(true, 1);
        assert_eq!(s.image_backlog(), 2000);
        assert_eq!(s.in_flight(), 0);
        s.reset(false, 2);
        assert!(s.is_idle());
    }

    #[test]
    fn outbox_blocks_until_timeout_when_empty() {
        let o = Outbox::new(6000);
        assert!(o.next_blocking(Duration::from_millis(30), 0).is_none());
        o.with(|s| s.push_control(ServerMsg::Clear));
        assert!(
            o.next_blocking(Duration::from_millis(30), 7).is_none(),
            "fremde Generation"
        );
        assert_eq!(o.next_blocking(Duration::from_millis(30), 0), Some(vec![5]));
    }
}
