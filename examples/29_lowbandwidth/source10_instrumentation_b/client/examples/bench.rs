//! Prüfstand mit echten Daten: spielt den *Bildschirminhalt* einer
//! Server-Aufzeichnung (`.lbwlog`) als Quelle in eine echte `serve_client`-
//! Session und misst, was ein Client hinter einem gedrosselten Kanal sieht.
//!
//! - Quelle: Kacheln des Logs werden durch den echten AV1-Decoder auf ein
//!   1280×720-Canvas gemalt, Texte aus `AddText`/`RemoveText` nachgeführt —
//!   zeitrichtig nach der Mono-Uhr der Aufnahme (Fenster `--from`/`--secs`).
//! - OCR: Ersatz, liefert die zum Zeitpunkt sichtbaren Texte des Logs.
//! - Kanal: Der Prüfstand-Client liest höchstens `--budget` Byte/s
//!   (Token-Bucket). Was der Server schneller schreibt, staut sich in den
//!   Kernel-Puffern — genau wie im SSH-Tunnel.
//! - Messung: Text-Latenz = Ankunft eines Textes beim Client − Zeitpunkt,
//!   zu dem er in der Quelle erschien; dazu größte Nachricht und Rate.
//!
//! Dieselbe Datei baut gegen source10 (`tools/bench10`) und source10_b
//! (`cargo run --release -p lbw-client --example bench`) — echter A/B.
//!
//! `bench <log> [--from S] [--secs N] [--budget B/s] [--drain S] [-- <Server-Args>]`

use std::collections::HashMap;
use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use clap::Parser as _;
use image::RgbImage;
use lbw_client::av1::Decoder;
use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, HEIGHT, PROTO_VERSION, Rect, ServerMsg, TextItem, WIDTH};
use lbw_log::Recorder;
use lbw_log::io::load_lenient;
use lbw_log::legacy::decode_server_logged;
use lbw_log::record::{Dir, LogRecord};
use lbw_log::stats::log_version;
use lbw_server::capture::FrameSource;
use lbw_server::config::Config;
use lbw_server::input::Injector;
use lbw_server::session::{Recognize, serve_client};

/// Bildschirm-Zustand zur Zeit `t` der Aufnahme.
struct Player {
    events: Vec<(u64, ServerMsg)>,
    next: usize,
    canvas: RgbImage,
    texts: Vec<TextItem>,
    dec: Decoder,
    /// Aufnahme-Zeit (µs, Mono) des Fensterbeginns.
    t_from: u64,
    start: Option<Instant>,
    /// Erstes Erscheinen je (Box, Text) im Fenster, ms seit Start.
    appeared: HashMap<(Rect, String), f64>,
}

impl Player {
    fn apply(&mut self, m: ServerMsg, now_ms: Option<f64>) {
        match m {
            ServerMsg::ClearText => self.texts.clear(),
            ServerMsg::AddText(t) => {
                if let Some(ms) = now_ms {
                    self.appeared.entry((t.rect, t.text.clone())).or_insert(ms);
                }
                if let Some(e) = self.texts.iter_mut().find(|e| e.id == t.id) {
                    *e = t;
                } else {
                    self.texts.push(t);
                }
            }
            ServerMsg::RemoveText(id) => self.texts.retain(|e| e.id != id),
            ServerMsg::Tile { x, y, data } => {
                if let Ok(px) = self.dec.decode(&data) {
                    // Zeilenweise RGBA → RGB (auch im Debug-Build schnell genug).
                    let (x0, y0) = (usize::from(x), usize::from(y));
                    let w = px.w.min((WIDTH as usize).saturating_sub(x0));
                    let stride = WIDTH as usize * 3;
                    let raw: &mut [u8] = &mut self.canvas;
                    for row in 0..px.h.min((HEIGHT as usize).saturating_sub(y0)) {
                        let src = &px.data[row * px.w * 4..][..w * 4];
                        let dst = &mut raw[(y0 + row) * stride + x0 * 3..][..w * 3];
                        for (d, s) in dst
                            .as_chunks_mut::<3>()
                            .0
                            .iter_mut()
                            .zip(src.as_chunks::<4>().0)
                        {
                            d.copy_from_slice(&s[..3]);
                        }
                    }
                }
            }
            ServerMsg::Hello => {}
        }
    }

    /// Spielt alle Ereignisse bis „jetzt" (Fensterbeginn + verstrichene Zeit) ab.
    fn advance(&mut self) {
        let start = *self.start.get_or_insert_with(Instant::now);
        let el = start.elapsed();
        let now = self.t_from + el.as_micros() as u64;
        while self.next < self.events.len() && self.events[self.next].0 <= now {
            let m = self.events[self.next].1.clone();
            self.next += 1;
            self.apply(m, Some(el.as_secs_f64() * 1000.0));
        }
    }
}

#[derive(Clone)]
struct Src(Arc<Mutex<Player>>);

impl FrameSource for Src {
    fn grab(&mut self) -> Result<RgbImage, String> {
        let mut p = self.0.lock().unwrap();
        p.advance();
        Ok(p.canvas.clone())
    }
}

impl Recognize for Src {
    fn text(&mut self, _img: &RgbImage) -> Result<Vec<TextItem>, String> {
        let p = self.0.lock().unwrap();
        // Wie echte OCR: ohne IDs (vergibt die Session), nach Zeilen sortiert.
        let mut v: Vec<TextItem> = p
            .texts
            .iter()
            .map(|t| TextItem { id: 0, ..t.clone() })
            .collect();
        v.sort_by_key(|t| (t.rect.y / 16, t.rect.x));
        Ok(v)
    }
}

fn pct(v: &mut [f64], p: f64) -> f64 {
    if v.is_empty() {
        return f64::NAN;
    }
    v.sort_by(|a, b| a.total_cmp(b));
    v[((p / 100.0 * v.len() as f64) as usize).min(v.len() - 1)]
}

/// Kennzahlen eines Prüfstand-Laufs.
#[derive(Debug)]
pub struct Report {
    pub text_n: usize,
    pub text_p50: f64,
    pub text_p90: f64,
    pub text_max: f64,
    pub text_missing: usize,
    pub tiles: usize,
    pub max_msg: usize,
    pub bps: f64,
}

#[allow(dead_code)] // im Integrationstest (`tests/realdata.rs`) ungenutzt
fn main() {
    let raw: Vec<String> = std::env::args().skip(1).collect();
    run(&raw);
}

/// Führt den Prüfstand aus (Argumente wie die Kommandozeile) und druckt
/// das Ergebnis; liefert die Kennzahlen (für Tests).
pub fn run(raw: &[String]) -> Report {
    let split = raw.iter().position(|a| a == "--").unwrap_or(raw.len());
    let (own, srv_args) = (&raw[..split], raw.get(split + 1..).unwrap_or(&[]));
    let mut file = None;
    let (mut from, mut secs, mut budget, mut drain) = (0.0f64, 30.0f64, 6000.0f64, 15.0f64);
    let mut i = 0;
    while i < own.len() {
        let v = || own.get(i + 1).and_then(|s| s.parse::<f64>().ok());
        match own[i].as_str() {
            "--from" => from = v().expect("--from S"),
            "--secs" => secs = v().expect("--secs N"),
            "--budget" => budget = v().expect("--budget B"),
            "--drain" => drain = v().expect("--drain S"),
            f => {
                file = Some(f.to_owned());
                i += 1;
                continue;
            }
        }
        i += 2;
    }
    let file =
        file.expect("Aufruf: bench <log> [--from S] [--secs N] [--budget B] [-- Server-Args]");
    let (records, _) = load_lenient(&file).unwrap_or_else(|e| panic!("{e}"));
    let version = log_version(&records);
    let mut events = Vec::new();
    for r in &records {
        if let LogRecord::Msg {
            stamp,
            dir: Dir::SrvToCli,
            body,
            ..
        } = r
            && let Ok(m) = decode_server_logged(body, version)
        {
            events.push((stamp.mono_us, m));
        }
    }
    assert!(!events.is_empty(), "keine Server-Nachrichten in {file}");
    let t0 = events[0].0;
    let t_from = t0 + (from * 1e6) as u64;
    let mut player = Player {
        events,
        next: 0,
        canvas: RgbImage::from_pixel(WIDTH, HEIGHT, image::Rgb([24, 24, 32])),
        texts: Vec::new(),
        dec: Decoder::new().unwrap(),
        t_from,
        start: None,
        appeared: HashMap::new(),
    };
    // Vorspulen bis Fensterbeginn (ohne Erscheinungs-Zeiten).
    while player.next < player.events.len() && player.events[player.next].0 < t_from {
        let m = player.events[player.next].1.clone();
        player.next += 1;
        player.apply(m, None);
    }
    let shared = Arc::new(Mutex::new(player));

    let mut args = vec![
        "lbw-server".to_owned(),
        "--budget".into(),
        (budget as u32).to_string(),
    ];
    args.extend(srv_args.iter().cloned());
    let cfg = Config::try_parse_from(&args).unwrap_or_else(|e| panic!("{e}"));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let max_frames = ((secs + drain) * 10.0) as u64 + 50;
    let src = Src(shared.clone());
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let (mut s, mut o) = (src.clone(), src);
        serve_client(
            stream,
            &cfg,
            &mut s,
            &mut o,
            Some(max_frames),
            &Recorder::none(),
            None::<Injector>,
        )
    });

    let mut sock = TcpStream::connect(addr).unwrap();
    write_msg(
        &mut sock,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    sock.set_read_timeout(Some(Duration::from_millis(5)))
        .unwrap();
    let mut fr = FrameReader::new();
    let mut buf = Vec::new();
    let t_start = Instant::now();
    let (mut tokens, mut last) = (budget, Instant::now());
    let (mut bytes, mut tiles, mut max_msg, mut texts_rx) = (0usize, 0usize, 0usize, 0usize);
    let mut arrived: HashMap<(Rect, String), f64> = HashMap::new();
    let deadline = Duration::from_secs_f64(secs + drain);
    'outer: while t_start.elapsed() < deadline {
        // Token-Bucket: höchstens `budget` B/s vom Socket nehmen.
        let now = Instant::now();
        tokens = (tokens + now.duration_since(last).as_secs_f64() * budget).min(budget);
        last = now;
        if tokens < 1.0 {
            std::thread::sleep(Duration::from_millis(2));
            continue;
        }
        let want = (tokens as usize).min(4096);
        buf.resize(want, 0);
        let n = match sock.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => n,
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                ) =>
            {
                continue;
            }
            Err(_) => break,
        };
        tokens -= n as f64;
        let mut cur = std::io::Cursor::new(buf[..n].to_vec());
        // Cursor leer (EOF/Idle) → nächster gedrosselter Lesevorgang.
        while let Ok(Read1::Frame(b)) = fr.read(&mut cur) {
            bytes += b.len() + 4;
            max_msg = max_msg.max(b.len() + 4);
            let ms = t_start.elapsed().as_secs_f64() * 1000.0;
            match decode_msg::<ServerMsg>(&b) {
                Ok(ServerMsg::Tile { .. }) => tiles += 1,
                Ok(ServerMsg::AddText(t)) => {
                    texts_rx += 1;
                    arrived.entry((t.rect, t.text)).or_insert(ms);
                }
                Ok(_) => {}
                Err(_) => break 'outer,
            }
        }
    }
    let elapsed = t_start.elapsed().as_secs_f64();
    drop(sock);
    let _ = server.join();

    let appeared = shared.lock().unwrap().appeared.clone();
    let mut lat: Vec<f64> = Vec::new();
    let mut missing = 0;
    for (k, t_app) in &appeared {
        if *t_app > secs * 1000.0 {
            continue; // nach Fensterende erschienen — nicht gewertet
        }
        match arrived.get(k) {
            Some(t_rx) => lat.push((t_rx - t_app).max(0.0)),
            None => missing += 1,
        }
    }
    let n = lat.len();
    let report = Report {
        text_n: n,
        text_p50: pct(&mut lat, 50.0),
        text_p90: pct(&mut lat, 90.0),
        text_max: pct(&mut lat, 100.0),
        text_missing: missing,
        tiles,
        max_msg,
        bps: bytes as f64 / elapsed,
    };
    println!(
        "bench: {file} Fenster {from:.0}+{secs:.0}s, Kanal {budget:.0} B/s, Server-Args {srv_args:?}"
    );
    println!(
        "bench: Text-Latenz n={n} p50={:.0} p90={:.0} max={:.0} ms, fehlend {missing}",
        report.text_p50, report.text_p90, report.text_max
    );
    println!(
        "bench: {tiles} Kacheln, {texts_rx} AddText, größte Nachricht {max_msg} B, {bytes} B in {elapsed:.1} s ({:.0} B/s)",
        report.bps
    );
    report
}
