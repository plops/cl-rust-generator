//! Pacing-/Prioritäts-Loopbacks (aus `loopback.rs` ausgelagert, 600-Zeilen-
//! Regel): Texte vor Kacheln, Kachel-Deckel, Budget unter Dauerlast.

mod common;
use common::*;

use std::net::{TcpListener, TcpStream};

use std::time::{Duration, Instant};

use clap::Parser;

use lbw_common::framing::{FrameReader, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg};
use lbw_log::io::{Reader, Recorder};
use lbw_log::{Dir, LogRecord};
use lbw_server::capture::{SharedSource, solid};
use lbw_server::input::Injector;
use lbw_server::session::serve_client;

fn cfg_budget(budget: u32) -> lbw_server::config::Config {
    lbw_server::config::Config::try_parse_from(["lbw-server", "--budget", &budget.to_string()])
        .unwrap()
}

/// Texte überholen Kacheln: Bei 1 B/s Budget kommt kein Tile je an,
/// aber ClearText + AddText treffen sofort ein (Vorfahrt ohne Warten).
#[test]
fn texts_jump_the_tile_queue() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = SharedSource::new(solid(128, 128, [40; 3]));
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &cfg_budget(1),
            &mut src,
            &mut ocr,
            Some(30),
            &Recorder::none(),
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    // Hello abwarten (ungepaced, winzig).
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::Hello),
    );
    // 2,5 s sammeln: Texte müssen da sein, ein Tile (≥ ~50 B bei
    // 1 B/s → ≥ ~50 s Wartezeit) darf es nicht geben.
    let (mut clear, mut adds, mut tiles) = (0, 0, 0);
    let until = Instant::now() + Duration::from_millis(2500);
    s.set_read_timeout(Some(Duration::from_millis(200)))
        .unwrap();
    while Instant::now() < until {
        match fr.read(&mut s).unwrap() {
            lbw_common::framing::Read1::Frame(b) => match decode_msg::<ServerMsg>(&b).unwrap() {
                ServerMsg::ClearText => clear += 1,
                ServerMsg::AddText(t) => {
                    assert_eq!(t.text, "hi");
                    adds += 1;
                }
                ServerMsg::Tile { .. } => tiles += 1,
                _ => {}
            },
            lbw_common::framing::Read1::Idle => {}
        }
    }
    assert_eq!((clear, adds, tiles), (1, 1, 0));
    // Abbruch: Reader sieht EOF, Pacing bricht in Scheiben ab.
    drop(s);
    server.join().unwrap().unwrap();
}

/// Gepacete Kacheln kommen vollständig und in Reihenfolge an
/// (Pacing dosiert, verwirft nichts).
#[test]
fn paced_tiles_arrive_whole() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let worker_src = shared.clone();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = worker_src;
        let mut ocr = StubOcr::fixed(vec![]);
        serve_client(
            stream,
            &cfg_budget(3000),
            &mut src,
            &mut ocr,
            Some(30),
            &Recorder::none(),
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::Hello),
    );
    // Vollbild-Kachel + eine Dirty-Kachel danach, beide ganz. Erst das
    // Vollbild abwarten, dann ändern (sonst kann die Änderung schon im
    // ersten Grab stecken — dann gäbe es nur eine Kachel).
    let mut got = Vec::new();
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::Tile { x, y, data } = m {
                got.push((*x, *y, data.len()));
            }
            !got.is_empty()
        },
    );
    let mut img = solid(128, 128, [40; 3]);
    img.put_pixel(100, 10, image::Rgb([9; 3]));
    shared.set(img);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(20),
        &mut |m| {
            if let ServerMsg::Tile { x, y, data } = m {
                assert!(!data.is_empty());
                got.push((*x, *y, data.len()));
            }
            got.len() >= 2
        },
    );
    assert_eq!(got.len(), 2, "{got:?}");
    assert_eq!((got[0].0, got[0].1), (0, 0)); // Vollbild zuerst
    drop(s);
    server.join().unwrap().unwrap();
}

/// Deterministisches Rauschbild (Xorshift, feste Saat) als
/// Animations-Ersatz: Vollbild-Dirty pro Frame.
fn noise(w: u32, h: u32, seed: u64) -> image::RgbImage {
    let mut img = image::RgbImage::new(w, h);
    let mut x = seed.max(1);
    for p in img.pixels_mut() {
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        let b = (x & 0xff) as u8;
        *p = image::Rgb([b, b.wrapping_mul(7), b.wrapping_add(31)]);
    }
    img
}

/// Animations-Soak: 12 Rausch-Vollbilder bei 6 kB/s Budget müssen im
/// Mittel im Budget bleiben (kein Burst) — der 0614er-Log zeigte
/// 37 kB/s in einen 6-kB/s-Kanal (~2 MB Queue). (source10 lief mit
/// 12 kB/s; mit Kachel-Deckel + Schattenbild ist die Bytemenge pro Wechsel
/// kleiner, daher das echte Budget, damit der Stau-Wächter greift.)
#[test]
fn soak_paced_animation_stays_on_budget() {
    let budget: u32 = 6000;
    let path = format!(
        "{}/lbw10-srv-{}-soak.lbwlog",
        std::env::temp_dir().display(),
        std::process::id()
    );
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(noise(128, 128, 1));
    let worker_src = shared.clone();
    let rec = Recorder::create(&path, "lbw-server-test", &["t".into()]).unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = worker_src;
        let mut ocr = StubOcr::fixed(vec![]);
        let r = serve_client(
            stream,
            &cfg_budget(budget),
            &mut src,
            &mut ocr,
            Some(40),
            &rec,
            None::<Injector>,
        );
        drop(rec); // `End` vor dem Lesen schreiben.
        r
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    // Pro Iteration genau ein Bildwechsel, dann genau eine Kachel
    // abwarten: Zwischen zwei Wechseln liegt immer ein Grab, also wird
    // jeder Wechsel genau eine Kachel (kein Verwerfen, kein Extra).
    // `max_frames` hat Headroom für Grab-ohne-Änderung zwischendrin.
    let mut seed = 2u64;
    let mut tiles = 0;
    for _ in 0..12 {
        shared.set(noise(128, 128, seed));
        seed += 1;
        let mut got = false;
        read_until(
            &mut fr,
            &mut s,
            Instant::now() + Duration::from_secs(30),
            &mut |m| {
                if matches!(m, ServerMsg::Tile { .. }) {
                    got = true;
                    return true;
                }
                false
            },
        );
        assert!(got, "Kachel blieb aus (Seed {})", seed - 1);
        tiles += 1;
    }
    drop(s);
    server.join().unwrap().unwrap();
    assert_eq!(tiles, 12);

    // Auswertung auf Mono-Uhr: Gesamt-Rate und jede Mono-Sekunde im Budget.
    let recs = Reader::all(&path).unwrap();
    let mut total = 0u64;
    let (mut t0, mut t1) = (u64::MAX, 0u64);
    let mut per_sec = std::collections::HashMap::new();
    for r in &recs {
        if let LogRecord::Msg {
            dir: Dir::SrvToCli,
            wire_bytes,
            stamp,
            ..
        } = r
        {
            total += *wire_bytes as u64;
            t0 = t0.min(stamp.mono_us);
            t1 = t1.max(stamp.mono_us);
            *per_sec.entry(stamp.mono_us / 1_000_000).or_insert(0u64) += *wire_bytes as u64;
        }
    }
    std::fs::remove_file(&path).unwrap();
    // Wächter: Ohne genug Bytes wäre der Test inhaltsleer (Pacing
    // griffe nie) — 3× Budget erzwingt echten Stau.
    assert!(total >= 3 * budget as u64, "zu wenig Stau: {total} B");
    let span_s = (t1 - t0) as f64 / 1_000_000.0;
    let rate = total as f64 / span_s;
    assert!(
        rate <= 1.5 * budget as f64,
        "Rate {rate:.0} B/s über Budget"
    );
    let peak = *per_sec.values().max().unwrap();
    assert!(peak <= (25 * budget as u64) / 10, "Bunch {peak} B in 1 s");
}

/// source10_b (Review B1/B2): Ein großer Bildwechsel (Rauschen 640×360,
/// monolithisch weit über 30 kB) darf einen *danach* geänderten Text nicht
/// aufhalten. In source10 stand die Schleife im Pacer und die eine
/// Riesen-Kachel blockierte den Kanal; hier kommt der Text binnen ~1 s,
/// und keine Kachel überschreitet den Deckel.
#[test]
fn texts_overtake_big_change() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(noise(640, 360, 3));
    let worker_src = shared.clone();
    let ocr = StubOcr::fixed(vec![item_at(8, 8, "vorher")]);
    let worker_ocr = ocr.clone();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let (mut src, mut ocr) = (worker_src, worker_ocr);
        let cfg = lbw_server::config::Config::try_parse_from([
            "lbw-server",
            "--budget",
            "3000",
            "--max-tile",
            "2400",
        ])
        .unwrap();
        serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(60),
            &Recorder::none(),
            None::<Injector>,
        )
    });
    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    // Erste Kachel abwarten: Ab jetzt ist der Kanal für viele Sekunden mit
    // dem Rest des Vollbilds belegt.
    let mut max_tile = 0;
    let mut tiles = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::Tile { data, .. } = m {
                max_tile = max_tile.max(data.len());
                tiles += 1;
                return true;
            }
            false
        },
    );
    assert_eq!(tiles, 1, "erste Kachel fehlt");
    // Bild wechselt erneut komplett, *und* der Text ändert sich.
    shared.set(noise(640, 360, 4));
    ocr.set(vec![item_at(8, 8, "nachher")]);
    let t0 = Instant::now();
    let mut echo = None;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| match m {
            ServerMsg::AddText(t) if t.text == "nachher" => {
                echo = Some(t0.elapsed());
                true
            }
            ServerMsg::Tile { data, .. } => {
                max_tile = max_tile.max(data.len());
                tiles += 1;
                false
            }
            _ => false,
        },
    );
    let echo = echo.expect("Text-Echo blieb aus");
    assert!(echo < Duration::from_millis(1500), "Text-Echo {echo:?}");
    assert!(max_tile <= 2400, "Kachel {max_tile} B über dem Deckel");
    // Vollbild ist noch längst nicht durch (Kanal belegt), Text war trotzdem da.
    assert!(tiles >= 1);
    drop(s);
    server.join().unwrap().unwrap();
}
