//! Loopback: echte Session (`SharedSource` + Stub-OCR) über echtes TCP.
//! Läuft ohne X11 und ohne Modelle.

mod common;
use common::*;

use std::net::{TcpListener, TcpStream};
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};

use clap::Parser;

use lbw_common::framing::{FrameReader, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg};
use lbw_log::io::{Reader, Recorder};
use lbw_log::{Dir, LogRecord, MsgKind};
use lbw_server::capture::{SharedSource, solid};
use lbw_server::input::Injector;
use lbw_server::session::{input_loop, serve_client};

#[test]
fn full_frame_then_single_dirty_tile() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let worker_src = shared.clone();

    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = worker_src;
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &test_cfg(),
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

    // Hello + Vollbild: ClearText, AddText, 1 Box (128×128).
    let mut hello = false;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::Hello) {
                hello = true;
                return true;
            }
            false
        },
    );
    assert!(hello);

    let (mut clear, mut texts, mut tiles) = (0, 0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::ClearText => clear += 1,
                ServerMsg::AddText(t) => {
                    assert_eq!(t.text, "hi");
                    texts += 1;
                }
                ServerMsg::Tile { .. } => tiles += 1,
                _ => {}
            }
            clear >= 1 && texts >= 1 && tiles >= 1
        },
    );
    assert_eq!((clear, texts, tiles), (1, 1, 1));

    // Ein Pixel ändern → genau eine 16×16-Box um den Pixel kommt neu.
    let mut img = solid(128, 128, [40; 3]);
    img.put_pixel(100, 10, image::Rgb([9; 3]));
    shared.set(img);
    let mut found = None;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::Tile { x, y, data } = m {
                assert!(!data.is_empty());
                found = Some((*x, *y));
                return true;
            }
            false
        },
    );
    assert_eq!(found, Some((100, 10)));

    server.join().unwrap().unwrap();
}

#[test]
fn wrong_version_is_rejected() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = SharedSource::new(solid(128, 128, [0; 3]));
        let mut ocr = StubOcr::fixed(vec![]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(1),
            &Recorder::none(),
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    write_msg(&mut s, &ClientMsg::Hello { version: 999 }).unwrap();
    // Server schließt: kein Hello, EOF beim Lesen.
    let mut fr = FrameReader::new();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    loop {
        match fr.read(&mut s) {
            Ok(lbw_common::framing::Read1::Frame(_)) => panic!("unerwarteter Frame"),
            Ok(lbw_common::framing::Read1::Idle) => {}
            Err(_) => break, // EOF: Abweisung bestätigt
        }
    }
    let r = server.join().unwrap();
    assert!(r.is_err(), "falsche Version muss Fehler sein");
}

#[test]
fn input_messages_reach_handler_in_order() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let sent = vec![
        ClientMsg::Hello {
            version: PROTO_VERSION,
        },
        ClientMsg::MouseMove { x: 100, y: 200 },
        ClientMsg::Button {
            button: 1,
            down: true,
        },
        ClientMsg::Button {
            button: 1,
            down: false,
        },
        ClientMsg::Text("hi".into()),
        ClientMsg::Key {
            key: "Enter".into(),
            down: true,
        },
    ];
    let writer = sent.clone();
    let client = std::thread::spawn(move || {
        let mut s = TcpStream::connect(addr).unwrap();
        for m in &writer {
            write_msg(&mut s, m).unwrap();
        }
        // Socket fällt hier: input_loop sieht EOF und endet.
    });

    let (rd, _) = listener.accept().unwrap();
    rd.set_read_timeout(Some(Duration::from_millis(200)))
        .unwrap();
    let (tx, rx) = std::sync::mpsc::channel();
    let stop = AtomicBool::new(false);
    input_loop(
        rd,
        FrameReader::new(),
        &stop,
        false,
        &Recorder::none(),
        |m| {
            tx.send(m).unwrap();
        },
    );
    client.join().unwrap();
    assert_eq!(rx.try_iter().collect::<Vec<_>>(), sent);
}

#[test]
fn recording_captures_messages_and_frames() {
    let path = format!(
        "{}/lbw10-srv-{}-rec.lbwlog",
        std::env::temp_dir().display(),
        std::process::id()
    );
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let rec = Recorder::create(&path, "lbw-server-test", &["t".into()]).unwrap();

    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = SharedSource::new(solid(128, 128, [40; 3]));
        let mut ocr = StubOcr::fixed(vec![item()]);
        let r = serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(3),
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
    // Vollbild abwarten (Hello, Clear, Add, Tile).
    let mut tiles = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::Tile { .. }) {
                tiles += 1;
                return true;
            }
            false
        },
    );
    assert_eq!(tiles, 1);
    drop(s);
    server.join().unwrap().unwrap();

    let recs = Reader::all(&path).unwrap();
    assert!(matches!(recs[0], LogRecord::Session { .. }), "{recs:?}");
    assert!(matches!(recs[recs.len() - 1], LogRecord::End { .. }));
    let kinds: Vec<(Dir, MsgKind)> = recs
        .iter()
        .filter_map(|r| match r {
            LogRecord::Msg { dir, kind, .. } => Some((*dir, *kind)),
            _ => None,
        })
        .collect();
    for want in [
        (Dir::CliToSrv, MsgKind::CliHello),
        (Dir::SrvToCli, MsgKind::SrvHello),
        (Dir::SrvToCli, MsgKind::ClearText),
        (Dir::SrvToCli, MsgKind::AddText),
        (Dir::SrvToCli, MsgKind::Tile),
    ] {
        assert!(kinds.contains(&want), "{kinds:?}");
    }
    // Bodies sind exakt re-dekodierbar, Leitungsbytes stimmen.
    for r in &recs {
        if let LogRecord::Msg {
            dir,
            wire_bytes,
            body,
            ..
        } = r
        {
            assert_eq!(*wire_bytes, 4 + body.len(), "{r:?}");
            match dir {
                Dir::SrvToCli => {
                    decode_msg::<ServerMsg>(body).unwrap();
                }
                Dir::CliToSrv => {
                    decode_msg::<ClientMsg>(body).unwrap();
                }
            }
        }
    }
    // Frame-Records: erster mit Textwechsel + Vollbild-Kachel 128×128.
    let frames: Vec<_> = recs
        .iter()
        .filter_map(|r| match r {
            LogRecord::Frame {
                frame,
                texts,
                text_changed,
                tile,
                ms,
                ..
            } => Some((*frame, *texts, *text_changed, *tile, *ms)),
            _ => None,
        })
        .collect();
    assert!(!frames.is_empty());
    assert_eq!((frames[0].0, frames[0].1, frames[0].2), (1, 1, true));
    let tile = frames[0].3.as_ref().unwrap();
    assert_eq!((tile.w, tile.h), (128, 128));
    assert!(tile.bytes > 0 && frames[0].4.total() >= 0.0);
    std::fs::remove_file(&path).unwrap();
}

#[test]
fn text_delta_sends_only_changes() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let ocr = StubOcr::fixed(vec![item_at(8, 8, "a"), item_at(8, 40, "b")]);
    let worker_ocr = ocr.clone();

    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = SharedSource::new(solid(128, 128, [40; 3]));
        let mut ocr = worker_ocr;
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(40),
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
    // Frame 1: Clear + beide Texte + Kachel; IDs merken.
    let mut adds = Vec::new();
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::AddText(t) = m {
                adds.push(t.clone());
            }
            matches!(m, ServerMsg::Tile { .. })
        },
    );
    assert_eq!(adds.len(), 2);
    assert_ne!(adds[0].id, adds[1].id);
    // Textwechsel: "a" bleibt (gleiche Box), "b" wird "b2" (getippt, gleiche
    // Box → gleiche id), dazu neue Zeile "c".
    ocr.set(vec![
        item_at(8, 8, "a"),
        item_at(8, 40, "b2"),
        item_at(8, 72, "c"),
    ]);
    // Delta einsammeln: genau 1× AddText(b2, alte id) + 1× AddText(c, neu),
    // kein ClearText, kein Resend von "a".
    let mut delta = Vec::new();
    let mut clears = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::ClearText => clears += 1,
                ServerMsg::AddText(_) | ServerMsg::Tile { .. } | ServerMsg::RemoveText(_) => {
                    delta.push(m.clone())
                }
                _ => {}
            }
            matches!(m, ServerMsg::Tile { .. })
        },
    );
    drop(s);
    server.join().unwrap().unwrap();
    assert_eq!(clears, 0, "Delta darf kein ClearText senden");
    let added: Vec<_> = delta
        .iter()
        .filter_map(|m| match m {
            ServerMsg::AddText(t) => Some(t.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(added.len(), 2, "{delta:?}");
    // "b2" trägt die alte id von "b", "c" eine frische.
    let b_old = adds.iter().find(|t| t.text == "b").unwrap();
    let b2 = added.iter().find(|t| t.text == "b2").unwrap();
    assert_eq!(b2.id, b_old.id);
    assert!(!added.iter().any(|t| t.text == "a"), "{added:?}");
    assert!(added.iter().any(|t| t.text == "c"), "{added:?}");
}

#[test]
fn removed_text_sends_remove_text() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let ocr = StubOcr::fixed(vec![item_at(8, 8, "a"), item_at(8, 40, "weg")]);
    let worker_ocr = ocr.clone();

    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = SharedSource::new(solid(128, 128, [40; 3]));
        let mut ocr = worker_ocr;
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(40),
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
    let mut first_id = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::AddText(t) = m
                && t.text == "weg"
            {
                first_id = t.id;
            }
            matches!(m, ServerMsg::Tile { .. })
        },
    );
    assert_ne!(first_id, 0);
    ocr.set(vec![item_at(8, 8, "a")]);
    let mut removed = Vec::new();
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::RemoveText(id) = m {
                removed.push(*id);
            }
            matches!(m, ServerMsg::Tile { .. })
        },
    );
    drop(s);
    server.join().unwrap().unwrap();
    assert_eq!(removed, vec![first_id]);
}

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
    // Vollbild-Kachel + eine Dirty-Kachel danach, beide ganz.
    let mut img = solid(128, 128, [40; 3]);
    img.put_pixel(100, 10, image::Rgb([9; 3]));
    shared.set(img);
    let mut got = Vec::new();
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

/// Animations-Soak: 12 Rausch-Vollbilder bei 12 kB/s Budget müssen im
/// Mittel im Budget bleiben (kein Burst) — der 0614er-Log zeigte
/// 37 kB/s in einen 6-kB/s-Kanal (~2 MB Queue).
#[test]
fn soak_paced_animation_stays_on_budget() {
    let budget: u32 = 12000;
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
