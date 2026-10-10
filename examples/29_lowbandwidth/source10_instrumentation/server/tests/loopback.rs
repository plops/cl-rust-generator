//! Loopback: echte Session (`SharedSource` + Stub-OCR) über echtes TCP.
//! Läuft ohne X11 und ohne Modelle.

use std::net::{TcpListener, TcpStream};
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};

use clap::Parser;
use image::RgbImage;
use lbw_common::framing::{FrameReader, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, Rect, ServerMsg, TextItem};
use lbw_log::io::{Reader, Recorder};
use lbw_log::{Dir, LogRecord, MsgKind};
use lbw_server::capture::{SharedSource, solid};
use lbw_server::config::Config;
use lbw_server::session::{Recognize, input_loop, serve_client};

struct StubOcr(Vec<TextItem>);

impl Recognize for StubOcr {
    fn text(&mut self, _img: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ok(self.0.clone())
    }
}

fn test_cfg() -> Config {
    // Ohne Display: Injector::open scheitert, die Session läuft ohne Eingabe.
    Config::try_parse_from(["lbw-server"]).unwrap()
}

fn item() -> TextItem {
    TextItem {
        rect: Rect::new(8, 8, 32, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: "hi".into(),
    }
}

/// Liest bis zur Deadline; `want` zählt relevante Nachrichten.
fn read_until(
    fr: &mut FrameReader,
    s: &mut TcpStream,
    until: Instant,
    want: &mut dyn FnMut(&ServerMsg) -> bool,
) {
    s.set_read_timeout(Some(Duration::from_millis(200)))
        .unwrap();
    while Instant::now() < until {
        if let Some(m) = fr.read_msg::<ServerMsg>(s).unwrap()
            && want(&m)
        {
            return;
        }
    }
}

#[test]
fn full_frame_then_single_dirty_tile() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let worker_src = shared.clone();

    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = worker_src;
        let mut ocr = StubOcr(vec![item()]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(30),
            &Recorder::none(),
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
        let mut ocr = StubOcr(vec![]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(1),
            &Recorder::none(),
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
        let mut ocr = StubOcr(vec![item()]);
        let r = serve_client(stream, &test_cfg(), &mut src, &mut ocr, Some(3), &rec);
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
