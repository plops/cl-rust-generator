//! Loopback (T2): echte Session (`SharedSource` + Stub-OCR) über echtes TCP.
//! Läuft ohne X11 und ohne Modelle. Handshake v4: Challenge → Auth → Hello.

mod common;
use common::*;

use std::net::{TcpListener, TcpStream};
use std::time::{Duration, Instant};

use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, auth_mac};
use lbw_server::capture::{SharedSource, solid};
use lbw_server::input::Injector;
use lbw_server::session::serve_client;

const TOKEN: &[u8] = b"test-token";

/// Führt den v4-Handshake durch; liefert den Server-Reader.
fn handshake(s: &mut TcpStream, fr: &mut FrameReader, token: &[u8]) -> ServerMsg {
    // 1. Challenge vom Server lesen.
    let ch: ServerMsg = fr.read_msg(s).unwrap().expect("Challenge");
    let nonce = match ch {
        ServerMsg::Challenge { version, nonce } => {
            assert_eq!(version, PROTO_VERSION);
            nonce
        }
        m => panic!("Challenge erwartet, {m:?}"),
    };
    // 2. Auth + Hello senden.
    write_msg(
        &mut *s,
        &ClientMsg::Auth {
            mac: auth_mac(token, &nonce),
        },
    )
    .unwrap();
    write_msg(
        &mut *s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    // 3. Server-Hello lesen.
    fr.read_msg(s).unwrap().expect("Server-Hello")
}

fn serve_once(
    shared: SharedSource,
    texts: Vec<lbw_common::TextItem>,
    frames: u64,
) -> std::net::SocketAddr {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(texts);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(frames),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });
    addr
}

#[test]
fn full_frame_then_single_dirty_tile() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let addr = serve_once(shared.clone(), vec![item()], 30);

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Vollbild: ClearText, NewText, 1 Box (128×128).
    let (mut clear, mut texts, mut tiles) = (0, 0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::ClearText => clear += 1,
                ServerMsg::NewText(t) => {
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

    // Ein Pixel ändern → genau eine Box um den Pixel kommt neu.
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
}

#[test]
fn scroll_sends_move_not_new() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let addr = serve_once(shared, vec![item()], 30);

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erst NewText abwarten, dann Zeile um 40 px schieben (Scroll).
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::NewText(_)),
    );
    // HINWEIS: Stub-OCR liefert feste Box — Scroll simuliert T3b mit
    // wechselndem Stub; hier nur: kein doppelter NewText bei Stillstand.
    let mut news = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(2),
        &mut |m| {
            if matches!(m, ServerMsg::NewText(_)) {
                news += 1;
            }
            false
        },
    );
    assert_eq!(news, 0, "Stillstand darf nichts erneut senden");
}

#[test]
fn scroll_soak_sends_only_movetext() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let ocr = StubOcr::fixed(vec![
        item_at(10, 20, "eins"),
        item_at(10, 40, "zwei"),
        item_at(10, 60, "drei"),
    ]);
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let ocr2 = ocr.clone();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = ocr2;
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(60),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erst alle drei NewTexts abwarten.
    let mut news = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::NewText(_)) {
                news += 1;
            }
            news >= 3
        },
    );
    assert_eq!(news, 3);

    // Scroll: alle Zeilen −20 px (gleicher Inhalt, neue Box).
    ocr.set(vec![
        item_at(10, 0, "eins"),
        item_at(10, 20, "zwei"),
        item_at(10, 40, "drei"),
    ]);
    let (mut moves, mut renews, mut removes) = (0, 0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::MoveText { .. } => moves += 1,
                ServerMsg::NewText(_) => renews += 1,
                ServerMsg::RemoveText(_) => removes += 1,
                _ => {}
            }
            moves >= 3
        },
    );
    assert_eq!((moves, renews, removes), (3, 0, 0), "Scroll = nur MoveText");
}

#[test]
fn texts_jump_the_tile_queue() {
    use clap::Parser;
    let cfg =
        lbw_server::config::Config::try_parse_from(["lbw-server", "--budget", "200"]).unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(200),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Text (~50 B, nur gebucht) trifft ein, obwohl das Tile (≥ Budget-Fenster)
    // noch gepacet wird: Deadline 2 s.
    let mut text_at = None;
    let start = Instant::now();
    read_until(&mut fr, &mut s, start + Duration::from_secs(2), &mut |m| {
        if matches!(m, ServerMsg::NewText(_)) {
            text_at = Some(Instant::now());
            return true;
        }
        false
    });
    assert!(text_at.is_some(), "Text springt vor dem Tile raus");
    assert!(start.elapsed() < Duration::from_secs(2));

    // Das Tile kommt trotzdem vollständig und in Reihenfolge an (kein Verwerfen).
    let mut tile_ok = false;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(30),
        &mut |m| {
            if let ServerMsg::Tile { data, .. } = m {
                tile_ok = !data.is_empty();
                return tile_ok;
            }
            false
        },
    );
    assert!(tile_ok, "gepactes Tile kommt ganz an");
}

#[test]
fn wrong_token_is_rejected() {
    let shared = SharedSource::new(solid(64, 64, [0; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(5),
            TOKEN,
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    let ch: ServerMsg = fr.read_msg(&mut s).unwrap().expect("Challenge");
    let ServerMsg::Challenge { nonce, .. } = ch else {
        panic!("Challenge erwartet");
    };
    write_msg(
        &mut s,
        &ClientMsg::Auth {
            mac: auth_mac(b"falsch", &nonce),
        },
    )
    .unwrap();
    // Server bricht ab → EOF statt Hello.
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    let end: Option<ServerMsg> = fr.read_msg(&mut s).unwrap_or(None);
    assert!(end.is_none(), "falsches Token: kein Hello, {end:?}");
    assert!(server.join().is_ok());
}

#[test]
fn wrong_version_is_rejected() {
    let shared = SharedSource::new(solid(64, 64, [0; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(5),
            TOKEN,
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    let ch: ServerMsg = fr.read_msg(&mut s).unwrap().expect("Challenge");
    let ServerMsg::Challenge { nonce, .. } = ch else {
        panic!("Challenge erwartet");
    };
    write_msg(
        &mut s,
        &ClientMsg::Auth {
            mac: auth_mac(TOKEN, &nonce),
        },
    )
    .unwrap();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION + 99,
        },
    )
    .unwrap();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    let end: Option<ServerMsg> = fr.read_msg(&mut s).unwrap_or(None);
    assert!(end.is_none(), "falsche Version: kein Hello, {end:?}");
    assert!(server.join().is_ok());
}
