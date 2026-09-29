//! Engine gegen einen Fake-Server: Größe aus `Hello`, Text-Blob, echte
//! AV1-Kachel (rav1e → rav1d) im Frame-Puffer, Eingaben kommen an,
//! `drop` schließt die Verbindung.

use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::time::{Duration, Instant};

use lbw_common::frame::{FrameReader, Read1, write_frame};
use lbw_common::{CHUNK, ClientMsg, Input, Rect, ServerMsg, TextItem};
use lbw_core::engine::{Engine, flags};
use lbw_server::av1::{Av1Params, encode_rgb};

fn send(s: &mut TcpStream, m: ServerMsg) {
    write_frame(s, &m.encode()).unwrap();
}

fn next_client_msg(s: &mut TcpStream, fr: &mut FrameReader) -> ClientMsg {
    let end = Instant::now() + Duration::from_secs(5);
    while Instant::now() < end {
        if let Read1::Frame(b) = fr.read(s).unwrap() {
            return ClientMsg::decode(&b).unwrap();
        }
    }
    panic!("keine Client-Nachricht");
}

/// Pollt, bis `want` gesetzt ist; Puffer folgt der Szenengröße.
fn poll_until(e: &mut Engine, buf: &mut Vec<u8>, want: i32) -> i32 {
    let end = Instant::now() + Duration::from_secs(10);
    while Instant::now() < end {
        let f = e.poll(Some(buf));
        if f & flags::SIZE != 0 {
            let (w, h) = e.size();
            *buf = vec![0; w * h * 4];
        }
        if f & want == want {
            return f;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    panic!("Flag {want} nicht erreicht");
}

#[test]
fn engine_end_to_end_against_fake_server() {
    let l = TcpListener::bind("127.0.0.1:0").unwrap();
    let mut e = Engine::new(
        &l.local_addr().unwrap().to_string(),
        Duration::from_secs(30),
    );
    let (mut s, _) = l.accept().unwrap();
    s.set_read_timeout(Some(Duration::from_millis(100)))
        .unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        next_client_msg(&mut s, &mut fr),
        ClientMsg::Hello { .. }
    ));

    send(
        &mut s,
        ServerMsg::Hello {
            server_id: 7,
            w: 320,
            h: 240,
            resumed: false,
        },
    );
    let item = TextItem {
        id: 5,
        rect: Rect::new(10, 100, 80, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: "Hallo Android".into(),
    };
    send(
        &mut s,
        ServerMsg::Text {
            seq: 1,
            remove: vec![],
            add: vec![item],
        },
    );
    let rgb = [220u8, 30, 30].repeat(32 * 32);
    let obu = encode_rgb(&rgb, 32, 32, Av1Params::default()).unwrap();
    let rect = Rect::new(16, 16, 32, 32);
    send(
        &mut s,
        ServerMsg::TileStart {
            tile_id: 1,
            seq: 2,
            rect,
            len: obu.len() as u32,
        },
    );
    for (i, c) in obu.chunks(CHUNK).enumerate() {
        send(
            &mut s,
            ServerMsg::TileData {
                tile_id: 1,
                offset: (i * CHUNK) as u32,
                data: c.to_vec(),
            },
        );
    }

    let mut buf = Vec::new();
    let f = poll_until(&mut e, &mut buf, flags::FRAME | flags::UP);
    assert_eq!(e.size(), (320, 240));
    assert_eq!(buf.len(), 320 * 240 * 4);
    let px = |x: usize, y: usize| {
        let i = (y * 320 + x) * 4;
        [buf[i], buf[i + 1], buf[i + 2]]
    };
    let red = px(30, 30);
    assert!(red[0] > 180 && red[1] < 80 && red[2] < 80, "{red:?}");
    assert_eq!(px(5, 5), [24, 24, 32], "Hintergrund");
    assert_ne!(f & flags::TEXT, 0);

    let blob = e.texts();
    assert_eq!(&blob[..4], &1u32.to_le_bytes());
    assert!(blob.ends_with(b"Hallo Android"));
    assert_eq!(e.poll(Some(&mut buf)) & (flags::TEXT | flags::FRAME), 0);
    assert_eq!(e.select((0.0, 90.0), (200.0, 120.0)), "Hallo Android");
    assert!(e.status().starts_with("verbunden"));

    e.mouse(500, 6, true); // wird auf 319 begrenzt
    e.send(Input::Button {
        button: 1,
        down: true,
    });
    e.paste("xy");
    let mut got = Vec::new();
    while got.len() < 3 {
        if let ClientMsg::Input(i) = next_client_msg(&mut s, &mut fr) {
            got.push(i);
        }
    }
    assert_eq!(
        got,
        [
            Input::MouseMove { x: 319, y: 6 },
            Input::Button {
                button: 1,
                down: true
            },
            Input::Text("xy".into()),
        ]
    );

    drop(e);
    s.set_read_timeout(Some(Duration::from_secs(2))).unwrap();
    let mut rest = Vec::new();
    // Acks dürfen noch kommen, dann EOF (kein Timeout).
    assert!(s.read_to_end(&mut rest).is_ok(), "Verbindung bleibt offen");
}
