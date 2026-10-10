//! Loopback: `Net` gegen einen Stub-Server (Hello, Text, echte AV1-Kachel,
//! Abriss + Reconnect). Läuft ohne Display und ohne Modelle.

use std::net::TcpListener;
use std::time::{Duration, Instant};

use lbw_client::net::{Event, Net};
use lbw_common::framing::{FrameReader, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, Rect, ServerMsg, TextItem};
use lbw_log::io::{Reader, Recorder};
use lbw_log::{Dir, GapEvent, LogRecord, MsgKind};

fn item() -> TextItem {
    TextItem {
        id: 5,
        rect: Rect::new(8, 8, 32, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: "hi".into(),
    }
}

/// Stub: Hello lesen, Hello + Text + Kachel schicken, dann `expect`
/// Client-Nachrichten lesen und zurückgeben. Danach 300 ms Stille fordern
/// (fängt stale/geflushte Inputs). Schließen → Client sieht EOF.
fn stub(
    listener: TcpListener,
    tile: Vec<u8>,
    expect: usize,
) -> std::thread::JoinHandle<Vec<ClientMsg>> {
    std::thread::spawn(move || {
        let (mut s, _) = listener.accept().unwrap();
        let mut fr = FrameReader::new();
        s.set_read_timeout(Some(Duration::from_secs(10))).unwrap();
        assert!(matches!(
            fr.read_msg::<ClientMsg>(&mut s).unwrap(),
            Some(ClientMsg::Hello {
                version: PROTO_VERSION,
            })
        ));
        write_msg(&mut s, &ServerMsg::Hello).unwrap();
        write_msg(&mut s, &ServerMsg::ClearText).unwrap();
        write_msg(&mut s, &ServerMsg::AddText(item())).unwrap();
        // Delta-Pfad: Text gleich wieder entfernen (Transport-Test).
        write_msg(&mut s, &ServerMsg::RemoveText(item().id)).unwrap();
        write_msg(
            &mut s,
            &ServerMsg::Tile {
                x: 0,
                y: 0,
                data: tile,
            },
        )
        .unwrap();
        let mut got = Vec::new();
        while got.len() < expect {
            match fr.read_msg::<ClientMsg>(&mut s).unwrap() {
                Some(m) => got.push(m),
                None => panic!("Timeout beim Warten auf Client-Nachrichten"),
            }
        }
        s.set_read_timeout(Some(Duration::from_millis(300)))
            .unwrap();
        match fr.read_msg::<ClientMsg>(&mut s).unwrap() {
            None => got,
            Some(m) => panic!("unerwartete Nachricht nach expect: {m:?}"),
        }
    })
}

fn recv_until(net: &Net, until: Instant, want: &mut dyn FnMut(Event) -> bool) {
    while Instant::now() < until {
        if let Ok(e) = net.events.recv_timeout(Duration::from_millis(200))
            && want(e)
        {
            return;
        }
    }
}

#[test]
fn hello_text_tile_and_reconnect() {
    let rgb = [40u8, 80, 160].repeat(64 * 64);
    let tile = lbw_server::av1::encode_rgb(&rgb, 64, 64, 180).unwrap();

    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    let addr = format!("127.0.0.1:{port}");
    let sent = vec![
        ClientMsg::MouseMove { x: 10, y: 20 },
        ClientMsg::Button {
            button: 1,
            down: true,
        },
        ClientMsg::Button {
            button: 1,
            down: false,
        },
        ClientMsg::Text("ab".into()),
    ];
    let stub1 = stub(listener, tile, sent.len());

    let net = Net::connect(&addr);

    // Erste Verbindung: Hello → Clear → Text → Remove → Kachel.
    let (mut connected, mut clear, mut texts, mut tiles, mut removed) = (0, 0, 0, 0, 0);
    recv_until(&net, Instant::now() + Duration::from_secs(10), &mut |e| {
        match e {
            Event::Connected => connected += 1,
            Event::Disconnected(_) => {}
            Event::ClearText => clear += 1,
            Event::AddText(t) => {
                assert_eq!((t.id, t.text.as_str()), (5, "hi"));
                texts += 1;
            }
            Event::RemoveText(id) => {
                assert_eq!(id, 5);
                removed += 1;
            }
            Event::Tile {
                x,
                y,
                w,
                h,
                rgba,
                bytes,
            } => {
                assert_eq!((x, y), (0, 0));
                assert_eq!((w, h), (64, 64));
                assert_eq!(rgba.len(), 64 * 64 * 4);
                assert!(bytes > 0);
                // Flache Kachel: überall fast die Quellfarbe, Alpha 255.
                assert!(rgba.chunks(4).all(|p| p[3] == 255));
                for (got, want) in rgba[0..3].iter().zip([40, 80, 160]) {
                    assert!(got.abs_diff(want) <= 3, "{rgba:?}");
                }
                tiles += 1;
            }
        }
        connected >= 1 && clear >= 1 && texts >= 1 && tiles >= 1 && removed >= 1
    });
    assert_eq!((connected, clear, texts, tiles, removed), (1, 1, 1, 1, 1));

    // Gegenrichtung: Net::send muss vollständig beim Server ankommen.
    for m in &sent {
        net.send(m.clone());
    }
    assert_eq!(stub1.join().unwrap(), sent);

    // Abriss bemerken, neu verbinden.
    let mut down = false;
    recv_until(&net, Instant::now() + Duration::from_secs(5), &mut |e| {
        if matches!(e, Event::Disconnected(_)) {
            down = true;
            return true;
        }
        false
    });
    assert!(down, "Abriss muss als Event kommen");

    let listener2 = TcpListener::bind(&addr).unwrap();
    let tile2 = lbw_server::av1::encode_rgb(&rgb, 64, 64, 180).unwrap();
    let stub2 = stub(listener2, tile2, 0);
    let mut reconnected = false;
    recv_until(&net, Instant::now() + Duration::from_secs(10), &mut |e| {
        if matches!(e, Event::Connected) {
            reconnected = true;
            return true;
        }
        false
    });
    assert!(reconnected, "Client muss neu verbinden");
    stub2.join().unwrap();
    drop(net);
}

/// Liest das Log, sobald der Netz-Thread per Drop ein `End` geschrieben hat
/// (der Thread endet asynchron nach `drop(Net)`).
fn wait_all_with_end(path: &str, timeout: Duration) -> Vec<LogRecord> {
    let until = Instant::now() + timeout;
    loop {
        if let Ok(recs) = Reader::all(path)
            && matches!(recs.last(), Some(LogRecord::End { .. }))
        {
            return recs;
        }
        assert!(Instant::now() < until, "kein End im Log");
        std::thread::sleep(Duration::from_millis(50));
    }
}

#[test]
fn recording_captures_both_directions_and_decode() {
    let path = format!(
        "{}/lbw10-cli-{}-rec.lbwlog",
        std::env::temp_dir().display(),
        std::process::id()
    );
    let rgb = [40u8, 80, 160].repeat(64 * 64);
    let tile = lbw_server::av1::encode_rgb(&rgb, 64, 64, 180).unwrap();
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    let addr = format!("127.0.0.1:{port}");
    let sent = vec![
        ClientMsg::MouseMove { x: 1, y: 2 },
        ClientMsg::Text("ab".into()),
    ];
    let stub1 = stub(listener, tile, sent.len());

    let rec = Recorder::create(&path, "lbw-client-test", &["t".into()]).unwrap();
    let net = Net::connect_recorder(&addr, rec);
    let mut tiles = 0;
    recv_until(&net, Instant::now() + Duration::from_secs(10), &mut |e| {
        if matches!(e, Event::Tile { .. }) {
            tiles += 1;
            return true;
        }
        false
    });
    assert_eq!(tiles, 1);
    for m in &sent {
        net.send(m.clone());
    }
    assert_eq!(stub1.join().unwrap(), sent);
    drop(net);

    let recs = wait_all_with_end(&path, Duration::from_secs(10));
    assert!(matches!(recs[0], LogRecord::Session { .. }), "{recs:?}");
    assert!(
        recs.iter().any(|r| matches!(
            r,
            LogRecord::Gap {
                event: GapEvent::Up { down_ms: None, .. },
                ..
            }
        )),
        "{recs:?}"
    );
    let kinds: Vec<(Dir, MsgKind)> = recs
        .iter()
        .filter_map(|r| match r {
            LogRecord::Msg { dir, kind, .. } => Some((*dir, *kind)),
            _ => None,
        })
        .collect();
    for want in [
        (Dir::CliToSrv, MsgKind::CliHello),
        (Dir::CliToSrv, MsgKind::MouseMove),
        (Dir::CliToSrv, MsgKind::Text),
        (Dir::SrvToCli, MsgKind::ClearText),
        (Dir::SrvToCli, MsgKind::AddText),
        (Dir::SrvToCli, MsgKind::Tile),
    ] {
        assert!(kinds.contains(&want), "{kinds:?}");
    }
    assert!(
        recs.iter().any(|r| matches!(
            r,
            LogRecord::Decode {
                ok: true,
                tile_bytes,
                ..
            } if *tile_bytes > 0
        )),
        "{recs:?}"
    );
    for r in &recs {
        if let LogRecord::Msg {
            dir: Dir::SrvToCli,
            body,
            ..
        } = r
        {
            decode_msg::<ServerMsg>(body).unwrap();
        }
    }
    std::fs::remove_file(&path).unwrap();
}

#[test]
fn stale_inputs_are_dropped_on_reconnect() {
    let rgb = [40u8, 80, 160].repeat(64 * 64);
    let tile = lbw_server::av1::encode_rgb(&rgb, 64, 64, 180).unwrap();
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    let addr = format!("127.0.0.1:{port}");
    let stub1 = stub(listener, tile, 0);

    let net = Net::connect(&addr);
    // Verbinden, dann Abriss abwarten (stub1 schließt nach der Sequenz).
    recv_until(&net, Instant::now() + Duration::from_secs(10), &mut |e| {
        matches!(e, Event::Connected)
    });
    stub1.join().unwrap();
    recv_until(&net, Instant::now() + Duration::from_secs(5), &mut |e| {
        matches!(e, Event::Disconnected(_))
    });
    // Offline-Phase: zwei stale Eingaben (müssen beim Reconnect verfallen).
    net.send(ClientMsg::MouseMove { x: 1, y: 1 });
    net.send(ClientMsg::MouseMove { x: 2, y: 2 });

    let listener2 = TcpListener::bind(&addr).unwrap();
    let tile2 = lbw_server::av1::encode_rgb(&rgb, 64, 64, 180).unwrap();
    let stub2 = stub(listener2, tile2, 1);
    recv_until(&net, Instant::now() + Duration::from_secs(10), &mut |e| {
        matches!(e, Event::Connected)
    });
    // Frische Eingabe NACH dem Reconnect: die muss ankommen.
    let fresh = ClientMsg::Text("frisch".into());
    net.send(fresh.clone());
    assert_eq!(stub2.join().unwrap(), vec![fresh]);
    drop(net);
}
