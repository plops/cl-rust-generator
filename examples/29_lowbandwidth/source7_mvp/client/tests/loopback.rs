//! Loopback: `Net` gegen einen Stub-Server (Hello, Text, echte AV1-Kachel,
//! Abriss + Reconnect). Läuft ohne Display und ohne Modelle.

use std::net::TcpListener;
use std::time::{Duration, Instant};

use lbw_client::net::{Event, Net};
use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, Rect, ServerMsg, TextItem};

fn item() -> TextItem {
    TextItem {
        rect: Rect::new(8, 8, 32, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: "hi".into(),
    }
}

/// Stub: Hello lesen, Hello + Text + Kachel schicken, dann `expect`
/// Client-Nachrichten lesen und zurückgeben. Schließen → Client sieht EOF.
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
            Some(ClientMsg::Hello { version: 1 })
        ));
        write_msg(&mut s, &ServerMsg::Hello).unwrap();
        write_msg(&mut s, &ServerMsg::ClearText).unwrap();
        write_msg(&mut s, &ServerMsg::AddText(item())).unwrap();
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
        got
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

    // Erste Verbindung: Hello → Clear → Text → Kachel.
    let (mut connected, mut clear, mut texts, mut tiles) = (0, 0, 0, 0);
    recv_until(&net, Instant::now() + Duration::from_secs(10), &mut |e| {
        match e {
            Event::Connected => connected += 1,
            Event::Disconnected(_) => {}
            Event::ClearText => clear += 1,
            Event::AddText(t) => {
                assert_eq!(t.text, "hi");
                texts += 1;
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
        connected >= 1 && clear >= 1 && texts >= 1 && tiles >= 1
    });
    assert_eq!((connected, clear, texts, tiles), (1, 1, 1, 1));

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
