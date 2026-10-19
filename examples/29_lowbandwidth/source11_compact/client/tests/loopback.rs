//! Client-Loopback (T4): `Net` gegen einen Mini-Server (Challenge/Auth,
//! New/Move/Remove, Tile-Fehlerpfad). Läuft ohne X11, ohne echten Server.

use std::net::TcpListener;
use std::time::{Duration, Instant};

use lbw_client::net::{Event, Net};
use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, Rect, ServerMsg, TextItem, verify_auth};

const TOKEN: &[u8] = b"client-test-token";

fn item(id: u64, x: u16, text: &str) -> TextItem {
    TextItem {
        id,
        rect: Rect::new(x, 20, 100, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: text.into(),
    }
}

/// Mini-Server: genau eine Session, dann Ende.
fn mini_server(listener: TcpListener) {
    let (mut s, _) = listener.accept().unwrap();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    let mut fr = FrameReader::new();
    let nonce = *b"0123456789abcdef";
    write_msg(
        &mut s,
        &ServerMsg::Challenge {
            version: PROTO_VERSION,
            nonce,
        },
    )
    .unwrap();
    let auth: ClientMsg = fr.read_msg(&mut s).unwrap().expect("Auth");
    let ClientMsg::Auth { mac } = auth else {
        panic!("Auth erwartet");
    };
    assert!(verify_auth(TOKEN, &nonce, &mac));
    let hello: ClientMsg = fr.read_msg(&mut s).unwrap().expect("Hello");
    assert!(matches!(
        hello,
        ClientMsg::Hello {
            version: PROTO_VERSION
        }
    ));
    for m in [
        ServerMsg::Hello,
        ServerMsg::ClearText,
        ServerMsg::NewText(item(1, 10, "hallo")),
        ServerMsg::MoveText {
            id: 1,
            rect: Rect::new(10, 60, 100, 16),
        },
        ServerMsg::RemoveText(1),
        // Kaputte AV1-Daten: Fehlerpfad, kein Absturz, kein Tile-Event.
        ServerMsg::Tile {
            x: 0,
            y: 0,
            w: 64,
            h: 32,
            data: vec![0xde, 0xad, 0xbe, 0xef],
        },
    ] {
        write_msg(&mut s, &m).unwrap();
    }
    // Kurz offen halten (Reconnect-Test braucht keinen — Net verbindet neu).
    std::thread::sleep(Duration::from_millis(500));
}

fn collect(net: &Net, want: usize, timeout: Duration) -> Vec<String> {
    let until = Instant::now() + timeout;
    let mut got = Vec::new();
    while Instant::now() < until && got.len() < want {
        while let Ok(e) = net.events.try_recv() {
            got.push(
                match e {
                    Event::Connected => "connected",
                    Event::Disconnected(_) => "down",
                    Event::ClearText => "clear",
                    Event::NewText(_) => "new",
                    Event::MoveText { .. } => "move",
                    Event::RemoveText(_) => "remove",
                    Event::Tile { .. } => "tile",
                }
                .to_string(),
            );
            if got.len() >= want {
                break;
            }
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    got
}

#[test]
fn handshake_text_delta_and_metrics() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap().to_string();
    std::thread::spawn(|| mini_server(listener));

    let net = Net::connect(&addr, TOKEN, None);
    let got = collect(&net, 5, Duration::from_secs(10));
    assert_eq!(got, vec!["connected", "clear", "new", "move", "remove"]);
    let m = net.metrics();
    assert!(m.rx_bytes > 0, "Leitungszähler läuft");
    assert_eq!(m.rx_texts, 1);
    // Senden ohne Abbruch (Server liest kaum — nur Rauchtest).
    net.send(ClientMsg::Text("x".into()));
    assert!(net.metrics().last_input.is_some());
}

/// Mini-Server für genau eine skalierte Kachel: 16×16 Pixel kodiert,
/// Fußabdruck 32×32 — der Client muss auf den Fußabdruck hochziehen.
fn mini_server_scaled(listener: TcpListener, tile: ServerMsg) {
    let (mut s, _) = listener.accept().unwrap();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    let mut fr = FrameReader::new();
    let nonce = *b"0123456789abcdef";
    write_msg(
        &mut s,
        &ServerMsg::Challenge {
            version: PROTO_VERSION,
            nonce,
        },
    )
    .unwrap();
    let auth: ClientMsg = fr.read_msg(&mut s).unwrap().expect("Auth");
    let ClientMsg::Auth { mac } = auth else {
        panic!("Auth erwartet");
    };
    assert!(verify_auth(TOKEN, &nonce, &mac));
    let hello: ClientMsg = fr.read_msg(&mut s).unwrap().expect("Hello");
    assert!(matches!(
        hello,
        ClientMsg::Hello {
            version: PROTO_VERSION
        }
    ));
    write_msg(&mut s, &ServerMsg::Hello).unwrap();
    write_msg(&mut s, &tile).unwrap();
    std::thread::sleep(Duration::from_millis(500));
}

#[test]
fn scaled_tile_upscales_to_footprint() {
    // Echte Kodierung (16×16 einfarbig), Fußabdruck 32×32.
    let data = lbw_server::av1::encode_rgb(&vec![200u8; 16 * 16 * 3], 16, 16, 255).unwrap();
    let tile = ServerMsg::Tile {
        x: 8,
        y: 8,
        w: 32,
        h: 32,
        data,
    };
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap().to_string();
    std::thread::spawn(|| mini_server_scaled(listener, tile));

    let net = Net::connect(&addr, TOKEN, None);
    let until = Instant::now() + Duration::from_secs(10);
    while Instant::now() < until {
        while let Ok(e) = net.events.try_recv() {
            if let Event::Tile {
                x,
                y,
                w,
                h,
                rgba,
                bytes,
            } = e
            {
                assert_eq!((x, y, w, h), (8, 8, 32, 32));
                assert_eq!(rgba.len(), 32 * 32 * 4);
                assert!(bytes > 0);
                // Einfarbig kodiert → fast einfarbig hochgezogen (Nearest
                // behält Blöcke, AV1-Rauschen minimal).
                let (chunks, _) = rgba.as_chunks::<4>();
                let (mn, mx) = chunks.iter().fold((u8::MAX, u8::MIN), |(mn, mx), p| {
                    (mn.min(p[0]), mx.max(p[0]))
                });
                assert!(mx - mn <= 4, "Farbverlauf {mn}..{mx}");
                return;
            }
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    panic!("keine skalierte Kachel angekommen");
}
