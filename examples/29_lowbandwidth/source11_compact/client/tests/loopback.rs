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

/// Stub-Erkennung mit umschaltbaren Boxen (Masken-Test): meldet je Box ein
/// TextItem und alle Boxen als Detektionen — wie echtes OCR, nur
/// deterministisch.
struct StubRec {
    boxes: std::sync::Arc<std::sync::Mutex<Vec<Rect>>>,
}

impl lbw_server::session::Recognize for StubRec {
    fn text(&mut self, _img: &image::RgbImage) -> Result<Vec<TextItem>, String> {
        let b = self.boxes.lock().unwrap().clone();
        Ok(b.iter()
            .map(|r| TextItem {
                id: 0,
                rect: *r,
                fg: [0; 3],
                bg: [255; 3],
                text: "x".into(),
            })
            .collect())
    }

    fn text_full(
        &mut self,
        img: &image::RgbImage,
        _max_new: usize,
        _hint: &[Rect],
    ) -> Result<(Vec<TextItem>, Vec<Rect>), String> {
        let items = self.text(img)?;
        let seen = items.iter().map(|t| t.rect).collect();
        Ok((items, seen))
    }
}

const BOX_A: Rect = Rect {
    x: 8,
    y: 8,
    w: 32,
    h: 16,
};
const BOX_B: Rect = Rect {
    x: 80,
    y: 80,
    w: 16,
    h: 16,
};

/// Schwarzer Schirm mit zwei weißen Quadraten (stellen Glyphen dar).
fn screen_with_squares() -> image::RgbImage {
    let mut img = image::RgbImage::new(128, 128);
    for r in [BOX_A, BOX_B] {
        for y in r.y..r.y + r.h {
            for x in r.x..r.x + r.w {
                img.put_pixel(x as u32, y as u32, image::Rgb([255; 3]));
            }
        }
    }
    img
}

/// Mittelwert über einem Rechteck: `tile` = Kachel-Ursprung/Breite,
/// `want` = gesuchter Ausschnitt (Global-Koordinaten).
fn region_mean(
    rgba: &[u8],
    tile: (usize, usize, usize),
    want: (usize, usize, usize, usize),
) -> f32 {
    let ((tx, ty, tw), (x0, y0, x1, y1)) = (tile, want);
    let (mut sum, mut n) = (0u64, 0u64);
    for y in y0..y1 {
        for x in x0..x1 {
            let i = ((y - ty) * tw + (x - tx)) * 4;
            sum += rgba[i] as u64;
            n += 1;
        }
    }
    sum as f32 / n.max(1) as f32
}

#[test]
fn mask_paints_current_not_stale_boxes() {
    // Phase A: Box A gemeldet (weißes Quadrat wird schwarz maskiert).
    // Phase B: nur Box B gemeldet — A ist Alt-Stand und darf NICHT mehr
    // übermalt werden (sonst Geister in der Kachel, live erlebt).
    use clap::Parser;
    let cfg = lbw_server::config::Config::try_parse_from(["lbw-server"]).unwrap();
    let boxes = std::sync::Arc::new(std::sync::Mutex::new(vec![BOX_A]));
    let feed = boxes.clone();
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap().to_string();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = lbw_server::capture::SharedSource::new(screen_with_squares());
        let mut ocr = StubRec {
            boxes: feed.clone(),
        };
        let _ = lbw_server::session::serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(500),
            TOKEN,
            None::<lbw_server::input::Injector>,
        );
    });

    let net = Net::connect(&addr, TOKEN, None);
    std::thread::sleep(Duration::from_secs(2));
    *boxes.lock().unwrap() = vec![BOX_B];
    // B bestätigen + A entfernen lassen (Stillstand trägt), dann alle
    // Kacheln einsammeln und suchen.
    std::thread::sleep(Duration::from_secs(8));
    let mut tiles = Vec::new();
    while let Ok(e) = net.events.try_recv() {
        if let Event::Tile {
            x,
            y,
            w,
            h,
            rgba,
            bytes: _,
        } = e
        {
            tiles.push((x, y, w, h, rgba));
        }
    }
    // A-Ausschnitt (12..36, 12..20): Quadrat steht wieder roh da = hell,
    // weil Alt-Box A nicht mehr übermalt wird.
    let a_lit = tiles.iter().any(|(x, y, w, h, rgba)| {
        let (x, y, w, h) = (*x as usize, *y as usize, *w, *h);
        x <= 12
            && y <= 12
            && x + w >= 36
            && y + h >= 20
            && region_mean(rgba, (x, y, w), (12, 12, 36, 20)) > 180.0
    });
    assert!(a_lit, "Alt-Box A wird noch übermalt (Geist in Kachel)");
    // B-Ausschnitt (84..92, 84..92): gemeldet = ausmaskiert = dunkel.
    let b_dark = tiles.iter().any(|(x, y, w, h, rgba)| {
        let (x, y, w, h) = (*x as usize, *y as usize, *w, *h);
        x <= 84
            && y <= 84
            && x + w >= 92
            && y + h >= 92
            && region_mean(rgba, (x, y, w), (84, 84, 92, 92)) < 75.0
    });
    assert!(b_dark, "aktuelle Box B nicht ausmaskiert");
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
