//! Reconnect-Loopback: Vollbild bei Zweitverbindung, Session-Ende bei
//! Abriss (Accept keilt sonst), Entkopplung bei hängendem Injektor.
//! Läuft ohne X11 und ohne Modelle.

mod common;
use common::*;

use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg};
use lbw_log::io::Recorder;
use lbw_server::capture::{SharedSource, solid};
use lbw_server::input::{Inject, Injector};
use lbw_server::session::serve_client;

/// Reconnect-Resync (Server-Seite): Zwei Verbindungen nacheinander mit
/// denselben `src`/`ocr` (wie `main.rs` im Accept-Loop) — beide müssen das
/// Vollbild (Clear + Text + Kachel) liefern, sonst bleibt der Client schwarz.
#[test]
fn second_connection_resends_full_state() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let worker_src = shared.clone();

    let server = std::thread::spawn(move || {
        let mut src = worker_src;
        let mut ocr = StubOcr::fixed(vec![item()]);
        for _ in 0..2 {
            let (stream, _) = listener.accept().unwrap();
            serve_client(
                stream,
                &test_cfg(),
                &mut src,
                &mut ocr,
                Some(30),
                &Recorder::none(),
                None::<Injector>,
            )
            .unwrap();
        }
    });

    for conn in 0..2 {
        let mut s = TcpStream::connect(addr).unwrap();
        let mut fr = FrameReader::new();
        write_msg(
            &mut s,
            &ClientMsg::Hello {
                version: PROTO_VERSION,
            },
        )
        .unwrap();
        let (mut hello, mut clear, mut texts, mut tiles) = (false, 0, 0, 0);
        read_until(
            &mut fr,
            &mut s,
            Instant::now() + Duration::from_secs(15),
            &mut |m| {
                match m {
                    ServerMsg::Hello => hello = true,
                    ServerMsg::ClearText => clear += 1,
                    ServerMsg::AddText(_) => texts += 1,
                    ServerMsg::Tile { .. } => tiles += 1,
                    _ => {}
                }
                hello && clear >= 1 && texts >= 1 && tiles >= 1
            },
        );
        assert!(
            hello && clear == 1 && texts == 1 && tiles >= 1,
            "conn{conn}: hello={hello} clear={clear} texts={texts} tiles={tiles}"
        );
    }
    server.join().unwrap();
}

/// Session-Liveness: Bei `max_frames=None` (Produktion, Idle-Loop) muss die
/// Session enden, sobald der Client weg ist — sonst keilt der Accept-Loop und
/// jeder Reconnect bleibt schwarz (kein Hello, kein Bild).
#[test]
fn session_ends_when_client_disconnects_idle() {
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
            None,
            &Recorder::none(),
            None::<Injector>,
        )
    });

    // Verbinden, Vollbild lesen, DANN schließen (statischer Bildschirm:
    // der Server schreibt danach nie wieder — EOF ist das einzige Signal).
    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    let mut tile = false;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::Tile { .. }) {
                tile = true;
                return true;
            }
            false
        },
    );
    assert!(tile, "Vollbild-Kachel muss kommen");
    drop(s);

    // Begrenzt warten (hängt die Session, failt der Test statt der Suite).
    let until = Instant::now() + Duration::from_secs(10);
    while !server.is_finished() && Instant::now() < until {
        std::thread::sleep(Duration::from_millis(50));
    }
    assert!(
        server.is_finished(),
        "Session muss nach Client-Abriss enden (Accept sonst verkeilt)"
    );
    server.join().unwrap().unwrap();
}

/// Attrappen-Injektor, der beim ersten `handle` dauerhaft parkt (simuliert
/// hängendes enigo/X11) und den Eintritt über `entered` meldet.
struct ParkForever {
    entered: std::sync::Arc<AtomicBool>,
}

impl Inject for ParkForever {
    fn handle(&mut self, _m: &ClientMsg) -> Result<(), String> {
        self.entered.store(true, Ordering::Relaxed);
        std::thread::park();
        Ok(())
    }
}

/// Entkopplung: Selbst ein dauerhaft hängender Injektor darf die Session
/// nicht keilen — der Reader sieht EOF trotzdem und beendet sie (Accept
/// bleibt frei, Reconnect funktioniert).
#[test]
fn stuck_injector_does_not_wedge_session() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let worker_src = shared.clone();
    let entered = std::sync::Arc::new(AtomicBool::new(false));
    let entered_w = entered.clone();

    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = worker_src;
        let mut ocr = StubOcr::fixed(vec![item()]);
        let inj = ParkForever { entered: entered_w };
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            None,
            &Recorder::none(),
            Some(inj),
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
    let mut tile = false;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::Tile { .. }) {
                tile = true;
                return true;
            }
            false
        },
    );
    assert!(tile, "Vollbild-Kachel muss kommen");
    // Eingabe schicken → Worker parkt in `handle`; darauf warten …
    write_msg(&mut s, &ClientMsg::MouseMove { x: 1, y: 1 }).unwrap();
    let until = Instant::now() + Duration::from_secs(5);
    while !entered.load(Ordering::Relaxed) && Instant::now() < until {
        std::thread::sleep(Duration::from_millis(20));
    }
    assert!(entered.load(Ordering::Relaxed), "Worker muss parken");
    // … dann schließen: Die Session muss trotzdem enden.
    drop(s);
    let until = Instant::now() + Duration::from_secs(10);
    while !server.is_finished() && Instant::now() < until {
        std::thread::sleep(Duration::from_millis(50));
    }
    assert!(
        server.is_finished(),
        "hängender Injektor darf die Session nicht keilen"
    );
    server.join().unwrap().unwrap();
}
