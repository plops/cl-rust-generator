//! Ablaufverfolgung end-to-end (ohne X11/Modelle): Session mit Stub-OCR
//! und `--trace`-Datei; danach muss die Datei Frame-, Text- und
//! Delta-Zeilen enthalten — sonst hängt die Verdrahtung daneben.

#[allow(dead_code)]
mod common;
use common::*;

use std::net::{TcpListener, TcpStream};
use std::time::{Duration, Instant};

use clap::Parser;
use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, auth_mac};
use lbw_server::capture::{SharedSource, solid};
use lbw_server::input::Injector;
use lbw_server::session::serve_client;

const TOKEN: &[u8] = b"test-token";

#[test]
fn trace_file_shows_frames_texts_and_deltas() {
    let path = std::env::temp_dir().join("lbw_trace_loopback.log");
    let _ = std::fs::remove_file(&path);
    let cfg = lbw_server::config::Config::try_parse_from([
        "lbw-server",
        "--trace",
        path.to_str().unwrap(),
    ])
    .unwrap();

    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = SharedSource::new(solid(128, 128, [40; 3]));
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &cfg,
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
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    s.set_read_timeout(Some(Duration::from_secs(10))).unwrap();
    let hello: ServerMsg = fr.read_msg(&mut s).unwrap().expect("Hello");
    assert!(matches!(hello, ServerMsg::Hello));
    // F2-Probe: Resync anfordern — zweites ClearText beweist Behandlung,
    // "note resync" belegt die Ankunft im Trace (sonst wäre F2 stumm).
    write_msg(&mut s, &ClientMsg::ResyncText).unwrap();
    let mut cleared = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::ClearText) {
                cleared += 1;
            }
            cleared >= 2
        },
    );
    server.join().unwrap();

    let log = std::fs::read_to_string(&path).unwrap();
    assert!(log.contains("frame=1\n"), "Frame-Kopf fehlt:\n{log}");
    assert!(
        log.contains("text id=") && log.contains("hi"),
        "OCR-Befund fehlt:\n{log}"
    );
    assert!(
        log.contains("delta still") || log.contains("NewText"),
        "Delta fehlt:\n{log}"
    );
    assert!(
        log.contains("note resync"),
        "F2-Ankunft nicht im Trace:\n{log}"
    );
    let _ = std::fs::remove_file(&path);
}
