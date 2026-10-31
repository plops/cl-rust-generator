//! Replay-Determinismus: zweimaliges Abspielen derselben Aufnahme liefert
//! identische Ausgabe (gleicher Canvas-Hash).

use std::process::Command;

use lbw_common::framing::encode_msg;
use lbw_common::{Rect, ServerMsg, TextItem};
use lbw_log::fnv1a64;
use lbw_log::io::Writer;
use lbw_log::record::{Dir, LOG_VERSION, LogRecord, MsgKind, Stamp};

fn tmp() -> String {
    format!(
        "{}/lbw10-replay-{}-det.lbwlog",
        std::env::temp_dir().display(),
        std::process::id()
    )
}

fn msg(mono: u64, m: &ServerMsg) -> LogRecord {
    let body = encode_msg(m).unwrap();
    LogRecord::Msg {
        stamp: Stamp {
            wall_us: 10_000_000 + mono,
            mono_us: mono,
        },
        dir: Dir::SrvToCli,
        kind: MsgKind::of_server(m),
        wire_bytes: 4 + body.len(),
        hash: fnv1a64(&body),
        body,
    }
}

#[test]
fn replay_is_deterministic() {
    let p = tmp();
    let rgb = [40u8, 80, 160].repeat(64 * 64);
    let data = lbw_server::av1::encode_rgb(&rgb, 64, 64, 180).unwrap();
    let mut w = Writer::create(&p).unwrap();
    w.write(&LogRecord::Session {
        app: "lbw-server".into(),
        version: LOG_VERSION,
        args: vec![],
    })
    .unwrap();
    for m in [
        ServerMsg::Hello,
        ServerMsg::ClearText,
        ServerMsg::AddText(TextItem {
            id: 9,
            rect: Rect::new(8, 8, 32, 16),
            fg: [0; 3],
            bg: [255; 3],
            text: "hi".into(),
        }),
        ServerMsg::Tile { x: 0, y: 0, data },
    ] {
        w.write(&msg(1000, &m)).unwrap();
    }
    drop(w);

    let bin = env!("CARGO_BIN_EXE_lbw-replay");
    let a = Command::new(bin).arg(&p).output().unwrap();
    assert!(a.status.success(), "{a:?}");
    let b = Command::new(bin).arg(&p).output().unwrap();
    assert!(b.status.success(), "{b:?}");
    assert_eq!(a.stdout, b.stdout, "Replay muss deterministisch sein");
    let s = String::from_utf8(a.stdout).unwrap();
    assert!(s.contains("replay: 1 Kacheln, 1 Texte"), "{s}");
    assert!(s.contains("(0 Fehler"), "{s}");
    std::fs::remove_file(&p).unwrap();
}
