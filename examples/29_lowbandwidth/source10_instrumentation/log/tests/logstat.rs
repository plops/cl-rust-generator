//! Golden-Test: `lbw-logstat` meldet über synthetischem Log die erwarteten
//! Summary-Zeilen (Human + JSON).

use std::process::Command;

use lbw_log::io::Writer;
use lbw_log::record::{Dir, FrameMs, GapEvent, LOG_VERSION, LogRecord, MsgKind, Stamp, TileStat};

fn tmp() -> String {
    format!(
        "{}/lbw10-logstat-{}-gold.lbwlog",
        std::env::temp_dir().display(),
        std::process::id()
    )
}

fn write_golden(path: &str) {
    let mut w = Writer::create(path).unwrap();
    w.write(&LogRecord::Session {
        app: "lbw-server".into(),
        version: LOG_VERSION,
        args: vec![],
    })
    .unwrap();
    let gap_up = LogRecord::Gap {
        stamp: Stamp {
            wall_us: 10_000_000,
            mono_us: 0,
        },
        event: GapEvent::Up {
            peer: "127.0.0.1:9".into(),
            down_ms: None,
        },
    };
    w.write(&gap_up).unwrap();
    w.write(&LogRecord::Msg {
        stamp: Stamp {
            wall_us: 10_100_000,
            mono_us: 100_000,
        },
        dir: Dir::SrvToCli,
        kind: MsgKind::Tile,
        wire_bytes: 7000,
        hash: 7,
        body: vec![],
    })
    .unwrap();
    w.write(&LogRecord::Frame {
        stamp: Stamp {
            wall_us: 10_100_000,
            mono_us: 100_000,
        },
        frame: 1,
        texts: 0,
        text_changed: false,
        text_bytes: 0,
        tile: Some(TileStat {
            x: 0,
            y: 0,
            w: 64,
            h: 64,
            bytes: 6996,
            hash: 7,
        }),
        ms: FrameMs {
            capture: 1.0,
            det: 2.0,
            rec: 4.0,
            mask_diff: 8.0,
            encode: 16.0,
            send: 32.0,
        },
    })
    .unwrap();
    w.write(&LogRecord::End {
        stamp: Stamp {
            wall_us: 10_200_000,
            mono_us: 200_000,
        },
    })
    .unwrap();
}

#[test]
fn summary_reports_expected_lines_and_json() {
    let p = tmp();
    write_golden(&p);
    let bin = env!("CARGO_BIN_EXE_lbw-logstat");

    let out = Command::new(bin).arg(&p).output().unwrap();
    assert!(out.status.success(), "{out:?}");
    let s = String::from_utf8(out.stdout).unwrap();
    assert!(s.contains("Nachrichten: 1 (Server→Client 7000 B"), "{s}");
    assert!(s.contains("1 s über Budget (6000 B/s)"), "{s}");
    assert!(
        s.contains("Frames: 1 (1 Tiles, 6996 B, 0 Textwechsel)"),
        "{s}"
    );
    assert!(
        s.contains("gesamt: n=1 avg=63.0 p50=63.0 p90=63.0 p99=63.0 max=63.0 ms"),
        "{s}"
    );
    assert!(s.contains("keine Wiederholungen"), "{s}");

    let out = Command::new(bin).arg("--json").arg(&p).output().unwrap();
    assert!(out.status.success(), "{out:?}");
    let j = String::from_utf8(out.stdout).unwrap();
    assert!(j.contains("\"msgs\":1"), "{j}");
    assert!(j.contains("\"tiles\":1"), "{j}");
    assert!(j.contains("\"clock_offsets\":[]"), "{j}");
    assert!(j.contains("\"p99\":"), "{j}");

    let out = Command::new(bin).arg("--deep").arg(&p).output().unwrap();
    assert!(out.status.success(), "{out:?}");
    let d = String::from_utf8(out.stdout).unwrap();
    assert!(d.contains("== Tiefenanalyse =="), "{d}");
    assert!(d.contains("conn1 127.0.0.1:9"), "{d}");
    assert!(d.contains("Dedup-Tile-Total: 1x"), "{d}");
    assert!(d.contains("Kachelgrößen: n=1"), "{d}");
    assert!(
        d.contains("Zustell-Verzögerung: — (braucht Server+Client"),
        "{d}"
    );

    std::fs::remove_file(&p).unwrap();
}
