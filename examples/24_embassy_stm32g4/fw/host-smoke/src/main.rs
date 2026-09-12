//! Contract smoke test against the real device over USB-CDC.
//!
//! Usage: `g474-host-smoke /dev/ttyACM0`
//! Sends the T5 matrix (text + binary, alternating, corrupt frame recovery)
//! and exits 0 only if every expectation holds.

use g474_common::frame::encode_cmd;
use g474_common::{DeviceResp, HostCmd};
use postcard::accumulator::{CobsAccumulator, FeedResult};
use std::io::{Read, Write};
use std::time::Duration;

fn main() {
    let port_name = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("usage: g474-host-smoke /dev/ttyACMx");
        std::process::exit(2);
    });
    let mut port = serialport::new(&port_name, 115_200)
        .timeout(Duration::from_millis(2000))
        .open()
        .unwrap_or_else(|e| {
            eprintln!("open {}: {}", port_name, e);
            std::process::exit(2);
        });
    // Give the device a moment after DTR/reset-triggering open.
    std::thread::sleep(Duration::from_millis(500));
    let _ = port.clear(serialport::ClearBuffer::All);

    let mut failures = 0;
    // T5 text matrix.
    for (send, expect) in [
        ("PING\n", "PONG"),
        ("HELP\n", "OK HELP"),
        ("GET VER\n", "OK VER proto=1"),
        ("G UID\n", "OK UID "),
        ("FOO\n", "ERR UNKNOWN"),
        ("PING\r\n", "PONG"),
    ] {
        if !check_text(&mut port, send, expect) {
            failures += 1;
        }
    }

    // Binary matrix.
    if !check_binary(
        &mut port,
        HostCmd::Ping,
        |r| r == &DeviceResp::Pong,
        "Ping->Pong",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::GetVer,
        |r| matches!(r, DeviceResp::Ver { proto: 1, .. }),
        "GetVer->Ver",
    ) {
        failures += 1;
    }

    // Corrupt frame: device must answer Err and stay alive afterwards.
    port.write_all(&[0xFF, 0xFF, 0xFF, 0x00]).unwrap();
    let resp = read_frame(&mut port);
    match resp {
        Some(DeviceResp::Err { .. }) => println!("ok   corrupt -> Err"),
        other => {
            println!("FAIL corrupt -> {:?} (expected Err)", other);
            failures += 1;
        }
    }
    if !check_text(&mut port, "PING\n", "PONG") {
        failures += 1;
    }

    if failures > 0 {
        eprintln!("{} FAILURES", failures);
        std::process::exit(1);
    }
    println!("all smoke checks passed");
}

fn check_text(port: &mut Box<dyn serialport::SerialPort>, send: &str, expect_prefix: &str) -> bool {
    port.write_all(send.as_bytes()).unwrap();
    let line = read_line(port).unwrap_or_else(|| "<TIMEOUT>".to_string());
    if line.starts_with(expect_prefix) {
        println!("ok   {:?} -> {:?}", send.trim(), line.trim());
        true
    } else {
        println!(
            "FAIL {:?} -> {:?} (expected prefix {:?})",
            send, line, expect_prefix
        );
        false
    }
}

fn read_line(port: &mut Box<dyn serialport::SerialPort>) -> Option<String> {
    let mut buf = Vec::new();
    let mut one = [0u8; 1];
    loop {
        if port.read_exact(&mut one).is_err() {
            return None; // timeout: device did not answer
        }
        if one[0] == b'\n' {
            break;
        }
        buf.push(one[0]);
    }
    Some(
        String::from_utf8_lossy(&buf)
            .trim_end_matches('\r')
            .to_string(),
    )
}

fn read_frame(port: &mut Box<dyn serialport::SerialPort>) -> Option<DeviceResp> {
    let mut acc: CobsAccumulator<256> = CobsAccumulator::new();
    let mut one = [0u8; 1];
    for _ in 0..512 {
        port.read_exact(&mut one).unwrap();
        match acc.feed::<DeviceResp>(&one) {
            FeedResult::Success { data, .. } => return Some(data),
            FeedResult::Consumed => {}
            _ => return None,
        }
    }
    None
}

fn check_binary(
    port: &mut Box<dyn serialport::SerialPort>,
    cmd: HostCmd,
    pred: impl Fn(&DeviceResp) -> bool,
    label: &str,
) -> bool {
    let mut buf = [0u8; 160];
    let n = encode_cmd(&cmd, &mut buf).expect("encode");
    port.write_all(&buf[..n]).unwrap();
    match read_frame(port) {
        Some(r) if pred(&r) => {
            println!("ok   bin {} -> {:?}", label, r);
            true
        }
        other => {
            println!("FAIL bin {} -> {:?}", label, other);
            false
        }
    }
}
