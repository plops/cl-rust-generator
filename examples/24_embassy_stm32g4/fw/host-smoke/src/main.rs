//! Contract smoke test against the real device over USB-CDC.
//!
//! Usage: `g474-host-smoke /dev/ttyACM0`
//! Sends the T5 matrix (text + binary, alternating, corrupt frame recovery)
//! and exits 0 only if every expectation holds.

use g474_common::frame::encode_cmd;
use g474_common::modes_04::{AwgConfig, CapConfig, FreqConfig, ScopeConfig, VnaConfig};
use g474_common::{DeviceResp, HostCmd, PROTO_VER};
use postcard::accumulator::{CobsAccumulator, FeedResult};
use std::io::{Read, Write};
use std::time::Duration;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let port_name = args.get(1).cloned().unwrap_or_else(|| {
        eprintln!("usage: g474-host-smoke /dev/ttyACMx [gate_ms] [level_mv] [--endurance N] [--reconnect]");
        std::process::exit(2);
    });
    // T2 hardening flags (positional numeric args keep working: flags do not
    // parse as numbers and fall back to defaults).
    let endurance: u32 = args
        .iter()
        .position(|a| a == "--endurance")
        .and_then(|i| args.get(i + 1))
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    let reconnect_only: bool = args.iter().any(|a| a == "--reconnect");
    // T2 size sweep: comma-separated scope lengths to download on the open port.
    let sweep: Vec<u16> = args
        .iter()
        .position(|a| a == "--sweep-lens")
        .and_then(|i| args.get(i + 1))
        .map(|s| {
            s.split(',')
                .filter_map(|p| p.parse().ok())
                .collect::<Vec<u16>>()
        })
        .unwrap_or_default();
    // Optional longer gate to catch slow/rare edges on a floating input.
    let gate_ms: u32 = std::env::args()
        .nth(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(200)
        .clamp(10, 10000);
    let level_mv: u16 = std::env::args()
        .nth(3)
        .and_then(|s| s.parse().ok())
        .unwrap_or(1650)
        .min(3300);
    let mut port = open_port(&port_name);
    if reconnect_only {
        // T2 reconnect: close + reopen must leave a live protocol session.
        let mut failures = 0;
        if !check_binary(
            &mut port,
            HostCmd::Ping,
            |r| r == &DeviceResp::Pong,
            "Ping#1",
        ) {
            failures += 1;
        }
        drop(port);
        std::thread::sleep(Duration::from_millis(500));
        let mut port = open_port(&port_name);
        if !check_binary(
            &mut port,
            HostCmd::Ping,
            |r| r == &DeviceResp::Pong,
            "Ping#2",
        ) {
            failures += 1;
        }
        if !check_text(&mut port, "PING\n", "PONG") {
            failures += 1;
        }
        if failures > 0 {
            eprintln!("{} FAILURES", failures);
            std::process::exit(1);
        }
        println!("reconnect check passed");
        return;
    }
    if !sweep.is_empty() {
        // Minimal resync: the first frame after open can lose its leading
        // marker in the VCP (→ one BAD_FRAME); retry once, then sweep.
        let mut failures = 0;
        for attempt in 0..2 {
            if check_binary(
                &mut port,
                HostCmd::Ping,
                |r| r == &DeviceResp::Pong,
                "Ping-resync",
            ) {
                break;
            }
            failures += (attempt == 1) as u32;
        }
        if !check_binary(
            &mut port,
            HostCmd::ScopeStart(ScopeConfig {
                interleaved: 0,
                level_mv: 100,
            }),
            |r| r == &DeviceResp::ModeOk { mode: 1 },
            "ScopeStart->ModeOk(A)",
        ) {
            failures += 1;
        }
        for len in &sweep {
            if !check_scope_read(&mut port, 0, *len) {
                failures += 1;
            }
        }
        if failures > 0 {
            eprintln!("{} FAILURES", failures);
            std::process::exit(1);
        }
        println!("sweep check passed");
        return;
    }
    if endurance > 0 {
        let failures = endurance_run(&mut port, endurance);
        if failures > 0 {
            eprintln!("{} FAILURES", failures);
            std::process::exit(1);
        }
        println!("endurance check passed");
        return;
    }

    let mut failures = 0;
    // T5 text matrix.
    let ver_expect = format!("OK VER proto={}", PROTO_VER);
    let text_matrix: Vec<(&str, String)> = vec![
        ("PING\n", "PONG".to_string()),
        ("HELP\n", "OK HELP".to_string()),
        ("GET VER\n", ver_expect),
        ("G UID\n", "OK UID ".to_string()),
        ("FOO\n", "ERR UNKNOWN".to_string()),
        ("PING\r\n", "PONG".to_string()),
    ];
    for (send, expect) in &text_matrix {
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
        |r| matches!(r, DeviceResp::Ver { proto: p, .. } if *p == PROTO_VER),
        "GetVer->Ver",
    ) {
        failures += 1;
    }

    // Mode protocol (v2): self-test, unimplemented scope, freq measure/read/stop.
    if !check_binary(
        &mut port,
        HostCmd::SelfTest,
        |r| matches!(r, DeviceResp::SelfTestOk { .. }),
        "SelfTest->SelfTestOk",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::ScopeStart(ScopeConfig {
            interleaved: 0,
            level_mv: 100,
        }),
        |r| r == &DeviceResp::ModeOk { mode: 1 },
        "ScopeStart->ModeOk(A)",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::ScopeStart(ScopeConfig {
            interleaved: 1,
            level_mv: 100,
        }),
        |r| matches!(r, DeviceResp::Err { code: 8 }),
        "ScopeStart(interleaved)->NOT_IMPL",
    ) {
        failures += 1;
    }
    // 96 samples = 192 bytes = 2 blocks; CRC over the downloaded bytes must
    // match BlockEnd. PA0 floats: values arbitrary, shape + CRC are checked.
    if !check_scope_read(&mut port, 0, 96) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::ScopeRead { off: 0, len: 0 },
        |r| matches!(r, DeviceResp::Err { code: 7 }),
        "ScopeRead(0)->BAD_ARG",
    ) {
        failures += 1;
    }
    // VNA: 3-point sweep (fast: ~15 ms settle per point), then download.
    // PA5/PA0 unconnected: amplitudes ~0, shape + CRC are checked.
    if !check_binary(
        &mut port,
        HostCmd::VnaStart(VnaConfig {
            f0_hz: 500,
            f1_hz: 2000,
            points: 3,
        }),
        |r| r == &DeviceResp::ModeOk { mode: 2 },
        "VnaStart->ModeOk(B)",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::VnaStart(VnaConfig {
            f0_hz: 500,
            f1_hz: 2000,
            points: 1,
        }),
        |r| matches!(r, DeviceResp::Err { code: 7 }),
        "VnaStart(1pt)->BAD_ARG",
    ) {
        failures += 1;
    }
    if !check_vna_read(&mut port, 0, 3) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::VnaRead { off: 0, len: 99 },
        |r| matches!(r, DeviceResp::Err { code: 7 }),
        "VnaRead(beyond)->BAD_ARG",
    ) {
        failures += 1;
    }
    // Capacitance: all three TDM channels answer with a discharge time
    // (floating pins read ~0, no timeout); bad index is rejected.
    for pin in 0..=2u8 {
        if !check_binary(
            &mut port,
            HostCmd::CapStart(CapConfig { pin }),
            |r| matches!(r, DeviceResp::Cap { pin: p, .. } if *p == pin),
            &format!("CapStart({})->Cap", pin),
        ) {
            failures += 1;
        }
    }
    if !check_binary(
        &mut port,
        HostCmd::CapStart(CapConfig { pin: 9 }),
        |r| matches!(r, DeviceResp::Err { code: 7 }),
        "CapStart(9)->BAD_ARG",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::CapRead,
        |r| matches!(r, DeviceResp::Cap { .. }),
        "CapRead->Cap",
    ) {
        failures += 1;
    }
    if !check_text(&mut port, "GET CAP\n", "OK CAP pin=") {
        failures += 1;
    }
    // Short gate (200 ms); PA7 floating or grounded gives an arbitrary but
    // well-formed count — the check asserts shape, plausibility is logged.
    let freq_cfg = FreqConfig {
        level_mv,
        hyst: 2,
        filter: 0,
        gate_ms,
    };
    if !check_binary(
        &mut port,
        HostCmd::FreqStart(freq_cfg),
        |r| matches!(r, DeviceResp::Freq { gate_ms: g, .. } if *g == gate_ms),
        "FreqStart->Freq",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::FreqRead,
        |r| matches!(r, DeviceResp::Freq { .. }),
        "FreqRead->Freq",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::AwgStart(AwgConfig { freq_hz: 1000 }),
        |r| r == &DeviceResp::ModeOk { mode: 3 },
        "AwgStart->ModeOk(C)",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::AwgStart(AwgConfig { freq_hz: 0 }),
        |r| matches!(r, DeviceResp::Err { .. }),
        "AwgStart(0)->Err",
    ) {
        failures += 1;
    }
    if !check_binary(
        &mut port,
        HostCmd::ModeStop,
        |r| r == &DeviceResp::ModeIdle,
        "ModeStop->ModeIdle",
    ) {
        failures += 1;
    }
    // T2: ModeStop issued mid-transfer. The serve loop is sequential, so the
    // device finishes the in-flight block stream first and answers ModeIdle
    // after it; the host must drain the trailing frames, not wedge on them.
    if !check_stop_during_transfer(&mut port) {
        failures += 1;
    }
    if !check_text(&mut port, "GET FREQ\n", "OK FREQ hz=") {
        failures += 1;
    }
    if !check_text(&mut port, "MODE STOP\n", "OK MODE IDLE") {
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
    let mut started = false;
    for _ in 0..1024 {
        port.read_exact(&mut one).unwrap();
        // Skip the leading 0x00 framing marker (COBS payload has none inside).
        if !started {
            if one[0] == 0x00 {
                continue;
            }
            started = true;
        }
        match acc.feed::<DeviceResp>(&one) {
            FeedResult::Success { data, .. } => return Some(data),
            FeedResult::Consumed => {}
            _ => return None,
        }
    }
    None
}

/// Download `len` samples at `off` and verify block order, byte count,
/// and the trailing CRC. Returns false on any mismatch.
fn check_scope_read(port: &mut Box<dyn serialport::SerialPort>, off: u32, len: u16) -> bool {
    use g474_common::blocks_05::crc16;
    let mut buf = [0u8; 160];
    let n = encode_cmd(&HostCmd::ScopeRead { off, len }, &mut buf).expect("encode");
    port.write_all(&buf[..n]).unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    let mut total = usize::MAX;
    let mut next_seq = 0u16;
    loop {
        match read_frame(port) {
            Some(DeviceResp::Block {
                seq,
                total: t,
                data,
            }) => {
                if seq != next_seq || (total != usize::MAX && t as usize != total) {
                    println!("FAIL scope block order seq={} total={}", seq, t);
                    return false;
                }
                total = t as usize;
                next_seq += 1;
                bytes.extend_from_slice(&data);
            }
            Some(DeviceResp::BlockEnd { total: t, crc }) => {
                if t as usize != total || next_seq as usize != total {
                    println!("FAIL scope BlockEnd total={} (saw {} blocks)", t, next_seq);
                    return false;
                }
                if bytes.len() != len as usize * 2 {
                    println!("FAIL scope byte count {}", bytes.len());
                    return false;
                }
                if crc16(&bytes) != crc {
                    println!("FAIL scope CRC {:04X} != {:04X}", crc16(&bytes), crc);
                    return false;
                }
                println!(
                    "ok   scope off={} len={} blocks={} crc={:04X}",
                    off, len, total, crc
                );
                return true;
            }
            other => {
                println!("FAIL scope unexpected {:?}", other);
                return false;
            }
        }
    }
}

/// Download `len` VNA points at `off` and verify order, frequencies,
/// byte count, and CRC. Returns false on any mismatch.
fn check_vna_read(port: &mut Box<dyn serialport::SerialPort>, off: u32, len: u16) -> bool {
    use g474_common::blocks_05::crc16;
    let mut buf = [0u8; 160];
    let n = encode_cmd(&HostCmd::VnaRead { off, len }, &mut buf).expect("encode");
    port.write_all(&buf[..n]).unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    let mut total = usize::MAX;
    let mut next_seq = 0u16;
    loop {
        match read_frame(port) {
            Some(DeviceResp::Block {
                seq,
                total: t,
                data,
            }) => {
                if seq != next_seq || (total != usize::MAX && t as usize != total) {
                    println!("FAIL vna block order seq={} total={}", seq, t);
                    return false;
                }
                total = t as usize;
                next_seq += 1;
                bytes.extend_from_slice(&data);
            }
            Some(DeviceResp::BlockEnd { total: t, crc }) => {
                if t as usize != total || next_seq as usize != total {
                    println!("FAIL vna BlockEnd total={} (saw {} blocks)", t, next_seq);
                    return false;
                }
                if bytes.len() != len as usize * 6 {
                    println!("FAIL vna byte count {}", bytes.len());
                    return false;
                }
                if crc16(&bytes) != crc {
                    println!("FAIL vna CRC {:04X} != {:04X}", crc16(&bytes), crc);
                    return false;
                }
                // Frequencies must be ascending starting at f0 of the sweep.
                let mut freqs = Vec::new();
                for chunk in bytes.as_chunks::<6>().0 {
                    freqs.push(u32::from_le_bytes(chunk[0..4].try_into().unwrap()));
                }
                if freqs.windows(2).any(|w| w[0] >= w[1]) {
                    println!("FAIL vna freqs not ascending {:?}", freqs);
                    return false;
                }
                println!(
                    "ok   vna off={} len={} freqs={:?} crc={:04X}",
                    off, len, freqs, crc
                );
                return true;
            }
            other => {
                println!("FAIL vna unexpected {:?}", other);
                return false;
            }
        }
    }
}

fn open_port(name: &str) -> Box<dyn serialport::SerialPort> {
    let port = serialport::new(name, 115_200)
        .timeout(Duration::from_millis(15000))
        .open()
        .unwrap_or_else(|e| {
            eprintln!("open {}: {}", name, e);
            std::process::exit(2);
        });
    // Give the device a moment after DTR/reset-triggering open.
    std::thread::sleep(Duration::from_millis(500));
    let _ = port.clear(serialport::ClearBuffer::All);
    port
}

/// T2: start a 10-block scope download, read one block, then stop. Accepts
/// both stream-completes-first (current firmware) and abort-first orderings;
/// afterwards the device must answer PING.
fn check_stop_during_transfer(port: &mut Box<dyn serialport::SerialPort>) -> bool {
    if !check_binary(
        port,
        HostCmd::ScopeStart(ScopeConfig {
            interleaved: 0,
            level_mv: 100,
        }),
        |r| r == &DeviceResp::ModeOk { mode: 1 },
        "ScopeStart->ModeOk(A)",
    ) {
        return false;
    }
    let mut buf = [0u8; 160];
    let n = encode_cmd(&HostCmd::ScopeRead { off: 0, len: 480 }, &mut buf).expect("encode");
    port.write_all(&buf[..n]).unwrap();
    match read_frame(port) {
        Some(DeviceResp::Block { seq: 0, .. }) => {}
        other => {
            println!("FAIL stop-mid-transfer first block -> {:?}", other);
            return false;
        }
    }
    let n = encode_cmd(&HostCmd::ModeStop, &mut buf).expect("encode");
    port.write_all(&buf[..n]).unwrap();
    let mut blocks = 1u32;
    let mut ended = false;
    for _ in 0..16 {
        match read_frame(port) {
            Some(DeviceResp::Block { .. }) => blocks += 1,
            Some(DeviceResp::BlockEnd { .. }) => ended = true,
            Some(DeviceResp::ModeIdle) => {
                println!(
                    "ok   stop-mid-transfer drained {} blocks ended={} -> ModeIdle",
                    blocks, ended
                );
                return check_binary(
                    port,
                    HostCmd::Ping,
                    |r| r == &DeviceResp::Pong,
                    "Ping-alive",
                );
            }
            other => {
                println!("FAIL stop-mid-transfer drain -> {:?}", other);
                return false;
            }
        }
    }
    println!("FAIL stop-mid-transfer: no ModeIdle within 16 frames");
    false
}

/// T2 endurance: cycle every mode with alternating text/binary traffic.
/// Short gates keep one iteration under ~1 s; N=60 approximates the 60-s
/// Dauerlauf across all modes.
fn endurance_run(port: &mut Box<dyn serialport::SerialPort>, iters: u32) -> u32 {
    use std::time::Instant;
    let mut failures = 0;
    let start = Instant::now();
    for i in 0..iters {
        let tag = format!("iter{}", i);
        if !check_text(port, "PING\n", "PONG") {
            failures += 1;
        }
        if !check_binary(port, HostCmd::Ping, |r| r == &DeviceResp::Pong, &tag) {
            failures += 1;
        }
        let cfg = FreqConfig {
            level_mv: 1650,
            hyst: 2,
            filter: 0,
            gate_ms: 50,
        };
        if !check_binary(
            port,
            HostCmd::FreqStart(cfg),
            |r| matches!(r, DeviceResp::Freq { gate_ms: 50, .. }),
            &tag,
        ) {
            failures += 1;
        }
        if !check_text(port, "GET FREQ\n", "OK FREQ hz=") {
            failures += 1;
        }
        if !check_binary(
            port,
            HostCmd::AwgStart(AwgConfig { freq_hz: 1000 }),
            |r| r == &DeviceResp::ModeOk { mode: 3 },
            &tag,
        ) {
            failures += 1;
        }
        if !check_scope_read(port, 0, 48) {
            failures += 1;
        }
        if !check_binary(
            port,
            HostCmd::VnaStart(VnaConfig {
                f0_hz: 500,
                f1_hz: 2000,
                points: 2,
            }),
            |r| r == &DeviceResp::ModeOk { mode: 2 },
            &tag,
        ) {
            failures += 1;
        }
        if !check_vna_read(port, 0, 2) {
            failures += 1;
        }
        let pin = (i % 3) as u8;
        if !check_binary(
            port,
            HostCmd::CapStart(CapConfig { pin }),
            |r| matches!(r, DeviceResp::Cap { .. }),
            &tag,
        ) {
            failures += 1;
        }
        if !check_binary(
            port,
            HostCmd::ModeStop,
            |r| r == &DeviceResp::ModeIdle,
            &tag,
        ) {
            failures += 1;
        }
        if !check_text(port, "MODE STOP\n", "OK MODE IDLE") {
            failures += 1;
        }
    }
    println!(
        "endurance: {} iters in {:.1}s ({} failures)",
        iters,
        start.elapsed().as_secs_f32(),
        failures
    );
    failures
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
