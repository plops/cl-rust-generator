//! Ende-zu-Ende über die C-ABI (wie iOS/Android sie nutzen):
//! Engine → lokaler Forward → **Jump-sshd** → genestete Session → **Ziel-sshd**
//! (beide Ed25519-Hostkeys, Ed25519-User-Key im OpenSSH-Format) →
//! `direct-tcpip` → echte `serve_client`-Session (source10_b) mit Stub-OCR
//! und protokollierendem Injector. Geprüft: Verbindung, Text, Kachel im
//! Canvas, TOFU-Pins = echte Hostkeys, Eingaben kommen beim Server an.
//!
//! Braucht `/usr/sbin/sshd` + `ssh-keygen` (CI: `openssh-server`);
//! fehlen sie, wird übersprungen.

#[path = "../../../tunnel/tests/common.rs"]
#[allow(dead_code)]
mod common;

use std::ffi::CString;
use std::net::TcpListener;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use clap::Parser;
use common::{host_fp, sshd, tmpdir, user, user_key};
use image::RgbImage;
use lbw_common::{ClientMsg, Rect, TextItem};
use lbw_log::Recorder;
use lbw_mobile_core::engine::flags;
use lbw_mobile_core::ffi::*;
use lbw_server::capture::{SharedSource, solid};
use lbw_server::config::Config;
use lbw_server::input::Inject;
use lbw_server::session::{Recognize, serve_client};

struct Stub;
impl Recognize for Stub {
    fn text(&mut self, _: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ok(vec![TextItem {
            id: 0,
            rect: Rect::new(8, 8, 120, 16),
            fg: [0; 3],
            bg: [255; 3],
            text: "über ssh".into(),
        }])
    }
}

#[derive(Clone, Default)]
struct Rec(Arc<Mutex<Vec<ClientMsg>>>);
impl Inject for Rec {
    fn handle(&mut self, m: &ClientMsg) -> Result<(), String> {
        self.0.lock().unwrap().push(m.clone());
        Ok(())
    }
}

/// Startet einen LBW-Server (eine Session) auf 127.0.0.1:<frei>.
fn lbw_server(inj: Rec) -> u16 {
    let l = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = l.local_addr().unwrap().port();
    std::thread::spawn(move || {
        let (s, _) = l.accept().unwrap();
        let mut src = SharedSource::new(solid(1280, 720, [200, 30, 30]));
        let cfg = Config::try_parse_from(["lbw-server"]).unwrap();
        let _ = serve_client(
            s,
            &cfg,
            &mut src,
            &mut Stub,
            Some(600),
            &Recorder::none(),
            Some(inj),
        );
    });
    port
}

fn read(
    f: unsafe extern "C" fn(*const LbwEngine, *mut u8, usize) -> usize,
    h: *const LbwEngine,
) -> Vec<u8> {
    // SAFETY: `h` gültig; erst Länge erfragen, dann passend puffern.
    unsafe {
        let n = f(h, std::ptr::null_mut(), 0);
        let mut b = vec![0u8; n];
        f(h, b.as_mut_ptr(), b.len());
        b
    }
}

#[test]
fn engine_through_jump_and_target_with_ed25519() {
    if !std::path::Path::new("/usr/sbin/sshd").exists() {
        eprintln!("übersprungen: kein sshd");
        return;
    }
    let _ = std::fs::create_dir_all("/run/sshd");
    let (jd, td) = (tmpdir("mjump"), tmpdir("mtarget"));
    let _jump = sshd(2242, &jd);
    let _target = sshd(2243, &td);
    // Ed25519 überall (Host- und User-Keys erzeugt `sshd()` per ssh-keygen -t ed25519).
    assert!(user_key(&jd).contains("OPENSSH PRIVATE KEY"));
    let inj = Rec::default();
    let lbw = lbw_server(inj.clone());

    let cfg = serde_json_like(
        &[
            (format!("{}@127.0.0.1:2242", user()), user_key(&jd)),
            (format!("{}@127.0.0.1:2243", user()), user_key(&td)),
        ],
        lbw,
    );
    let c = CString::new(cfg).unwrap();
    let h = lbw_new(c.as_ptr());
    assert!(!h.is_null());

    let mut canvas = vec![0u8; 1280 * 720 * 4];
    let (mut up, mut frame, mut text) = (false, false, false);
    let t0 = Instant::now();
    while !(up && frame && text) && t0.elapsed() < Duration::from_secs(30) {
        // SAFETY: gültiger Griff, Puffer passend.
        let f = unsafe { lbw_poll(h, canvas.as_mut_ptr(), canvas.len()) };
        up |= f & flags::UP != 0;
        // AV1 bei q=180 ist grob (200,30,30 → gemessen 228,6,27): Rot dominiert.
        frame |= f & flags::FRAME != 0 && canvas[0] > 150 && canvas[1] < 80 && canvas[2] < 80;
        text |= String::from_utf8_lossy(&read(lbw_texts, h)).contains("über ssh");
        std::thread::sleep(Duration::from_millis(20));
    }
    let status = String::from_utf8(read(lbw_status, h)).unwrap();
    assert!(
        up && frame && text,
        "up={up} frame={frame} text={text} status={status} px={:?}",
        &canvas[..4]
    );

    // TOFU: gelernte Pins = echte Hostkeys von Jump und Ziel.
    let pins = String::from_utf8(read(lbw_pins, h)).unwrap();
    assert_eq!(pins, format!("{}\n{}", host_fp(&jd), host_fp(&td)));

    // Eingaben über die C-ABI → beim Server injiziert.
    let hi = CString::new("hi").unwrap();
    let enter = CString::new("Enter").unwrap();
    // SAFETY: gültiger Griff, nullterminierte Strings.
    unsafe {
        lbw_text(h, hi.as_ptr());
        lbw_key(h, enter.as_ptr());
        lbw_mouse(h, 10, 20);
        lbw_button(h, 1, true);
    }
    let t0 = Instant::now();
    while inj.0.lock().unwrap().len() < 6 && t0.elapsed() < Duration::from_secs(10) {
        std::thread::sleep(Duration::from_millis(20));
    }
    let got = inj.0.lock().unwrap().clone();
    let want = [
        ClientMsg::Text("h".into()),
        ClientMsg::Text("i".into()),
        ClientMsg::Key {
            key: "Enter".into(),
            down: true,
        },
        ClientMsg::Key {
            key: "Enter".into(),
            down: false,
        },
        ClientMsg::MouseMove { x: 10, y: 20 },
        ClientMsg::Button {
            button: 1,
            down: true,
        },
    ];
    assert_eq!(&got[got.len().saturating_sub(6)..], &want[..], "{got:?}");
    // SAFETY: einmalige Freigabe.
    unsafe { lbw_free(h) };
}

/// Falscher Pin am Jump: keine Verbindung, Fehler in der Statuszeile.
#[test]
fn wrong_pin_never_connects() {
    if !std::path::Path::new("/usr/sbin/sshd").exists() {
        return;
    }
    let _ = std::fs::create_dir_all("/run/sshd");
    let jd = tmpdir("mpin");
    let _jump = sshd(2244, &jd);
    let cfg = format!(
        r#"{{"hops":["{}@127.0.0.1:2244"],"identities":[{}],"pins":["SHA256:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"],"lbw_port":1}}"#,
        user(),
        json_str(&user_key(&jd))
    );
    let c = CString::new(cfg).unwrap();
    let h = lbw_new(c.as_ptr());
    let t0 = Instant::now();
    let mut status = String::new();
    while t0.elapsed() < Duration::from_secs(8) && !status.contains("SSH:") {
        // SAFETY: gültiger Griff.
        let f = unsafe { lbw_poll(h, std::ptr::null_mut(), 0) };
        assert_eq!(f & flags::UP, 0, "darf nie verbinden");
        status = String::from_utf8(read(lbw_status, h)).unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }
    assert!(status.contains("SSH:"), "{status}");
    // SAFETY: einmalige Freigabe.
    unsafe { lbw_free(h) };
}

fn json_str(s: &str) -> String {
    let mut o = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            '\n' => o.push_str("\\n"),
            c => o.push(c),
        }
    }
    o.push('"');
    o
}

/// Konfig wie die Apps sie bauen; Ziel-sshd aus Jump-Sicht = 127.0.0.1:2243.
fn serde_json_like(hops: &[(String, String)], lbw: u16) -> String {
    let h: Vec<String> = hops.iter().map(|(a, _)| json_str(a)).collect();
    let k: Vec<String> = hops.iter().map(|(_, b)| json_str(b)).collect();
    format!(
        r#"{{"hops":[{}],"identities":[{}],"target_sshd_host":"127.0.0.1","target_sshd_port":2243,"lbw_port":{lbw}}}"#,
        h.join(","),
        k.join(",")
    )
}
