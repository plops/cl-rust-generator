//! Baut `tests/c/header_check.c` mit `cc` und die iOS-Hülle
//! `mobile/ios/Sources/LbwCore.swift` (+ `mobile/ios/Tests/main.swift`) mit
//! `swiftc` gegen den Host-Build (`liblbw_mobile_core.so`) und lässt beide
//! gegen einen echten `serve_client` (Stub-OCR) laufen: belegt, dass der
//! Header `include/lbw_core.h` und der Swift-Code der iOS-App exakt zur
//! Rust-ABI passen. Ohne `cc`/`swiftc` jeweils übersprungen.

use std::net::TcpListener;
use std::path::PathBuf;
use std::process::Command;

use clap::Parser;
use image::RgbImage;
use lbw_common::{Rect, TextItem};
use lbw_log::Recorder;
use lbw_server::capture::{SharedSource, solid};
use lbw_server::config::Config;
use lbw_server::input::Injector;
use lbw_server::session::{Recognize, serve_client};

struct Stub;
impl Recognize for Stub {
    fn text(&mut self, _: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ok(vec![TextItem {
            id: 0,
            rect: Rect::new(8, 8, 64, 16),
            fg: [0; 3],
            bg: [255; 3],
            text: "C".into(),
        }])
    }
}

/// Verzeichnis der Host-cdylib (Testbinary liegt in target/<profil>/deps).
fn libdir() -> PathBuf {
    let exe = std::env::current_exe().unwrap();
    let d = exe.parent().unwrap().parent().unwrap().to_path_buf();
    assert!(
        d.join("liblbw_mobile_core.so").exists(),
        "cdylib fehlt in {}",
        d.display()
    );
    d
}

#[test]
fn c_header_matches_rust_abi() {
    if Command::new("cc").arg("--version").output().is_err() {
        eprintln!("übersprungen: kein cc");
        return;
    }
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let libdir = libdir();
    let bin = std::env::temp_dir().join(format!("lbw-header-check-{}", std::process::id()));
    let st = Command::new("cc")
        .args(["-std=gnu11", "-Wall", "-Werror", "-o"])
        .arg(&bin)
        .arg(dir.join("tests/c/header_check.c"))
        .arg(format!("-L{}", libdir.display()))
        .arg("-llbw_mobile_core")
        .status()
        .unwrap();
    assert!(st.success(), "C-Test kompiliert nicht gegen lbw_core.h");
    run_against_server(&bin, &libdir);
}

#[test]
fn ios_swift_wrapper_works_against_rust_core() {
    if Command::new("swiftc").arg("--version").output().is_err() {
        eprintln!("übersprungen: kein swiftc");
        return;
    }
    let ios = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../ios");
    let libdir = libdir();
    let bin = std::env::temp_dir().join(format!("lbw-swift-check-{}", std::process::id()));
    let st = Command::new("swiftc")
        .arg("-import-objc-header")
        .arg(ios.join("Bridging.h"))
        .arg("-I")
        .arg(ios.join("../core/include"))
        .arg(ios.join("Sources/LbwCore.swift"))
        .arg(ios.join("Tests/main.swift"))
        .arg(format!("-L{}", libdir.display()))
        .arg("-llbw_mobile_core")
        .arg("-o")
        .arg(&bin)
        .status()
        .unwrap();
    assert!(
        st.success(),
        "Swift-Hülle kompiliert nicht gegen lbw_core.h"
    );
    run_against_server(&bin, &libdir);
}

/// Startet eine Session (rotes Bild, ein Text) und das Prüfprogramm dagegen.
fn run_against_server(bin: &PathBuf, libdir: &PathBuf) {
    let l = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = l.local_addr().unwrap().port();
    let srv = std::thread::spawn(move || {
        let (s, _) = l.accept().unwrap();
        let mut src = SharedSource::new(solid(1280, 720, [220, 20, 20]));
        let cfg = Config::try_parse_from(["lbw-server"]).unwrap();
        serve_client(
            s,
            &cfg,
            &mut src,
            &mut Stub,
            Some(100),
            &Recorder::none(),
            None::<Injector>,
        )
    });
    let out = Command::new(bin)
        .arg(port.to_string())
        .env("LD_LIBRARY_PATH", libdir)
        .output()
        .unwrap();
    let _ = std::fs::remove_file(bin);
    let txt = String::from_utf8_lossy(&out.stdout);
    eprintln!("{txt}");
    assert!(out.status.success(), "{txt}");
    srv.join().unwrap().unwrap();
}
