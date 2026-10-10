//! `lbw-server` — nur Verdrahtung: Konfiguration → Quelle, OCR-Modelle,
//! TCP-Accept, Session-Schleife (ein Client nach dem anderen).

use std::net::TcpListener;

use clap::Parser;
use lbw_common::{HEIGHT, WIDTH};
use lbw_log::Recorder;

use lbw_server::capture::ScrapSource;
use lbw_server::config::Config;
use lbw_server::detect::Provider;
use lbw_server::input::Injector;
use lbw_server::ocr::Ocr;
use lbw_server::session::serve_client;

/// ONNX-Threads (fest: kein `--threads`).
const OCR_THREADS: usize = 8;

fn main() {
    let cfg = Config::parse();
    if let Err(e) = cfg.validate() {
        eprintln!("{e}");
        std::process::exit(2);
    }
    if let Err(e) = run(cfg) {
        eprintln!("lbw-server: {e}");
        std::process::exit(1);
    }
}

fn run(cfg: Config) -> Result<(), String> {
    if cfg.is_public() {
        eprintln!(
            "WARNUNG: {} ist nicht localhost — das Protokoll hat keine Authentifizierung!",
            cfg.listen
        );
    }
    if std::env::var("WAYLAND_DISPLAY").is_ok()
        || std::env::var("XDG_SESSION_TYPE").is_ok_and(|t| t == "wayland")
    {
        eprintln!(
            "WARNUNG: Wayland-Sitzung erkannt — X11-Capture sieht dort nur Schwarz. \
             Für echte Bildschirminhalte eine Xorg-Sitzung verwenden!"
        );
    }
    let mut src = ScrapSource::open(cfg.x, cfg.y, WIDTH, HEIGHT)?;
    let provider = if cfg.cpu {
        Provider::Cpu
    } else {
        Provider::Auto
    };
    let mut ocr = Ocr::load(&cfg.models, OCR_THREADS, provider)?;
    let rec = match &cfg.record {
        Some(p) => Recorder::create(p, "lbw-server", &std::env::args().collect::<Vec<_>>())?,
        None => Recorder::none(),
    };
    let listener = TcpListener::bind(&cfg.listen).map_err(|e| format!("{}: {e}", cfg.listen))?;
    eprintln!(
        "[server] lauscht auf {} ({}x{}@{},{}, q={})",
        cfg.listen, WIDTH, HEIGHT, cfg.x, cfg.y, cfg.quantizer
    );
    for stream in listener.incoming() {
        match stream {
            Ok(s) => {
                let peer = s
                    .peer_addr()
                    .map(|a| a.to_string())
                    .unwrap_or_else(|_| "?".into());
                eprintln!("[server] Client verbunden");
                rec.gap_up(&peer);
                let inj = match Injector::open((cfg.x, cfg.y)) {
                    Ok(i) => Some(i),
                    Err(e) => {
                        eprintln!("[input] {e} — laufe ohne Eingabe");
                        None
                    }
                };
                match serve_client(s, &cfg, &mut src, &mut ocr, None, &rec, inj) {
                    Ok(()) => rec.gap_down("ok"),
                    Err(e) => {
                        eprintln!("[server] Session-Fehler: {e}");
                        rec.gap_down(&e);
                    }
                }
                eprintln!("[server] Client getrennt");
            }
            Err(e) => eprintln!("[server] Accept: {e}"),
        }
    }
    Ok(())
}
