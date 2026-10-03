//! `lbw-server` — nur Verdrahtung: Konfiguration → Quelle, OCR-Modelle,
//! TCP-Accept, Session-Schleife (ein Client nach dem anderen).

use std::net::TcpListener;

use clap::Parser;
use lbw_common::SIZE;

use lbw_server::capture::ScrapSource;
use lbw_server::config::Config;
use lbw_server::ocr::Ocr;
use lbw_server::session::serve_client;

/// ONNX-Threads (fest: MVP ohne `--threads`).
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
    let mut src = ScrapSource::open(cfg.x, cfg.y, SIZE, SIZE)?;
    let mut ocr = if cfg.no_ocr {
        Ocr::Disabled
    } else {
        Ocr::load(&cfg.models, OCR_THREADS)?
    };
    let listener = TcpListener::bind(&cfg.listen).map_err(|e| format!("{}: {e}", cfg.listen))?;
    eprintln!(
        "[server] lauscht auf {} ({}x{}@{},{}, q={})",
        cfg.listen, SIZE, SIZE, cfg.x, cfg.y, cfg.quantizer
    );
    for stream in listener.incoming() {
        match stream {
            Ok(s) => {
                eprintln!("[server] Client verbunden");
                if let Err(e) = serve_client(s, &cfg, &mut src, &mut ocr, None) {
                    eprintln!("[server] Session-Fehler: {e}");
                }
                eprintln!("[server] Client getrennt");
            }
            Err(e) => eprintln!("[server] Accept: {e}"),
        }
    }
    Ok(())
}
