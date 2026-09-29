//! `lbw-server` — nur Verdrahtung: Konfiguration → X11-Quelle, Modelle,
//! Eingabe-Thread, TCP-Accept, Pipeline-Thread.

use std::net::TcpListener;
use std::sync::mpsc;

use lbw_server::analyze::Analyzer;
use lbw_server::capture::X11Source;
use lbw_server::config::Config;
use lbw_server::input::Injector;
use lbw_server::pipeline::Pipeline;
use lbw_server::scheduler::Outbox;
use lbw_server::session::{Shared, serve};

fn main() {
    let cfg = match Config::parse(std::env::args().skip(1)) {
        Ok(c) => c,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };
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
    let d = cfg.display.as_deref();
    let src = X11Source::open(d, cfg.x, cfg.y, cfg.size, cfg.size)?;
    let an = Analyzer::load(&cfg.models)?;
    if let Some(gs) = an.gui_size()
        && gs != (cfg.size, cfg.size)
    {
        return Err(format!(
            "GUI-Modell erwartet {gs:?}, Ausschnitt ist {0}x{0} (--gui none?)",
            cfg.size
        ));
    }

    let (tx, rx) = mpsc::channel();
    if cfg.input {
        let mut inj = Injector::open(d, (cfg.x, cfg.y), (cfg.size, cfg.size))?;
        std::thread::spawn(move || {
            for i in rx {
                if let Err(e) = inj.handle(&i) {
                    eprintln!("[input] {e}");
                }
            }
        });
    }

    let size = (cfg.size as u16, cfg.size as u16);
    let sh = Shared::new(Outbox::new(cfg.rate), size, tx, cfg.dead_after);
    let listener = TcpListener::bind(&cfg.listen).map_err(|e| format!("{}: {e}", cfg.listen))?;
    eprintln!(
        "[server] lauscht auf {} ({}x{}@{},{}; {} B/s)",
        cfg.listen, cfg.size, cfg.size, cfg.x, cfg.y, cfg.rate
    );
    serve(listener, sh.clone());
    Pipeline::new(Box::new(src), Box::new(an), sh, cfg.pipe).run();
    Ok(())
}
