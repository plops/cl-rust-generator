//! `07_session` — Client-Bedienung: Handshake, Input-Thread und
//! Capture → OCR → Maske → Bounding-Box mit direktem TCP.
//! Kein Scheduler, kein Client-State: Reconnect beginnt bei Vollbild.

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use image::RgbImage;

use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, TextItem};

use crate::av1::encode_rgb;
use crate::capture::FrameSource;
use crate::config::Config;
use crate::input::Injector;
use crate::ocr::Ocr;
use crate::tiles::{MASK_PAD, crop_rgb, dirty_bbox, fill_rect, pad_rect};

/// Was die Session zum Erkennen braucht (Tests nutzen Attrappen).
pub trait Recognize {
    fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String>;
}

impl Recognize for Ocr {
    fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ocr::text(self, img)
    }
}

/// Abstand zweier Frames (10 fps genügen fürs MVP).
const FRAME_GAP: Duration = Duration::from_millis(100);
/// Zeit für das Client-`Hello`.
const HELLO_TIMEOUT: Duration = Duration::from_secs(10);

/// Bedient genau einen Client bis zum Abriss. `max_frames` begrenzt die
/// Schleife (Tests); `None` läuft für immer. Verbindungsabbrüche sind
/// `Ok(())`, nur lokale Fehler (Capture, OCR) sind `Err`.
pub fn serve_client<S: FrameSource, R: Recognize>(
    stream: TcpStream,
    cfg: &Config,
    src: &mut S,
    ocr: &mut R,
    max_frames: Option<u64>,
) -> Result<(), String> {
    stream
        .set_read_timeout(Some(HELLO_TIMEOUT))
        .map_err(|e| e.to_string())?;
    let mut rd = stream.try_clone().map_err(|e| e.to_string())?;
    let mut wr = stream;
    let mut fr = FrameReader::new();

    // Handshake: erstes Client-`Hello` prüfen (Timeout → Abbruch).
    let hello = match fr
        .read_msg::<ClientMsg>(&mut rd)
        .map_err(|e| e.to_string())?
    {
        Some(m) => m,
        None => return Err("kein Hello vom Client".into()),
    };
    let ClientMsg::Hello { version } = hello else {
        return Err("erste Nachricht war kein Hello".into());
    };
    if version != PROTO_VERSION {
        return Err(format!("Protokoll {version}, erwartet {PROTO_VERSION}"));
    }
    write_msg(&mut wr, &ServerMsg::Hello).map_err(|e| e.to_string())?;

    // Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet.
    rd.set_read_timeout(Some(Duration::from_millis(200)))
        .map_err(|e| e.to_string())?;
    let stop = Arc::new(AtomicBool::new(false));
    let input = if cfg.no_input {
        if cfg.verbose {
            eprintln!("[input] deaktiviert (--no-input)");
        }
        None
    } else {
        match Injector::open((cfg.x, cfg.y)) {
            Ok(inj) => Some(spawn_input(rd, fr, inj, stop.clone(), cfg.verbose)),
            Err(e) => {
                eprintln!("[input] {e} — laufe ohne Eingabe");
                None
            }
        }
    };

    let mut prev: Option<RgbImage> = None;
    let mut last_texts: Vec<TextItem> = Vec::new();
    let mut frames: u64 = 0;
    let result = loop {
        if max_frames.is_some_and(|n| frames >= n) {
            break Ok(());
        }
        frames += 1;
        let img = match src.grab() {
            Ok(i) => i,
            Err(e) => break Err(e),
        };
        let texts = match ocr.text(&img) {
            Ok(t) => t,
            Err(e) => break Err(e),
        };
        // Text nur bei Änderung senden (sonst Dauerlast bei Standbild).
        let mut bytes = 0;
        if texts != last_texts {
            if write_msg(&mut wr, &ServerMsg::ClearText).is_err() {
                break Ok(());
            }
            let mut failed = false;
            for t in &texts {
                if write_msg(&mut wr, &ServerMsg::AddText(t.clone())).is_err() {
                    failed = true;
                    break;
                }
            }
            if failed {
                break Ok(());
            }
            last_texts = texts.clone();
        }
        let mut masked = img.clone();
        for t in &texts {
            let m = pad_rect(t.rect, MASK_PAD, masked.width(), masked.height());
            fill_rect(&mut masked, m, t.bg);
        }
        // Genau ein AV1-Bild pro Frame (ein Header-Overhead statt N×).
        let mut tiles = 0;
        if let Some(r) = dirty_bbox(prev.as_ref(), &masked) {
            let rgb = crop_rgb(&masked, r);
            let data = match encode_rgb(&rgb, r.w as usize, r.h as usize, cfg.quantizer) {
                Ok(d) => d,
                Err(e) => break Err(e),
            };
            let msg = ServerMsg::Tile {
                x: r.x,
                y: r.y,
                data,
            };
            match write_msg(&mut wr, &msg) {
                Ok(n) => bytes += n,
                Err(_) => break Ok(()),
            }
            tiles = 1;
        }
        if cfg.verbose {
            eprintln!(
                "[frame {frames}] {} Texte, {tiles} Kacheln, {bytes} B",
                texts.len()
            );
        }
        prev = Some(masked);
        std::thread::sleep(FRAME_GAP);
    };

    stop.store(true, Ordering::Relaxed);
    if let Some(h) = input {
        let _ = h.join();
    }
    result
}

/// Liest Client-Nachrichten bis EOF/Stop und ruft `on_msg` je Nachricht.
/// Eigenständig (und `pub`), damit Tests die Eingabe-Anlieferung ohne
/// Display prüfen können; die Produktion übergibt den enigo-Injector.
pub fn input_loop(
    mut rd: TcpStream,
    mut fr: FrameReader,
    stop: &AtomicBool,
    verbose: bool,
    mut on_msg: impl FnMut(ClientMsg),
) {
    loop {
        if stop.load(Ordering::Relaxed) {
            break;
        }
        match fr.read_msg::<ClientMsg>(&mut rd) {
            Ok(Some(m)) => {
                if verbose {
                    eprintln!("[input] {m:?}");
                }
                on_msg(m);
            }
            Ok(None) => {}   // Idle: erneut prüfen (Stop-Flag)
            Err(_) => break, // EOF/Fehler: Client weg
        }
    }
}

fn spawn_input(
    rd: TcpStream,
    fr: FrameReader,
    mut inj: Injector,
    stop: Arc<AtomicBool>,
    verbose: bool,
) -> std::thread::JoinHandle<()> {
    if verbose {
        eprintln!("[input] bereit");
    }
    std::thread::spawn(move || {
        input_loop(rd, fr, &stop, verbose, |m| {
            if let Err(e) = inj.handle(&m) {
                eprintln!("[input] {e}");
            }
        });
    })
}
