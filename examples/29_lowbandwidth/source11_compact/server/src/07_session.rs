//! `07_session` — Client-Bedienung: Auth-Handshake, Input-Thread und
//! Capture → OCR → Maske → Bounding-Box mit direktem TCP.
//!
//! Kein Scheduler, kein Client-State: Reconnect beginnt bei Vollbild.
//! Der Server spricht nur localhost (SSH-Tunnel davor); zusätzlich beweist
//! der Client per Token-HMAC, dass er das LBW-Token kennt (Challenge/Auth).

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::sync_channel;
use std::time::{Duration, Instant};

use image::RgbImage;

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, TextItem, verify_auth};

use crate::av1::encode_rgb;
use crate::capture::FrameSource;
use crate::config::Config;
use crate::input::Inject;
use crate::ocr::Ocr;
use crate::pace::{Pacer, wait_paced};
use crate::stabilize::Stabilizer;
use crate::tiles::{MASK_PAD, crop_rgb, dirty_bbox, fill_rect, pad_rect};

/// Was die Session zum Erkennen braucht (Tests nutzen Attrappen).
pub trait Recognize {
    fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String>;
    /// Letzte Inferenz-Zeiten (Detektion, Erkennung) in ms; Default 0.
    fn last_ms(&self) -> (f64, f64) {
        (0.0, 0.0)
    }
    /// Cache-Statistik (Treffer, Zugriffe); Default 0 (nur echtes `Ocr`).
    fn cache_stats(&self) -> (u64, u64) {
        (0, 0)
    }
}

impl Recognize for Ocr {
    fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ocr::text(self, img)
    }

    fn last_ms(&self) -> (f64, f64) {
        self.last_ms
    }

    fn cache_stats(&self) -> (u64, u64) {
        self.cache_stats()
    }
}

/// Abstand zweier Frames (10 fps genügen für 720p).
const FRAME_GAP: Duration = Duration::from_millis(100);
/// Zeit für Auth + Client-`Hello`.
const HELLO_TIMEOUT: Duration = Duration::from_secs(10);
/// Eingabe-Kanal (bounded: Volllaufen lässt stale Inputs fallen statt zu wachsen).
const INPUT_CAP: usize = 64;

fn ms(t: Instant) -> f32 {
    t.elapsed().as_secs_f32() * 1000.0
}

fn nonce16() -> Result<[u8; 16], String> {
    let mut n = [0u8; 16];
    getrandom::getrandom(&mut n).map_err(|e| e.to_string())?;
    Ok(n)
}

/// Sendet eine Text-Nachricht; `false` bei Abriss.
/// Texte haben Vorfahrt: Sie gehen sofort raus und buchen nur
/// (`Pacer::note`) — die nächste Kachel trägt ihre Schuld ab.
fn send_text(wr: &mut TcpStream, pacer: &mut Pacer, m: &ServerMsg, text_bytes: &mut usize) -> bool {
    match write_msg(wr, m) {
        Ok(n) => {
            *text_bytes += n;
            pacer.note(n);
            true
        }
        Err(_) => false,
    }
}

/// Leitungsbytes einer Tile-Nachricht (Schätzung ohne Nach-Kodieren:
/// 4 B Frame-Header + ~12 B bincode-Overhead für Variante/x/y/Länge).
fn tile_wire_len(data_len: usize) -> usize {
    data_len + 16
}

/// Bedient genau einen Client bis zum Abriss. `max_frames` begrenzt die
/// Schleife (Tests); `None` läuft für immer. Verbindungsabbrüche sind
/// `Ok(())`, nur lokale Fehler (Capture, OCR) sind `Err`. `inj` injiziert
/// Eingaben (`None`: kein Display — Reader läuft als Drain).
#[allow(clippy::too_many_arguments)]
pub fn serve_client<S: FrameSource, R: Recognize, I: Inject + Send + 'static>(
    stream: TcpStream,
    cfg: &Config,
    src: &mut S,
    ocr: &mut R,
    max_frames: Option<u64>,
    token: &[u8],
    inj: Option<I>,
) -> Result<(), String> {
    stream
        .set_read_timeout(Some(HELLO_TIMEOUT))
        .map_err(|e| e.to_string())?;
    let mut rd = stream.try_clone().map_err(|e| e.to_string())?;
    let mut wr = stream;
    let mut fr = FrameReader::new();

    // Auth-Handshake: Challenge → Auth (Token-HMAC) → Hello.
    let nonce = nonce16()?;
    write_msg(
        &mut wr,
        &ServerMsg::Challenge {
            version: PROTO_VERSION,
            nonce,
        },
    )
    .map_err(|e| e.to_string())?;
    let auth: ClientMsg = fr
        .read_msg(&mut rd)
        .map_err(|e| e.to_string())?
        .ok_or("kein Auth vom Client")?;
    let ClientMsg::Auth { mac } = auth else {
        return Err("erste Nachricht war kein Auth".into());
    };
    if !verify_auth(token, &nonce, &mac) {
        return Err("falsches LBW-Token".into());
    }
    let hello: ClientMsg = fr
        .read_msg(&mut rd)
        .map_err(|e| e.to_string())?
        .ok_or("kein Hello vom Client")?;
    let ClientMsg::Hello { version } = hello else {
        return Err("zweite Nachricht war kein Hello".into());
    };
    if version != PROTO_VERSION {
        return Err(format!("Protokoll {version}, erwartet {PROTO_VERSION}"));
    }
    let mut pacer = Pacer::new(cfg.budget);
    let n = write_msg(&mut wr, &ServerMsg::Hello).map_err(|e| e.to_string())?;
    pacer.note(n);

    // Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet.
    // Der Reader läuft IMMER (ohne Injektor als Drain): Sein Ende ist das
    // einzige Abriss-Signal — bei statischem Bild schreibt die Hauptschleife
    // nie, sähe den toten Client also nie.
    // Die Injektion läuft dahinter entkoppelt (eigener Thread, begrenzter
    // Kanal): Volllaufen lässt stale Inputs fallen statt zu wachsen.
    rd.set_read_timeout(Some(Duration::from_millis(200)))
        .map_err(|e| e.to_string())?;
    let stop = Arc::new(AtomicBool::new(false));
    let input = match inj {
        Some(inj) => {
            let (tx, rx) = sync_channel::<ClientMsg>(INPUT_CAP);
            std::thread::spawn(move || {
                let mut inj = inj;
                for m in rx {
                    if let Err(e) = inj.handle(&m) {
                        eprintln!("[input] {e}");
                    }
                }
            });
            spawn_reader(rd, fr, stop.clone(), cfg.verbose, move |m| {
                let _ = tx.try_send(m);
            })
        }
        None => spawn_reader(rd, fr, stop.clone(), cfg.verbose, |_| {}),
    };

    let mut prev: Option<RgbImage> = None;
    let mut stabilizer = Stabilizer::new();
    let mut sent_texts: Vec<TextItem> = Vec::new();
    let mut frames: u64 = 0;
    let result = loop {
        if input.is_finished() {
            // Reader-Thread weg (EOF/Protokollfehler): Client ist tot.
            break Ok(());
        }
        if max_frames.is_some_and(|n| frames >= n) {
            break Ok(());
        }
        frames += 1;
        let frame_start = Instant::now();
        let img = match src.grab() {
            Ok(i) => i,
            Err(e) => break Err(e),
        };
        let texts = match ocr.text(&img) {
            Ok(t) => t,
            Err(e) => break Err(e),
        };
        // Text als Delta (v4): 1. Frame ClearText + NewText je Zeile,
        // danach RemoveText + NewText (Inhalt neu) + MoveText (nur Box neu).
        let mut text_bytes = 0;
        let delta = stabilizer.step(texts, frames == 1);
        // Maske aus gesendeten Rechtecken: Stabilizer-IDs → aktuelle Boxen.
        for m in &delta {
            match m {
                ServerMsg::NewText(t) => {
                    sent_texts.retain(|p| p.id != t.id);
                    sent_texts.push(t.clone());
                }
                ServerMsg::MoveText { id, rect } => {
                    if let Some(p) = sent_texts.iter_mut().find(|p| p.id == *id) {
                        p.rect = *rect;
                    }
                }
                ServerMsg::RemoveText(id) => {
                    sent_texts.retain(|p| p.id != *id);
                }
                ServerMsg::ClearText => sent_texts.clear(),
                _ => {}
            }
        }
        let cur = sent_texts.clone();
        if !delta.is_empty() {
            let mut ok = true;
            for m in &delta {
                ok = send_text(&mut wr, &mut pacer, m, &mut text_bytes);
                if !ok {
                    break;
                }
            }
            if !ok {
                break Ok(());
            }
        }
        let t = Instant::now();
        let mut masked = img.clone();
        for tm in &cur {
            let m = pad_rect(tm.rect, MASK_PAD, masked.width(), masked.height());
            fill_rect(&mut masked, m, tm.bg);
        }
        let dirty = dirty_bbox(prev.as_ref(), &masked);
        let _mask_diff_ms = ms(t);
        // Genau ein AV1-Bild pro Frame (ein Header-Overhead statt N×).
        let mut tiles = 0;
        if let Some(r) = dirty {
            let rgb = crop_rgb(&masked, r);
            let data = match encode_rgb(&rgb, r.w as usize, r.h as usize, cfg.quantizer) {
                Ok(d) => d,
                Err(e) => break Err(e),
            };
            // Pacing: Kachel auf Budget-Rate dosieren, damit der Kanal
            // einen gleichmäßigen Rinnsal statt Burst-Pause sieht. In
            // Scheiben, damit ein Disconnect die Session nicht keilt.
            if !wait_paced(&mut pacer, tile_wire_len(data.len()), &|| {
                input.is_finished()
            }) {
                break Ok(());
            }
            let msg = ServerMsg::Tile {
                x: r.x,
                y: r.y,
                data,
            };
            match write_msg(&mut wr, &msg) {
                Ok(_) => {}
                Err(_) => break Ok(()),
            }
            tiles = 1;
        }
        if cfg.verbose {
            let (det_ms, rec_ms) = ocr.last_ms();
            let (chits, clook) = ocr.cache_stats();
            eprintln!(
                "[frame {frames}] {} Texte (det {det_ms:.1} ms, rec {rec_ms:.1} ms, cache {chits}/{clook}), {tiles} Kacheln",
                cur.len()
            );
        }
        prev = Some(masked);
        // Feste Taktung: nur die Restzeit schlafen, sonst kostet jede
        // Pipeline-Phase (OCR+AV1) zusätzlich volle 100 ms (~6 statt 10 fps).
        std::thread::sleep(FRAME_GAP.saturating_sub(frame_start.elapsed()));
    };

    stop.store(true, Ordering::Relaxed);
    let _ = input.join();
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
        match fr.read(&mut rd) {
            Ok(Read1::Frame(b)) => match decode_msg::<ClientMsg>(&b) {
                Ok(m) => {
                    if verbose {
                        eprintln!("[input] {m:?}");
                    }
                    on_msg(m);
                }
                Err(_) => break, // Protokollfehler: Client weg
            },
            Ok(Read1::Idle) => {} // Idle: erneut prüfen (Stop-Flag)
            Err(_) => break,      // EOF/Fehler: Client weg
        }
    }
}

/// Startet den Eingabe-Reader (Injektion oder Drain — s. `serve_client`).
fn spawn_reader(
    rd: TcpStream,
    fr: FrameReader,
    stop: Arc<AtomicBool>,
    verbose: bool,
    on_msg: impl FnMut(ClientMsg) + Send + 'static,
) -> std::thread::JoinHandle<()> {
    if verbose {
        eprintln!("[input] bereit");
    }
    std::thread::spawn(move || {
        input_loop(rd, fr, &stop, verbose, on_msg);
    })
}
