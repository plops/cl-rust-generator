//! `07_session` — Client-Bedienung: Handshake, Input-Thread und
//! Capture → OCR → Maske → Bounding-Box mit direktem TCP.
//! Kein Scheduler, kein Client-State: Reconnect beginnt bei Vollbild.
//!
//! Instrumentierung: Jede gesendete/empfangene Nachricht sowie pro Frame die
//! Pipeline-Stufen (Capture/det/rec/Mask+Diff/Encode/Send) gehen an den
//! `Recorder` (No-op ohne `--record`).

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

use image::RgbImage;

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, TextItem};
use lbw_log::{FrameMs, MsgKind, Recorder, TileStat, fnv1a64};

use crate::av1::encode_rgb;
use crate::capture::FrameSource;
use crate::config::Config;
use crate::input::Injector;
use crate::ocr::Ocr;
use crate::textids::{assign_ids, diff_texts};
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
/// Zeit für das Client-`Hello`.
const HELLO_TIMEOUT: Duration = Duration::from_secs(10);

fn ms(t: Instant) -> f32 {
    t.elapsed().as_secs_f32() * 1000.0
}

/// Sendet eine Text-Nachricht mit Recording; `false` bei Abriss.
fn send_text(wr: &mut TcpStream, rec: &Recorder, m: &ServerMsg, text_bytes: &mut usize) -> bool {
    match write_msg(wr, m) {
        Ok(n) => {
            *text_bytes += n;
            rec.msg_server(m, n);
            true
        }
        Err(_) => false,
    }
}

/// Bedient genau einen Client bis zum Abriss. `max_frames` begrenzt die
/// Schleife (Tests); `None` läuft für immer. Verbindungsabbrüche sind
/// `Ok(())`, nur lokale Fehler (Capture, OCR) sind `Err`.
pub fn serve_client<S: FrameSource, R: Recognize>(
    stream: TcpStream,
    cfg: &Config,
    src: &mut S,
    ocr: &mut R,
    max_frames: Option<u64>,
    rec: &Recorder,
) -> Result<(), String> {
    stream
        .set_read_timeout(Some(HELLO_TIMEOUT))
        .map_err(|e| e.to_string())?;
    let mut rd = stream.try_clone().map_err(|e| e.to_string())?;
    let mut wr = stream;
    let mut fr = FrameReader::new();

    // Handshake: erstes Client-`Hello` prüfen (Timeout → Abbruch).
    let hello_body = match fr.read(&mut rd).map_err(|e| e.to_string())? {
        Read1::Frame(b) => b,
        Read1::Idle => return Err("kein Hello vom Client".into()),
    };
    let hello: ClientMsg = decode_msg(&hello_body).map_err(|e| e.to_string())?;
    rec.msg_client_raw(&hello, &hello_body);
    let ClientMsg::Hello { version } = hello else {
        return Err("erste Nachricht war kein Hello".into());
    };
    if version != PROTO_VERSION {
        return Err(format!("Protokoll {version}, erwartet {PROTO_VERSION}"));
    }
    let n = write_msg(&mut wr, &ServerMsg::Hello).map_err(|e| e.to_string())?;
    rec.msg_server(&ServerMsg::Hello, n);

    // Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet.
    rd.set_read_timeout(Some(Duration::from_millis(200)))
        .map_err(|e| e.to_string())?;
    let stop = Arc::new(AtomicBool::new(false));
    let input = match Injector::open((cfg.x, cfg.y)) {
        Ok(inj) => Some(spawn_input(
            rd,
            fr,
            inj,
            stop.clone(),
            cfg.verbose,
            rec.clone(),
        )),
        Err(e) => {
            eprintln!("[input] {e} — laufe ohne Eingabe");
            None
        }
    };

    let mut prev: Option<RgbImage> = None;
    let mut last_texts: Vec<TextItem> = Vec::new();
    let mut next_text_id: u64 = 1;
    let mut frames: u64 = 0;
    let result = loop {
        if max_frames.is_some_and(|n| frames >= n) {
            break Ok(());
        }
        frames += 1;
        let frame_start = Instant::now();
        let img = match src.grab() {
            Ok(i) => i,
            Err(e) => break Err(e),
        };
        let cap_ms = ms(frame_start);
        let texts = match ocr.text(&img) {
            Ok(t) => t,
            Err(e) => break Err(e),
        };
        let (det_ms, ocr_rec_ms) = ocr.last_ms();
        // Text als Delta (v3): 1. Frame sync_all, danach nur remove/change.
        let mut bytes = 0;
        let mut text_bytes = 0;
        let mut send_ms = 0.0;
        let cur = assign_ids(&last_texts, texts, &mut next_text_id);
        let (removed, changed) = diff_texts(&last_texts, &cur);
        let sync_all = frames == 1 && !cur.is_empty();
        let text_changed = sync_all || !removed.is_empty() || !changed.is_empty();
        if text_changed {
            let t = Instant::now();
            let mut ok = true;
            if sync_all {
                ok = send_text(&mut wr, rec, &ServerMsg::ClearText, &mut text_bytes);
            }
            if ok {
                if sync_all {
                    for tm in &cur {
                        ok = send_text(
                            &mut wr,
                            rec,
                            &ServerMsg::AddText(tm.clone()),
                            &mut text_bytes,
                        );
                        if !ok {
                            break;
                        }
                    }
                } else {
                    for id in &removed {
                        ok = send_text(&mut wr, rec, &ServerMsg::RemoveText(*id), &mut text_bytes);
                        if !ok {
                            break;
                        }
                    }
                    if ok {
                        for tm in &changed {
                            ok = send_text(
                                &mut wr,
                                rec,
                                &ServerMsg::AddText(tm.clone()),
                                &mut text_bytes,
                            );
                            if !ok {
                                break;
                            }
                        }
                    }
                }
            }
            send_ms += ms(t);
            if !ok {
                break Ok(());
            }
            last_texts = cur.clone();
        }
        let t = Instant::now();
        let mut masked = img.clone();
        for tm in &cur {
            let m = pad_rect(tm.rect, MASK_PAD, masked.width(), masked.height());
            fill_rect(&mut masked, m, tm.bg);
        }
        let dirty = dirty_bbox(prev.as_ref(), &masked);
        let mask_diff_ms = ms(t);
        // Genau ein AV1-Bild pro Frame (ein Header-Overhead statt N×).
        let mut tiles = 0;
        let mut tile_stat = None;
        let mut enc_ms = 0.0;
        if let Some(r) = dirty {
            let rgb = crop_rgb(&masked, r);
            let t = Instant::now();
            let data = match encode_rgb(&rgb, r.w as usize, r.h as usize, cfg.quantizer) {
                Ok(d) => d,
                Err(e) => break Err(e),
            };
            enc_ms = ms(t);
            let msg = ServerMsg::Tile {
                x: r.x,
                y: r.y,
                data,
            };
            let t = Instant::now();
            let n = match write_msg(&mut wr, &msg) {
                Ok(n) => n,
                Err(_) => break Ok(()),
            };
            send_ms += ms(t);
            bytes += n;
            let ServerMsg::Tile { data, .. } = &msg else {
                unreachable!()
            };
            tile_stat = Some(TileStat {
                x: r.x,
                y: r.y,
                w: r.w,
                h: r.h,
                bytes: data.len(),
                hash: fnv1a64(data),
            });
            rec.msg_server(&msg, n);
            tiles = 1;
        }
        rec.frame(
            frames,
            cur.len(),
            text_changed,
            text_bytes,
            tile_stat,
            FrameMs {
                capture: cap_ms,
                det: det_ms as f32,
                rec: ocr_rec_ms as f32,
                mask_diff: mask_diff_ms,
                encode: enc_ms,
                send: send_ms,
            },
        );
        if cfg.verbose {
            let (det_ms, rec_ms) = ocr.last_ms();
            let (chits, clook) = ocr.cache_stats();
            eprintln!(
                "[frame {frames}] {} Texte (det {det_ms:.1} ms, rec {rec_ms:.1} ms, cache {chits}/{clook}), {tiles} Kacheln, {bytes} B",
                cur.len()
            );
        }
        prev = Some(masked);
        // Feste Taktung: nur die Restzeit schlafen, sonst kostet jede
        // Pipeline-Phase (OCR+AV1) zusätzlich volle 100 ms (~6 statt 10 fps).
        std::thread::sleep(FRAME_GAP.saturating_sub(frame_start.elapsed()));
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
    rec: &Recorder,
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
                    rec.msg_client_raw(&m, &b);
                    on_msg(m);
                }
                Err(_) => break, // Protokollfehler: Client weg
            },
            Ok(Read1::Idle) => {} // Idle: erneut prüfen (Stop-Flag)
            Err(_) => break,      // EOF/Fehler: Client weg
        }
    }
}

fn spawn_input(
    rd: TcpStream,
    fr: FrameReader,
    mut inj: Injector,
    stop: Arc<AtomicBool>,
    verbose: bool,
    rec: Recorder,
) -> std::thread::JoinHandle<()> {
    if verbose {
        eprintln!("[input] bereit");
    }
    std::thread::spawn(move || {
        input_loop(rd, fr, &stop, verbose, &rec, |m| {
            let t = Instant::now();
            let ok = match inj.handle(&m) {
                Ok(()) => true,
                Err(e) => {
                    eprintln!("[input] {e}");
                    false
                }
            };
            rec.inject(MsgKind::of_client(&m), ms(t), ok);
        });
    })
}
