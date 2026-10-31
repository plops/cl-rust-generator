//! `10_session` — Client-Bedienung: Handshake, Input-Thread, Sender-Thread
//! und Capture → OCR → Maske → gedeckelte AV1-Kacheln.
//! Reconnect beginnt bei Vollbild (kein Client-State über Verbindungen).
//!
//! source10_b (Review B1–B3): Die Schleife **wartet nie auf den Kanal**.
//! Texte gehen in die Express-Spur des Senders (`12_send`); kodiert wird
//! nur, wenn die Kachel-Spur frei ist, und zwar gegen das *Schattenbild*
//! (= was der Client schon hat), nicht gegen den Vorframe — eine wegen
//! vollem Kanal übersprungene Änderung steckt so automatisch im nächsten
//! Diff. Kacheln sind auf `--max-tile` Byte gedeckelt (`encode_capped`).
//!
//! Instrumentierung: pro Frame die Pipeline-Stufen (Capture/det/rec/
//! Mask+Diff/Encode/Einreihen) an den `Recorder`; die `Msg`-Records schreibt
//! der Sender zum echten Sendezeitpunkt. `FrameMs.pace` ist hier immer 0
//! (die Schleife pacet nicht mehr) — das Log-Format v3 bleibt unverändert.

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::channel;
use std::time::{Duration, Instant};

use image::RgbImage;

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, Rect, ServerMsg, TextItem};
use lbw_log::{Fnv1a64, FrameMs, MsgKind, Recorder, TileStat};

use crate::av1::encode_capped;
use crate::capture::FrameSource;
use crate::config::Config;
use crate::input::Inject;
use crate::ocr::Ocr;
use crate::pace::Pacer;
use crate::send::Sender;
use crate::textids::{assign_ids, diff_texts};
use crate::tiles::{MASK_PAD, dirty_bbox, fill_rect, pad_rect};

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

/// Kodier-Vorlauf: Die nächste Kachel darf schon kodiert werden, wenn der
/// Pacer noch höchstens so lange Restschuld hat (Encode p50 ~50 ms) — so
/// steht der Kanal nach dem Abfluss nicht leer, während kodiert wird.
const ENCODE_LEAD: Duration = Duration::from_millis(250);

/// Vereinigungsbox mehrerer Rechtecke (für den `Frame.tile`-Record).
fn union(rs: impl Iterator<Item = Rect>) -> Option<Rect> {
    rs.reduce(|a, b| {
        let (x0, y0) = (a.x.min(b.x), a.y.min(b.y));
        let (x1, y1) = (a.x2().max(b.x2()), a.y2().max(b.y2()));
        Rect::new(x0, y0, x1 - x0, y1 - y0)
    })
}

/// Bedient genau einen Client bis zum Abriss. `max_frames` begrenzt die
/// Schleife (Tests); `None` läuft für immer. Verbindungsabbrüche sind
/// `Ok(())`, nur lokale Fehler (Capture, OCR) sind `Err`. `inj` injiziert
/// Eingaben (`None`: kein Display — Reader läuft als Drain).
pub fn serve_client<S: FrameSource, R: Recognize, I: Inject + Send + 'static>(
    stream: TcpStream,
    cfg: &Config,
    src: &mut S,
    ocr: &mut R,
    max_frames: Option<u64>,
    rec: &Recorder,
    inj: Option<I>,
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
    let mut pacer = Pacer::new(cfg.budget);
    let n = write_msg(&mut wr, &ServerMsg::Hello).map_err(|e| e.to_string())?;
    pacer.note(n);
    rec.msg_server(&ServerMsg::Hello, n);
    let sender = Sender::spawn(&wr, pacer, rec.clone())?;

    // Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet.
    // Der Reader läuft IMMER (ohne Injektor als Drain): Sein Ende ist das
    // einzige Abriss-Signal — bei statischem Bild schreibt die Hauptschleife
    // nie, sähe den toten Client also nie (Accept verkeilt, Reconnect
    // schwarz; s. Test `session_ends_when_client_disconnects_idle`).
    // Die Injektion läuft dahinter entkoppelt (eigener Thread, unbegrenzter
    // Kanal): Ein hängendes `handle` (enigo/X11, 1× live beobachtet) darf
    // weder Reader noch Session je keilen. Der Worker ist bewusst detached
    // (kein Join — genau der würde wieder keilen); Normalfall-Ende per
    // Kanal-`Disconnect`, im Hängefall leckt ein Thread pro Session.
    rd.set_read_timeout(Some(Duration::from_millis(200)))
        .map_err(|e| e.to_string())?;
    let stop = Arc::new(AtomicBool::new(false));
    let input = match inj {
        Some(inj) => {
            let (tx, rx) = channel::<ClientMsg>();
            let r = rec.clone();
            std::thread::spawn(move || {
                let mut inj = inj;
                for m in rx {
                    let t = Instant::now();
                    let ok = match inj.handle(&m) {
                        Ok(()) => true,
                        Err(e) => {
                            eprintln!("[input] {e}");
                            false
                        }
                    };
                    r.inject(MsgKind::of_client(&m), ms(t), ok);
                }
            });
            spawn_reader(rd, fr, stop.clone(), cfg.verbose, rec.clone(), move |m| {
                let _ = tx.send(m);
            })
        }
        None => spawn_reader(rd, fr, stop.clone(), cfg.verbose, rec.clone(), |_| {}),
    };

    // Schattenbild: maskierter Stand, den der Client (nach Abfluss der
    // eingereihten Kacheln) zeigt. `None` = noch nichts → Vollbild.
    let mut shadow: Option<RgbImage> = None;
    let mut last_texts: Vec<TextItem> = Vec::new();
    let mut next_text_id: u64 = 1;
    let mut frames: u64 = 0;
    let result = loop {
        if input.is_finished() || sender.is_dead() {
            // Reader weg (EOF/Protokollfehler) oder Schreibfehler: Client tot.
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
        let cap_ms = ms(frame_start);
        let texts = match ocr.text(&img) {
            Ok(t) => t,
            Err(e) => break Err(e),
        };
        let (det_ms, ocr_rec_ms) = ocr.last_ms();
        // Text als Delta (v3): 1. Frame sync_all, danach nur remove/change.
        // Alles in die Express-Spur — wartet nie hinter Kacheln.
        let t = Instant::now();
        let cur = assign_ids(&last_texts, texts, &mut next_text_id);
        let (removed, changed) = diff_texts(&last_texts, &cur);
        let sync_all = frames == 1 && !cur.is_empty();
        let text_changed = sync_all || !removed.is_empty() || !changed.is_empty();
        let mut text_bytes = 0;
        if sync_all {
            text_bytes += sender.express(ServerMsg::ClearText);
            for tm in &cur {
                text_bytes += sender.express(ServerMsg::AddText(tm.clone()));
            }
        } else if text_changed {
            for id in &removed {
                text_bytes += sender.express(ServerMsg::RemoveText(*id));
            }
            for tm in &changed {
                text_bytes += sender.express(ServerMsg::AddText(tm.clone()));
            }
        }
        let mut send_ms = ms(t);
        last_texts = cur;

        let t = Instant::now();
        let mut masked = img;
        for tm in &last_texts {
            let m = pad_rect(tm.rect, MASK_PAD, masked.width(), masked.height());
            fill_rect(&mut masked, m, tm.bg);
        }
        // Erst prüfen, ob der Kanal frei ist — sonst gar nicht diffen/kodieren.
        let lane_free = sender.tile_lane_free(ENCODE_LEAD);
        let dirty = if lane_free {
            dirty_bbox(shadow.as_ref(), &masked)
        } else {
            None
        };
        let mask_diff_ms = ms(t);
        let mut tile_stat = None;
        let mut enc_ms = 0.0;
        let mut parts_n = 0;
        let mut bytes = 0;
        if let Some(r) = dirty {
            let t = Instant::now();
            let parts =
                match encode_capped(shadow.as_ref(), &masked, r, cfg.quantizer, cfg.max_tile) {
                    Ok(p) => p,
                    Err(e) => break Err(e),
                };
            enc_ms = ms(t);
            let t = Instant::now();
            let mut h = Fnv1a64::new();
            let mut data_bytes = 0;
            let ubox = union(parts.iter().map(|p| p.0));
            parts_n = parts.len();
            for (pr, data) in parts {
                h.update(&data);
                data_bytes += data.len();
                bytes += sender.tile(ServerMsg::Tile {
                    x: pr.x,
                    y: pr.y,
                    data,
                });
            }
            send_ms += ms(t);
            if let Some(u) = ubox {
                tile_stat = Some(TileStat {
                    x: u.x,
                    y: u.y,
                    w: u.w,
                    h: u.h,
                    bytes: data_bytes,
                    hash: h.finish(),
                });
            }
            shadow = Some(masked);
        }
        rec.frame(
            frames,
            last_texts.len(),
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
                pace: 0.0,
            },
        );
        if cfg.verbose {
            let (det_ms, rec_ms) = ocr.last_ms();
            let (chits, clook) = ocr.cache_stats();
            eprintln!(
                "[frame {frames}] {} Texte (det {det_ms:.1} ms, rec {rec_ms:.1} ms, cache {chits}/{clook}), {parts_n} Kacheln, {bytes} B{}",
                last_texts.len(),
                if lane_free { "" } else { " (Kanal belegt)" }
            );
        }
        // Feste Taktung: nur die Restzeit schlafen.
        std::thread::sleep(FRAME_GAP.saturating_sub(frame_start.elapsed()));
    };

    drop(sender);
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

/// Startet den Eingabe-Reader (Injektion oder Drain — s. `serve_client`).
fn spawn_reader(
    rd: TcpStream,
    fr: FrameReader,
    stop: Arc<AtomicBool>,
    verbose: bool,
    rec: Recorder,
    on_msg: impl FnMut(ClientMsg) + Send + 'static,
) -> std::thread::JoinHandle<()> {
    if verbose {
        eprintln!("[input] bereit");
    }
    std::thread::spawn(move || {
        input_loop(rd, fr, &stop, verbose, &rec, on_msg);
    })
}
