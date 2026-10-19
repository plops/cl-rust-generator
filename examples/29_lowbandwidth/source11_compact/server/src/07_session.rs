//! `07_session` — Client-Bedienung: Auth-Handshake, Input-Thread und
//! Capture → OCR → Maske → Bounding-Box mit direktem TCP.
//!
//! Kein Scheduler, kein Client-State: Reconnect beginnt bei Vollbild.
//! Der Server spricht nur localhost (SSH-Tunnel davor); zusätzlich beweist
//! der Client per Token-HMAC, dass er das LBW-Token kennt (Challenge/Auth).

use std::net::TcpStream;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{channel, sync_channel};
use std::time::{Duration, Instant};

use image::RgbImage;

use lbw_common::framing::{FrameReader, Read1, decode_msg, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, Rect, ServerMsg, TextItem, verify_auth};

use crate::av1::encode_rgb;
use crate::capture::FrameSource;
use crate::config::Config;
use crate::input::Inject;
use crate::ocr::{Ocr, REC_BUDGET, sample_colors};
use crate::pace::Pacer;
use crate::stabilize::Stabilizer;
use crate::tiles::{MASK_PAD, crop_rgb, dirty_bbox, fill_rect, pad_rect, scale_rgb};
use crate::trace::Tracer;

/// Anteil geänderter Roh-Pixel, ab dem das Fenster als „bewegt" gilt: Die
/// Maske (aus alten Text-Rechtecken) würde dann Geisterlöcher über den
/// neuen Inhalt malen — lieber eine rohe, aktuelle Kachel, scharfer Text
/// folgt über OCR nach.
const BIG_CHANGE_FRAC: f32 = 0.30;

/// Was die Session zum Erkennen braucht (Tests nutzen Attrappen).
pub trait Recognize {
    fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String>;
    /// Wie [`Recognize::text`], deckelt teure Neuerkennungen auf `max_new`
    /// (Priorität nahe `hint`) und meldet alle Detektions-Boxen zurück.
    /// Default: alles auf einmal, keine Boxen (Still-da aus).
    fn text_full(
        &mut self,
        img: &RgbImage,
        _max_new: usize,
        _hint: &[Rect],
    ) -> Result<(Vec<TextItem>, Vec<Rect>), String> {
        Ok((self.text(img)?, Vec::new()))
    }
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

    fn text_full(
        &mut self,
        img: &RgbImage,
        max_new: usize,
        hint: &[Rect],
    ) -> Result<(Vec<TextItem>, Vec<Rect>), String> {
        self.text_full(img, max_new, hint)
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
/// Der Lock ist nur eine kurze Abrechnung (kein Schlaf): Text-Latenz hängt
/// nie von der Kachelgröße ab — Kodieren + Dosieren läuft im Tile-Thread.
fn send_text(
    wr: &Mutex<TcpStream>,
    pacer: &Mutex<Pacer>,
    m: &ServerMsg,
    text_bytes: &mut usize,
) -> bool {
    match write_msg(
        &mut *wr.lock().unwrap_or_else(std::sync::PoisonError::into_inner),
        m,
    ) {
        Ok(n) => {
            *text_bytes += n;
            pacer
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .note(n);
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
    let pacer = Arc::new(Mutex::new(Pacer::new(cfg.budget)));
    let n = write_msg(&mut wr, &ServerMsg::Hello).map_err(|e| e.to_string())?;
    pacer
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .note(n);

    // Eingaben laufen in eigenem Thread, damit Tippen nie auf AV1 wartet.
    // Der Reader läuft IMMER (ohne Injektor als Drain): Sein Ende ist das
    // einzige Abriss-Signal — bei statischem Bild schreibt die Hauptschleife
    // nie, sähe den toten Client also nie.
    // Die Injektion läuft dahinter entkoppelt (eigener Thread, begrenzter
    // Kanal): Volllaufen lässt stale Inputs fallen statt zu wachsen.
    rd.set_read_timeout(Some(Duration::from_millis(200)))
        .map_err(|e| e.to_string())?;
    let stop = Arc::new(AtomicBool::new(false));
    // Steuerung (F2/F3) läuft am Injektor vorbei direkt in die
    // Hauptschleife — sonst würde ein Textwunsch getippt und ein
    // Kachelwunsch verworfen.
    let resync = Arc::new(AtomicBool::new(false));
    let quality = Arc::new(AtomicBool::new(false));
    // Kachel-Thread (Latest-wins): Kodieren (rav1e) + Budget-Dosierung
    // blockieren nie die Text-Schleife — sonst stünde jeder Move erst nach
    // der großen Kachel an. `slot` trägt die neueste Kachel, `kick` weckt.
    // Ein Schreiber für beide Threads: Texte (Hauptschleife) und Kacheln
    // (Tile-Thread) teilen sich den Socket — ohne Lock würden sich große
    // Tile-`send()`s mit Text-`send()`s auf Byte-Ebene verzahnen und der
    // Client sähe Protokoll-Müll (Session-Abriss, erlebt im Emulator-E2E).
    let writer = Arc::new(Mutex::new(wr));
    let tile_slot = Arc::new(Mutex::new(None::<TileJob>));
    let (kick_tx, kick_rx) = channel::<()>();
    let tile_thread = spawn_tile_writer(
        writer.clone(),
        kick_rx,
        tile_slot.clone(),
        pacer.clone(),
        stop.clone(),
    );
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
            let rs = resync.clone();
            let qs = quality.clone();
            spawn_reader(rd, fr, stop.clone(), cfg.verbose, move |m| match m {
                ClientMsg::ResyncText => rs.store(true, Ordering::Relaxed),
                ClientMsg::QualityTile => qs.store(true, Ordering::Relaxed),
                m => {
                    let _ = tx.try_send(m);
                }
            })
        }
        None => {
            let rs = resync.clone();
            let qs = quality.clone();
            spawn_reader(rd, fr, stop.clone(), cfg.verbose, move |m| match m {
                ClientMsg::ResyncText => rs.store(true, Ordering::Relaxed),
                ClientMsg::QualityTile => qs.store(true, Ordering::Relaxed),
                _ => {}
            })
        }
    };

    let mut trace = match &cfg.trace {
        Some(p) => Tracer::open(p)?,
        None => Tracer::disabled(),
    };
    let mut prev: Option<RgbImage> = None;
    let mut prev_raw: Option<RgbImage> = None;
    let mut stabilizer = Stabilizer::new();
    let mut sent_texts: Vec<TextItem> = Vec::new();
    // F3-Keyframe angefordert: nächster Stabilisator-Schritt läuft wie der
    // erste Frame (ClearText + Vollbestand), statt gegen Altlasten zu
    // differenzieren.
    let mut key_first = false;
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
        // Hint: zuletzt gesendete Rechtecke — beim Verschieben wird
        // sichtbarer Text im Budget zuerst erkannt.
        let hint: Vec<Rect> = sent_texts.iter().map(|t| t.rect).collect();
        let (texts, seen) = match ocr.text_full(&img, REC_BUDGET, &hint) {
            Ok(t) => t,
            Err(e) => break Err(e),
        };
        // Text als Delta (v4): 1. Frame ClearText + NewText je Zeile,
        // danach RemoveText + NewText (Inhalt neu) + MoveText (nur Box neu).
        let mut text_bytes = 0;
        trace.frame(frames);
        trace.ocr(frames, &seen, &texts);
        let first = frames == 1 || key_first;
        key_first = false;
        let delta = stabilizer.step(texts, &seen, first, Instant::now());
        trace.delta(frames, &delta);
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
                ok = send_text(&writer, &pacer, m, &mut text_bytes);
                if !ok {
                    break;
                }
            }
            if !ok {
                break Ok(());
            }
        }
        // Bewegt? Roh-Vergleich (Maske aus alten Rechtecken würde sonst
        // Geisterlöcher über den neuen Inhalt malen) → rohe Kachel, Text
        // folgt über OCR nach. Sonst maskierter Pfad wie bisher.
        let frame_px = img.width() as f32 * img.height() as f32;
        let raw_dirty = dirty_bbox(prev_raw.as_ref(), &img);
        // Erster Frame (kein Vorbild): Vollbild-Kachel, Maske wie sonst auch.
        let big_move = prev_raw.is_some()
            && raw_dirty
                .is_some_and(|r| f32::from(r.w) * f32::from(r.h) / frame_px >= BIG_CHANGE_FRAC);
        let t = Instant::now();
        let mut masked = img.clone();
        if !big_move {
            // Maske aus AKTUELLEN Detektionen (`seen`), nicht aus gesendeten
            // Texten: Gehaltene Alt-Boxen (Still-da, Budget) würden sonst
            // alten Inhalt über das neue Bild malen (Geister in der Kachel,
            // live erlebt). Hintergrund frisch am Bild gesampelt statt aus
            // womöglich altem Stand — auch unerkannte Boxen (Budget, Gatter)
            // werden so ausmaskiert.
            for r in &seen {
                let m = pad_rect(*r, MASK_PAD, masked.width(), masked.height());
                // sample_colors liefert (Schrift, Hintergrund) zurück.
                let (_, bg) = sample_colors(&img, m);
                fill_rect(&mut masked, m, bg);
            }
        }
        let mut dirty = dirty_bbox(prev.as_ref(), &masked);
        if big_move {
            // Szenenwechsel (z. B. Tab): kein Ausschnitt-Flicken, sondern
            // Vollbild-Reset in laufender Skalierung/Qualität — sonst
            // bleiben alte Kachel-Reste stehen, die kein Dirty je berührt.
            // Rohbild (s. oben), kein Alt-Anstrich.
            dirty = Some(Rect::new(0, 0, img.width() as u16, img.height() as u16));
        }
        let _mask_diff_ms = ms(t);
        // F2: Voll-Resync aus dem Server-Stand (ClearText + je ein NewText)
        // statt OCR-Neulauf — repariert auseinander gelaufene Client-Szenen.
        if resync.swap(false, Ordering::Relaxed) {
            trace.note("resync");
            let mut ok = send_text(&writer, &pacer, &ServerMsg::ClearText, &mut text_bytes);
            if ok {
                for t in &sent_texts {
                    ok = send_text(
                        &writer,
                        &pacer,
                        &ServerMsg::NewText(t.clone()),
                        &mut text_bytes,
                    );
                    if !ok {
                        break;
                    }
                }
            }
            if !ok {
                break Ok(());
            }
        }
        // Genau eine Kachel-Anfrage pro Frame an den Tile-Thread (eigener
        // Socket, eigenes Pacing): Kodieren + Dosieren halten die
        // Text-Schleife nie auf. Toter Thread → Kacheln entfallen still.
        let mut tiles = 0;
        if quality.swap(false, Ordering::Relaxed) && !tile_thread.is_finished() {
            trace.note("keyframe");
            // F3-Keyframe: kompletter Neuaufbau ohne Rücksicht auf vorherige
            // Frames — räumt alle Artefakte weg (stale Texte, alte
            // Kachel-Reste, gehaltene Stabilisator-Zeilen). Genau EINE
            // hochwertige Vollbild-Kachel (volle Auflösung,
            // `--hq-quantizer`), danach wieder Normalbetrieb — ein guter
            // Blick zum Orientieren, kein Dauerfeuer. Der Text-Vollstand
            // folgt im nächsten Frame über `key_first` (ClearText +
            // NewText je Zeile, wie bei Verbindungsstart); `prev` läuft
            // ohnehin auf das Keyframe-Bild weiter, danach also wieder
            // echte Deltas statt Vollbild-Flut.
            stabilizer = Stabilizer::new();
            sent_texts.clear();
            key_first = true;
            let rgb = masked.clone().into_raw();
            let (fw, fh) = (masked.width(), masked.height());
            *tile_slot
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(TileJob {
                x: 0,
                y: 0,
                fw: fw as u16,
                fh: fh as u16,
                rgb,
                ew: fw as usize,
                eh: fh as usize,
                quantizer: cfg.hq_quantizer,
            });
            let _ = kick_tx.send(());
            tiles = 1;
        } else if let Some(r) = dirty
            && !tile_thread.is_finished()
        {
            let rgb = crop_rgb(&masked, r);
            let (rgb, ew, eh) = scale_rgb(rgb, u32::from(r.w), u32::from(r.h), cfg.tile_scale);
            *tile_slot
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(TileJob {
                x: r.x,
                y: r.y,
                fw: r.w,
                fh: r.h,
                rgb,
                ew: ew as usize,
                eh: eh as usize,
                quantizer: cfg.quantizer,
            });
            let _ = kick_tx.send(());
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
        prev_raw = Some(img);
        // Feste Taktung: nur die Restzeit schlafen, sonst kostet jede
        // Pipeline-Phase (OCR+AV1) zusätzlich volle 100 ms (~6 statt 10 fps).
        std::thread::sleep(FRAME_GAP.saturating_sub(frame_start.elapsed()));
    };

    stop.store(true, Ordering::Relaxed);
    drop(kick_tx);
    let _ = input.join();
    let _ = tile_thread.join();
    result
}

/// Eine zu kodierende Kachel (Roh-Crop aus der Hauptschleife): Fußabdruck
/// (`x`, `y`, `fw`, `fh`) im Capture-Raum, Pixel (`rgb`) in Kodiergröße
/// (`ew`×`eh` — bei `--tile-scale` kleiner, Client skaliert hoch).
struct TileJob {
    x: u16,
    y: u16,
    fw: u16,
    fh: u16,
    rgb: Vec<u8>,
    ew: usize,
    eh: usize,
    quantizer: usize,
}

/// Kodiert + dosiert + sendet Kacheln auf eigenem Socket, Latest-wins:
/// Überholte Kacheln verfallen (Slot + Kick-Drain), die Text-Schleife läuft
/// unbehindert weiter. Der Pacer-Lock deckt nur die Rechenfunktion ab —
/// geschlafen wird ohne Lock, Texte buchen jederzeit. Ende bei `stop`,
/// vollem Kanal-Abriss oder geschlossenem `kick` (Session-Ende).
fn spawn_tile_writer(
    wr: Arc<Mutex<TcpStream>>,
    kick_rx: std::sync::mpsc::Receiver<()>,
    slot: Arc<Mutex<Option<TileJob>>>,
    pacer: Arc<Mutex<Pacer>>,
    stop: Arc<AtomicBool>,
) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || {
        for _ in &kick_rx {
            while kick_rx.try_recv().is_ok() {}
            let job = slot
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .take();
            let Some(job) = job else { continue };
            let data = match encode_rgb(&job.rgb, job.ew, job.eh, job.quantizer) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("[tile] {e}");
                    return;
                }
            };
            let wait = pacer
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .pace(tile_wire_len(data.len()));
            let mut left = wait;
            while !left.is_zero() {
                if stop.load(Ordering::Relaxed) {
                    return;
                }
                let s = left.min(Duration::from_millis(50));
                std::thread::sleep(s);
                left -= s;
            }
            if stop.load(Ordering::Relaxed) {
                return;
            }
            let msg = ServerMsg::Tile {
                x: job.x,
                y: job.y,
                w: job.fw,
                h: job.fh,
                data,
            };
            if write_msg(
                &mut *wr.lock().unwrap_or_else(std::sync::PoisonError::into_inner),
                &msg,
            )
            .is_err()
            {
                return;
            }
        }
    })
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
