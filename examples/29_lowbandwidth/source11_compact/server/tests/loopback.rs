//! Loopback (T2): echte Session (`SharedSource` + Stub-OCR) über echtes TCP.
//! Läuft ohne X11 und ohne Modelle. Handshake v4: Challenge → Auth → Hello.

mod common;
use common::*;

use std::net::{TcpListener, TcpStream};
use std::time::{Duration, Instant};

use clap::Parser;
use lbw_common::framing::{FrameReader, write_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, auth_mac};
use lbw_server::capture::{SharedSource, solid};
use lbw_server::input::Injector;
use lbw_server::session::serve_client;

const TOKEN: &[u8] = b"test-token";

/// Führt den v4-Handshake durch; liefert den Server-Reader.
fn handshake(s: &mut TcpStream, fr: &mut FrameReader, token: &[u8]) -> ServerMsg {
    // 1. Challenge vom Server lesen.
    let ch: ServerMsg = fr.read_msg(s).unwrap().expect("Challenge");
    let nonce = match ch {
        ServerMsg::Challenge { version, nonce } => {
            assert_eq!(version, PROTO_VERSION);
            nonce
        }
        m => panic!("Challenge erwartet, {m:?}"),
    };
    // 2. Auth + Hello senden.
    write_msg(
        &mut *s,
        &ClientMsg::Auth {
            mac: auth_mac(token, &nonce),
        },
    )
    .unwrap();
    write_msg(
        &mut *s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .unwrap();
    // 3. Server-Hello lesen.
    fr.read_msg(s).unwrap().expect("Server-Hello")
}

fn serve_once(
    shared: SharedSource,
    texts: Vec<lbw_common::TextItem>,
    frames: u64,
) -> std::net::SocketAddr {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(texts);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(frames),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });
    addr
}

#[test]
fn full_frame_then_single_dirty_tile() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let addr = serve_once(shared.clone(), vec![item()], 30);

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Vollbild: ClearText, NewText, 1 Box (128×128).
    let (mut clear, mut texts, mut tiles) = (0, 0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::ClearText => clear += 1,
                ServerMsg::NewText(t) => {
                    assert_eq!(t.text, "hi");
                    texts += 1;
                }
                ServerMsg::Tile { .. } => tiles += 1,
                _ => {}
            }
            clear >= 1 && texts >= 1 && tiles >= 1
        },
    );
    assert_eq!((clear, texts, tiles), (1, 1, 1));

    // Ein Pixel ändern → genau eine Box um den Pixel kommt neu.
    let mut img = solid(128, 128, [40; 3]);
    img.put_pixel(100, 10, image::Rgb([9; 3]));
    shared.set(img);
    let mut found = None;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::Tile { x, y, w, h, data } = m {
                assert!(!data.is_empty());
                // Fußabdruck in Originalgröße: 1-px-Dirty → 16×16
                // (Mindestgröße, hier unskaliert 1:1).
                assert_eq!((*w, *h), (16, 16));
                found = Some((*x, *y));
                return true;
            }
            false
        },
    );
    assert_eq!(found, Some((100, 10)));
}

#[test]
fn scroll_sends_move_not_new() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let addr = serve_once(shared, vec![item()], 30);

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erst NewText abwarten, dann Zeile um 40 px schieben (Scroll).
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::NewText(_)),
    );
    // HINWEIS: Stub-OCR liefert feste Box — Scroll simuliert T3b mit
    // wechselndem Stub; hier nur: kein doppelter NewText bei Stillstand.
    let mut news = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(2),
        &mut |m| {
            if matches!(m, ServerMsg::NewText(_)) {
                news += 1;
            }
            false
        },
    );
    assert_eq!(news, 0, "Stillstand darf nichts erneut senden");
}

#[test]
fn scroll_soak_sends_only_movetext() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let ocr = StubOcr::fixed(vec![
        item_at(10, 20, "eins"),
        item_at(10, 40, "zwei"),
        item_at(10, 60, "drei"),
    ]);
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let ocr2 = ocr.clone();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = ocr2;
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(60),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erst alle drei NewTexts abwarten.
    let mut news = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::NewText(_)) {
                news += 1;
            }
            news >= 3
        },
    );
    assert_eq!(news, 3);

    // Scroll: alle Zeilen −20 px (gleicher Inhalt, neue Box).
    ocr.set(vec![
        item_at(10, 0, "eins"),
        item_at(10, 20, "zwei"),
        item_at(10, 40, "drei"),
    ]);
    let (mut moves, mut renews, mut removes) = (0, 0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::MoveText { .. } => moves += 1,
                ServerMsg::NewText(_) => renews += 1,
                ServerMsg::RemoveText(_) => removes += 1,
                _ => {}
            }
            moves >= 3
        },
    );
    assert_eq!((moves, renews, removes), (3, 0, 0), "Scroll = nur MoveText");
}

#[test]
fn texts_jump_the_tile_queue() {
    use clap::Parser;
    let cfg =
        lbw_server::config::Config::try_parse_from(["lbw-server", "--budget", "200"]).unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(200),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Text (~50 B, nur gebucht) trifft ein, obwohl das Tile (≥ Budget-Fenster)
    // noch gepacet wird: Deadline 2 s.
    let mut text_at = None;
    let start = Instant::now();
    read_until(&mut fr, &mut s, start + Duration::from_secs(2), &mut |m| {
        if matches!(m, ServerMsg::NewText(_)) {
            text_at = Some(Instant::now());
            return true;
        }
        false
    });
    assert!(text_at.is_some(), "Text springt vor dem Tile raus");
    assert!(start.elapsed() < Duration::from_secs(2));

    // Das Tile kommt trotzdem vollständig und in Reihenfolge an (kein Verwerfen).
    let mut tile_ok = false;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(30),
        &mut |m| {
            if let ServerMsg::Tile { data, .. } = m {
                tile_ok = !data.is_empty();
                return tile_ok;
            }
            false
        },
    );
    assert!(tile_ok, "gepactes Tile kommt ganz an");
}

#[test]
fn resync_request_repeats_clear_and_texts() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let addr = serve_once(shared, vec![item()], 60);

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erstbestand abwarten (ClearText + NewText sind damit verbraucht).
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::NewText(_)),
    );
    // F2: Resync anfordern — ClearText + NewText müssen erneut kommen,
    // ohne dass sich am Bild etwas ändert (kein OCR-Neulauf nötig).
    write_msg(&mut s, &ClientMsg::ResyncText).unwrap();
    let (mut clear, mut texts) = (0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::ClearText => clear += 1,
                ServerMsg::NewText(t) => {
                    assert_eq!(t.text, "hi");
                    texts += 1;
                }
                _ => {}
            }
            clear >= 1 && texts >= 1
        },
    );
    assert_eq!((clear, texts), (1, 1));
}

#[test]
fn quality_request_sends_exactly_one_full_tile() {
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let addr = serve_once(shared, vec![item()], 200);

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erste Vollbild-Kachel (Normalbetrieb) abwarten.
    let mut full = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::Tile { w, h, .. } = m
                && (*w, *h) == (128, 128)
            {
                full += 1;
                return true;
            }
            false
        },
    );
    assert_eq!(full, 1);
    // F3: genau EINE hochwertige Vollbild-Kachel anfordern …
    write_msg(&mut s, &ClientMsg::QualityTile).unwrap();
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(30),
        &mut |m| {
            if let ServerMsg::Tile { w, h, data, .. } = m {
                assert!(!data.is_empty());
                if (*w, *h) == (128, 128) {
                    full += 1;
                    return true;
                }
            }
            false
        },
    );
    assert_eq!(full, 2);
    // … und danach kein Dauerfeuer: 2 s Stille (Standbild → keine Tiles).
    let mut extra = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(2),
        &mut |m| {
            if matches!(m, ServerMsg::Tile { .. }) {
                extra += 1;
            }
            false
        },
    );
    assert_eq!(extra, 0, "HQ muss ein Einzelschuss bleiben");
}

#[test]
fn quality_keyframe_rebuilds_texts_without_stale_state() {
    // Tab-Wechsel-Szenario: alter Text ("alt-tab") steht, neuer ("neu-tab")
    // kommt — F3 muss danach ClearText + Vollbestand (nur neu) + genau eine
    // Vollbild-Kachel liefern, ohne Rücksicht auf vorherige Frames.
    // Kleines Bild + großes Budget: Der HQ-Encode (Debug-rav1e) + Pacing
    // dürfen nicht länger brauchen als die Test-Session (sonst verwirft
    // das Session-Ende die Kachel — korrekt bei Disconnect, hier Rauschen).
    use clap::Parser;
    let cfg =
        lbw_server::config::Config::try_parse_from(["lbw-server", "--budget", "200000"]).unwrap();
    let shared = SharedSource::new(solid(64, 64, [40; 3]));
    let ocr = StubOcr::fixed(vec![item_at(8, 8, "alt-tab")]);
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let ocr2 = ocr.clone();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = ocr2;
        serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(300),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erstbestand: Vollbild-Kachel + NewText("alt-tab").
    let (mut full, mut old) = (0, 0);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            match m {
                ServerMsg::Tile { w: 64, h: 64, .. } => full += 1,
                ServerMsg::NewText(t) if t.text == "alt-tab" => old += 1,
                _ => {}
            }
            full >= 1 && old >= 1
        },
    );
    assert_eq!((full, old), (1, 1));

    // Tab-Wechsel: neuer Text, auf Bestätigung warten (Folgeframe-Regel).
    ocr.set(vec![item_at(8, 8, "neu-tab")]);
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::NewText(t) if t.text == "neu-tab"),
    );

    // F3-Keyframe: ClearText, danach nur noch "neu-tab", plus Vollbild.
    // Reihenfolge offen: Die Kachel kann (schnelles Netz) VOR dem
    // Text-Vollstand des nächsten Frames eintreffen — nur NewTexts an
    // ClearText koppeln, die Kachel zählt für sich.
    write_msg(&mut s, &ClientMsg::QualityTile).unwrap();
    let (mut clear, mut full2, mut stale) = (0, 0, 0);
    let mut fresh = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(30),
        &mut |m| {
            match m {
                ServerMsg::ClearText if clear == 0 => clear += 1,
                ServerMsg::NewText(t) if clear >= 1 => {
                    if t.text == "neu-tab" {
                        fresh += 1;
                    } else {
                        stale += 1;
                    }
                }
                ServerMsg::Tile { w: 64, h: 64, .. } => {
                    full2 += 1;
                }
                _ => {}
            }
            clear >= 1 && fresh >= 1 && full2 >= 1
        },
    );
    assert_eq!((clear, fresh, full2), (1, 1, 1));
    assert_eq!(stale, 0, "Keyframe darf keinen Alt-Text neu senden");
}

/// Skript-OCR: liefert pro Frame den nächsten Eintrag (Texte + Boxen),
/// danach den letzten im Dauerlauf. Scroll + Dirty kommen aus EINER Quelle
/// (atomar): Die wandernde Maske macht jeden Frame dirty, der uniforme
/// Shift meldet Scroll — ganz ohne Wall-Clock-Steuerung vom Test-Thread.
struct ScriptOcr(std::sync::Mutex<Vec<(Vec<lbw_common::TextItem>, Vec<lbw_common::Rect>)>>);

impl ScriptOcr {
    fn new(script: Vec<(Vec<lbw_common::TextItem>, Vec<lbw_common::Rect>)>) -> Self {
        Self(std::sync::Mutex::new(script))
    }
}

/// Skript-Quelle: ein Bild pro Grab, danach das letzte im Dauerlauf.
/// Zusammen mit `ScriptOcr` (eigener Zähler, gleiche Länge) laufen Bild und
/// Befund frame-synchron — der Test-Thread greift nie dazwischen.
#[derive(Clone)]
struct ScriptSource(std::sync::Arc<std::sync::Mutex<Vec<image::RgbImage>>>);

impl ScriptSource {
    fn new(images: Vec<image::RgbImage>) -> Self {
        Self(std::sync::Arc::new(std::sync::Mutex::new(images)))
    }

    /// Ab jetzt dauerhaft `img` liefern (Szenenwechsel nach Stillstand).
    fn set(&self, img: image::RgbImage) {
        let mut v = self.0.lock().unwrap();
        v.clear();
        v.push(img);
    }
}

impl lbw_server::capture::FrameSource for ScriptSource {
    fn grab(&mut self) -> Result<image::RgbImage, String> {
        let mut v = self.0.lock().unwrap();
        if v.len() > 1 {
            Ok(v.remove(0))
        } else {
            Ok(v[0].clone())
        }
    }
}

impl lbw_server::session::Recognize for ScriptOcr {
    fn text(&mut self, img: &image::RgbImage) -> Result<Vec<lbw_common::TextItem>, String> {
        self.text_full(img, usize::MAX, &[]).map(|(t, _)| t)
    }

    fn text_full(
        &mut self,
        _img: &image::RgbImage,
        _max_new: usize,
        _hint: &[lbw_common::Rect],
    ) -> Result<(Vec<lbw_common::TextItem>, Vec<lbw_common::Rect>), String> {
        let mut s = self.0.lock().unwrap();
        if s.len() > 1 {
            Ok(s.remove(0))
        } else {
            Ok(s[0].clone())
        }
    }
}

#[test]
fn scroll_holds_tiles_until_settle() {
    // Gleichmäßiger Scroll (+20 px/Frame, 3 Zeilen) bei wanderndem
    // Masken-Dirty: kein einziger Tile während des Scrolls (Settle!),
    // danach läuft der Bildwechsel (Vollbild nach Szenenwechsel) normal.
    use clap::Parser;
    let cfg =
        lbw_server::config::Config::try_parse_from(["lbw-server", "--budget", "200000"]).unwrap();
    let row = |y: u16, text: &str| lbw_common::TextItem {
        id: 0,
        rect: lbw_common::Rect::new(8, y, 100, 16),
        fg: [10; 3],
        bg: [250; 3],
        text: text.into(),
    };
    let frame_texts = |y0: u16| {
        let ts = vec![row(y0, "eins"), row(y0 + 32, "zwei"), row(y0 + 64, "drei")];
        let seen = ts.iter().map(|t| t.rect).collect();
        (ts, seen)
    };
    let mut script = vec![frame_texts(8); 3];
    for i in 1..=8 {
        script.push(frame_texts(8 + i * 20));
    }
    script.extend(vec![frame_texts(168); 6]);
    // Bildskript im Gleichschritt: statisch, dann jeden Scroll-Frame
    // alternierend (Roh-Dirty jeden Frame), dann statisch. Auf flachem
    // Bild trüge die wandernde Maske allein kein Dirty — der Test wäre
    // hohl (besteht mit wie ohne Settle).
    let shade = |v: u8| solid(128, 128, [v; 3]);
    let mut images = vec![shade(40); 3];
    for i in 0..8 {
        images.push(shade(if i % 2 == 0 { 41 } else { 40 }));
    }
    images.push(shade(41));
    let feed = ScriptSource::new(images);
    let src_feed = feed.clone();
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = src_feed;
        let mut ocr = ScriptOcr::new(script);
        serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(200),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erstbild: genau ein Vollbild-Tile.
    let mut full = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if matches!(m, ServerMsg::Tile { w: 128, h: 128, .. }) {
                full += 1;
                return true;
            }
            false
        },
    );
    assert_eq!(full, 1);

    // Scroll-Fenster: Der wandernde Masken-Dirty würde ohne Settle jeden
    // Frame ein Tile auslösen (großes Budget → sofort sichtbar). Stattdessen:
    // Stille auf dem Kachelkanal, Moves holen nach Stillstand auf.
    let mut moves = 0;
    let mut tiles = 0;
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_millis(2500),
        &mut |m| {
            match m {
                ServerMsg::MoveText { .. } => moves += 1,
                ServerMsg::Tile { .. } => tiles += 1,
                _ => {}
            }
            false
        },
    );
    assert!(moves >= 3, "Scroll-Positionen kommen an ({moves})");
    assert_eq!(tiles, 0, "Scroll hält Kacheln zurück");

    // Bildwechsel nach Stillstand: Vollbild kommt ganz normal.
    feed.set(solid(128, 128, [200; 3]));
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::Tile { w: 128, h: 128, .. }),
    );
}

#[test]
fn big_move_resets_full_frame() {
    let shared = SharedSource::new(solid(128, 128, [0; 3]));
    let feed = shared.clone();
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(300),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erste Vollbild-Kachel (leeres Vorbild).
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::Tile { w: 128, h: 128, .. }),
    );
    // Szenenwechsel (z. B. Tab): komplett anderes Bild — der Reset muss
    // wieder Vollbild sein, kein Ausschnitt-Flicken (sonst bleiben alte
    // Kachel-Reste stehen, die kein Dirty je berührt).
    feed.set(solid(128, 128, [255; 3]));
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| matches!(m, ServerMsg::Tile { w: 128, h: 128, .. }),
    );
}

#[test]
fn tile_scale_keeps_full_footprint() {
    let cfg =
        lbw_server::config::Config::try_parse_from(["lbw-server", "--tile-scale", "2"]).unwrap();
    let shared = SharedSource::new(solid(128, 128, [40; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![item()]);
        serve_client(
            stream,
            &cfg,
            &mut src,
            &mut ocr,
            Some(60),
            TOKEN,
            None::<Injector>,
        )
        .unwrap();
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    assert!(matches!(
        handshake(&mut s, &mut fr, TOKEN),
        ServerMsg::Hello
    ));

    // Erste Kachel: Vollbild 128×128 trotz halb kodierter Pixel — der
    // Fußabdruck bleibt voll, Hochskalieren ist Clientsache.
    read_until(
        &mut fr,
        &mut s,
        Instant::now() + Duration::from_secs(10),
        &mut |m| {
            if let ServerMsg::Tile { w, h, data, .. } = m {
                assert!(!data.is_empty());
                assert_eq!((*w, *h), (128, 128));
                return true;
            }
            false
        },
    );
}

#[test]
fn wrong_token_is_rejected() {
    let shared = SharedSource::new(solid(64, 64, [0; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(5),
            TOKEN,
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    let ch: ServerMsg = fr.read_msg(&mut s).unwrap().expect("Challenge");
    let ServerMsg::Challenge { nonce, .. } = ch else {
        panic!("Challenge erwartet");
    };
    write_msg(
        &mut s,
        &ClientMsg::Auth {
            mac: auth_mac(b"falsch", &nonce),
        },
    )
    .unwrap();
    // Server bricht ab → EOF statt Hello.
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    let end: Option<ServerMsg> = fr.read_msg(&mut s).unwrap_or(None);
    assert!(end.is_none(), "falsches Token: kein Hello, {end:?}");
    assert!(server.join().is_ok());
}

#[test]
fn wrong_version_is_rejected() {
    let shared = SharedSource::new(solid(64, 64, [0; 3]));
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut src = shared;
        let mut ocr = StubOcr::fixed(vec![]);
        serve_client(
            stream,
            &test_cfg(),
            &mut src,
            &mut ocr,
            Some(5),
            TOKEN,
            None::<Injector>,
        )
    });

    let mut s = TcpStream::connect(addr).unwrap();
    let mut fr = FrameReader::new();
    let ch: ServerMsg = fr.read_msg(&mut s).unwrap().expect("Challenge");
    let ServerMsg::Challenge { nonce, .. } = ch else {
        panic!("Challenge erwartet");
    };
    write_msg(
        &mut s,
        &ClientMsg::Auth {
            mac: auth_mac(TOKEN, &nonce),
        },
    )
    .unwrap();
    write_msg(
        &mut s,
        &ClientMsg::Hello {
            version: PROTO_VERSION + 99,
        },
    )
    .unwrap();
    s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
    let end: Option<ServerMsg> = fr.read_msg(&mut s).unwrap_or(None);
    assert!(end.is_none(), "falsche Version: kein Hello, {end:?}");
    assert!(server.join().is_ok());
}
