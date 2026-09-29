//! End-to-end ohne X11/Modelle: synthetische Bildquelle + Attrappen-Analyse
//! → Pipeline → Session → Drossel-Proxy (6 kB/s, 30 ms) → echter
//! Client-Netz-Thread (Reassembly, rav1d, Resume).

use std::sync::mpsc::{Receiver, channel};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use lbw_client::net::{Event, NetCfg, spawn};
use lbw_client::scene::Scene;
use lbw_common::{Input, Rect};
use lbw_server::capture::SharedSource;
use lbw_server::image::Rgb;
use lbw_server::pipeline::{Analyze, PipeCfg, Pipeline};
use lbw_server::scheduler::Outbox;
use lbw_server::session::{Shared, serve};
use lbw_server::text_diff::Detected;
use lbw_throttle::pipe::{Cfg, Proxy, start};

/// Attrappe: liefert die Texte, die der Test vorgibt.
#[derive(Clone, Default)]
struct FakeAn(Arc<Mutex<Vec<Detected>>>);

impl Analyze for FakeAn {
    fn text(&mut self, _: &Rgb) -> Result<Vec<Detected>, String> {
        Ok(self.0.lock().unwrap().clone())
    }
    fn gui_boxes(&mut self, _: &Rgb) -> Result<Vec<Rect>, String> {
        Ok(Vec::new())
    }
}

fn line(text: &str) -> Detected {
    Detected {
        rect: Rect::new(32, 32, 200, 16),
        fg: [0; 3],
        bg: [255; 3],
        text: text.into(),
    }
}

/// UI-artiges Bild: Verlauf (Hintergrund) plus farbige „Fotos“ mit weichen
/// Mustern; `seed` verschiebt/verfärbt alles (≈ einige kB AV1).
fn busy(seed: u32) -> Rgb {
    let mut img = Rgb::filled(640, 640, [255; 3]);
    for y in 64..640 {
        for x in 0..640 {
            let g = (y as u32 * 255 / 640) as u8;
            img.put(x, y, [g / 2 + 60, g / 3 + 90, 200 - g / 4]);
        }
    }
    for k in 0..6u32 {
        let (x0, y0) = (
            ((k * 97 + seed * 53) % 480) as usize,
            (100 + (k * 71 + seed * 29) % 440) as usize,
        );
        for y in y0..(y0 + 96).min(640) {
            for x in x0..(x0 + 128).min(640) {
                let v = ((x - x0) as f32 / 9.0).sin() * ((y - y0) as f32 / 7.0).cos();
                let c = (128.0 + 100.0 * v) as u8;
                img.put(x, y, [c, (k * 40 + seed * 17) as u8, 255 - c]);
            }
        }
    }
    img
}

struct Rig {
    src: SharedSource,
    an: FakeAn,
    proxy: Proxy,
    inputs: Receiver<Input>,
    sh: Arc<Shared>,
}

fn rig(rate: u32, img: Rgb, text: &str) -> Rig {
    let src = SharedSource::new(img);
    let an = FakeAn::default();
    *an.0.lock().unwrap() = vec![line(text)];
    let (tx, inputs) = channel();
    let sh = Shared::new(Outbox::new(rate), (640, 640), tx, Duration::from_secs(90));
    let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = l.local_addr().unwrap();
    serve(l, sh.clone());
    let sh2 = sh.clone();
    let cfg = PipeCfg {
        poll: Duration::from_millis(20),
        min_ocr: Duration::from_millis(50),
        enc_threads: 2,
        verbose: std::env::var("LBW_VERBOSE").is_ok(),
        ..Default::default()
    };
    let p = Pipeline::new(Box::new(src.clone()), Box::new(an.clone()), sh, cfg);
    std::thread::spawn(move || p.run());
    let proxy = start(Cfg {
        listen: "127.0.0.1:0".into(),
        to: addr.to_string(),
        down_rate: rate,
        up_rate: rate,
        delay: Duration::from_millis(30),
    })
    .unwrap();
    Rig {
        src,
        an,
        proxy,
        inputs,
        sh: sh2,
    }
}

/// Wendet Ereignisse an, bis `pred` gilt; liefert die gesehenen Ereignisse.
fn until(
    rx: &Receiver<Event>,
    s: &mut Scene,
    secs: u64,
    mut pred: impl FnMut(&Scene, &Event) -> bool,
) -> Vec<String> {
    let end = Instant::now() + Duration::from_secs(secs);
    let mut log = Vec::new();
    while Instant::now() < end {
        let Ok(e) = rx.recv_timeout(Duration::from_millis(50)) else {
            continue;
        };
        log.push(format!("{e:?}").chars().take(60).collect());
        let done_before = pred(s, &e);
        s.apply(e);
        if done_before {
            return log;
        }
    }
    panic!("Zeitüberschreitung; Ereignisse: {log:#?}");
}

fn has_text(s: &Scene, e: &Event, want: &str) -> bool {
    matches!(e, Event::Text { add, .. } if add.iter().any(|t| t.text == want))
        || s.texts.values().any(|t| t.text == want)
}

#[test]
fn text_is_fast_images_follow_and_rate_holds() {
    let r = rig(6000, busy(1), "hello");
    let net = spawn(NetCfg {
        addr: r.proxy.addr.to_string(),
        dead_after: Duration::from_secs(90),
        verbose: false,
    });
    let mut s = Scene::new(640, 640);
    let t0 = Instant::now();
    until(&net.events, &mut s, 10, |s, e| has_text(s, e, "hello"));
    let first_text = t0.elapsed();
    until(&net.events, &mut s, 30, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. }) && s.tiles > 0
    });
    let initial = t0.elapsed();
    let bytes = r
        .proxy
        .stats
        .down
        .load(std::sync::atomic::Ordering::Relaxed);
    eprintln!(
        "erster Text {first_text:?}, Bild komplett {initial:?}, {bytes} B, {} Kacheln",
        s.tiles
    );
    assert!(
        first_text < Duration::from_secs(1),
        "Text muss vor dem Bild kommen"
    );
    let (want, got) = (busy(1).get(5, 600), s.pixel(5, 600));
    assert!(
        want.iter().zip(got).all(|(a, b)| a.abs_diff(b) < 24),
        "{want:?} vs {got:?}"
    );
    assert_eq!(s.pixel(40, 40), [255, 255, 255], "Textbox maskiert (weiß)");

    // Nur Text ändert sich → Latenz ≈ OCR-Takt + Leitung, kein Bildverkehr.
    let t1 = Instant::now();
    *r.an.0.lock().unwrap() = vec![line("hello world")];
    r.src.set({
        let mut i = busy(1);
        i.put(33, 33, [0; 3]); // Pixel in der Textbox → Frame „geändert“
        i
    });
    until(&net.events, &mut s, 5, |s, e| has_text(s, e, "hello world"));
    let text_lat = t1.elapsed();
    eprintln!("Text-Latenz {text_lat:?}");
    assert!(text_lat < Duration::from_millis(600), "{text_lat:?}");

    // Bild + Text ändern sich → Text überholt die Kacheln.
    let tiles_before = s.tiles;
    *r.an.0.lock().unwrap() = vec![line("new page")];
    r.src.set(busy(2));
    let log = until(&net.events, &mut s, 10, |s, e| has_text(s, e, "new page"));
    assert_eq!(
        s.tiles, tiles_before,
        "Text muss vor der ersten neuen Kachel da sein: {log:?}"
    );
    let t2 = Instant::now();
    until(&net.events, &mut s, 30, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. }) && s.tiles > tiles_before
    });
    let total = t0.elapsed().as_secs_f64();
    let bytes = r
        .proxy
        .stats
        .down
        .load(std::sync::atomic::Ordering::Relaxed) as f64;
    eprintln!(
        "Seitenwechsel-Bild {:?}, Mittel {:.0} B/s",
        t2.elapsed(),
        bytes / total
    );
    assert!(bytes / total <= 6300.0, "Rate {:.0}", bytes / total);

    // Eingaben erreichen den Server.
    net.send(Input::Char { ch: 'x' as u32 });
    net.send(Input::MouseMove { x: 10, y: 20 });
    let got: Vec<Input> = (0..2)
        .map(|_| r.inputs.recv_timeout(Duration::from_secs(3)).unwrap())
        .collect();
    assert_eq!(
        got,
        vec![
            Input::Char { ch: 'x' as u32 },
            Input::MouseMove { x: 10, y: 20 }
        ]
    );
}

#[test]
fn reconnect_resumes_or_refreshes_and_blackout_survives() {
    let r = rig(6000, Rgb::filled(640, 640, [200, 220, 240]), "stable");
    let net = spawn(NetCfg {
        addr: r.proxy.addr.to_string(),
        dead_after: Duration::from_secs(90),
        verbose: false,
    });
    let mut s = Scene::new(640, 640);
    until(&net.events, &mut s, 20, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. }) && s.tiles > 0 && !s.texts.is_empty()
    });

    // 1) Abriss im Ruhezustand → Resume ohne Refresh.
    r.proxy.cut();
    let log = until(&net.events, &mut s, 15, |_, e| {
        matches!(e, Event::Connected { .. })
    });
    let resumed = log.iter().any(|l| l.contains("resumed: true"));
    assert!(resumed, "{log:?}");
    let tiles = s.tiles;

    // 2) Blackout (3 s) mitten im Bildversand → Verbindung bleibt, Bild kommt.
    r.src.set(busy(5));
    std::thread::sleep(Duration::from_millis(400));
    r.proxy.set_blackout(true);
    std::thread::sleep(Duration::from_secs(3));
    r.proxy.set_blackout(false);
    let log = until(&net.events, &mut s, 30, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. }) && s.tiles > tiles
    });
    assert!(
        !log.iter().any(|l| l.starts_with("Disconnected")),
        "{log:?}"
    );

    // 3) Abriss mitten in einer Kachel → Resume, Kachel kommt erneut komplett.
    let tiles = s.tiles;
    r.src.set(busy(6));
    let end = Instant::now() + Duration::from_secs(20);
    while !r
        .sh
        .outbox
        .with(|q| q.image_backlog() > 0 && q.in_flight() > 0)
    {
        assert!(Instant::now() < end, "Kachel wurde nie teilweise gesendet");
        std::thread::sleep(Duration::from_millis(5));
    }
    r.proxy.cut();
    let log = until(&net.events, &mut s, 30, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. }) && s.tiles > tiles
    });
    assert!(log.iter().any(|l| l.contains("resumed: true")), "{log:?}");

    // 4) Verlorene Daten (Blackout + Abriss): Text steckt im Proxy fest und
    //    geht verloren → Seq passt nicht → Clear + Voll-Refresh.
    r.proxy.set_blackout(true);
    *r.an.0.lock().unwrap() = vec![line("lost text")];
    r.src.set(busy(7));
    std::thread::sleep(Duration::from_millis(800));
    r.proxy.cut();
    r.proxy.set_blackout(false);
    let log = until(&net.events, &mut s, 30, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. })
            && s.texts.values().any(|t| t.text == "lost text")
    });
    assert!(log.iter().any(|l| l.contains("resumed: false")), "{log:?}");
    assert!(log.iter().any(|l| l == "Clear"), "{log:?}");
    assert_eq!(s.texts.len(), 1);

    // 5) Client-Neustart (unbekannte server_id) → Voll-Refresh.
    drop(net);
    std::thread::sleep(Duration::from_secs(3)); // alter Netz-Thread merkt es beim nächsten Stats
    let net2 = spawn(NetCfg {
        addr: r.proxy.addr.to_string(),
        dead_after: Duration::from_secs(90),
        verbose: false,
    });
    let mut s2 = Scene::new(640, 640);
    let log = until(&net2.events, &mut s2, 30, |s, e| {
        matches!(e, Event::Stats { backlog: 0, .. }) && s.tiles > 0 && !s.texts.is_empty()
    });
    assert!(log.iter().any(|l| l.contains("resumed: false")), "{log:?}");
    let (want, got) = (busy(7).get(5, 600), s2.pixel(5, 600));
    assert!(
        want.iter().zip(got).all(|(a, b)| a.abs_diff(b) < 24),
        "{want:?} vs {got:?}"
    );
}
