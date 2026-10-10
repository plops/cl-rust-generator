//! Headless-Smoke-Client: verbindet, wartet auf Text + Kachel, schickt
//! Eingaben und meldet Erfolg. Mit zweitem Argument bleibt er noch N
//! Sekunden verbunden und meldet Summen (Durchsatz-Messung). Mit
//! `--record <pfad>` schreibt er zusätzlich eine `.lbwlog`-Aufzeichnung.
//! Nutzung:
//! `cargo run --release -p lbw-client --example probe -- 127.0.0.1:7878 [N] [--record f.lbwlog]`

use std::time::{Duration, Instant};

use lbw_client::net::{Event, Net};
use lbw_client::scene::Scene;
use lbw_common::ClientMsg;
use lbw_log::Recorder;

fn main() {
    let raw: Vec<String> = std::env::args().skip(1).collect();
    let mut addr = "127.0.0.1:7878".to_string();
    let mut stay: u64 = 0;
    let mut record: Option<String> = None;
    let mut positional = 0;
    let mut i = 0;
    while i < raw.len() {
        if raw[i] == "--record" && i + 1 < raw.len() {
            record = Some(raw[i + 1].clone());
            i += 2;
        } else if positional == 0 {
            addr = raw[i].clone();
            positional += 1;
            i += 1;
        } else if positional == 1 {
            stay = raw[i].parse().unwrap_or(0);
            positional += 1;
            i += 1;
        } else {
            i += 1;
        }
    }
    let rec = match &record {
        Some(p) => {
            match Recorder::create(p, "lbw-client-probe", &std::env::args().collect::<Vec<_>>()) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("probe: {e}");
                    std::process::exit(1);
                }
            }
        }
        None => Recorder::none(),
    };
    let net = Net::connect_recorder(&addr, rec);
    let mut scene = Scene::new();
    let deadline = Instant::now() + Duration::from_secs(60);
    let mut sent_input = false;
    let mut connected = false;
    while Instant::now() < deadline {
        match net.events.recv_timeout(Duration::from_millis(500)) {
            Ok(Event::Connected) => {
                connected = true;
                println!("probe: verbunden");
            }
            Ok(e) => scene.apply(e),
            Err(_) => {}
        }
        let got_text = !scene.texts.is_empty();
        let got_tile = scene.tiles > 0;
        if connected && got_tile && !sent_input {
            net.send(ClientMsg::MouseMove { x: 100, y: 100 });
            net.send(ClientMsg::Button {
                button: 1,
                down: true,
            });
            net.send(ClientMsg::Button {
                button: 1,
                down: false,
            });
            net.send(ClientMsg::Text("hi".into()));
            net.send(ClientMsg::Key {
                key: "Enter".into(),
                down: true,
            });
            net.send(ClientMsg::Key {
                key: "Enter".into(),
                down: false,
            });
            sent_input = true;
            println!("probe: Eingaben geschickt");
        }
        if connected && got_text && got_tile && sent_input {
            // Netz-Thread braucht einen Schleifendurchlauf (≤50 ms), um die
            // eben geschickten Eingaben zu flushen — sonst sterben sie mit
            // dem Prozess, bevor der Server sie sieht.
            std::thread::sleep(Duration::from_secs(1));
            println!(
                "probe: OK ({} Texte, {} Kacheln, {} B)",
                scene.texts.len(),
                scene.tiles,
                scene.tile_bytes
            );
            for t in scene.texts.iter().take(5) {
                println!("probe: Text {:?} {:?}", t.rect, t.text);
            }
            if stay == 0 {
                return;
            }
            let end = Instant::now() + Duration::from_secs(stay);
            while Instant::now() < end {
                if let Ok(e) = net.events.recv_timeout(Duration::from_millis(500)) {
                    scene.apply(e);
                }
            }
            println!(
                "probe: nach {stay}s: {} Texte, {} Kacheln, {} B ({} B/s)",
                scene.texts.len(),
                scene.tiles,
                scene.tile_bytes,
                scene.tile_bytes / stay.max(1)
            );
            return;
        }
    }
    eprintln!(
        "probe: TIMEOUT (connected={connected} texte={} kacheln={})",
        scene.texts.len(),
        scene.tiles
    );
    std::process::exit(1);
}
