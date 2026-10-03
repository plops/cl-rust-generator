//! Headless-Smoke-Client: verbindet, wartet auf Text + Kachel, schickt
//! Eingaben und meldet Erfolg. Mit zweitem Argument bleibt er noch N
//! Sekunden verbunden und meldet Summen (Durchsatz-Messung). Nutzung:
//! `cargo run --release -p lbw-client --example probe -- 127.0.0.1:7878 [N]`

use std::time::{Duration, Instant};

use lbw_client::net::{Event, Net};
use lbw_client::scene::Scene;
use lbw_common::ClientMsg;

fn main() {
    let mut args = std::env::args().skip(1);
    let addr = args.next().unwrap_or_else(|| "127.0.0.1:7878".into());
    let stay: u64 = args.next().and_then(|s| s.parse().ok()).unwrap_or(0);
    let net = Net::connect(&addr);
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
