# Implementierungsplan: Graphical Remote Browser Proxy

Dieser Plan gliedert sich in 5 Phasen. Die Architektur basiert auf einem **Actor-Modell**, um asynchrone Netzwerk-I/O (Tokio) strikt von rechenintensiver Videokompression (rav1e/rav1d) und dem synchronen Rendering (Macroquad) zu trennen.

## 1. Architektur-Aufteilung & Crate-Struktur

Der Cargo-Workspace besteht aus folgenden Crates:

```text
graphical-proxy-workspace/
├── Cargo.toml                # Workspace-Definition, Profile (opt-level="z", LTO)
├── proto-def/                # Beinhaltet browser_stream.proto und build.rs (tonic-build)
├── core-utils/               # YUV <-> RGB Konvertierung, Math, geteilte Konstanten
├── cloud-render-srv/         # Server: Tokio, tonic, chromiumoxide, rav1e
└── macroquad-client/         # Client: Macroquad (UI), tokio (Netzwerk), rav1d (Decoder), clap (CLI)
```

---

## 2. Datenstrukturen (Der Systemzustand)

### 2.1. Der Protobuf-Vertrag (`proto-def/proto/browser_stream.proto`)
Hier ist die exakte Definition, inklusive der Viewport-Koordinaten für den virtuellen Framebuffer:

```protobuf
syntax = "proto3";
package graphical_proxy;

service RemoteBrowser {
  rpc StreamSession(stream ClientEvent) returns (stream ServerUpdate);
}

message ClientEvent {
  oneof event {
    NavigateRequest navigate = 1;
    ScrollInput scroll = 2;
    SearchRequest search = 3;
    // ... MouseInput, KeyboardInput etc.
  }
}

message NavigateRequest { string url = 1; }
message ScrollInput { int32 absolute_y = 1; }
message SearchRequest { string query = 1; }

message ServerUpdate {
  oneof update {
    VideoFrame frame = 1;
    SpatialMetadata metadata = 2;
    SearchResults search_results = 3;
  }
}

message VideoFrame {
  bytes av1_data = 1;
  bool is_keyframe = 2;
  int32 viewport_x = 3;
  int32 viewport_y = 4;
}

message SpatialMetadata {
  uint32 document_width = 1;
  uint32 document_height = 2;
  repeated LinkBox links = 3;
}

message LinkBox {
  uint32 id = 1; string url = 2; string label = 3;
  int32 x = 4; int32 y = 5; int32 width = 6; int32 height = 7;
}
```

### 2.2. Client-Seitiger Status (`macroquad-client/src/state.rs`)
Der Client benötigt einen Thread-sicheren Zustand, den der Tokio-Netzwerk-Task befüllt und der Macroquad-Render-Task (mit 60 FPS) ausliest.

```rust
use std::sync::{Arc, Mutex};
use core_utils::ImageRgba;

pub struct PageState {
    // Der zuletzt dekodierte AV1 Frame als RGB
    pub latest_frame: Option<ImageRgba>,
    // Wo genau dieses Bild auf der Serverseite aufgenommen wurde
    pub server_viewport_y: i32,
    
    // Wo der Benutzer LOKAL gerade hingescrollt hat (Client-Side Prediction)
    pub local_scroll_y: i32,
    
    // Die Metadaten für das Skeleton UI
    pub doc_height: u32,
    pub links: Vec<proto_def::LinkBox>,
    pub search_results: Vec<proto_def::SearchMatch>,
}

pub type SharedState = Arc<Mutex<PageState>>;
```

---

## 3. Server-Implementierung (`cloud-render-srv`)

Der Server nutzt `chromiumoxide` im Hintergrund und extrahiert sowohl das Bild als auch den DOM.

### 3.1. DOM Metadaten Extraktion (JavaScript Injektion)
Anstatt den HTML-Baum in Rust zu parsen, lassen wir die Chrome-V8-Engine die Arbeit machen. Dieser Code wird nach jedem Scrollen oder Navigation-Complete injiziert:

```rust
// cloud-render-srv/src/browser.rs

pub async fn extract_spatial_metadata(page: &chromiumoxide::Page) -> Result<proto_def::SpatialMetadata, Box<dyn std::error::Error>> {
    let js_script = r#"
        (() => {
            const links = Array.from(document.querySelectorAll('a')).map((a, i) => {
                const rect = a.getBoundingClientRect();
                return {
                    id: i,
                    url: a.href,
                    label: a.innerText.substring(0, 50),
                    x: Math.round(rect.x + window.scrollX),
                    y: Math.round(rect.y + window.scrollY),
                    width: Math.round(rect.width),
                    height: Math.round(rect.height)
                };
            });
            return {
                doc_width: document.documentElement.scrollWidth,
                doc_height: document.documentElement.scrollHeight,
                links: links
            };
        })();
    "#;

    // Führe JS in Chrome aus und parse das resultierende JSON
    let result: serde_json::Value = page.evaluate(js_script).await?.into_value()?;
    
    // ... Mapping von serde_json::Value zu proto_def::SpatialMetadata ...
    
    Ok(metadata)
}
```

### 3.2. Volltextsuche (Serverseitig)
Wenn der Client `SearchRequest` sendet:

```rust
pub async fn execute_search(page: &chromiumoxide::Page, query: &str) -> proto_def::SearchResults {
    let js_script = format!(r#"
        (() => {{
            // Nutze window.find oder TreeWalker um den Text '{query}' zu finden
            // Extrahiere offsetTop und umgebenden Text
            // ... (JS Logik)
        }})();
    "#);
    // ... auswerten und als SearchResults via gRPC zurücksenden
}
```

### 3.3. AV1 Video Pipeline (Pseudocode)
```rust
// cloud-render-srv/src/video.rs
use rav1e::prelude::*;

pub fn encode_frame(encoder: &mut Context<u16>, rgba_pixels: &[u8], width: usize, height: usize) -> Vec<u8> {
    // 1. Konvertiere RGBA (von Chrome) zu YUV420
    let yuv_frame = core_utils::rgba_to_yuv420(rgba_pixels, width, height);
    
    // 2. Sende Frame an rav1e
    let mut frame = encoder.new_frame();
    // ... fülle frame.planes ...
    encoder.send_frame(frame).unwrap();
    
    // 3. Empfange komprimierte AV1-Bytes
    let packet = encoder.receive_packet().unwrap();
    packet.data.to_vec()
}
```

---

## 4. Client-Implementierung (`macroquad-client`)

Der Client ist das Herzstück für die UX. Hier läuft die Magie des **virtuellen Framebuffers**.

### 4.1. Thread-Setup & Netzwerk-Schleife
Der `macroquad` Main-Thread darf niemals blockieren. Die gRPC-Schleife läuft in einem Tokio-Background-Task.

```rust
// macroquad-client/src/main.rs

#[macroquad::main("AV1 Remote Browser")]
async fn main() {
    let shared_state = Arc::new(Mutex::new(PageState::default()));
    let state_clone = shared_state.clone();

    // Starte den asynchronen Netzwerk-Task in einem separaten Thread
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            network_loop(state_clone).await;
        });
    });

    // Main Render Loop (läuft mit 60 FPS synchron zur GPU)
    render_loop(shared_state).await;
}

async fn network_loop(state: SharedState) {
    let mut client = RemoteBrowserClient::connect("http://127.0.0.1:50051").await.unwrap();
    // ... baue Stream auf ...
    while let Some(update) = stream.next().await {
        match update.update {
            Some(ServerUpdate::VideoFrame(frame)) => {
                // 1. Dekodiere AV1 via rav1d zu RGB
                let rgb_pixels = decode_av1(&frame.av1_data);
                // 2. Speichere im State
                let mut lock = state.lock().unwrap();
                lock.latest_frame = Some(rgb_pixels);
                lock.server_viewport_y = frame.viewport_y;
            }
            Some(ServerUpdate::SpatialMetadata(meta)) => {
                let mut lock = state.lock().unwrap();
                lock.links = meta.links;
                lock.doc_height = meta.document_height;
            }
            // ...
        }
    }
}
```

### 4.2. Render-Schleife & Zero-Latency Scrolling (Skeleton UI)
Hier verrechnen wir Serverealität (`server_viewport_y`) mit lokaler Vorhersage (`local_scroll_y`).

```rust
// macroquad-client/src/render.rs
use macroquad::prelude::*;

pub async fn render_loop(state: SharedState) {
    let mut video_texture: Option<Texture2D> = None;

    loop {
        clear_background(color_u8!(30, 30, 30, 255)); // Dunkelgrauer Hintergrund

        let (server_y, local_y, links, doc_height) = {
            let mut lock = state.lock().unwrap();
            
            // Behandle lokales Scrollen (Mausrad)
            let (_, mouse_wheel_y) = mouse_wheel();
            if mouse_wheel_y != 0.0 {
                lock.local_scroll_y -= mouse_wheel_y as i32 * 30; // 30px pro Tick
                lock.local_scroll_y = lock.local_scroll_y.clamp(0, lock.doc_height as i32);
                
                // TODO: Sende neues ScrollInput(lock.local_scroll_y) an Server via Channel
            }

            // Update Textur in der GPU, falls ein neuer Frame kam
            if let Some(rgb_data) = lock.latest_frame.take() {
                if video_texture.is_none() {
                    video_texture = Some(Texture2D::from_rgba8(1920, 1080, &rgb_data.pixels));
                } else {
                    video_texture.as_mut().unwrap().update(&rgb_data.pixels);
                }
            }
            
            (lock.server_viewport_y, lock.local_scroll_y, lock.links.clone(), lock.doc_height)
        };

        // 1. Berechne den Offset: Wie weit hinkt das Video dem lokalen Scroll hinterher?
        let render_offset_y = (server_y - local_y) as f32;

        // 2. Zeichne das verzögerte Video an der verschobenen Position
        if let Some(tex) = &video_texture {
            draw_texture(*tex, 0.0, render_offset_y, WHITE);
        }

        // 3. Zeichne das Skeleton UI (Links und Metadaten)
        // Diese werden ABSOLUT zum lokalen Scroll gezeichnet. Sie sind immer sofort da!
        for link in links {
            let screen_y = (link.y - local_y) as f32;
            let screen_x = link.x as f32;

            // Ist der Link im sichtbaren Fenster?
            if screen_y > -link.height as f32 && screen_y < screen_height() {
                // Zeichne eine semi-transparente Hitbox
                draw_rectangle_lines(screen_x, screen_y, link.width as f32, link.height as f32, 2.0, BLUE);
                
                // Zeichne den Label-Text
                draw_text(&link.label, screen_x, screen_y + 15.0, 16.0, WHITE);
                
                // Hit-Testing für Mausklicks (Zero-Latency!)
                if is_mouse_button_pressed(MouseButton::Left) {
                    let (mx, my) = mouse_position();
                    if mx >= screen_x && mx <= screen_x + link.width as f32 &&
                       my >= screen_y && my <= screen_y + link.height as f32 {
                        println!("Link geklickt: {}", link.url);
                        // TODO: Sende NavigateRequest(link.url) an Server
                    }
                }
            }
        }

        // 4. Zeichne Scrollbar (Optional)
        let scroll_ratio = local_y as f32 / doc_height as f32;
        draw_rectangle(screen_width() - 10.0, scroll_ratio * screen_height(), 10.0, 50.0, GRAY);

        next_frame().await;
    }
}
```

---

## 5. CLI-Design & KI-Agent Testinfrastruktur

Damit der KI-Agent (wie in der Anforderung gewünscht) das System ohne GUI testen kann, überspringen wir Macroquad komplett, wenn `--test-mode` gesetzt ist.

```rust
// macroquad-client/src/main.rs (Modifiziert)

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about = "AV1 Remote Browser Client")]
struct Cli {
    #[arg(long)]
    test_mode: bool,

    #[arg(long)]
    url: Option<String>,

    #[arg(long)]
    search_query: Option<String>,

    #[arg(long)]
    dump_search: Option<String>,
    
    #[arg(long)]
    disable_video: bool,
}

fn main() {
    let cli = Cli::parse();

    if cli.test_mode {
        // Starte NUR die Tokio Runtime, kein Macroquad Fenster!
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            run_headless_test_agent(cli).await;
        });
    } else {
        // Normaler GUI Start
        macroquad::Window::from_config(conf(), async_main());
    }
}

async fn run_headless_test_agent(cli: Cli) {
    println!("Starte Agenten-Testmodus...");
    let mut client = RemoteBrowserClient::connect("http://127.0.0.1:50051").await.unwrap();
    
    // 1. Konfiguriere Stream (Agent kann Video abschalten, um Traffic zu sparen)
    client.send(StreamConfig { enable_video: !cli.disable_video, enable_spatial_links: true }).await;
    
    // 2. Navigiere
    if let Some(url) = cli.url {
        client.send(NavigateRequest { url }).await;
    }
    
    // 3. Suche ausführen und in Datei dumpen
    if let Some(query) = cli.search_query {
        client.send(SearchRequest { query }).await;
        
        // Warte auf SearchResults vom Server
        while let Some(update) = stream.next().await {
            if let Some(ServerUpdate::SearchResults(res)) = update.update {
                if let Some(dump_path) = cli.dump_search {
                    let json = serde_json::to_string_pretty(&res).unwrap();
                    std::fs::write(dump_path, json).unwrap();
                    println!("Suchergebnisse erfolgreich nach {} geschrieben.", dump_path);
                    return; // Test erfolgreich beendet
                }
            }
        }
    }
}
