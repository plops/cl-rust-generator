
wir koennen die bildgroesse auf 640x640 festsetzen um skalierungscode und variablen zu vermeiden

Das ist eine hervorragende Idee. Die Festlegung auf exakt 640x640 Pixel reduziert die Komplexität noch weiter:

1. **Keine Teil-Kacheln mehr:** 640 ist restlos durch unsere Kachelgröße (64) teilbar. Wir haben also exakt ein 10x10 Raster. Sämtliche `min()`-Berechnungen am Rand entfallen.
2. **Schlankeres Protokoll:** Wir müssen `w` und `h` nicht mehr in `Hello` oder `Tile` mitschicken.
3. **Kein Skalierungscode im Client:** Das Fenster wird fest auf 640x640 initialisiert, und die Textur behält dauerhaft diese Größe.

Hier ist der angepasste, nochmals simplifizierte Code:

### 1. Das Protokoll (`common/src/lib.rs`)
`w` und `h` fliegen aus den Netzwerk-Nachrichten raus (außer bei Text, da Textboxen variabel sind).

```rust
use serde::{Deserialize, Serialize};

pub const SCREEN_SIZE: u32 = 640;
pub const TILE_SIZE: u32 = 64;

#[derive(Serialize, Deserialize, Debug)]
pub enum ServerMsg {
    Hello,
    ClearText,
    AddText { x: u32, y: u32, w: u32, h: u32, fg: [u8; 3], bg: [u8; 3], text: String },
    Tile { x: u32, y: u32, data: Vec<u8> }, // Breite/Höhe sind immer TILE_SIZE
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ClientMsg {
    MouseMove { x: i32, y: i32 },
    MouseClick { button: u8, down: bool },
    KeyPress { keycode: String, down: bool },
    Text(String),
}
```

---

### 2. Der Server (`server/src/main.rs`)
Der Server schneidet den Bildschirm einfach hart auf 640x640 zu. Das 10x10 Raster wird blind iteriert.

```rust
use clap::Parser;
use enigo::{Enigo, Keyboard, Mouse, Coordinate, MouseButton, Direction};
use image::{GenericImageView, RgbaImage};
use lbw_common::{ClientMsg, ServerMsg, SCREEN_SIZE, TILE_SIZE};
use rav1e::prelude::*;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use xcap::Monitor;

#[derive(Parser, Debug)]
#[command(name = "lbw-server", about = "Minimal 640x640 Remote Desktop")]
struct Config {
    #[arg(long, default_value = "127.0.0.1:7878")]
    listen: String,
    #[arg(long, default_value_t = 64)]
    quantizer: usize,
}

fn main() {
    let cfg = Config::parse();
    let listener = TcpListener::bind(&cfg.listen).unwrap();
    println!("Server lauscht auf {} (Fixe Größe: 640x640)", cfg.listen);

    for stream in listener.incoming() {
        if let Ok(stream) = stream {
            println!("Client verbunden!");
            handle_client(stream, &cfg);
        }
    }
}

fn handle_client(mut stream: TcpStream, cfg: &Config) {
    let mut enigo = Enigo::new(&enigo::Settings::default()).unwrap();
    let mut read_stream = stream.try_clone().unwrap();
    
    // Input Thread
    std::thread::spawn(move || loop {
        let mut len_buf = [0u8; 4];
        if read_stream.read_exact(&mut len_buf).is_err() { break; }
        let len = u32::from_le_bytes(len_buf) as usize;
        let mut data = vec![0u8; len];
        if read_stream.read_exact(&mut data).is_err() { break; }
        
        match bincode::deserialize::<ClientMsg>(&data).unwrap() {
            ClientMsg::MouseMove { x, y } => { enigo.move_mouse(x, y, Coordinate::Abs).unwrap(); }
            ClientMsg::MouseClick { button: _, down } => {
                let dir = if down { Direction::Press } else { Direction::Release };
                enigo.button(MouseButton::Left, dir).unwrap();
            }
            ClientMsg::Text(t) => { enigo.text(&t).unwrap(); }
            ClientMsg::KeyPress { .. } => { /* Mapping */ }
        }
    });

    // Capture Loop
    let monitor = Monitor::all().unwrap().into_iter().next().unwrap();
    let mut prev_frame: Option<RgbaImage> = None;

    send_msg(&mut stream, &ServerMsg::Hello);

    loop {
        // Screenshot machen und hart auf 640x640 zuschneiden (oben links)
        let full_img = monitor.capture_image().unwrap();
        let current_frame = full_img.view(0, 0, SCREEN_SIZE, SCREEN_SIZE).to_image();

        // Fixes 10x10 Raster
        for y in (0..SCREEN_SIZE).step_by(TILE_SIZE as usize) {
            for x in (0..SCREEN_SIZE).step_by(TILE_SIZE as usize) {
                let tile = current_frame.view(x, y, TILE_SIZE, TILE_SIZE).to_image();
                
                let is_dirty = prev_frame.as_ref().map_or(true, |prev| {
                    tile.as_raw() != prev.view(x, y, TILE_SIZE, TILE_SIZE).to_image().as_raw()
                });

                if is_dirty {
                    let av1_data = encode_tile_av1(&tile, cfg.quantizer);
                    send_msg(&mut stream, &ServerMsg::Tile { x, y, data: av1_data });
                }
            }
        }
        prev_frame = Some(current_frame);
        std::thread::sleep(std::time::Duration::from_millis(100)); // ~10 FPS
    }
}

fn encode_tile_av1(img: &RgbaImage, q: usize) -> Vec<u8> {
    // 64x64 Tile in rav1e encodieren (wie gehabt)
    let mut enc = EncoderConfig::with_speed_preset(10);
    enc.width = TILE_SIZE as usize; 
    enc.height = TILE_SIZE as usize;
    enc.still_picture = true; 
    enc.quantizer = q;
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(1);
    let mut ctx: Context<u8> = cfg.new_context().unwrap();
    // ctx.send_frame(...) // YUV Logik hier einfügen
    ctx.flush();

    let mut out = Vec::new();
    while let Ok(pkt) = ctx.receive_packet() { out.extend_from_slice(&pkt.data); }
    out
}

fn send_msg(stream: &mut TcpStream, msg: &ServerMsg) {
    let data = bincode::serialize(msg).unwrap();
    stream.write_all(&(data.len() as u32).to_le_bytes()).unwrap();
    stream.write_all(&data).unwrap();
}
```

---

### 3. Der Client (`client/src/main.rs`)
Der Client benötigt keinen Resizing-Code mehr. `Scene` hat keine Felder für `w` und `h` mehr, sondern nutzt fixe Arrays. Das Macroquad-Fenster ist nicht vergrößerbar.

```rust
use clap::Parser;
use lbw_common::{ClientMsg, ServerMsg, SCREEN_SIZE, TILE_SIZE};
use macroquad::prelude::*;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::sync::{Arc, Mutex};

#[derive(Parser)]
struct Config {
    #[arg(long, default_value = "127.0.0.1:7878")]
    connect: String,
}

struct Scene {
    pub canvas: Vec<u8>,
    pub texts: Vec<ServerMsg>,
    pub dirty: bool,
}

fn window_conf() -> Conf {
    Conf { 
        window_title: "lbw-client".to_string(), 
        window_width: SCREEN_SIZE as i32, 
        window_height: SCREEN_SIZE as i32,
        window_resizable: false, // Fix auf 640x640 zwingen
        ..Default::default() 
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    let cfg = Config::parse();
    let mut stream = TcpStream::connect(&cfg.connect).expect("Verbindung fehlgeschlagen");

    // Canvas fest auf 640x640x4 (RGBA) initialisieren
    let scene = Arc::new(Mutex::new(Scene { 
        canvas: vec![0; (SCREEN_SIZE * SCREEN_SIZE * 4) as usize], 
        texts: vec![], 
        dirty: true 
    }));
    let scene_clone = scene.clone();

    // Netzwerk-Lese-Thread
    std::thread::spawn(move || {
        let mut stream_clone = stream.try_clone().unwrap();
        
        loop {
            let mut len_buf = [0u8; 4];
            if stream_clone.read_exact(&mut len_buf).is_err() { break; }
            let len = u32::from_le_bytes(len_buf) as usize;
            let mut data = vec![0u8; len];
            if stream_clone.read_exact(&mut data).is_err() { break; }

            let mut s = scene_clone.lock().unwrap();

            match bincode::deserialize::<ServerMsg>(&data).unwrap() {
                ServerMsg::Hello => { s.canvas.fill(255); s.dirty = true; }
                ServerMsg::ClearText => s.texts.clear(),
                ServerMsg::AddText { .. } => s.texts.push(msg),
                ServerMsg::Tile { x, y, data } => {
                    // AV1 decodieren... let rgba_tile = decode_av1(&data); 
                    
                    // Kachel hart in den 640x640 Puffer kopieren
                    /* 
                    for ty in 0..TILE_SIZE {
                        let dst_idx = (((y + ty) * SCREEN_SIZE + x) * 4) as usize;
                        let src_idx = (ty * TILE_SIZE * 4) as usize;
                        s.canvas[dst_idx..dst_idx + (TILE_SIZE*4) as usize].copy_from_slice(&rgba_tile[src_idx..]);
                    }
                    */
                    s.dirty = true;
                }
            }
        }
    });

    let mut texture = Texture2D::empty();

    loop {
        // Render
        {
            let mut s = scene.lock().unwrap();
            
            if s.dirty {
                let img = Image { bytes: s.canvas.clone(), width: SCREEN_SIZE as u16, height: SCREEN_SIZE as u16 };
                texture = Texture2D::from_image(&img);
                s.dirty = false;
            }

            clear_background(BLACK);
            draw_texture(&texture, 0.0, 0.0, WHITE);

            for text_msg in &s.texts {
                if let ServerMsg::AddText { x, y, w, h, fg, bg, text } = text_msg {
                    draw_rectangle(*x as f32, *y as f32, *w as f32, *h as f32, Color::from_rgba(bg[0], bg[1], bg[2], 255));
                    draw_text(text, *x as f32, *y as f32 + 16.0, 16.0, Color::from_rgba(fg[0], fg[1], fg[2], 255));
                }
            }
        }

        // Input Handling
        let (mx, my) = mouse_position();
        if is_mouse_button_down(MouseButton::Left) {
            send_msg(&mut stream, &ClientMsg::MouseMove { x: mx as i32, y: my as i32 });
        }

        if let Some(c) = get_char_pressed() {
            send_msg(&mut stream, &ClientMsg::Text(c.to_string()));
        }

        next_frame().await;
    }
}

fn send_msg(stream: &mut TcpStream, msg: &ClientMsg) {
    let data = bincode::serialize(msg).unwrap();
    let _ = stream.write_all(&(data.len() as u32).to_le_bytes());
    let _ = stream.write_all(&data);
}
```

Durch das Hardcoding von 640x640 verschwinden alle Variablen für Skalierung (`w`, `h`, `scale`, Resizing-Callbacks). Die Logik besteht jetzt quasi nur noch aus zwei for-Schleifen im Server (Raster generieren) und einer Zuweisung im Client (Raster malen). Absolutes MVP.
