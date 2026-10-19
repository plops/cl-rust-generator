//! `01_engine` — eine Verbindung samt Szene, von Kotlin im UI-Takt gepollt.
//!
//! Der Worker-Thread (eigene Tokio-Runtime) baut die SSH-Kette
//! (`connect_chain_retry`), beweist das LBW-Token (Challenge → Auth →
//! Hello) und schickt fertige `Event`s an die Szene, die dem UI-Thread
//! gehört. `poll` wendet sie an und kopiert den Canvas nur bei Änderung
//! in den Puffer der UI (Direct-`ByteBuffer`).

use std::sync::{
    Arc, Mutex,
    mpsc::{Receiver, Sender, channel},
};

use lbw_client::av1::Decoder;
use lbw_client::net::Event;
use lbw_client::scene::{Link, Scene};
use lbw_common::framing::{decode_msg, encode_msg};
use lbw_common::{ClientMsg, PROTO_VERSION, ServerMsg, auth_mac};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use crate::blob::encode_texts;

/// Bits im Rückgabewert von [`Engine::poll`].
pub mod flags {
    /// Canvas wurde in den Puffer kopiert.
    pub const FRAME: i32 = 1;
    /// Texte geändert → `texts()` abholen.
    pub const TEXT: i32 = 2;
    /// Canvasgröße geändert → Puffer neu anlegen (`size()`), nichts kopiert.
    pub const SIZE: i32 = 4;
    /// Verbindung steht.
    pub const UP: i32 = 8;
}

/// Verbindungskonfiguration aus Kotlin (JSON).
#[derive(Debug, serde::Deserialize)]
pub struct EngineCfg {
    /// Jump-Hops + Ziel in Reihenfolge (`user@host[:port]`).
    pub hops: Vec<String>,
    /// Identity-Texte pro Hop (OpenSSH-Format, gleiche Reihenfolge).
    pub identities: Vec<String>,
    /// Hostkey-Pins pro Hop (`SHA256:…`, leere Liste = lernen).
    pub pins: Vec<String>,
    /// Ziel-sshd aus Jump-Sicht (`host`, `port`).
    pub target_sshd_host: String,
    pub target_sshd_port: u16,
    /// LBW-Port am Ziel.
    pub lbw_port: u16,
    /// LBW-Token.
    pub token: String,
}

pub struct Engine {
    scene: Scene,
    texts_changed: bool,
    ev_rx: Receiver<Event>,
    tx: Sender<ClientMsg>,
    pins: Arc<Mutex<Option<Vec<String>>>>,
    _worker: std::thread::JoinHandle<()>,
}

impl Engine {
    /// Startet den Worker zu `json`-[`EngineCfg`].
    pub fn new(json: &str) -> Result<Self, String> {
        let cfg: EngineCfg = serde_json::from_str(json).map_err(|e| format!("Config: {e}"))?;
        if cfg.hops.is_empty() || cfg.hops.len() != cfg.identities.len() {
            return Err("hops/identities brauchen gleiche Länge (≥ 1)".into());
        }
        let (tx, rx) = channel::<ClientMsg>();
        let (ev_tx, ev_rx) = channel::<Event>();
        let pins = Arc::new(Mutex::new(None));
        let worker = std::thread::spawn({
            let pins = pins.clone();
            move || worker_main(cfg, rx, ev_tx, pins)
        });
        Ok(Self {
            scene: Scene::new(),
            texts_changed: true,
            ev_rx,
            tx,
            pins,
            _worker: worker,
        })
    }

    /// Ereignisse anwenden; Canvas nach `out` kopieren, falls geändert
    /// und `out` groß genug ist. Liefert [`flags`].
    pub fn poll(&mut self, out: Option<&mut [u8]>) -> i32 {
        while let Ok(e) = self.ev_rx.try_recv() {
            match &e {
                Event::NewText(_)
                | Event::MoveText { .. }
                | Event::RemoveText(_)
                | Event::ClearText => self.texts_changed = true,
                _ => {}
            }
            self.scene.apply(e);
        }
        let mut f = 0;
        if self.scene.dirty
            && let Some(buf) = out
            && buf.len() >= self.scene.canvas.len()
        {
            buf[..self.scene.canvas.len()].copy_from_slice(&self.scene.canvas);
            self.scene.dirty = false;
            f |= flags::FRAME;
        }
        if self.texts_changed {
            self.texts_changed = false;
            f |= flags::TEXT;
        }
        if self.scene.link == Link::Up {
            f |= flags::UP;
        }
        f
    }

    /// Canvasgröße `(w, h)` in Server-Pixeln (fest 1280×720).
    #[must_use]
    pub fn size(&self) -> (usize, usize) {
        (lbw_common::WIDTH as usize, lbw_common::HEIGHT as usize)
    }

    /// Texte als Blob für Kotlin (`02_blob`).
    #[must_use]
    pub fn texts(&self) -> Vec<u8> {
        encode_texts(self.scene.texts.iter())
    }

    /// Statuszeile.
    #[must_use]
    pub fn status(&self) -> String {
        crate::blob::status_line(&self.scene)
    }

    /// Gelernte/geprüfte Hostkey-Fingerprints pro Hop (`SHA256:…`),
    /// sobald die Kette einmal stand — für TOFU-Persistenz in Kotlin.
    #[must_use]
    pub fn pins(&self) -> Vec<String> {
        self.pins
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
            .unwrap_or_default()
    }

    /// Eingabe an den Server (geht verloren ohne Verbindung).
    pub fn send(&self, m: ClientMsg) {
        let _ = self.tx.send(m);
    }
}

async fn read_msg<S>(s: &mut S) -> Result<ServerMsg, String>
where
    S: tokio::io::AsyncRead + Unpin,
{
    let mut h = [0u8; 4];
    s.read_exact(&mut h)
        .await
        .map_err(|e| format!("read: {e}"))?;
    let n = u32::from_le_bytes(h) as usize;
    if n > lbw_common::MAX_MSG {
        return Err("Frame zu groß".into());
    }
    let mut b = vec![0u8; n];
    s.read_exact(&mut b)
        .await
        .map_err(|e| format!("read: {e}"))?;
    decode_msg(&b).map_err(|e| format!("protokoll: {e}"))
}

async fn write_msg_async<S>(s: &mut S, m: &ClientMsg) -> Result<(), String>
where
    S: tokio::io::AsyncWrite + Unpin,
{
    let body = encode_msg(m).map_err(|e| e.to_string())?;
    s.write_all(&(body.len() as u32).to_le_bytes())
        .await
        .map_err(|e| format!("write: {e}"))?;
    s.write_all(&body)
        .await
        .map_err(|e| format!("write: {e}"))?;
    Ok(())
}

fn worker_main(
    cfg: EngineCfg,
    input: Receiver<ClientMsg>,
    ev: Sender<Event>,
    pins: Arc<Mutex<Option<Vec<String>>>>,
) {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("tokio");
    rt.block_on(worker_async(cfg, input, ev, pins));
}

async fn worker_async(
    cfg: EngineCfg,
    input: Receiver<ClientMsg>,
    ev: Sender<Event>,
    pins: Arc<Mutex<Option<Vec<String>>>>,
) {
    let mut backoff = std::time::Duration::from_millis(500);
    loop {
        match connect_once(&cfg, &input, &ev, &pins).await {
            Ok(()) => backoff = std::time::Duration::from_millis(500),
            Err(e) => {
                let _ = ev.send(Event::Disconnected(e));
            }
        }
        tokio::time::sleep(backoff).await;
        backoff = (backoff * 2).min(std::time::Duration::from_secs(5));
    }
}

async fn connect_once(
    cfg: &EngineCfg,
    input: &Receiver<ClientMsg>,
    ev: &Sender<Event>,
    pins: &Arc<Mutex<Option<Vec<String>>>>,
) -> Result<(), String> {
    let pinned: Vec<lbw_tunnel::PinnedHop> = cfg
        .hops
        .iter()
        .enumerate()
        .map(|(i, h)| {
            Ok(lbw_tunnel::PinnedHop {
                hop: lbw_tunnel::parse_hop(h).map_err(|e| format!("Hop {h}: {e}"))?,
                identity_openssh: cfg.identities.get(i).cloned().unwrap_or_default(),
                passphrase: None,
                pin: cfg.pins.get(i).cloned(),
            })
        })
        .collect::<Result<_, String>>()?;
    let policy = lbw_tunnel::TunnelPolicy::default();
    let (mut chain, learned) = lbw_tunnel::connect_chain_retry_with_pins(
        &policy,
        &pinned,
        (&cfg.target_sshd_host, cfg.target_sshd_port),
        cfg.lbw_port,
    )
    .await?;
    *pins
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(learned);
    let s = &mut chain.stream;

    // Auth: Challenge lesen → Auth + Hello senden → Server-Hello lesen.
    let nonce = match read_msg(s).await? {
        ServerMsg::Challenge { version, nonce } => {
            if version != PROTO_VERSION {
                return Err("falsche Version".into());
            }
            nonce
        }
        _ => return Err("kein Challenge".into()),
    };
    write_msg_async(
        s,
        &ClientMsg::Auth {
            mac: auth_mac(cfg.token.as_bytes(), &nonce),
        },
    )
    .await?;
    write_msg_async(
        s,
        &ClientMsg::Hello {
            version: PROTO_VERSION,
        },
    )
    .await?;
    match read_msg(s).await? {
        ServerMsg::Hello => {
            let _ = ev.send(Event::Connected);
        }
        _ => return Err("kein Hello".into()),
    }

    let mut dec = Decoder::new()?;
    loop {
        while let Ok(m) = input.try_recv() {
            write_msg_async(s, &m)
                .await
                .map_err(|_| "sendefehler".to_string())?;
        }
        let m = match tokio::time::timeout(std::time::Duration::from_millis(10), read_msg(s)).await
        {
            Ok(Ok(m)) => m,
            Ok(Err(e)) => return Err(e),
            Err(_) => continue,
        };
        match m {
            ServerMsg::Challenge { .. } => return Err("doppelte Challenge".into()),
            ServerMsg::Hello => {
                let _ = ev.send(Event::Connected);
            }
            ServerMsg::ClearText => {
                let _ = ev.send(Event::ClearText);
            }
            ServerMsg::NewText(t) => {
                let _ = ev.send(Event::NewText(t));
            }
            ServerMsg::MoveText { id, rect } => {
                let _ = ev.send(Event::MoveText { id, rect });
            }
            ServerMsg::RemoveText(id) => {
                let _ = ev.send(Event::RemoveText(id));
            }
            ServerMsg::Tile { x, y, w, h, data } => match dec.decode(&data) {
                Ok(rgba) => {
                    // Wie Desktop: kleiner kodiert → auf Fußabdruck hoch.
                    let rgba = lbw_client::av1::upscale_rgba(rgba, w as usize, h as usize);
                    let _ = ev.send(Event::Tile {
                        x,
                        y,
                        w: rgba.w,
                        h: rgba.h,
                        rgba: rgba.data,
                        bytes: data.len(),
                    });
                }
                Err(e) => {
                    let _ = ev.send(Event::Disconnected(format!("AV1: {e}")));
                    return Err(format!("AV1: {e}"));
                }
            },
        }
    }
}
