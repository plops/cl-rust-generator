//! `01_engine` — eine Verbindung samt Szene, von der App im UI-Takt gepollt.
//!
//! Aufbau: optional SSH-Kette als lokaler Forward (`lbw_tunnel::Forward`,
//! Jump-Hosts, Ed25519-Keys im OpenSSH-Format, TOFU-Pins) → unveränderter
//! Netz-Thread des Desktop-Clients (`lbw_client::net::Net`: Protokoll v3,
//! Reconnect, AV1-Decode, Maus-Zusammenfassung) → `Scene`. Die App ruft
//! `poll` im Bildtakt; der Canvas wird nur bei Änderung kopiert.

use lbw_client::net::Net;
use lbw_client::scene::{Link, Scene};
use lbw_common::{ClientMsg, HEIGHT, WIDTH};
use lbw_tunnel::{Forward, ForwardCfg, PinnedHop, TunnelPolicy, parse_hop};

use crate::blob::encode_texts;

/// Bits im Rückgabewert von [`Engine::poll`] (wie source11, damit die
/// Android-App unverändert bleibt).
pub mod flags {
    /// Canvas wurde in den Puffer kopiert.
    pub const FRAME: i32 = 1;
    /// Texte geändert → `texts()` abholen.
    pub const TEXT: i32 = 2;
    /// Canvasgröße geändert (v3: nie, immer 1280×720).
    pub const SIZE: i32 = 4;
    /// Verbindung steht.
    pub const UP: i32 = 8;
}

/// Verbindungskonfiguration aus der App (JSON). Unbekannte Felder (z. B.
/// `token` der source11-App) werden ignoriert.
#[derive(Debug, Default, serde::Deserialize)]
#[serde(default)]
pub struct EngineCfg {
    /// Direkt verbinden (`host:port`, ohne SSH) — nur wenn `hops` leer.
    pub direct: String,
    /// Jump-Hops + Ziel in Reihenfolge (`user@host[:port]`).
    pub hops: Vec<String>,
    /// Private Keys pro Hop (OpenSSH-Text, z. B. Ed25519; gleiche Reihenfolge).
    pub identities: Vec<String>,
    /// Passphrasen pro Hop (leer = unverschlüsselter Key).
    pub passphrases: Vec<String>,
    /// Hostkey-Pins pro Hop (`SHA256:…`; leer/fehlend = beim 1. Mal lernen).
    pub pins: Vec<String>,
    /// Ziel-sshd aus Sicht des letzten Jumps (z. B. `127.0.0.1`, 22).
    pub target_sshd_host: String,
    pub target_sshd_port: u16,
    /// LBW-Port am Ziel (Default 7878).
    pub lbw_port: u16,
}

impl EngineCfg {
    /// Prüft und baut die Forward-Konfiguration (`None` = direkt).
    pub fn forward(&self) -> Result<Option<ForwardCfg>, String> {
        if self.hops.is_empty() {
            if self.direct.is_empty() {
                return Err("weder hops noch direct angegeben".into());
            }
            return Ok(None);
        }
        if self.identities.len() != self.hops.len() {
            return Err(format!(
                "{} Hops, aber {} Keys",
                self.hops.len(),
                self.identities.len()
            ));
        }
        let mut hops = Vec::new();
        for (i, h) in self.hops.iter().enumerate() {
            let pick = |v: &[String]| v.get(i).filter(|s| !s.is_empty()).cloned();
            hops.push(PinnedHop {
                hop: parse_hop(h)?,
                identity_openssh: self.identities[i].clone(),
                passphrase: pick(&self.passphrases),
                pin: pick(&self.pins),
            });
        }
        let last = &hops[hops.len() - 1].hop;
        let target_sshd = if self.target_sshd_host.is_empty() {
            (last.host.clone(), last.port)
        } else {
            (
                self.target_sshd_host.clone(),
                if self.target_sshd_port == 0 {
                    22
                } else {
                    self.target_sshd_port
                },
            )
        };
        Ok(Some(ForwardCfg {
            hops,
            target_sshd,
            lbw_port: if self.lbw_port == 0 {
                lbw_common::DEFAULT_PORT
            } else {
                self.lbw_port
            },
            local_port: 0,
            policy: TunnelPolicy::default(),
        }))
    }
}

/// Laufende Verbindung. Feld-Reihenfolge = Drop-Reihenfolge: erst den
/// Netz-Thread stoppen, dann den Forward.
pub struct Engine {
    net: Net,
    forward: Option<Forward>,
    scene: Scene,
    texts_changed: bool,
}

impl Engine {
    /// Startet Forward (falls Hops) und Netz-Thread.
    pub fn new(json: &str) -> Result<Self, String> {
        let cfg: EngineCfg = serde_json::from_str(json).map_err(|e| format!("Config: {e}"))?;
        let (forward, addr) = match cfg.forward()? {
            Some(f) => {
                let fw = Forward::spawn(f)?;
                let a = fw.addr.to_string();
                (Some(fw), a)
            }
            None => (None, cfg.direct.clone()),
        };
        Ok(Self {
            net: Net::connect(&addr),
            forward,
            scene: Scene::new(),
            texts_changed: true,
        })
    }

    /// Wendet anstehende Ereignisse an; kopiert den Canvas (RGBA8,
    /// 1280×720×4) nach `frame`, wenn er sich geändert hat und passt.
    pub fn poll(&mut self, frame: Option<&mut [u8]>) -> i32 {
        let n_texts = self.scene.texts.len();
        let mut text_event = false;
        while let Ok(e) = self.net.events.try_recv() {
            text_event |= matches!(
                e,
                lbw_client::net::Event::ClearText
                    | lbw_client::net::Event::AddText(_)
                    | lbw_client::net::Event::RemoveText(_)
            );
            self.scene.apply(e);
        }
        let mut f = 0;
        if text_event || n_texts != self.scene.texts.len() || self.texts_changed {
            self.texts_changed = false;
            f |= flags::TEXT;
        }
        if self.scene.dirty
            && let Some(buf) = frame
            && buf.len() >= self.scene.canvas.len()
        {
            buf[..self.scene.canvas.len()].copy_from_slice(&self.scene.canvas);
            self.scene.dirty = false;
            f |= flags::FRAME;
        }
        if self.scene.link == Link::Up {
            f |= flags::UP;
        }
        f
    }

    /// Canvasgröße (v3: fest 1280×720).
    #[must_use]
    pub fn size(&self) -> (u32, u32) {
        (WIDTH, HEIGHT)
    }

    /// Texte als Binärblob (s. `02_blob`).
    #[must_use]
    pub fn texts(&self) -> Vec<u8> {
        encode_texts(self.scene.texts.iter())
    }

    /// Statuszeile (Verbindung, Kacheln, Texte, letzter SSH-Fehler).
    #[must_use]
    pub fn status(&self) -> String {
        let mut s = crate::blob::status_line(&self.scene);
        if let Some(e) = self.forward.as_ref().and_then(Forward::last_error) {
            s.push_str(&format!(" | SSH: {e}"));
        }
        s
    }

    /// Hostkey-Fingerprints pro Hop (TOFU), leer ohne SSH/vor dem 1. Aufbau.
    #[must_use]
    pub fn pins(&self) -> Vec<String> {
        self.forward.as_ref().map(Forward::pins).unwrap_or_default()
    }

    /// Schickt eine Eingabe (geht verloren, wenn gerade keine Verbindung steht).
    pub fn send(&self, m: ClientMsg) {
        self.net.send(m);
    }

    /// Sondertaste drücken + loslassen (Namen wie `lbw-server` `08_input`).
    pub fn key(&self, name: &str) {
        for down in [true, false] {
            self.send(ClientMsg::Key {
                key: name.into(),
                down,
            });
        }
    }

    /// Rad als Bild-auf/-ab (Protokoll v3 kennt kein Rad).
    pub fn wheel(&self, dy: i32) {
        if dy != 0 {
            self.key(if dy < 0 { "PageUp" } else { "PageDown" });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cfg_direct_and_chain() {
        let c: EngineCfg = serde_json::from_str(r#"{"direct":"127.0.0.1:7878"}"#).unwrap();
        assert!(c.forward().unwrap().is_none());
        let c: EngineCfg = serde_json::from_str(
            r#"{"hops":["a@jump:2222","b@ziel"],"identities":["k1","k2"],
                "pins":["SHA256:x"],"target_sshd_host":"127.0.0.1",
                "target_sshd_port":2332,"lbw_port":7878,"token":"ignoriert"}"#,
        )
        .unwrap();
        let f = c.forward().unwrap().unwrap();
        assert_eq!(f.hops.len(), 2);
        assert_eq!((f.hops[0].hop.port, f.hops[1].hop.port), (2222, 22));
        assert_eq!(f.hops[0].pin.as_deref(), Some("SHA256:x"));
        assert_eq!(f.hops[1].pin, None);
        assert_eq!(f.target_sshd, ("127.0.0.1".into(), 2332));
    }

    #[test]
    fn cfg_errors_are_reported() {
        let c: EngineCfg = serde_json::from_str("{}").unwrap();
        assert!(c.forward().is_err());
        let c: EngineCfg = serde_json::from_str(r#"{"hops":["a@b"],"identities":[]}"#).unwrap();
        assert!(c.forward().unwrap_err().contains("Keys"));
        assert!(Engine::new("kein json").is_err());
    }
}
