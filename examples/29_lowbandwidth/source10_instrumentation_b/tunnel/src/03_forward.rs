//! `03_forward` — lokaler Port-Forward über die SSH-Kette (wie `ssh -L`).
//!
//! Ein eigener Thread mit Tokio-Runtime lauscht auf `127.0.0.1:<port>`.
//! Jede angenommene Verbindung baut die Kette neu auf
//! (`connect_chain_retry_with_pins`) und kopiert Bytes in beide Richtungen.
//! Damit läuft der unveränderte Netz-Thread des Clients
//! (`lbw_client::net::Net`: Reconnect, AV1-Decode, Maus-Zusammenfassung)
//! über SSH, ohne selbst SSH zu kennen — auf Desktop, Android und iOS.
//! Scheitert die Kette, wird die lokale Verbindung geschlossen; der Client
//! verbindet mit Backoff neu (sichtbar als „getrennt").

use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

use tokio::sync::oneshot;

use crate::chain::{ChainStream, PinnedHop, connect_chain_with_pins};
use crate::config::TunnelPolicy;

/// Was der Forward zum Aufbau braucht.
#[derive(Clone, Debug)]
pub struct ForwardCfg {
    /// Jumps + Ziel in Reihenfolge (mind. ein Hop).
    pub hops: Vec<PinnedHop>,
    /// Ziel-sshd aus Sicht des letzten Jumps (ohne Jump ignoriert).
    pub target_sshd: (String, u16),
    /// LBW-Port am Ziel (`lbw-server --listen 127.0.0.1:<port>`).
    pub lbw_port: u16,
    /// Lokaler Port (0 = frei wählen).
    pub local_port: u16,
    pub policy: TunnelPolicy,
}

/// Laufender Forward; `Drop` beendet Thread und offene Verbindungen.
pub struct Forward {
    /// Lokale Adresse, mit der sich der Client verbindet.
    pub addr: SocketAddr,
    pins: Arc<Mutex<Vec<String>>>,
    last_error: Arc<Mutex<Option<String>>>,
    stop: Option<oneshot::Sender<()>>,
    thread: Option<JoinHandle<()>>,
}

impl Forward {
    /// Bindet den lokalen Port und startet den Thread.
    pub fn spawn(cfg: ForwardCfg) -> Result<Self, String> {
        if cfg.hops.is_empty() {
            return Err("kein Hop".into());
        }
        let l = std::net::TcpListener::bind(("127.0.0.1", cfg.local_port))
            .map_err(|e| format!("lokaler Port {}: {e}", cfg.local_port))?;
        l.set_nonblocking(true).map_err(|e| e.to_string())?;
        let addr = l.local_addr().map_err(|e| e.to_string())?;
        let pins = Arc::new(Mutex::new(Vec::new()));
        let last_error = Arc::new(Mutex::new(None));
        let (tx, rx) = oneshot::channel();
        let (p, le) = (pins.clone(), last_error.clone());
        let thread = std::thread::spawn(move || {
            let rt = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => rt,
                Err(e) => {
                    *le.lock().unwrap() = Some(format!("tokio: {e}"));
                    return;
                }
            };
            rt.block_on(async move {
                let Ok(l) = tokio::net::TcpListener::from_std(l) else {
                    return;
                };
                tokio::select! {
                    () = accept_loop(l, Arc::new(cfg), p, le) => {}
                    _ = rx => {}
                }
            });
        });
        Ok(Self {
            addr,
            pins,
            last_error,
            stop: Some(tx),
            thread: Some(thread),
        })
    }

    /// Hostkey-Fingerprints pro Hop der zuletzt aufgebauten Kette
    /// (`SHA256:…`; leer, solange die Kette nie stand) — für TOFU-Pins.
    #[must_use]
    pub fn pins(&self) -> Vec<String> {
        self.pins.lock().unwrap().clone()
    }

    /// Letzter Kettenfehler (für die Statuszeile), `None` nach Erfolg.
    #[must_use]
    pub fn last_error(&self) -> Option<String> {
        self.last_error.lock().unwrap().clone()
    }
}

impl Drop for Forward {
    fn drop(&mut self) {
        if let Some(tx) = self.stop.take() {
            let _ = tx.send(());
        }
        if let Some(t) = self.thread.take() {
            let _ = t.join();
        }
    }
}

async fn accept_loop(
    l: tokio::net::TcpListener,
    cfg: Arc<ForwardCfg>,
    pins: Arc<Mutex<Vec<String>>>,
    last_error: Arc<Mutex<Option<String>>>,
) {
    loop {
        let Ok((mut local, _)) = l.accept().await else {
            continue;
        };
        let _ = local.set_nodelay(true);
        let (cfg, pins, last_error) = (cfg.clone(), pins.clone(), last_error.clone());
        tokio::spawn(async move {
            if let Some((mut chain, fps)) = connect(&cfg, &last_error).await {
                *pins.lock().unwrap() = fps;
                *last_error.lock().unwrap() = None;
                let _ = tokio::io::copy_bidirectional(&mut local, &mut chain.stream).await;
            }
        });
    }
}

/// Kette mit den Wartezeiten der Politik; jeder Fehlversuch landet sofort
/// in `last_error` (Statuszeile der App zeigt ihn, ohne alle Versuche
/// abzuwarten — z. B. falscher Hostkey-Pin).
async fn connect(
    cfg: &ForwardCfg,
    last_error: &Mutex<Option<String>>,
) -> Option<(ChainStream, Vec<String>)> {
    let target = (cfg.target_sshd.0.as_str(), cfg.target_sshd.1);
    let waits =
        std::iter::once(std::time::Duration::ZERO).chain(cfg.policy.retry_delays.iter().copied());
    for (n, wait) in waits.enumerate() {
        tokio::time::sleep(wait).await;
        let attempt = tokio::time::timeout(
            cfg.policy.attempt_timeout,
            connect_chain_with_pins(&cfg.policy, &cfg.hops, target, cfg.lbw_port),
        );
        let e = match attempt.await {
            Ok(Ok(c)) => return Some(c),
            Ok(Err(e)) => e,
            Err(_) => format!("Timeout nach {:?}", cfg.policy.attempt_timeout),
        };
        eprintln!("[tunnel] Versuch {n}: {e}");
        *last_error.lock().unwrap() = Some(e);
    }
    None
}
