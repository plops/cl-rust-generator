//! 04 — Raumbuch: welche Peers sind in welchem Raum?
//!
//! Datenfluss: `06_relay` meldet Ein-/Austritt, `07_serve` liest fuer
//! `/api/rooms`. Leere Raeume werden sofort geloescht.
//!
//! Intern transportiert der Raum immer PCM 24 kHz (`Out::Pcm24`) oder
//! Untertitel-Text (`Out::Text`). Ob ein Peer das Audio roh oder
//! Opus-kodiert bekommt, entscheidet spaeter der Peer-Task selbst.

use crate::config::Transport;
use std::collections::HashMap;
use tokio::sync::{Mutex, mpsc};

/// Kanalpuffer pro Peer (Nachrichten, nicht Bytes).
pub const PEER_BUFFER: usize = 256;

/// Oeffentliche Beschreibung eines Peers (fuer `peers`-Events, `/api/rooms`).
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct PeerInfo {
    /// Sprache, die der Peer spricht (z. B. `de`).
    pub lang: String,
    /// Zielsprache, die der Peer hoeren will (z. B. `zh-Hans`).
    pub target: String,
    /// Transportart dieses Peers (PCM oder Opus-Sparmodus).
    pub transport: Transport,
}

/// Was an einen Peer geht. Audio ist intern IMMER PCM 24 kHz;
/// die Opus-Kodierung macht spaeter der Peer-Task selbst.
#[derive(Debug, Clone)]
pub enum Out {
    /// Audio als PCM 24 kHz, 16 Bit mono, Little-Endian.
    Pcm24(Vec<u8>),
    /// Text-JSON (z. B. `peers`-Event oder Untertitel).
    Text(String),
}

/// Sendekanal zu genau einem Peer.
pub type PeerTx = mpsc::Sender<Out>;

/// Ein Peer-Eintrag: Sendekanal plus oeffentliche Info.
#[derive(Debug, Clone)]
struct Peer {
    tx: PeerTx,
    info: PeerInfo,
}

/// Thread-sichere Registry: Raumname → (Peer-Id → Peer).
#[derive(Debug, Default)]
pub struct Rooms {
    inner: Mutex<HashMap<String, HashMap<u64, Peer>>>,
    max_peers: usize,
}

impl Rooms {
    /// Neues Raumbuch mit Peer-Obergrenze je Raum.
    pub fn new(max_peers: usize) -> Self {
        Self {
            inner: Mutex::new(HashMap::new()),
            max_peers,
        }
    }

    /// Peer eintragen. Gibt `false` zurueck, wenn der Raum voll ist.
    pub async fn join(&self, room: &str, id: u64, tx: PeerTx, info: PeerInfo) -> bool {
        let mut g = self.inner.lock().await;
        let peers = g.entry(room.to_owned()).or_default();
        if peers.len() >= self.max_peers {
            return false;
        }
        peers.insert(id, Peer { tx, info });
        true
    }

    /// Peer austragen; leere Raeume verschwinden.
    pub async fn leave(&self, room: &str, id: u64) {
        let mut g = self.inner.lock().await;
        if let Some(peers) = g.get_mut(room) {
            peers.remove(&id);
            if peers.is_empty() {
                g.remove(room);
            }
        }
    }

    /// Anzahl Peers in einem Raum.
    pub async fn count(&self, room: &str) -> usize {
        let g = self.inner.lock().await;
        g.get(room).map_or(0, HashMap::len)
    }

    /// Nachricht an alle Peers im Raum ausser `from` zustellen.
    /// Langsame Verbraucher werden uebersprungen (`try_send`).
    /// Rueckgabe: Anzahl belieferter Peers.
    pub async fn broadcast(&self, room: &str, from: u64, out: Out) -> usize {
        let g = self.inner.lock().await;
        let Some(peers) = g.get(room) else {
            return 0;
        };
        let mut n = 0;
        for (id, peer) in peers.iter() {
            if *id == from {
                continue;
            }
            if peer.tx.try_send(out.clone()).is_ok() {
                n += 1;
            }
        }
        n
    }

    /// Nachricht an alle Peers im Raum inkl. Sender (fuer `peers`-Events).
    pub async fn send_all(&self, room: &str, out: Out) {
        let g = self.inner.lock().await;
        let Some(peers) = g.get(room) else {
            return;
        };
        for peer in peers.values() {
            let _ = peer.tx.try_send(out.clone());
        }
    }

    /// Nachricht an genau einen Peer (`to`) zustellen. `true`, wenn der Peer
    /// existiert und die Nachricht angenommen wurde. Fuer direkte Antworten
    /// (`ready`, eigener Untertitel, Fehler) an einen einzelnen Peer.
    pub async fn send_to(&self, room: &str, to: u64, out: Out) -> bool {
        let g = self.inner.lock().await;
        g.get(room)
            .and_then(|peers| peers.get(&to))
            .map(|peer| peer.tx.try_send(out).is_ok())
            .unwrap_or(false)
    }

    /// Peer-Infos eines Raums (fuer `peers`-Events).
    pub async fn infos(&self, room: &str) -> Vec<PeerInfo> {
        let g = self.inner.lock().await;
        g.get(room)
            .map(|peers| peers.values().map(|p| p.info.clone()).collect())
            .unwrap_or_default()
    }

    /// Momentaufnahme Raum → Peer-Infos (fuer `/api/rooms`).
    pub async fn snapshot(&self) -> HashMap<String, Vec<PeerInfo>> {
        let g = self.inner.lock().await;
        g.iter()
            .map(|(r, peers)| (r.clone(), peers.values().map(|p| p.info.clone()).collect()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn info(lang: &str, transport: Transport) -> PeerInfo {
        PeerInfo {
            lang: lang.to_owned(),
            target: "zh-Hans".to_owned(),
            transport,
        }
    }

    fn chan() -> PeerTx {
        mpsc::channel::<Out>(PEER_BUFFER).0
    }

    #[tokio::test]
    async fn dritter_peer_bei_max_zwei_abgewiesen() {
        let r = Rooms::new(2);
        assert!(r.join("a", 1, chan(), info("de", Transport::Pcm)).await);
        assert!(
            r.join("a", 2, chan(), info("zh-Hans", Transport::Opus))
                .await
        );
        assert!(!r.join("a", 3, chan(), info("en", Transport::Pcm)).await);
        assert_eq!(r.count("a").await, 2);
    }

    #[tokio::test]
    async fn broadcast_ohne_echo_leave_loescht_raum() {
        let r = Rooms::new(2);
        let (tx1, mut rx1) = mpsc::channel::<Out>(PEER_BUFFER);
        let (tx2, mut rx2) = mpsc::channel::<Out>(PEER_BUFFER);
        r.join("a", 1, tx1, info("de", Transport::Pcm)).await;
        r.join("a", 2, tx2, info("zh-Hans", Transport::Opus)).await;

        // Broadcast erreicht nur den anderen Peer, nicht den Sender.
        assert_eq!(r.broadcast("a", 1, Out::Pcm24(vec![7, 8])).await, 1);
        assert!(rx1.try_recv().is_err());
        assert!(matches!(rx2.try_recv().unwrap(), Out::Pcm24(b) if b == [7, 8]));

        // Leave des letzten Peers loescht den leeren Raum.
        r.leave("a", 1).await;
        r.leave("a", 2).await;
        assert_eq!(r.count("a").await, 0);
        assert!(r.snapshot().await.is_empty());
    }

    #[tokio::test]
    async fn send_all_erreicht_beide_snapshot_mit_transport() {
        let r = Rooms::new(2);
        let (tx1, mut rx1) = mpsc::channel::<Out>(PEER_BUFFER);
        let (tx2, mut rx2) = mpsc::channel::<Out>(PEER_BUFFER);
        r.join("a", 1, tx1, info("de", Transport::Pcm)).await;
        r.join("a", 2, tx2, info("zh-Hans", Transport::Opus)).await;

        // send_all erreicht beide inkl. Sender.
        r.send_all("a", Out::Text("{\"type\":\"peers\"}".to_owned()))
            .await;
        assert!(matches!(rx1.try_recv().unwrap(), Out::Text(t) if t.contains("peers")));
        assert!(matches!(rx2.try_recv().unwrap(), Out::Text(t) if t.contains("peers")));

        // Snapshot enthaelt die Transportart je Peer.
        let snap = r.snapshot().await;
        let mut transports: Vec<Transport> =
            snap.get("a").unwrap().iter().map(|p| p.transport).collect();
        transports.sort_by_key(|t| matches!(t, Transport::Opus));
        assert_eq!(transports, vec![Transport::Pcm, Transport::Opus]);
    }
}
