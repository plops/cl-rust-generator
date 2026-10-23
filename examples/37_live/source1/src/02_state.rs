//! 02 — Raumbuch: welche Peers sind in welchem Raum?
//!
//! Datenfluss: `03_relay` meldet Ein-/Austritt, `04_serve` liest fuer
//! `/api/rooms`. Leere Raeume werden sofort geloescht.

use std::collections::HashMap;
use tokio::sync::{Mutex, mpsc};

/// Kanalpuffer pro Peer (Frames, nicht Bytes).
pub const PEER_BUFFER: usize = 64;

/// Ein Peer-Eintrag: Kanal zum Senden an genau diesen Peer.
pub type PeerTx = mpsc::Sender<Vec<u8>>;

/// Thread-sichere Registry: Raumname → (Peer-Id → Sendekanal).
#[derive(Debug, Default)]
pub struct Rooms {
    inner: Mutex<HashMap<String, HashMap<u64, PeerTx>>>,
    max_peers: usize,
}

impl Rooms {
    pub fn new(max_peers: usize) -> Self {
        Self {
            inner: Mutex::new(HashMap::new()),
            max_peers,
        }
    }

    /// Peer eintragen. Gibt `false` zurueck, wenn der Raum voll ist.
    pub async fn join(&self, room: &str, id: u64, tx: PeerTx) -> bool {
        let mut g = self.inner.lock().await;
        let peers = g.entry(room.to_owned()).or_default();
        if peers.len() >= self.max_peers {
            return false;
        }
        peers.insert(id, tx);
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

    /// Momentaufnahme Raum → Peerzahl (fuer `/api/rooms`).
    pub async fn snapshot(&self) -> HashMap<String, usize> {
        let g = self.inner.lock().await;
        g.iter().map(|(r, p)| (r.clone(), p.len())).collect()
    }

    /// Binaer-Frame an alle Peers im Raum ausser `from` zustellen.
    /// Langsame Verbraucher werden uebersprungen (`try_send`).
    /// Rueckgabe: Anzahl belieferter Peers.
    pub async fn broadcast(&self, room: &str, from: u64, bytes: Vec<u8>) -> usize {
        let g = self.inner.lock().await;
        let Some(peers) = g.get(room) else {
            return 0;
        };
        let mut n = 0;
        for (id, tx) in peers.iter() {
            if *id == from {
                continue;
            }
            if tx.try_send(bytes.clone()).is_ok() {
                n += 1;
            }
        }
        n
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chan() -> PeerTx {
        mpsc::channel::<Vec<u8>>(PEER_BUFFER).0
    }

    #[tokio::test]
    async fn join_leave_loescht_leeren_raum() {
        let r = Rooms::new(8);
        assert!(r.join("a", 1, chan()).await);
        assert_eq!(r.count("a").await, 1);
        r.leave("a", 1).await;
        assert_eq!(r.count("a").await, 0);
        assert!(r.snapshot().await.is_empty());
    }

    #[tokio::test]
    async fn raumlimit_wird_eingehalten() {
        let r = Rooms::new(1);
        assert!(r.join("a", 1, chan()).await);
        assert!(!r.join("a", 2, chan()).await);
        assert_eq!(r.count("a").await, 1);
    }

    #[tokio::test]
    async fn broadcast_schliesst_sender_aus() {
        let r = Rooms::new(8);
        let (tx1, mut rx1) = mpsc::channel::<Vec<u8>>(PEER_BUFFER);
        let (tx2, mut rx2) = mpsc::channel::<Vec<u8>>(PEER_BUFFER);
        r.join("a", 1, tx1).await;
        r.join("a", 2, tx2).await;
        assert_eq!(r.broadcast("a", 1, vec![7, 8]).await, 1);
        assert!(rx1.try_recv().is_err());
        assert_eq!(rx2.try_recv().unwrap(), vec![7, 8]);
    }
}
