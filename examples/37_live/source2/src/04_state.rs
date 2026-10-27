//! 04 — Raumbuch: welche Peers sind in welchem Raum?
//!
//! Datenfluss: `06_relay` meldet Ein-/Austritt, `07_serve` liest fuer
//! `/api/rooms`. Leere Raeume werden sofort geloescht.
//!
//! Neu (Nutzer-Feedback): Jeder Peer waehlt nur die EIGENE Sprache. Die
//! Zielsprache seiner Uebersetzung ist automatisch die Sprache des Partners.
//! Dafuer haelt jeder Peer einen `watch::Sender<Option<PeerInfo>>` mit der
//! aktuellen Partner-Info; `join`/`leave` aktualisieren die Watches aller
//! Peers des Raums (max. 2 Peers, also hoechstens ein Partner).
//!
//! Intern transportiert der Raum immer PCM 24 kHz (`Out::Pcm24`) oder
//! Untertitel-Text (`Out::Text`). Ob ein Peer das Audio roh oder
//! Opus-kodiert bekommt, entscheidet spaeter der Peer-Task selbst.

use crate::config::Transport;
use std::collections::HashMap;
use tokio::sync::{Mutex, mpsc, watch};

/// Kanalpuffer pro Peer (Nachrichten, nicht Bytes).
pub const PEER_BUFFER: usize = 256;

/// Oeffentliche Beschreibung eines Peers (fuer `/api/rooms`, Partner-Info).
///
/// Es gibt KEIN `target` mehr: die Zielsprache ergibt sich aus der Sprache
/// des Partners (siehe Modul-Doku).
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct PeerInfo {
    /// Sprache, die der Peer spricht (z. B. `de`).
    pub lang: String,
    /// Transportart dieses Peers (PCM oder Opus-Sparmodus).
    pub transport: Transport,
}

/// Was an einen Peer geht. Audio ist intern IMMER PCM 24 kHz;
/// die Opus-Kodierung macht spaeter der Peer-Task selbst.
#[derive(Debug, Clone)]
pub enum Out {
    /// Audio als PCM 24 kHz, 16 Bit mono, Little-Endian.
    Pcm24(Vec<u8>),
    /// Text-JSON (z. B. `partner`-Event oder Untertitel).
    Text(String),
}

/// Sendekanal zu genau einem Peer.
pub type PeerTx = mpsc::Sender<Out>;

/// Empfaenger fuer die Partner-Info dieses Peers (`None` = kein Partner).
pub type PartnerRx = watch::Receiver<Option<PeerInfo>>;

/// Ein Peer-Eintrag: Sendekanal, oeffentliche Info und Partner-Watch.
#[derive(Debug)]
struct Peer {
    tx: PeerTx,
    info: PeerInfo,
    /// Sender fuer die Partner-Info DIESES Peers. `leave`/`join` setzen ihn.
    partner_tx: watch::Sender<Option<PeerInfo>>,
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

    /// Peer eintragen. Gibt `Some(receiver)` mit der Partner-Watch zurueck,
    /// oder `None`, wenn der Raum voll ist. Beim Beitritt werden die Watches
    /// aller Peers des Raums auf den (nun) aktuellen Partner aktualisiert.
    pub async fn join(&self, room: &str, id: u64, tx: PeerTx, info: PeerInfo) -> Option<PartnerRx> {
        let mut g = self.inner.lock().await;
        let peers = g.entry(room.to_owned()).or_default();
        if peers.len() >= self.max_peers {
            return None;
        }
        let (partner_tx, partner_rx) = watch::channel(None);
        peers.insert(
            id,
            Peer {
                tx,
                info,
                partner_tx,
            },
        );
        refresh_partners(peers);
        Some(partner_rx)
    }

    /// Peer austragen; leere Raeume verschwinden. Die Partner-Watches der
    /// verbliebenen Peers werden aktualisiert (ihr Partner ist jetzt `None`).
    pub async fn leave(&self, room: &str, id: u64) {
        let mut g = self.inner.lock().await;
        if let Some(peers) = g.get_mut(room) {
            peers.remove(&id);
            if peers.is_empty() {
                g.remove(room);
            } else {
                refresh_partners(peers);
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

    /// Nachricht an genau einen Peer (`to`) zustellen. `true`, wenn der Peer
    /// existiert und die Nachricht angenommen wurde. Fuer direkte Antworten
    /// (`ready`, `waiting`, eigener Untertitel, Fehler) an einen Peer.
    pub async fn send_to(&self, room: &str, to: u64, out: Out) -> bool {
        let g = self.inner.lock().await;
        g.get(room)
            .and_then(|peers| peers.get(&to))
            .map(|peer| peer.tx.try_send(out).is_ok())
            .unwrap_or(false)
    }

    /// Peer-Infos eines Raums (fuer Tests/Diagnose).
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

/// Jedem Peer des Raums seinen aktuellen Partner in die Watch schreiben.
/// Bei max. 2 Peers ist der Partner der jeweils ANDERE Peer (oder `None`).
fn refresh_partners(peers: &HashMap<u64, Peer>) {
    for (id, peer) in peers.iter() {
        let partner = peers
            .iter()
            .find(|(other_id, _)| *other_id != id)
            .map(|(_, other)| other.info.clone());
        // Nur senden, wenn sich etwas geaendert hat (watch dedupliziert via
        // send_if_modified; hier reicht send, der Receiver sieht den Wert).
        let _ = peer.partner_tx.send(partner);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn info(lang: &str, transport: Transport) -> PeerInfo {
        PeerInfo {
            lang: lang.to_owned(),
            transport,
        }
    }

    fn chan() -> PeerTx {
        mpsc::channel::<Out>(PEER_BUFFER).0
    }

    #[tokio::test]
    async fn dritter_peer_bei_max_zwei_abgewiesen() {
        let r = Rooms::new(2);
        assert!(
            r.join("a", 1, chan(), info("de", Transport::Pcm))
                .await
                .is_some()
        );
        assert!(
            r.join("a", 2, chan(), info("zh-Hans", Transport::Opus))
                .await
                .is_some()
        );
        assert!(
            r.join("a", 3, chan(), info("en", Transport::Pcm))
                .await
                .is_none()
        );
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
    async fn snapshot_mit_transport() {
        let r = Rooms::new(2);
        r.join("a", 1, chan(), info("de", Transport::Pcm)).await;
        r.join("a", 2, chan(), info("zh-Hans", Transport::Opus))
            .await;

        // Snapshot enthaelt die Transportart je Peer.
        let snap = r.snapshot().await;
        let mut transports: Vec<Transport> =
            snap.get("a").unwrap().iter().map(|p| p.transport).collect();
        transports.sort_by_key(|t| matches!(t, Transport::Opus));
        assert_eq!(transports, vec![Transport::Pcm, Transport::Opus]);
    }

    #[tokio::test]
    async fn partner_watch_bei_join_und_leave() {
        let r = Rooms::new(2);
        // Erster Peer: Partner-Watch startet ohne Partner.
        let mut rx1 = r
            .join("a", 1, chan(), info("de", Transport::Pcm))
            .await
            .unwrap();
        assert_eq!(*rx1.borrow_and_update(), None);

        // Zweiter Peer tritt bei: beide sehen jetzt den jeweils anderen.
        let mut rx2 = r
            .join("a", 2, chan(), info("zh-Hans", Transport::Opus))
            .await
            .unwrap();
        assert!(rx1.has_changed().unwrap());
        assert_eq!(
            *rx1.borrow_and_update(),
            Some(info("zh-Hans", Transport::Opus))
        );
        assert_eq!(*rx2.borrow_and_update(), Some(info("de", Transport::Pcm)));

        // Peer 2 geht: Peer 1 sieht wieder keinen Partner.
        r.leave("a", 2).await;
        assert!(rx1.has_changed().unwrap());
        assert_eq!(*rx1.borrow_and_update(), None);
    }
}
