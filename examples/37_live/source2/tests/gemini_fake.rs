//! Integrationstest: Gemini-Client gegen den wiederverwendbaren Fake.
//!
//! connect → setup → setupComplete (Ready) → Audio schicken →
//! modelTurn-Audio (4800 Bytes) + outputTranscription "hallo".

mod common;

use live_silk::gemini::{Event, connect};
use std::time::Duration;
use tokio::sync::mpsc;

/// Naechstes Event mit Zeitlimit holen (Panik bei Timeout).
async fn naechstes(rx: &mut mpsc::Receiver<Event>) -> Event {
    tokio::time::timeout(Duration::from_secs(5), rx.recv())
        .await
        .expect("kein event rechtzeitig")
        .expect("event-kanal lebt")
}

#[tokio::test]
async fn fake_liefert_ready_audio_und_outtext() {
    let url = common::spawn_fake_gemini().await;
    let (etx, mut erx) = mpsc::channel::<Event>(32);

    // Translate-Setup (AUTO) fuer de→zh-Hans.
    let setup = live_silk::gemini::translate_setup_json("zh-Hans", false);
    let session = connect(&url, "testkey", &setup, false, etx)
        .await
        .expect("connect gegen fake");

    // Erst Ready (setupComplete) erwarten.
    assert_eq!(naechstes(&mut erx).await, Event::Ready);

    // Ein PCM16-Chunk (320 Bytes = 10 ms @ 16 kHz) hochschicken.
    session.tx.send(vec![0u8; 320]).await.unwrap();

    // Danach Audio (4800 Bytes PCM24) und OutText "hallo:zh-Hans" erwarten
    // (der Fake spiegelt das Ziel, damit das Auto-Ziel pruefbar ist).
    let mut audio_ok = false;
    let mut out_ok = false;
    for _ in 0..4 {
        match naechstes(&mut erx).await {
            Event::Audio(b) => {
                assert_eq!(b.len(), 4800, "100 ms PCM24 = 4800 bytes");
                audio_ok = true;
            }
            Event::OutText(t) => {
                assert_eq!(t, "hallo:zh-Hans");
                out_ok = true;
            }
            _ => {}
        }
        if audio_ok && out_ok {
            break;
        }
    }
    assert!(audio_ok && out_ok, "audio und outtext erhalten");
}
