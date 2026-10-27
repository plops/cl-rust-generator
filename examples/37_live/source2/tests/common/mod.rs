//! Wiederverwendbarer Mini-Fake-Gemini fuer Integrationstests.
//!
//! Verhalten: erwartet die erste `setup`-Nachricht, antwortet mit
//! `{"setupComplete":{}}` und schickt auf jedes `realtimeInput` einen
//! `modelTurn` mit 100 ms Sinus-PCM24 (4800 Bytes) plus
//! `outputTranscription` "hallo". Auch von `tests/ws_relay.rs` nutzbar.

use base64::{Engine, engine::general_purpose::STANDARD};
use futures_util::{SinkExt, StreamExt};
use tokio::net::TcpListener;
use tokio_tungstenite::{accept_async, tungstenite::Message};

/// 100 ms Sinus als PCM16LE 24 kHz mono = 2400 Samples = 4800 Bytes.
fn sinus_pcm24() -> Vec<u8> {
    let n = 2400usize;
    let mut out = Vec::with_capacity(n * 2);
    for i in 0..n {
        let t = i as f32 / 24_000.0;
        let s = (2.0 * std::f32::consts::PI * 440.0 * t).sin();
        let v = (s * 8000.0) as i16;
        out.extend_from_slice(&v.to_le_bytes());
    }
    out
}

/// Eine Antwortnachricht: modelTurn (Audio) + outputTranscription "hallo".
fn antwort_json() -> String {
    let data = STANDARD.encode(sinus_pcm24());
    serde_json::json!({
        "serverContent": {
            "modelTurn": { "parts": [{ "inlineData": { "data": data } }] },
            "outputTranscription": { "text": "hallo" }
        }
    })
    .to_string()
}

/// Startet den Fake auf `127.0.0.1:0` und liefert die `ws://…`-Basis-URL.
pub async fn spawn_fake_gemini() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        while let Ok((stream, _)) = listener.accept().await {
            tokio::spawn(async move {
                let Ok(ws) = accept_async(stream).await else {
                    return;
                };
                let (mut sink, mut rx) = ws.split();
                while let Some(Ok(msg)) = rx.next().await {
                    let txt = match msg {
                        Message::Text(t) => t.to_string(),
                        Message::Binary(b) => match std::str::from_utf8(&b) {
                            Ok(t) => t.to_owned(),
                            Err(_) => continue,
                        },
                        Message::Close(_) => break,
                        _ => continue,
                    };
                    let Ok(v) = serde_json::from_str::<serde_json::Value>(&txt) else {
                        continue;
                    };
                    if v.get("setup").is_some() {
                        let _ = sink
                            .send(Message::Text(r#"{"setupComplete":{}}"#.into()))
                            .await;
                    } else if v.get("realtimeInput").is_some() {
                        let _ = sink.send(Message::Text(antwort_json().into())).await;
                    }
                }
            });
        }
    });
    format!("ws://{addr}")
}
