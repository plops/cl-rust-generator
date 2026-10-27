//! Gemeinsamer Fake-Gemini (von `tests/common/mod.rs` und vom Beispiel
//! `examples/fake_gemini.rs` genutzt — ein einziger Protokoll-Ort).
//!
//! Verhalten: erwartet die erste `setup`-Nachricht, antwortet mit
//! `{"setupComplete":{}}`. Auf `realtimeInput` schickt der Fake einen
//! `modelTurn` mit 100 ms Sinus-PCM24 (4800 Bytes) plus
//! `outputTranscription` "hallo" — aber hoechstens so viel Audio-Zeit, wie
//! an Eingangs-Audio angekommen ist (Echtzeit-Drossel). So bleibt der
//! Opus-Downlink im Sparmodus realistisch (~12 statt ~28 kbit/s). Der erste
//! Chunk loest immer eine Antwort aus, damit Einzel-Chunk-Tests greifen.

use base64::{Engine, engine::general_purpose::STANDARD};
use futures_util::{SinkExt, StreamExt};
use tokio::net::TcpListener;
use tokio_tungstenite::{accept_async, tungstenite::Message};

/// Audio-Dauer je Antwort in Millisekunden (100 ms = 4800 Bytes PCM24).
const RESPONSE_MS: i64 = 100;

/// 100 ms Sinus als PCM16LE 24 kHz mono = 2400 Samples = 4800 Bytes.
pub fn sinus_pcm24() -> Vec<u8> {
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

/// Eingangs-Audio-Dauer (ms) aus einer `realtimeInput`-Nachricht schaetzen:
/// Base64-PCM16-16-kHz → Bytes/2 Samples → *1000/16000 ms.
fn input_ms(v: &serde_json::Value) -> i64 {
    let Some(data) = v
        .get("realtimeInput")
        .and_then(|r| r.get("audio"))
        .and_then(|a| a.get("data"))
        .and_then(|d| d.as_str())
    else {
        return 0;
    };
    let bytes = STANDARD.decode(data).map(|b| b.len()).unwrap_or(0);
    (bytes as i64 / 2) * 1000 / 16_000
}

/// Eine Antwortnachricht: modelTurn (Audio) + outputTranscription "hallo".
pub fn antwort_json() -> String {
    let data = STANDARD.encode(sinus_pcm24());
    serde_json::json!({
        "serverContent": {
            "modelTurn": { "parts": [{ "inlineData": { "data": data } }] },
            "outputTranscription": { "text": "hallo" }
        }
    })
    .to_string()
}

/// Eine angenommene WebSocket-Verbindung bedienen (bis sie schliesst).
pub async fn handle_conn(stream: tokio::net::TcpStream) {
    let Ok(ws) = accept_async(stream).await else {
        return;
    };
    let (mut sink, mut rx) = ws.split();
    // Konto: empfangene minus bereits beantwortete Audio-Zeit (ms). Startet
    // bei RESPONSE_MS, damit der erste Chunk sofort eine Antwort ausloest.
    let mut guthaben_ms: i64 = RESPONSE_MS;
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
            guthaben_ms += input_ms(&v);
            // Nur antworten, wenn genug Eingangs-Audio da ist (Echtzeit-Drossel).
            if guthaben_ms >= RESPONSE_MS {
                guthaben_ms -= RESPONSE_MS;
                let _ = sink.send(Message::Text(antwort_json().into())).await;
            }
        }
    }
}

/// Endlos auf dem Listener Verbindungen annehmen und bedienen.
pub async fn serve(listener: TcpListener) {
    while let Ok((stream, _)) = listener.accept().await {
        tokio::spawn(handle_conn(stream));
    }
}
