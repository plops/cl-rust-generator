//! Integrationstests fuer Relay und APIs (Server auf 127.0.0.1:0).
//!
//! Deckt ab:
//! (a) PCM<->PCM: Partner bekommt Binary, kein Echo beim Sender.
//! (b) 3. Client im vollen Raum bekommt `raum voll`.
//! (c) `peers`-Event mit zwei Eintraegen nach dem zweiten Beitritt.
//! (d) Opus-Client (hello ohne Key, Server-Key "test") + PCM-Partner:
//!     A bekommt `ready`, B bekommt PCM24-Binary + Untertitel who=partner.
//! (e) PCM-Partner schickt 1 s PCM24-Sinus → Opus-Client bekommt dekodierbare
//!     SILK-Pakete, Summe Bytes < 1/10 der PCM-Bytes.
//! (f) `/api/languages` listet 78 Sprachen + Defaults; `/api/rooms` zeigt
//!     Transport.
//!
//! HTTP ohne reqwest: winziges GET ueber tokio `TcpStream`.

mod common;

use std::sync::Arc;
use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use live_silk::config::MAX_PEERS;
use live_silk::relay::AppState;
use live_silk::serve::router;
use live_silk::state::Rooms;
use opus_pure::{Application, Bandwidth, OpusDecoder, OpusEncoder, OpusMode, Signal, packet};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async};

type Ws = WebSocketStream<MaybeTlsStream<TcpStream>>;

/// Server mit Fake-Gemini und optionalem Server-Key starten; `(ws-base, http-addr)`.
async fn spawn_server(server_key: Option<&str>) -> (String, String) {
    let gemini_url = common::spawn_fake_gemini().await;
    let rooms = Arc::new(Rooms::new(MAX_PEERS));
    let state = AppState {
        rooms,
        gemini_url,
        server_key: server_key.map(str::to_owned),
    };
    let app = router(state, "public");
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    (format!("ws://{addr}"), addr.to_string())
}

/// WS-Verbindung mit Query oeffnen.
async fn connect(base: &str, query: &str) -> Ws {
    let (ws, _) = connect_async(format!("{base}/ws?{query}")).await.unwrap();
    ws
}

/// Naechste Nachricht mit Zeitlimit (Panik bei Timeout).
async fn naechste(ws: &mut Ws, ms: u64) -> Message {
    tokio::time::timeout(Duration::from_millis(ms), ws.next())
        .await
        .expect("nachricht rechtzeitig")
        .expect("stream lebt")
        .unwrap()
}

/// Naechste TEXT-Nachricht (Binary wird uebersprungen).
async fn naechster_text(ws: &mut Ws, ms: u64) -> String {
    loop {
        match naechste(ws, ms).await {
            Message::Text(t) => return t.to_string(),
            Message::Binary(_) => continue,
            other => panic!("unerwartet: {other:?}"),
        }
    }
}

/// Auf eine Binary-Nachricht warten (Text wird uebersprungen).
async fn naechste_binary(ws: &mut Ws, ms: u64) -> Vec<u8> {
    loop {
        match naechste(ws, ms).await {
            Message::Binary(b) => return b.to_vec(),
            Message::Text(_) => continue,
            other => panic!("unerwartet: {other:?}"),
        }
    }
}

#[tokio::test]
async fn pcm_partner_bekommt_binary_sender_kein_echo() {
    let (base, _) = spawn_server(None).await;
    let mut a = connect(&base, "room=r1&transport=pcm").await;
    let mut b = connect(&base, "room=r1&transport=pcm").await;
    tokio::time::sleep(Duration::from_millis(200)).await;

    a.send(Message::Binary(vec![1u8, 2, 3].into()))
        .await
        .unwrap();

    assert_eq!(naechste_binary(&mut b, 1000).await, vec![1u8, 2, 3]);

    // Kein Echo an den Sender (nur peers-Events als Text erlaubt).
    let echo = tokio::time::timeout(Duration::from_millis(300), async {
        loop {
            match a.next().await {
                Some(Ok(Message::Binary(b))) => return Some(b),
                Some(Ok(_)) => continue,
                _ => return None,
            }
        }
    })
    .await;
    assert!(
        matches!(echo, Err(_) | Ok(None)),
        "sender darf kein binary-echo bekommen"
    );
}

#[tokio::test]
async fn dritter_client_bekommt_raum_voll() {
    let (base, _) = spawn_server(None).await;
    let _a = connect(&base, "room=voll&transport=pcm").await;
    let _b = connect(&base, "room=voll&transport=pcm").await;
    tokio::time::sleep(Duration::from_millis(150)).await;

    let mut c = connect(&base, "room=voll&transport=pcm").await;
    let t = naechster_text(&mut c, 1000).await;
    let v: serde_json::Value = serde_json::from_str(&t).unwrap();
    assert_eq!(v["type"], "error");
    assert_eq!(v["msg"], "raum voll");
}

#[tokio::test]
async fn peers_event_mit_zwei_eintraegen() {
    let (base, _) = spawn_server(None).await;
    let mut a = connect(&base, "room=p&transport=pcm&lang=de&target=zh-Hans").await;
    let _b = connect(&base, "room=p&transport=pcm&lang=zh-Hans&target=de").await;

    // A bekommt zwei peers-Events (erst 1, nach B-Beitritt 2); wir warten auf
    // das Event mit zwei Eintraegen.
    let mut saw_two = false;
    for _ in 0..4 {
        let t = naechster_text(&mut a, 1000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "peers" && v["peers"].as_array().map(|a| a.len()) == Some(2) {
            saw_two = true;
            break;
        }
    }
    assert!(saw_two, "peers-event mit zwei eintraegen erwartet");
}

#[tokio::test]
async fn opus_client_mit_server_key_und_pcm_partner() {
    let (base, _) = spawn_server(Some("test")).await;
    // B zuerst (PCM-Partner), damit er fuer die Uebersetzung bereitsteht.
    let mut b = connect(&base, "room=o1&transport=pcm&lang=de&target=zh-Hans").await;
    let mut a = connect(&base, "room=o1&transport=opus&lang=zh-Hans&target=de").await;

    // A: hello OHNE key → Server nutzt server_key "test".
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();

    // A bekommt `ready`.
    let mut got_ready = false;
    for _ in 0..5 {
        let t = naechster_text(&mut a, 2000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "ready" {
            got_ready = true;
            break;
        }
    }
    assert!(got_ready, "opus-client bekommt ready");

    // A schickt ein Opus-Paket (echter Encoder, 16 kHz, 60 ms) hoch.
    let pkt = test_opus_paket();
    a.send(Message::Binary(pkt.into())).await.unwrap();

    // B bekommt PCM24-Binary (Fake-Gemini: 4800 Bytes) vom Partner.
    let bin = naechste_binary(&mut b, 2000).await;
    assert_eq!(bin.len(), 4800, "100 ms PCM24 vom fake-gemini");

    // B bekommt Untertitel who=partner (Fake liefert outputTranscription).
    let mut saw_partner_text = false;
    for _ in 0..6 {
        let t = naechster_text(&mut b, 2000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "text" && v["who"] == "partner" {
            assert_eq!(v["text"], "hallo");
            saw_partner_text = true;
            break;
        }
    }
    assert!(saw_partner_text, "partner-untertitel erwartet");
}

#[tokio::test]
async fn pcm_zu_opus_liefert_silk_pakete() {
    let (base, _) = spawn_server(Some("test")).await;
    let mut a = connect(&base, "room=o2&transport=opus&lang=zh-Hans&target=de").await;
    let mut b = connect(&base, "room=o2&transport=pcm&lang=de&target=zh-Hans").await;

    // A anmelden (hello mit server_key).
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();
    // Auf ready warten, damit beide registriert sind.
    for _ in 0..5 {
        let t = naechster_text(&mut a, 2000).await;
        if serde_json::from_str::<serde_json::Value>(&t).unwrap()["type"] == "ready" {
            break;
        }
    }
    tokio::time::sleep(Duration::from_millis(100)).await;

    // B schickt 1 s PCM24-Sinus (48000 Bytes) an A.
    let pcm = sinus_pcm24_le(1.0);
    assert_eq!(pcm.len(), 48_000);
    b.send(Message::Binary(pcm.clone().into())).await.unwrap();

    // A bekommt mehrere Opus-Binary-Pakete; alle SilkOnly, dekodierbar.
    let mut dec = OpusDecoder::new(24_000, 1).unwrap();
    let mut gesamt = 0usize;
    let mut pakete = 0usize;
    // 1 s / 60 ms ≈ 16 Pakete; mit Puffer bis 16 lesen.
    for _ in 0..16 {
        let pkt = match tokio::time::timeout(Duration::from_millis(1500), async {
            loop {
                match a.next().await {
                    Some(Ok(Message::Binary(b))) => return Some(b.to_vec()),
                    Some(Ok(_)) => continue,
                    _ => return None,
                }
            }
        })
        .await
        {
            Ok(Some(p)) => p,
            _ => break,
        };
        assert_eq!(packet::mode(&pkt).unwrap(), OpusMode::SilkOnly, "nur SILK");
        let mut out = vec![0f32; 5760];
        assert!(dec.decode(&pkt, 5760, &mut out).is_ok(), "dekodierbar");
        gesamt += pkt.len();
        pakete += 1;
    }
    assert!(pakete >= 10, "mindestens 10 pakete, waren {pakete}");
    assert!(
        gesamt < 48_000 / 10,
        "opus-bytes {gesamt} >= 1/10 von 48000"
    );
}

#[tokio::test]
async fn api_languages_und_rooms() {
    let (_, addr) = spawn_server(None).await;

    let langs = http_get(&addr, "/api/languages").await;
    let v: serde_json::Value = serde_json::from_str(&langs).unwrap();
    assert_eq!(v["default_lang"], "de");
    assert_eq!(v["default_target"], "zh-Hans");
    assert_eq!(v["languages"].as_array().unwrap().len(), 78, "78 sprachen");

    // Leeres /api/rooms.
    let empty = http_get(&addr, "/api/rooms").await;
    assert_eq!(empty.trim(), "{}");

    // Ein Opus- und ein PCM-Peer → Transport sichtbar.
    let base = format!("ws://{addr}");
    let _a = connect(&base, "room=rA&transport=opus&lang=de&target=zh-Hans").await;
    let _b = connect(&base, "room=rA&transport=pcm&lang=zh-Hans&target=de").await;
    tokio::time::sleep(Duration::from_millis(200)).await;

    let rooms = http_get(&addr, "/api/rooms").await;
    assert!(rooms.contains("\"opus\""), "transport opus im json");
    assert!(rooms.contains("\"pcm\""), "transport pcm im json");
    assert!(rooms.contains("rA"), "raumname im json");
}

/// Ein 60-ms-Opus-Paket (16 kHz, SILK) wie vom Browser-Encoder.
fn test_opus_paket() -> Vec<u8> {
    let mut enc = OpusEncoder::new(16_000, 1, Application::Voip).unwrap();
    enc.bitrate_bps = 12_000;
    enc.max_bandwidth = Bandwidth::Wideband;
    enc.signal_type = Some(Signal::Voice);
    // 960 Samples = 60 ms @ 16 kHz.
    let mut pcm = [0i16; 960];
    for (i, s) in pcm.iter_mut().enumerate() {
        let t = i as f32 / 16_000.0;
        *s = ((2.0 * std::f32::consts::PI * 300.0 * t).sin() * 8000.0) as i16;
    }
    let mut out = vec![0u8; 1500];
    let n = enc.encode_s16(&pcm, 960, &mut out).unwrap();
    out.truncate(n);
    out
}

/// `secs` Sekunden 440-Hz-Sinus als PCM16LE 24 kHz mono.
fn sinus_pcm24_le(secs: f32) -> Vec<u8> {
    let n = (24_000.0 * secs) as usize;
    let mut out = Vec::with_capacity(n * 2);
    for i in 0..n {
        let t = i as f32 / 24_000.0;
        let v = ((2.0 * std::f32::consts::PI * 440.0 * t).sin() * 8000.0) as i16;
        out.extend_from_slice(&v.to_le_bytes());
    }
    out
}

/// Winziges HTTP/1.0-GET ueber tokio `TcpStream` (kein reqwest).
async fn http_get(addr: &str, path: &str) -> String {
    let mut stream = TcpStream::connect(addr).await.unwrap();
    let req = format!("GET {path} HTTP/1.0\r\nHost: {addr}\r\nConnection: close\r\n\r\n");
    stream.write_all(req.as_bytes()).await.unwrap();
    let mut raw = Vec::new();
    stream.read_to_end(&mut raw).await.unwrap();
    let text = String::from_utf8_lossy(&raw);
    text.rsplit("\r\n\r\n").next().unwrap_or("").to_owned()
}
