//! Integrationstests fuer Relay und APIs (Server auf 127.0.0.1:0).
//!
//! Deckt ab:
//! (a) PCM<->PCM: Partner bekommt Binary, kein Echo beim Sender.
//! (b) 3. Client im vollen Raum bekommt `raum voll`.
//! (c) `partner`-Event mit korrektem Inhalt fuer beide Seiten.
//! (d) Opus-Peer ALLEIN → `waiting`, KEIN `ready`.
//! (e) Opus + PCM-Partner (lang=de): A bekommt `ready`, Untertitel enthaelt
//!     `:de` (automatisch gewaehltes Ziel = Partner-Sprache). Partner geht →
//!     `waiting`; neuer Partner (lang=ja) → wieder `ready` mit `:ja`.
//! (f) PCM-Partner schickt PCM24 → Opus-Client bekommt dekodierbare
//!     SILK-Pakete, Summe Bytes < 1/10 der PCM-Bytes.
//! (g) `/api/languages` listet 78 Sprachen + Defaults; `/api/rooms` zeigt
//!     Transport; Raum voll bleibt bei genau 2 Peers.
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

/// Auf das naechste Text-Event eines bestimmten `type` warten (max `tries`).
async fn warte_auf_typ(ws: &mut Ws, typ: &str, tries: usize, ms: u64) -> serde_json::Value {
    for _ in 0..tries {
        let t = naechster_text(ws, ms).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == typ {
            return v;
        }
    }
    panic!("kein event vom typ {typ} erhalten");
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

    // Kein Echo an den Sender.
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
async fn dritter_client_bekommt_raum_voll_und_raum_bleibt_zwei() {
    let (base, addr) = spawn_server(None).await;
    let _a = connect(&base, "room=voll&transport=pcm").await;
    let _b = connect(&base, "room=voll&transport=pcm").await;
    tokio::time::sleep(Duration::from_millis(150)).await;

    let mut c = connect(&base, "room=voll&transport=pcm").await;
    let v = warte_auf_typ(&mut c, "error", 3, 1000).await;
    assert_eq!(v["msg"], "raum voll");

    // Raum bleibt bei genau 2 Peers.
    let rooms = http_get(&addr, "/api/rooms").await;
    let map: serde_json::Value = serde_json::from_str(rooms.trim()).unwrap();
    assert_eq!(
        map["voll"].as_array().unwrap().len(),
        2,
        "raum voll bleibt 2"
    );
}

#[tokio::test]
async fn partner_event_inhalt_fuer_beide_seiten() {
    let (base, _) = spawn_server(None).await;
    let mut a = connect(&base, "room=p&transport=pcm&lang=de").await;

    // A allein: erstes partner-Event ist null.
    let v = warte_auf_typ(&mut a, "partner", 3, 1000).await;
    assert!(v["partner"].is_null(), "A allein hat keinen Partner");

    // B tritt bei (opus, zh-Hans).
    let mut b = connect(&base, "room=p&transport=opus&lang=zh-Hans").await;

    // A sieht B als Partner: lang=zh-Hans, transport=opus.
    let v = warte_auf_typ(&mut a, "partner", 4, 1500).await;
    assert_eq!(v["partner"]["lang"], "zh-Hans");
    assert_eq!(v["partner"]["transport"], "opus");

    // B sieht A als Partner: lang=de, transport=pcm.
    let v = warte_auf_typ(&mut b, "partner", 4, 1500).await;
    assert_eq!(v["partner"]["lang"], "de");
    assert_eq!(v["partner"]["transport"], "pcm");
}

#[tokio::test]
async fn opus_allein_bekommt_waiting_kein_ready() {
    let (base, _) = spawn_server(Some("test")).await;
    let mut a = connect(&base, "room=o0&transport=opus&lang=zh-Hans").await;
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();

    // Ohne Partner: waiting kommt, ready NICHT.
    let mut saw_waiting = false;
    for _ in 0..6 {
        let t = naechster_text(&mut a, 1500).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        match v["type"].as_str() {
            Some("waiting") => saw_waiting = true,
            Some("ready") => panic!("ohne Partner darf kein ready kommen"),
            _ => {}
        }
        if saw_waiting {
            break;
        }
    }
    assert!(saw_waiting, "opus allein bekommt waiting");

    // Noch kurz sicherstellen, dass kein ready nachkommt.
    let kein_ready = tokio::time::timeout(Duration::from_millis(400), async {
        loop {
            let t = naechster_text(&mut a, 1000).await;
            let v: serde_json::Value = serde_json::from_str(&t).unwrap();
            if v["type"] == "ready" {
                return true;
            }
        }
    })
    .await;
    assert!(kein_ready.is_err(), "kein ready ohne Partner");
}

#[tokio::test]
async fn feste_stimmen_nutzen_live_modell() {
    // zh-Hans-Opus-Peer mit voice=Kore → feste Stimme → Live-Modell
    // (de ↔ zh-Hans sind beide live). Fake spiegelt die Stimme:
    // `hallo:live:Kore`. Der PCM-Partner (de, voice=Charon) bekommt im
    // partner-Event model=live + Setup mit voiceName Charon.
    let (base, _) = spawn_server(Some("test")).await;
    let mut a = connect(&base, "room=v1&transport=opus&lang=zh-Hans&voice=Kore").await;
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();
    let _ = warte_auf_typ(&mut a, "waiting", 4, 1500).await;

    let mut b = connect(&base, "room=v1&transport=pcm&lang=de&voice=Charon").await;
    let _ = warte_auf_typ(&mut a, "ready", 6, 2000).await;

    // B (PCM) sieht den Partner A: model=live, Setup mit eigener Stimme Charon.
    let mut sah_setup = false;
    for _ in 0..6 {
        let t = naechster_text(&mut b, 1500).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "partner" && !v["partner"].is_null() {
            assert_eq!(v["model"], "live", "B→Gemini via Live-Modell");
            assert!(
                v["setup"]["model"]
                    .as_str()
                    .unwrap()
                    .contains("gemini-3.8-live"),
                "Setup fuer gemini-3.8-live"
            );
            assert_eq!(
                v["setup"]["generationConfig"]["speechConfig"]["voiceConfig"]["prebuiltVoiceConfig"]
                    ["voiceName"],
                "Charon",
                "B spricht mit eigener Stimme Charon"
            );
            sah_setup = true;
            break;
        }
    }
    assert!(sah_setup, "B bekommt Setup im partner-Event");

    // A schickt ~4 s Audio → eigene Segmentierung schneidet (spaetestens bei
    // MAX_SEGMENT_MS=3 s) → Fake sieht >= 1 activityEnd und antwortet mit
    // `hallo:live:Kore` (A spricht mit Kore). Partner B bekommt Audio.
    sende_opus_audio(&mut a, 4.0).await;
    let mut saw = false;
    for _ in 0..40 {
        let t = naechster_text(&mut b, 3000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "text" && v["who"] == "partner" {
            assert_eq!(v["text"], "hallo:live:Kore", "Live mit Stimme Kore");
            saw = true;
            break;
        }
    }
    assert!(saw, "untertitel hallo:live:Kore erwartet");
}

#[tokio::test]
async fn opus_auto_translate_und_live_fallback() {
    let (base, _) = spawn_server(Some("test")).await;
    // A: Opus, spricht en mit voice=auto (en-Default ist auto).
    let mut a = connect(&base, "room=auto&transport=opus&lang=en&voice=auto").await;
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();
    let _ = warte_auf_typ(&mut a, "waiting", 4, 1500).await;

    // B: pcm, lang=de. auto + beide translate → Translate, Ziel = de.
    let mut b = connect(&base, "room=auto&transport=pcm&lang=de").await;
    let _ = warte_auf_typ(&mut a, "ready", 6, 2000).await;
    let pkt = test_opus_paket();
    a.send(Message::Binary(pkt.clone().into())).await.unwrap();
    let mut saw_de = false;
    for _ in 0..8 {
        let t = naechster_text(&mut b, 2000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "text" && v["who"] == "partner" {
            assert_eq!(v["text"], "hallo:de", "auto + beide translate → Translate");
            saw_de = true;
            break;
        }
    }
    assert!(saw_de, "untertitel hallo:de erwartet");

    // B verlaesst → A bekommt wieder waiting.
    drop(b);
    let _ = warte_auf_typ(&mut a, "waiting", 6, 2000).await;

    // Neuer Partner C mit lang=cy (nur-Live): auto → Live-Fallback.
    // A-Default-Stimme fuer en ist auto → Fallback-Stimme Zephyr.
    let mut c = connect(&base, "room=auto&transport=pcm&lang=cy").await;
    let _ = warte_auf_typ(&mut a, "ready", 6, 2000).await;
    // Live braucht eigene Segmentierung: ~4 s Audio → >= 1 activityEnd.
    sende_opus_audio(&mut a, 4.0).await;
    let mut saw_live = false;
    for _ in 0..40 {
        let t = naechster_text(&mut c, 3000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "text" && v["who"] == "partner" {
            assert_eq!(
                v["text"], "hallo:live:Zephyr",
                "auto + nur-Live-Sprache → Live-Fallback mit Zephyr"
            );
            saw_live = true;
            break;
        }
    }
    assert!(saw_live, "Live-Fallback hallo:live:Zephyr erwartet");
}

#[tokio::test]
async fn opus_feste_stimme_translate_fallback_bei_nur_translate_sprache() {
    // A: Opus, voice=Kore, spricht de. Partner spricht jv (nur-Translate).
    // Feste Stimme + Live unmoeglich → Translate-Fallback (Stimme ignoriert).
    let (base, _) = spawn_server(Some("test")).await;
    let mut a = connect(&base, "room=fb&transport=opus&lang=de&voice=Kore").await;
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();
    let _ = warte_auf_typ(&mut a, "waiting", 4, 1500).await;

    let mut b = connect(&base, "room=fb&transport=pcm&lang=jv").await;
    let _ = warte_auf_typ(&mut a, "ready", 6, 2000).await;
    let pkt = test_opus_paket();
    a.send(Message::Binary(pkt.into())).await.unwrap();
    let mut saw = false;
    for _ in 0..8 {
        let t = naechster_text(&mut b, 2000).await;
        let v: serde_json::Value = serde_json::from_str(&t).unwrap();
        if v["type"] == "text" && v["who"] == "partner" {
            assert_eq!(
                v["text"], "hallo:jv",
                "feste Stimme + jv → Translate-Fallback"
            );
            saw = true;
            break;
        }
    }
    assert!(saw, "Translate-Fallback hallo:jv erwartet");
}

#[tokio::test]
async fn pcm_zu_opus_liefert_silk_pakete() {
    let (base, _) = spawn_server(Some("test")).await;
    let mut a = connect(&base, "room=o2&transport=opus&lang=zh-Hans").await;
    let mut b = connect(&base, "room=o2&transport=pcm&lang=de").await;

    // A anmelden (hello mit server_key); Partner B ist schon da → ready.
    a.send(Message::Text(r#"{"type":"hello"}"#.into()))
        .await
        .unwrap();
    let _ = warte_auf_typ(&mut a, "ready", 6, 2000).await;
    tokio::time::sleep(Duration::from_millis(100)).await;

    // B schickt 1 s PCM24-Sinus (48000 Bytes) an A.
    let pcm = sinus_pcm24_le(1.0);
    assert_eq!(pcm.len(), 48_000);
    b.send(Message::Binary(pcm.clone().into())).await.unwrap();

    // A bekommt mehrere Opus-Binary-Pakete; alle SilkOnly, dekodierbar.
    let mut dec = OpusDecoder::new(24_000, 1).unwrap();
    let mut gesamt = 0usize;
    let mut pakete = 0usize;
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
    assert_eq!(
        v["languages"].as_array().unwrap().len(),
        101,
        "101 sprachen"
    );
    // Flags je Sprache vorhanden.
    let de = v["languages"]
        .as_array()
        .unwrap()
        .iter()
        .find(|l| l["code"] == "de")
        .unwrap();
    assert_eq!(de["en"], "German");
    assert_eq!(de["translate"], true);
    assert_eq!(de["live"], true);
    // 30 Stimmen mit Geschlecht.
    assert_eq!(v["voices"].as_array().unwrap().len(), 30, "30 stimmen");
    assert!(
        v["voices"]
            .as_array()
            .unwrap()
            .iter()
            .any(|x| x["name"] == "Kore" && x["gender"] == "female"),
        "Kore ist weiblich"
    );
    // Default-Stimme ist fuer alle Automatisch (feste Stimmen experimentell).
    // (die Map listet nur Sprachen mit fester Default-Stimme → jetzt leer).
    assert!(v["default_voice"].get("zh-Hans").is_none());
    assert!(v["default_voice"].get("de").is_none());
    assert_eq!(v["auto"], "auto");

    // Ohne Server-Key meldet /api/config false (Sparmodus nicht moeglich).
    let cfg = http_get(&addr, "/api/config").await;
    assert_eq!(cfg.trim(), r#"{"server_key":false}"#);
    let (_, addr_k) = spawn_server(Some("geheim")).await;
    let cfg = http_get(&addr_k, "/api/config").await;
    assert_eq!(cfg.trim(), r#"{"server_key":true}"#);
    assert!(!cfg.contains("geheim"), "key darf nie ausgeliefert werden");

    // Leeres /api/rooms.
    let empty = http_get(&addr, "/api/rooms").await;
    assert_eq!(empty.trim(), "{}");

    // Ein Opus- und ein PCM-Peer → Transport sichtbar.
    let base = format!("ws://{addr}");
    let _a = connect(&base, "room=rA&transport=opus&lang=de").await;
    let _b = connect(&base, "room=rA&transport=pcm&lang=zh-Hans").await;
    tokio::time::sleep(Duration::from_millis(200)).await;

    let rooms = http_get(&addr, "/api/rooms").await;
    assert!(rooms.contains("\"opus\""), "transport opus im json");
    assert!(rooms.contains("\"pcm\""), "transport pcm im json");
    assert!(rooms.contains("rA"), "raumname im json");
    assert!(rooms.contains("\"voice\""), "voice-feld im json");
    // Kein target-Feld mehr.
    assert!(!rooms.contains("\"target\""), "kein target mehr im json");
}

/// `secs` Sekunden lautes Sprachsignal als 60-ms-Opus-Pakete (16 kHz) an einen
/// Opus-Peer schicken. Lautes Signal (Amplitude 8000) sorgt dafuer, dass der
/// Segmentierer spaetestens bei `MAX_SEGMENT_MS` (3 s) schneidet; so sieht der
/// Fake-Gemini mindestens ein `activityEnd` und antwortet. Je Paket ein Binary,
/// mit kurzer Pause, damit das Relay die Chunks einzeln dekodiert.
async fn sende_opus_audio(ws: &mut Ws, secs: f32) {
    let mut enc = OpusEncoder::new(16_000, 1, Application::Voip).unwrap();
    enc.bitrate_bps = 12_000;
    enc.max_bandwidth = Bandwidth::Wideband;
    enc.signal_type = Some(Signal::Voice);
    let pakete = (secs * 1000.0 / 60.0).ceil() as usize; // 60 ms je Paket
    let mut phase = 0.0f32;
    for _ in 0..pakete {
        let mut pcm = [0i16; 960]; // 60 ms @ 16 kHz
        for s in pcm.iter_mut() {
            phase += 300.0 / 16_000.0;
            *s = ((2.0 * std::f32::consts::PI * phase).sin() * 8000.0) as i16;
        }
        let mut out = vec![0u8; 1500];
        let n = enc.encode_s16(&pcm, 960, &mut out).unwrap();
        out.truncate(n);
        ws.send(Message::Binary(out.into())).await.unwrap();
        tokio::time::sleep(Duration::from_millis(5)).await;
    }
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
