//! 05 — Gemini-Live-Client: haelt eine Uebersetzungs-Sitzung je Opus-Peer.
//!
//! Datenfluss: `06_relay` oeffnet pro Opus-Peer eine `Session`, schiebt
//! PCM 16 kHz (vom Opus-Decoder) in `Session::tx` und liest die Antworten
//! (Audio PCM 24 kHz, Untertitel) als `Event` aus dem Event-Kanal.
//!
//! Protokoll (per echter Sonde belegt): Verbindung zu `{url}?key={key}`,
//! erste Nachricht `setup` mit `translationConfig` in `generationConfig`,
//! die Transkriptions-Felder aber auf `setup`-Ebene (sonst Close 1007).
//! Audio geht als `realtimeInput.audio` (Base64 PCM16LE 16 kHz) raus.
//!
//! Der Key wird NIE geloggt und aus keiner Fehlermeldung durchgereicht.

use base64::{Engine, engine::general_purpose::STANDARD};
use futures_util::{SinkExt, StreamExt};
use tokio::sync::mpsc;
use tokio_tungstenite::{connect_async, tungstenite::Message};

/// Kanalpuffer fuer ausgehende PCM-Chunks bzw. eingehende Events.
const CHAN: usize = 64;

/// Setup-JSON fuer eine neue Sitzung.
///
/// `target` ist der Ziel-Sprachcode (z. B. `zh-Hans`). WICHTIG:
/// `inputAudioTranscription`/`outputAudioTranscription` liegen auf
/// `setup`-Ebene, NICHT in `generationConfig` (sonst Close 1007).
///
/// `echo`: wenn `true`, wird `translationConfig.echoTargetLanguage = true`
/// gesetzt. Das ist noetig, wenn eigene Sprache == Partner-Sprache (Ziel ==
/// Quelle): sonst schwiege Gemini (nichts zu uebersetzen). Mit `echo` spricht
/// Gemini das Gesagte in derselben Sprache nach. Bei `false` entfaellt das
/// Feld ganz.
pub fn setup_json(target: &str, echo: bool) -> String {
    let mut translation = serde_json::json!({ "targetLanguageCode": target });
    if echo {
        translation["echoTargetLanguage"] = serde_json::Value::Bool(true);
    }
    let v = serde_json::json!({
        "setup": {
            "model": crate::config::GEMINI_MODEL,
            "generationConfig": {
                "responseModalities": ["AUDIO"],
                "translationConfig": translation
            },
            "inputAudioTranscription": {},
            "outputAudioTranscription": {}
        }
    });
    v.to_string()
}

/// Audio-JSON fuer einen PCM16LE-16-kHz-Chunk (`realtimeInput.audio`).
pub fn audio_json(pcm16_le: &[u8]) -> String {
    let data = STANDARD.encode(pcm16_le);
    let v = serde_json::json!({
        "realtimeInput": {
            "audio": { "mimeType": "audio/pcm;rate=16000", "data": data }
        }
    });
    v.to_string()
}

/// Was der Server geschickt hat. Eine Nachricht kann mehrere Events liefern.
#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    /// Uebersetztes Audio als PCM16LE 24 kHz.
    Audio(Vec<u8>),
    /// Transkript der Eingabe (was der Sprecher gesagt hat).
    InText(String),
    /// Transkript der Ausgabe (die Uebersetzung als Text).
    OutText(String),
    /// `setupComplete` empfangen — die Sitzung nimmt jetzt Audio an.
    Ready,
    /// Verbindung beendet; Grund enthaelt keinen Key und keine URL.
    Closed(String),
}

/// Server-Nachricht (Text-JSON) in null oder mehr Events zerlegen.
///
/// Unbekanntes (`usageMetadata`, `sessionResumptionUpdate`, Muell) ergibt
/// einen leeren Vektor.
pub fn parse_server_msg(txt: &str) -> Vec<Event> {
    let Ok(v) = serde_json::from_str::<serde_json::Value>(txt) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    if v.get("setupComplete").is_some() {
        out.push(Event::Ready);
    }
    if let Some(sc) = v.get("serverContent") {
        // Uebersetztes Audio aus modelTurn.parts[].inlineData.data.
        if let Some(parts) = sc
            .get("modelTurn")
            .and_then(|m| m.get("parts"))
            .and_then(|p| p.as_array())
        {
            for part in parts {
                if let Some(data) = part
                    .get("inlineData")
                    .and_then(|i| i.get("data"))
                    .and_then(|d| d.as_str())
                    && let Ok(bytes) = STANDARD.decode(data)
                {
                    out.push(Event::Audio(bytes));
                }
            }
        }
        if let Some(t) = sc
            .get("inputTranscription")
            .and_then(|i| i.get("text"))
            .and_then(|t| t.as_str())
        {
            out.push(Event::InText(t.to_owned()));
        }
        if let Some(t) = sc
            .get("outputTranscription")
            .and_then(|o| o.get("text"))
            .and_then(|t| t.as_str())
        {
            out.push(Event::OutText(t.to_owned()));
        }
    }
    out
}

/// Eine laufende Gemini-Sitzung. PCM16LE-16-kHz-Chunks in `tx` schieben.
/// Wird `tx` gedroppt, schliesst der Sende-Task die Verbindung sauber.
pub struct Session {
    /// Eingang fuer PCM16LE 16 kHz (Chunks beliebiger Laenge).
    pub tx: mpsc::Sender<Vec<u8>>,
}

/// Verbindet zu `{url}?key={key}`, sendet das Setup und startet die Tasks.
///
/// Fehlermeldungen enthalten weder Key noch URL. Audio-Chunks aus
/// `Session::tx` werden als `realtimeInput` verschickt; Server-Antworten
/// landen als `Event` im `events`-Kanal. Bei Close/Fehler kommt genau ein
/// `Event::Closed(grund)` (ohne Key).
pub async fn connect(
    url: &str,
    key: &str,
    target: &str,
    echo: bool,
    events: mpsc::Sender<Event>,
) -> Result<Session, String> {
    // Query anhaengen; ein fehlender Pfad (z. B. "ws://host:port") wuerde
    // sonst ein ungueltiges Request-Target "?key=..." erzeugen.
    let base = if needs_slash(url) {
        format!("{url}/")
    } else {
        url.to_owned()
    };
    let full = format!("{base}?key={key}");
    let (ws, _) = connect_async(&full)
        .await
        .map_err(|e| sanitize(&e.to_string(), url, key))?;
    let (mut sink, mut stream) = ws.split();

    sink.send(Message::Text(setup_json(target, echo).into()))
        .await
        .map_err(|e| sanitize(&e.to_string(), url, key))?;

    let (tx, mut rx) = mpsc::channel::<Vec<u8>>(CHAN);

    // Owned Kopien fuer den Empfangs-Task (nur zum Bereinigen von Fehlern).
    let (url_owned, key_owned) = (url.to_owned(), key.to_owned());

    // Sende-Task: PCM aus rx -> audio_json. Endet, wenn tx gedroppt wird.
    tokio::spawn(async move {
        while let Some(pcm) = rx.recv().await {
            if sink
                .send(Message::Text(audio_json(&pcm).into()))
                .await
                .is_err()
            {
                break;
            }
        }
        // tx gedroppt oder Fehler: Verbindung sauber schliessen.
        let _ = sink.close().await;
    });

    // Empfangs-Task: Nachrichten parsen -> events. Close/Fehler -> Closed.
    tokio::spawn(async move {
        let grund = loop {
            match stream.next().await {
                Some(Ok(Message::Text(t))) => {
                    for ev in parse_server_msg(&t) {
                        if events.send(ev).await.is_err() {
                            return;
                        }
                    }
                }
                Some(Ok(Message::Binary(b))) => {
                    // setupComplete kann als Binary-JSON kommen.
                    if let Ok(t) = std::str::from_utf8(&b) {
                        for ev in parse_server_msg(t) {
                            if events.send(ev).await.is_err() {
                                return;
                            }
                        }
                    }
                }
                Some(Ok(Message::Close(c))) => {
                    break c
                        .map(|f| format!("close {}", f.code))
                        .unwrap_or_else(|| "close".to_owned());
                }
                Some(Ok(_)) => {}
                Some(Err(e)) => break sanitize(&e.to_string(), &url_owned, &key_owned),
                None => break "stream beendet".to_owned(),
            }
        };
        let _ = events.send(Event::Closed(grund)).await;
    });

    Ok(Session { tx })
}

/// `true`, wenn die URL nach der Authority keinen Pfad hat
/// (z. B. `ws://host:port`); dann muss ein `/` vor die Query.
fn needs_slash(url: &str) -> bool {
    match url.split_once("://") {
        Some((_, rest)) => !rest.contains('/'),
        None => false,
    }
}

/// Entfernt Key und URL aus einer Fehlermeldung.
fn sanitize(msg: &str, url: &str, key: &str) -> String {
    let mut s = msg.replace(key, "<key>");
    s = s.replace(url, "<url>");
    // Vollstaendige URL inkl. Query ebenfalls tilgen.
    s.replace(&format!("{url}?key={key}"), "<url>")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn setup_json_transkription_auf_setup_ebene() {
        let v: serde_json::Value = serde_json::from_str(&setup_json("zh-Hans", false)).unwrap();
        let setup = &v["setup"];
        assert_eq!(setup["model"], crate::config::GEMINI_MODEL);
        assert_eq!(
            setup["generationConfig"]["translationConfig"]["targetLanguageCode"],
            "zh-Hans"
        );
        assert_eq!(setup["generationConfig"]["responseModalities"][0], "AUDIO");
        // Ohne echo kein echoTargetLanguage-Feld.
        assert!(setup["generationConfig"]["translationConfig"]["echoTargetLanguage"].is_null());
        // Transkription auf setup-Ebene ...
        assert!(setup["inputAudioTranscription"].is_object());
        assert!(setup["outputAudioTranscription"].is_object());
        // ... und NICHT in generationConfig (sonst Close 1007).
        assert!(setup["generationConfig"]["inputAudioTranscription"].is_null());
        assert!(setup["generationConfig"]["outputAudioTranscription"].is_null());
    }

    #[test]
    fn setup_json_echo_bei_gleicher_sprache() {
        // Gleiche Sprache wie der Partner (Ziel == Quelle): echo setzt
        // echoTargetLanguage = true, damit Gemini nachspricht statt schweigt.
        let v: serde_json::Value = serde_json::from_str(&setup_json("de", true)).unwrap();
        assert_eq!(
            v["setup"]["generationConfig"]["translationConfig"]["echoTargetLanguage"],
            true
        );
        assert_eq!(
            v["setup"]["generationConfig"]["translationConfig"]["targetLanguageCode"],
            "de"
        );
    }

    #[test]
    fn audio_json_base64_roundtrip() {
        let pcm = vec![1u8, 2, 3, 250, 0, 7];
        let v: serde_json::Value = serde_json::from_str(&audio_json(&pcm)).unwrap();
        let ai = &v["realtimeInput"]["audio"];
        assert_eq!(ai["mimeType"], "audio/pcm;rate=16000");
        let data = ai["data"].as_str().unwrap();
        assert_eq!(STANDARD.decode(data).unwrap(), pcm);
    }

    #[test]
    fn parse_model_turn_und_output_transcription() {
        let data = STANDARD.encode([9u8, 8, 7, 6]);
        let msg = serde_json::json!({
            "serverContent": {
                "modelTurn": { "parts": [{ "inlineData": { "data": data } }] },
                "inputTranscription": { "text": "hallo welt" },
                "outputTranscription": { "text": "你好" }
            }
        })
        .to_string();
        let evs = parse_server_msg(&msg);
        assert!(evs.contains(&Event::Audio(vec![9, 8, 7, 6])));
        assert!(evs.contains(&Event::InText("hallo welt".to_owned())));
        assert!(evs.contains(&Event::OutText("你好".to_owned())));
    }

    #[test]
    fn parse_setup_complete_ist_ready() {
        assert_eq!(
            parse_server_msg(r#"{"setupComplete":{}}"#),
            vec![Event::Ready]
        );
    }

    #[test]
    fn parse_muell_und_unbekanntes_leer() {
        assert!(parse_server_msg("kein json {").is_empty());
        assert!(parse_server_msg(r#"{"usageMetadata":{"x":1}}"#).is_empty());
        assert!(parse_server_msg(r#"{"sessionResumptionUpdate":{}}"#).is_empty());
    }

    #[test]
    fn sanitize_entfernt_key_und_url() {
        let s = sanitize(
            "fehler bei wss://host/x?key=GEHEIM nicht gut",
            "wss://host/x",
            "GEHEIM",
        );
        assert!(!s.contains("GEHEIM"));
        assert!(!s.contains("wss://host/x"));
    }
}
