# Abhaengigkeiten (`<organization>/<projekt>`)

Stand 2026-10-27, `cargo upgrade --dry-run`: alle 12 auf `latest`.
Frontend ohne Bibliotheken (WebCodecs ist Browser-API).

| Crate | GitHub | Version | Zweck |
|---|---|---|---|
| `axum` | `tokio-rs/axum` | 0.8.9 (`ws`) | Router, `/ws`-Upgrade, `/api/*` |
| `tokio` | `tokio-rs/tokio` | 1.54 | Runtime, `Mutex`, `mpsc` |
| `tower-http` | `tower-rs/tower-http` | 0.7 (`fs`) | `ServeDir` fuer `public/` |
| `tracing` / `tracing-subscriber` | `tokio-rs/tracing` | 0.1 / 0.3 | Logs (nie Audio, nie Key) |
| `futures-util` | `rust-lang/futures-rs` | 0.3 | `split` von WebSockets |
| `serde` / `serde_json` | `serde-rs/serde` / `serde-rs/json` | 1 | Query, JSON-Protokoll |
| `opus-pure` | `stephenberry/opus-pure` | 0.2.2 | Opus-SILK Encoder (24 kHz → 12 kbit/s) und Decoder (→ 16 kHz), reines Rust, null transitive Deps |
| `tokio-tungstenite` | `snapview/tokio-tungstenite` | 0.30 (`rustls-tls-webpki-roots`) | Gemini-Live-Client (Server haelt Sitzung im Sparmodus), Test-Clients |
| `rustls` | `rustls/rustls` | 0.23 (`ring`, `std`, ohne default) | genau ein Crypto-Provider; ohne ihn panict `connect_async` bei `wss://` |
| `base64` | `marshallpierce/rust-base64` | 0.23 | PCM in/aus Gemini-JSON |

## Usage-Muster (verifiziert)

```rust
// opus-pure (DeepWiki stephenberry/opus-pure + Spike, siehe auch ../../36_opus/source1/deps.md)
let mut enc = OpusEncoder::new(24_000, 1, Application::Voip)?;
enc.bitrate_bps = 12_000;
enc.max_bandwidth = Bandwidth::Wideband; // => SilkOnly
enc.signal_type = Some(Signal::Voice);
let n = enc.encode(&f32_1440, 1440, &mut out)?; // 60 ms
let mut dec = OpusDecoder::new(16_000, 1)?;      // direkt 16 kHz fuer Gemini

// tokio-tungstenite + rustls(ring)
let (ws, _) = tokio_tungstenite::connect_async(format!("{url}?key={key}")).await?;
```

DeepWiki-Hinweis (`snapview/tokio-tungstenite`, 2026-10-27): behauptete,
mit `rustls-tls-webpki-roots` sei kein Crypto-Provider noetig — der
Spike widerlegte das (Panic „Could not automatically determine the
process-level CryptoProvider"); Abhilfe: `rustls` mit nur `ring`.

## Test-Werkzeuge (nicht im Repo, `scripts/browser-setup.sh`)

`microsoft/playwright` (npm `playwright-core`), `websockets/ws` (npm
`ws`), Chrome for Testing (`GoogleChromeLabs/chrome-for-testing`),
`FFmpeg/FFmpeg`.
