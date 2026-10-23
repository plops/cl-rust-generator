# Abhaengigkeiten (`<organization>/<projekt>`)

Stand 2026-10-23, `cargo upgrade --dry-run` geprüft: alle auf `latest`.
Absichtlich klein: Relay + 4 Sprachmodi + TUI-light, kein Web-Framework
neben axum, kein TUI-Framework (nur `std`).

| Crate | GitHub | Version | Zweck |
|---|---|---|---|
| `axum` | `tokio-rs/axum` | 0.8 | Router, `ws`-Upgrade (`/ws?room=`), Fallback-Static |
| `tokio` | `tokio-rs/tokio` | 1 | Runtime `full`, `Mutex`, `mpsc`-Kanäle |
| `tower-http` | `tower-rs/tower-http` | 0.7 | `ServeDir` für `public/index.html` |
| `tracing` | `tokio-rs/tracing` | 0.1 | Beitritt/Austritt-Logs (nie Audio/Key) |
| `tracing-subscriber` | `tokio-rs/tracing` | 0.3 (`fmt`) | Log-Ausgabe, per `--verbose` DEBUG sonst WARN |
| `futures-util` | `rust-lang/futures-rs` | 0.3 | `split`/`next` am WebSocket |
| `serde` / `serde_json` | `serde-rs/serde` / `serde-rs/json` | 1 | Query-Parse, `/api/*`-JSON |

Dev-only: `tokio-tungstenite` (`snapview/tokio-tungstenite`, WS-Testclient),
`reqwest` (`seanmonstar/reqwest`, HTTP-Assert in Tests).

## DeepWiki-Muster (abgefragt 2026-10-23)

- `tokio-rs/axum`: `WebSocketUpgrade` + `State<Arc<AppState>>`,
  `ws.on_upgrade(|sock| ...)`, `socket.split()` in Sende-/Empfangs-Task,
  Broadcast via `mpsc::Sender<Message>` je Peer; statisch via
  `tower-http`-`ServeDir` als `fallback_service`. Genau so gebaut
  (`03_relay.rs`, `04_serve.rs`); einzige Abweichung: statt
  `Vec<Sender>` nutzen wir `HashMap<PeerId, Sender<Vec<u8>>>` mit
  `try_send` (Slow-Consumer wird übersprungen statt blockiert) und
  Pfad-Param durch Query-Param `?room=` ersetzt (kompatibel zu
  `plan/misc/live.md`-Frontend).
