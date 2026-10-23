# live-relay — Raum-Relay für den Live-AI-Dolmetscher

Kleiner Rust-Server (axum): verteilt binäre PCM-Audioframes an alle Peers
im gleichen Raum (`/ws?room=NAME`), liefert das Frontend aus und meldet
Status (`/healthz`, `/api/rooms`, `/api/modes`). Übersetzt wird **nicht**
hier, sondern direkt zwischen Browser und Gemini-Live-API.

## Start

```bash
cd source1
cargo run --release -- --listen 0.0.0.0:3000 --dir public
```

Browser öffnen: `http://localhost:3000`
Status: `http://localhost:3000/healthz`, Räume: `http://localhost:3000/api/rooms`

## Nutzung (2 Personen, gleicher Raum)

1. Beide öffnen die Seite und tragen **denselben Raumnamen** ein.
2. Jede Seite trägt ihren **eigenen Gemini-API-Key** ein (bleibt in
   `localStorage`, geht nie ans Relay).
3. A wählt z. B. `de-to-zh`, B wählt `zh-to-de`, beide klicken **Starten**.

## Wichtig für Handys (Android/iPhone)

Mikrofon geht nur über **sichere Kontexte**: `localhost` oder HTTPS.
Im WLAN (`http://192.168.x.y:3000`) blockieren Chrome/Safari das Mikrofon —
dann einen Tunnel davorlegen, z. B. `npx ngrok http 3000` oder
Cloudflare-Tunnel, und die `https://…`-URL auf beiden Geräten öffnen.

## TUI-light (Terminal-Status, nur `std`)

```bash
cargo run --release -- --check http://127.0.0.1:3000   # einmal
cargo run --release -- --watch http://127.0.0.1:3000   # alle 2 s
```

## Dateien

`src/01_config.rs` CLI + 4 Sprachmodi · `src/02_state.rs` Raumbuch ·
`src/03_relay.rs` WS-Broadcast · `src/04_serve.rs` Router · `main.rs`/`lib.rs`
nur Verdrahtung · `public/index.html` Frontend · `tests/` Host-Tests ·
`scripts/smoke.sh` Smoke-Test · `scripts/hil-*.sh` Browser-/Inhalts-HIL
(siehe `docs/browser-tests.md`).
