# live-silk — Live-AI-Dolmetscher mit Opus-SILK-Sparmodus

Rust-Server (axum) für Zwei-Personen-Räume: jeder Nutzer wählt beim Verbinden,
ob sein Audio **normal** (PCM, Browser spricht direkt mit Gemini) oder im
**Sparmodus** (Opus-SILK, ~12 kbit/s, Server hält die Gemini-Sitzung) läuft.
Alle Sprachen des Live-Übersetzungsmodells sind wählbar, Default
Deutsch ↔ Chinesisch, höchstens **2 Clients pro Raum**.

## Start

```bash
cd source2
cargo run --release -- --listen 0.0.0.0:3000 --dir public
# optionaler Server-Key (dann brauchen Sparmodus-Nutzer keinen eigenen):
cargo run --release -- --listen 0.0.0.0:3000 --dir public --key-file api_key.txt
# ausfuehrliche Logs (nie Audio, nie Key):
cargo run --release -- --listen 0.0.0.0:3000 --dir public --verbose
```

Browser öffnen: `http://localhost:3000`
Produktion hinter Nginx (`/silk/`, systemd, Key/Kosten): [`INSTALL.md`](INSTALL.md).
Status: `/healthz`, Räume: `/api/rooms`, Sprachen: `/api/languages`.

## Modi

| Modus | Netz je Nutzer | Wie |
|---|---|---|
| **Normal (PCM)** | ~725 kbit/s ↑ / ~900 kbit/s ↓ | Browser spricht selbst mit Gemini (eigener Key nötig); PCM24 über den Relay. |
| **Sparmodus (Opus SILK)** | **~12 kbit/s** je Richtung | Browser schickt/empfängt nur Opus; der **Server** hält die Gemini-Sitzung. |

Im Frontend: „Ich spreche" / „Partner hört" (Sprachen aus `/api/languages`,
Tausch-Knopf), Transport-Auswahl, Status, Partner-Anzeige, Untertitel,
kbit/s-Anzeige hoch/runter. Fehlt **WebCodecs** (alte Browser), ist der
Sparmodus deaktiviert mit Hinweis.

## Key-Hinweis

- **Normalmodus:** der Gemini-API-Key bleibt lokal (geht nie an den Server).
- **Sparmodus:** der Key geht **an den Server** (er spricht mit Gemini). Mit
  `--key-file` am Server kann das Feld leer bleiben. Der Key wird nie geloggt
  und aus keiner Fehlermeldung durchgereicht.

## Protokoll (`/ws?room=&transport=pcm|opus&lang=&target=`)

- **pcm:** Binär rein = eigene Übersetzung PCM24 → Partner; Binär raus =
  Partner-PCM24; Untertitel als `{"type":"text","dir":"in|out","text":…}`.
- **opus:** erst `{"type":"hello","key":…}` (Key optional → Server-Key), dann
  Opus-Pakete (16 kHz mono, 60 ms) hoch; runter Opus-Pakete (SILK, 24-kHz-
  Encoder, Browser-Decoder mit `sampleRate: 24000`).
- Server → Client: `ready`, `peers`, `error`, `text{who,dir,text}`.

## Tests

```bash
cargo test                 # Unit + Integration (Fake-Gemini)
scripts/smoke.sh           # Build, Health, /api/languages, Frontend
scripts/browser-setup.sh   # einmalig: Chrome-for-Testing, Treiber, Fake-Mikro
scripts/browser-fake.sh    # zwei Headless-Tabs (A Opus, B PCM) gegen Fake-Gemini
```

Details und Messwerte: [`docs/browser-tests.md`](docs/browser-tests.md).

## Dateien

`src/01_config.rs` CLI/Transport · `src/02_languages.rs` 78 Sprachen ·
`src/03_codec.rs` Opus-SILK-Codec · `src/04_state.rs` Raumbuch ·
`src/05_gemini.rs` Gemini-Client · `src/06_relay.rs` WS-Relay ·
`src/07_serve.rs` Router · `src/08_status.rs` TUI-light · `main.rs`/`lib.rs`
nur Verdrahtung · `public/` Frontend (`index.html`, `app.js`, `01_audio.js`,
`02_opus.js`) · `examples/fake_gemini.rs` Test-Gemini · `tests/` Host-Tests ·
`scripts/` Smoke- und Browser-Test.
