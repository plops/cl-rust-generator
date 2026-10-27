# live-silk — Live-AI-Dolmetscher mit Opus-SILK-Sparmodus

Rust-Server (axum) für Zwei-Personen-Räume: jeder Nutzer wählt beim Verbinden,
ob sein Audio **normal** (PCM, Browser spricht direkt mit Gemini) oder im
**Sparmodus** (Opus-SILK, ~12 kbit/s, Server hält die Gemini-Sitzung) läuft.
Alle Sprachen des Live-Übersetzungsmodells sind wählbar, Default
Deutsch ↔ Chinesisch, höchstens **2 Clients pro Raum**.

## Start

```bash
cd source2
# Sparmodus (Default im Frontend) braucht einen Server-Key:
cargo run --release -- --listen 0.0.0.0:3000 --dir public --key-file api_key.txt
# ohne Server-Key: nur Normalmodus (Nutzer tragen ihren eigenen Key ein)
cargo run --release -- --listen 0.0.0.0:3000 --dir public
# ausfuehrliche Logs (nie Audio, nie Key):
cargo run --release -- --listen 0.0.0.0:3000 --dir public --verbose
```

Browser öffnen: `http://localhost:3000`
Produktion hinter Nginx (`/silk/`, systemd, Key/Kosten): [`INSTALL.md`](INSTALL.md).
Status: `/healthz`, Räume: `/api/rooms`, Sprachen: `/api/languages`,
Server-Key vorhanden: `/api/config` (`{"server_key":true|false}`, nie der Key).

## Modi

| Modus | Netz je Nutzer | Wie |
|---|---|---|
| **Normal (PCM)** | ~725 kbit/s ↑ / ~900 kbit/s ↓ | Browser spricht selbst mit Gemini (eigener Key nötig); PCM24 über den Relay. |
| **Sparmodus (Opus SILK)** — Default | **~12 kbit/s** je Richtung | Browser schickt/empfängt nur Opus; der **Server** hält die Gemini-Sitzung mit seinem Key. |

Im Frontend: „Ich spreche" (eigene Sprache aus `/api/languages`),
Transport-Auswahl, Status, Partner-Anzeige, Untertitel, kbit/s-Anzeige
hoch/runter. **Die Zielsprache wird nicht mehr gewählt** — sie ist automatisch
die Sprache des Partners im Raum (Nutzer-Feedback: „das weiß der Partner am
besten"). Gemini erkennt die Quellsprache selbst. Fehlt **WebCodecs** (alte
Browser) oder hat der Server keinen Key, ist der Sparmodus deaktiviert (mit
Hinweis) und der Normalmodus vorausgewählt.

## Key-Hinweis

- **Sparmodus (Default):** kein Key-Feld; der Server nutzt seinen Key
  (`--key-file`). Der Browser sendet keinen Key.
- **Normalmodus:** erst bei Auswahl erscheint das Key-Feld; der Key bleibt
  lokal (`localStorage`) und geht nur direkt an Gemini, nie an den Server.
- Der Server-Key wird nie geloggt und aus keiner Fehlermeldung durchgereicht.

## Protokoll (`/ws?room=&transport=pcm|opus&lang=`)

Nur noch `room`, `transport`, `lang` (eigene Sprache). Ein evtl.
mitgeschicktes `target` wird ignoriert — die Zielsprache ist automatisch die
Sprache des Partners.

- **pcm:** Binär rein = eigene Übersetzung PCM24 → Partner; Binär raus =
  Partner-PCM24; Untertitel als `{"type":"text","dir":"in|out","text":…}`. Der
  Browser verbindet Gemini erst, wenn ein Partner da ist (Ziel = Partner-lang).
- **opus:** erst `{"type":"hello"}` (das Protokoll erlaubt weiterhin ein
  optionales `"key"`, das Frontend schickt keins → Server-Key), dann
  Opus-Pakete (16 kHz mono, 60 ms) hoch; runter Opus-Pakete (SILK, 24-kHz-
  Encoder, Browser-Decoder mit `sampleRate: 24000`). Die Server-Gemini-Sitzung
  startet erst mit Partner; ohne Partner kommt `waiting` statt `ready`.
- Server → Client: persönliches `{"type":"partner","partner":{lang,transport}}`
  bzw. `partner:null` bei Join/Leave; dazu `ready`, `waiting`, `error`,
  `text{who,dir,text}`.
- Gleiche Sprache wie der Partner: `translationConfig.echoTargetLanguage=true`,
  damit Gemini das Gesagte nachspricht statt zu schweigen.

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
