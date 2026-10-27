# live-silk — Live-AI-Dolmetscher mit Opus-SILK-Sparmodus

Rust-Server (axum) für Zwei-Personen-Räume: jeder Nutzer wählt beim Verbinden,
ob sein Audio **normal** (PCM, Browser spricht direkt mit Gemini) oder im
**Sparmodus** (Opus-SILK, ~12 kbit/s, Server hält die Gemini-Sitzung) läuft.
Jeder wählt zusätzlich **seine Sprecherstimme** (die Stimme, mit der der
Partner die Übersetzung hört): „Automatisch – eigene Stimme nachbilden"
(Übersetzungsmodell, fortlaufend) oder eine der 30 festen Prebuilt-Stimmen
(Konversationsmodell `gemini-3.8-live`, spricht nach kurzer Pause). Alle
**101 Sprachen** beider Modelle sind wählbar, Default Deutsch ↔ Chinesisch,
höchstens **2 Clients pro Raum**.

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
**„Meine Stimme (so hört mich der Partner)"** (Automatisch oder eine feste
Stimme), Transport-Auswahl, Status, Partner-Anzeige, Untertitel,
kbit/s-Anzeige hoch/runter. **Die Zielsprache wird nicht gewählt** — sie ist
automatisch die Sprache des Partners im Raum (Nutzer-Feedback: „das weiß der
Partner am besten"). Gemini erkennt die Quellsprache selbst. Fehlt
**WebCodecs** (alte Browser) oder hat der Server keinen Key, ist der Sparmodus
deaktiviert (mit Hinweis) und der Normalmodus vorausgewählt.

## Stimme und Modelle

Jeder Nutzer wählt seine Sprecherstimme. Daraus und aus beiden Sprachen folgt
das Gemini-Modell (eine Quelle der Wahrheit: `voices::choose` → `setup_for`):

| Wahl | Bedingung | Modell | Verhalten |
|---|---|---|---|
| **Automatisch** | beide Sprachen `translate` | `gemini-3.5-live-translate-preview` | bildet die eigene Stimme nach (Voice Replication), **fortlaufend/schnell** |
| **Feste Stimme** | beide Sprachen `live` | `gemini-3.8-live` + Dolmetsch-Anweisung | spricht in der **gewählten Stimme**, antwortet nach kurzer Sprechpause |

Fallbacks: „Automatisch", aber eine Sprache ist nur-Live (z. B. Walisisch) →
Live-Modell mit Ersatzstimme; feste Stimme, aber eine Sprache ist
nur-Translate (`jv`/`su`) → Translate-Modell (Stimme ignoriert); kein
gemeinsames Modell → Fehler.

**Default-Stimmen:** Chinesisch (`zh-Hans`, `zh-Hant`) → weiblich **Kore**,
Deutsch (`de`) → männlich **Charon**, alle anderen → **Automatisch**. Im
Frontend zieht ein Sprachwechsel die Default-Stimme nach, solange der Nutzer
die Stimme nicht selbst geändert hat (Auswahl bleibt in `localStorage`).

**Sprachmengen** (`/api/languages`, Flags `translate`/`live` je Sprache):
Übersetzungsmodell **78**, Live-Modell **99**, Vereinigung **101**. `jv`
(Javanisch) und `su` (Sundanesisch) können nur `translate`; 23 Sprachen (u. a.
Walisisch, Irisch, Maori, Somali) nur `live`.

## Key-Hinweis

- **Sparmodus (Default):** kein Key-Feld; der Server nutzt seinen Key
  (`--key-file`). Der Browser sendet keinen Key.
- **Normalmodus:** erst bei Auswahl erscheint das Key-Feld; der Key bleibt
  lokal (`localStorage`) und geht nur direkt an Gemini, nie an den Server.
- Der Server-Key wird nie geloggt und aus keiner Fehlermeldung durchgereicht.

## Protokoll (`/ws?room=&transport=pcm|opus&lang=&voice=`)

`room`, `transport`, `lang` (eigene Sprache) und `voice` (Sprecherstimme:
`auto` oder ein Prebuilt-Name; fehlt sie, gilt die Default-Stimme der Sprache).
Ein evtl. mitgeschicktes `target` wird ignoriert — die Zielsprache ist
automatisch die Sprache des Partners.

- **pcm:** Binär rein = eigene Übersetzung PCM24 → Partner; Binär raus =
  Partner-PCM24; Untertitel als `{"type":"text","dir":"in|out","text":…}`. Der
  Browser verbindet Gemini erst, wenn ein Partner da ist (Ziel = Partner-lang).
- **opus:** erst `{"type":"hello"}` (das Protokoll erlaubt weiterhin ein
  optionales `"key"`, das Frontend schickt keins → Server-Key), dann
  Opus-Pakete (16 kHz mono, 60 ms) hoch; runter Opus-Pakete (SILK, 24-kHz-
  Encoder, Browser-Decoder mit `sampleRate: 24000`). Die Server-Gemini-Sitzung
  startet erst mit Partner; ohne Partner kommt `waiting` statt `ready`.
- Server → Client: persönliches
  `{"type":"partner","partner":{lang,transport,voice},"model":"translate|live","setup":{…}}`
  bzw. `partner:null` bei Join/Leave; dazu `ready`, `waiting`, `error`,
  `text{who,dir,text}`. Das `setup`-Objekt enthält das für genau diesen Peer
  fertig gebaute Gemini-Setup (nur für PCM-Peers) — der Browser schickt es
  unverändert an Gemini, keine Duplikat-Logik im JS. `model` dient Opus-Peers
  zur Anzeige.
- Gleiche Sprache wie der Partner (Translate-Modell):
  `translationConfig.echoTargetLanguage=true`, damit Gemini das Gesagte
  nachspricht statt zu schweigen. Beim Live-Modell lautet die Anweisung dann
  „repeat verbatim".

## Tests

```bash
cargo test                 # Unit + Integration (Fake-Gemini)
scripts/smoke.sh           # Build, Health, /api/languages, Frontend
scripts/browser-setup.sh   # einmalig: Chrome-for-Testing, Treiber, Fake-Mikro
scripts/browser-fake.sh    # zwei Headless-Tabs (A Opus, B PCM) gegen Fake-Gemini
```

Details und Messwerte: [`docs/browser-tests.md`](docs/browser-tests.md).

## Dateien

`src/01_config.rs` CLI/Transport/Modelle · `src/02_languages.rs` 101 Sprachen
(Flags translate/live) · `src/02b_voices.rs` 30 Stimmen + Modell-Wahl
(`choose`) · `src/03_codec.rs` Opus-SILK-Codec · `src/04_state.rs` Raumbuch ·
`src/05_gemini.rs` Gemini-Client (`setup_for`) · `src/06_relay.rs` WS-Relay ·
`src/07_serve.rs` Router · `src/08_status.rs` TUI-light · `main.rs`/`lib.rs`
nur Verdrahtung · `public/` Frontend (`index.html`, `app.js`, `01_audio.js`,
`02_opus.js`) · `examples/fake_gemini.rs` Test-Gemini · `tests/` Host-Tests ·
`scripts/` Smoke- und Browser-Test.
