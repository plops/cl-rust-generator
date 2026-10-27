# Browser-Tests (automatisch, ohne echtes Gemini)

Der automatische Browser-Test fährt zwei Headless-Chrome-Tabs gegen einen
**Fake-Gemini** und beweist, dass beide Transportarten (Normal/PCM und
Sparmodus/Opus) im echten Browser laufen — ohne Netz, ohne Key, ohne Kosten.

## Einrichtung (einmalig, idempotent)

```bash
scripts/browser-setup.sh
```

Lädt nach `$HIL_DIR` (Default `/tmp/live-hil`, **nicht** ins Repo):

- **Chrome for Testing** (stable, linux64).
- npm-Treiber **playwright-core** und **ws**.
- Fake-Mikro `mic-zh.wav` (30 s Mandarin, 16 kHz mono) aus dem `chinese/`-Clip
  via `ffmpeg`.

Fehlende System-Libs für Headless-Chrome zieht das Skript per `apt-get` nach.
In dieser Umgebung war nur **libnss3** nicht vorhanden; die übrigen Libs
(`libnspr4`, `libatk*`, `libcups2`, `libdrm2`, `libxkbcommon0`, `libxcomposite1`,
`libxdamage1`, `libxfixes3`, `libxrandr2`, `libgbm1`, `libasound2t64`,
`libpango-1.0-0`, `libcairo2`, `libatspi2.0-0`) werden vorsorglich mit
installiert.

## Lauf

```bash
scripts/browser-fake.sh
```

Startet `fake_gemini` (cargo-Beispiel) auf `:8900` und `live-silk` auf `:3311`
mit `--gemini-url ws://127.0.0.1:8900` und einer **Dummy**-Key-Datei. Dann
`scripts/browser-fake.js` (Playwright): zwei Tabs im selben Raum —

- **Tab A — Sparmodus (Opus, Default):** `lang=zh-Hans`, kein Key-Feld (→ Server-Key). Die
  Zielsprache folgt automatisch dem Partner (= de). Mikro → WebCodecs-Opus
  hoch; Partner-Übersetzung als Opus runter → WebCodecs-Decoder → Lautsprecher.
- **Tab B — Normal (PCM):** `?gemini=ws://127.0.0.1:8900`, Dummy-Key,
  `lang=de`. Spricht „direkt" mit dem Fake (Ziel = Partner-lang zh-Hans),
  schickt seine PCM24-Übersetzung an den Relay.

Laufzeit ~20 s. Chrome-Flags: `--use-fake-device-for-media-stream`,
`--use-file-for-fake-audio-capture=…mic-zh.wav`, `--use-fake-ui-for-media-stream`,
`--autoplay-policy=no-user-gesture-required`, `--no-sandbox`.

## PASS-Kriterien

- Beide Tabs erreichen Status **Live**.
- A: `__stats.opusUp > 0` **und** `opusDown > 0` (A sendet WebCodecs-Opus und
  empfängt B's Übersetzung als Opus).
- B: `__stats.pcmDown > 0` (A's Übersetzung wird serverseitig geliefert).
- Untertitel bei beiden (`texts > 0`).
- A gesamt (up+down) **< 40 kbit/s**.
- A sieht **„Partner spricht: Deutsch"**, B sieht **„Chinesisch"**
  (automatische Zielsprache = Partner-Sprache).
- keine `pageerror`.

Screenshots landen in `$HIL_OUT` (`fake-A-opus.png`, `fake-B-pcm.png`).

## Beispiel-Messwerte (ein Lauf, 20 s)

| Tab | up | down | opusUp | opusDown | pcmDown | Untertitel | kbit/s (up+down) |
|---|---|---|---|---|---|---|---|
| A (Opus) | 28 872 B | 30 151 B | 28 872 B | 30 151 B | — | 398 | **24** |
| B (PCM)  | 1 841 990 B | 960 000 B | — | — | 960 000 B | 398 | 1 121 |

Partner-Anzeige: A „Partner spricht: Deutsch · Normal", B „Partner spricht:
Chinesisch (vereinfacht) · Sparmodus". Der Sparmodus braucht also rund
**24 kbit/s** gegenüber ~**1 121 kbit/s** im Normalmodus — etwa Faktor 47
weniger Daten über das Netz des Mobil-Clients. (Der Fake drosselt seine
Audio-Antwort auf Echtzeit, damit der gemessene Opus-Downlink realistisch bei
~12 kbit/s liegt.)

## Mit echtem Gemini (T9, manuell)

`scripts/hil-real.sh` + `scripts/hil-real.js` fahren denselben Aufbau gegen das
**echte** Gemini (Default-URL, Key aus `api_key.txt`) und prüfen die
Untertitel-**Inhalte** beider Richtungen. Der Key wird **nie** ausgegeben — nur
maskiert (`AIza…msBM`) — und das Skript bricht ab, falls er doch in einem Log
auftaucht.

Weil der Fake-Mikro-Stream **pro Browser-Prozess** gilt, startet `hil-real.js`
**zwei separate** Chrome-Instanzen:

- **A — Sparmodus (Opus):** `mic-zh.wav` (Mandarin), `lang=zh-Hans`, kein
  Key-Feld (Sparmodus) → der **Server**-Key spricht mit Gemini. Ziel folgt automatisch dem
  Partner (= de).
- **B — Normal (PCM):** `mic-de.wav` (deutsche TTS-Referenz), `lang=de`, Key
  **im Feld** → B verbindet **direkt** mit Gemini. Ziel folgt dem Partner
  (= zh-Hans).

Die deutsche Referenz `mic-de.wav` wird **einmalig** per Gemini-TTS
(`gemini-2.5-flash-preview-tts`) erzeugt: „Guten Morgen! Heute ist ein schöner
Tag. Wie geht es dir?" 3× mit Pausen, als 48-kHz-mono-WAV in `$HIL_DIR`.

```bash
scripts/browser-setup.sh   # einmalig (Chrome, Treiber, mic-zh.wav)
scripts/hil-real.sh        # echtes Gemini, ~50 s Laufzeit
```

### PASS-Kriterien

- Beide Tabs **Live**; A `opusDown > 0` **und** B `pcmDown > 0`.
- **A** sieht Partner-Untertitel mit **chinesischen** Zeichen (`/[\u4e00-\u9fff]/`).
- **B** sieht Partner-Untertitel mit **deutschem** Text (lateinische Wörter).
- **A** zeigt „Partner spricht: Deutsch", **B** „Chinesisch" (automatische
  Zielsprache = Partner-Sprache).
- frische Seite: Sparmodus vorausgewählt, Key-Feld nur nach Wahl von
  „Normal" sichtbar (`browser-fake`);
- A gesamt (up+down) **< 40 kbit/s**; keine `pageerror`; Key in keinem Log.

### Beispiel-Messwerte (ein echter Lauf, 50 s, Key maskiert `AIza…msBM`)

| Tab | up | down | opusDown | pcmDown | Untertitel | kbit/s |
|---|---|---|---|---|---|---|
| A (Opus) | 70 329 B | 29 077 B | 29 077 B | — | 86 | **16** |
| B (PCM)  | 4 491 284 B | 2 232 000 B | — | 2 232 000 B | 86 | 1 076 |

Erstes Partner-Audio: A nach ~4,7 s, B nach ~4,7 s. `/api/rooms` während des
Laufs zeigt beide Peers im selben Raum ohne `target`-Feld:
`[{"lang":"de","transport":"pcm"},{"lang":"zh-Hans","transport":"opus"}]`.

Beispiel-Untertitel (gekürzt):

- **A sieht Partner (B, de→zh):** `[in] Guten Morgen. Heute` · `[out] 早上好。` ·
  `[in] ist ein schöner` · `[out] 今天是个好` · `[in] Tag. Wie geht es`
- **B sieht Partner (A, zh→de):** `[out] Nein, nein, nein, nein,` · `[out] nein. Nein.`

Der Sparmodus liegt im echten Lauf bei **~16 kbit/s** gegenüber ~**1 076 kbit/s**
im Normalmodus — Faktor ~67 weniger Netzdaten. Screenshots: `real-A-opus.png`,
`real-B-pcm.png` in `$HIL_OUT`.
