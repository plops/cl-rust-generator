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

- **Tab A — Sparmodus (Opus):** `lang=zh-Hans`, `target=de`, Key leer
  (→ Server-Key). Mikro → WebCodecs-Opus hoch; Partner-Übersetzung als Opus
  runter → WebCodecs-Decoder → Lautsprecher.
- **Tab B — Normal (PCM):** `?gemini=ws://127.0.0.1:8900`, Dummy-Key. Spricht
  „direkt" mit dem Fake, schickt seine PCM24-Übersetzung an den Relay.

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
- keine `pageerror`.

Screenshots landen in `$HIL_OUT` (`fake-A-opus.png`, `fake-B-pcm.png`).

## Beispiel-Messwerte (ein Lauf, 20 s)

| Tab | up | down | opusUp | opusDown | pcmDown | Untertitel | kbit/s (up+down) |
|---|---|---|---|---|---|---|---|
| A (Opus) | 28 931 B | 29 966 B | 28 931 B | 29 966 B | — | 398 | **24** |
| B (PCM)  | 1 841 990 B | 964 800 B | — | — | 964 800 B | 399 | 1 123 |

Der Sparmodus braucht also rund **24 kbit/s** gegenüber ~**1 123 kbit/s** im
Normalmodus — etwa Faktor 47 weniger Daten über das Netz des Mobil-Clients.
(Der Fake drosselt seine Audio-Antwort auf Echtzeit, damit der gemessene
Opus-Downlink realistisch bei ~12 kbit/s liegt.)

## Mit echtem Gemini (T9, manuell)

`scripts/hil-real.sh` (separat) fährt denselben Aufbau gegen das echte Gemini
mit Key aus Datei und prüft die Untertitel-Inhalte (Chinesisch rein → deutscher
Text). Der Key wird nie geloggt.
