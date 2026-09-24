# scripts/ — Beschaffung, Download und Browser-Tests für source6

## fetch_assets.sh — Gewichte + Vokabular laden

Lädt Detektions-/Erkennungs-Modell + `inference.yml` von HuggingFace
(Apache-2.0) mit SHA256-Prüfung, idempotent:

```sh
cd examples/26_onnx/source6
./scripts/fetch_assets.sh [ZIEL-VERZEICHNIS]
```

Ohne Argument landen die Dateien in `source6/` (dort erwartet sie der Build
per `include_bytes!`/`include_str!`). Die `*.onnx`-Dateien stehen in
`source6/.gitignore` und werden nie committet — frische Checkouts holen sie
per Skript nach. Verifiziert am 2026-09-24 (Download + Re-Run mit
`ok (cached)`).

Quellen:

- <https://huggingface.co/PaddlePaddle/PP-OCRv6_small_det_onnx>
- <https://huggingface.co/PaddlePaddle/PP-OCRv6_small_rec_onnx>

## test_duckai.sh / test_duckai.py — Duck.ai-Integrationstest

End-to-End-Nachweis „Browser fernsteuern ohne Bildübertragung": echter
Chrome unter Xvfb öffnet <https://duck.ai>, klickt ins „Ask anything
privately"-Feld, tippt „Tell me a joke about programming", klickt „Ask"
(unten rechts) und prüft die Antwort (Anonymitäts-Hinweis + Witz).

```sh
cd examples/26_onnx/source6
./scripts/test_duckai.sh [OUT-DIR]
```

- Klicks laufen über echte CDP-Maus-Events auf DOM-Koordinaten — derselbe
  Pfad (Koordinaten → Klick), den später die source6-XTEST-Automation nutzt.
- Nur Python-Stdlib (eigener minimaler WebSocket/CDP-Client in
  `test_duckai.py`), keine Zusatzabhängigkeiten.
- `OUT-DIR` (Default `/tmp/duckai-test`): Screenshots
  (`01_landing.png`, `02_question_typed.png`, `03_answer.png`),
  `answer.txt`, `chrome.log`, `xvfb.log`.
- Env: `DISPLAY_NUM` (Default `99`), `CDP_PORT` (Default `9222`),
  `CHROME_BIN` (Default `/opt/chrome-test/chrome-linux64/chrome`).
- Je Lauf frisches Browser-Profil (idempotent, kein Vorlauf-Einfluss).
- Exit `0` = PASS (Hinweis + Witz gefunden), `1` = FAIL.

### Voraussetzungen (einmalig, als root)

```sh
apt-get install xvfb
# Chrome-for-Testing (portabel, kein Snap nötig):
ver=$(curl -s https://googlechromelabs.github.io/chrome-for-testing/ \
  last-known-good-versions.json | python3 -c \
  "import json,sys; print(json.load(sys.stdin)['channels']['Stable']['version'])")
mkdir -p /opt/chrome-test && cd /opt/chrome-test
curl -sSL -o chrome.zip "https://storage.googleapis.com/ \
  chrome-for-testing-public/$ver/linux64/chrome-linux64.zip"
unzip -o chrome.zip
# Laufzeit-Bibs (je nach Basis-Image nötig):
apt-get install -y libnss3 libatk1.0-0t64 libatk-bridge2.0-0t64 \
  libcups2t64 libdrm2 libxkbcommon0 libxcomposite1 libxdamage1 \
  libxfixes3 libxrandr2 libgbm1 libpango-1.0-0 libcairo2 libasound2t64
```

Docker-Pakete fürs Image (werden im Walkthrough finalisiert):
`xvfb`, Chrome-for-Testing (s. oben), `ca-certificates` (Modell-Download),
`fonts-unifont` (nur für source5-Viewer nötig, nicht für source6/TUI).

## smoke_xvfb.sh — Rauchtest für das source6-Binary

Startet Xvfb + xterm mit Testtext und beweist in vier Stufen:

1. XTEST-Pfad (ignorierter Rust-Test `xtest_path_against_real_server`).
2. `--dry-run --headless-frames 1`: OCR-Nachweis im Klartext-Mitschrieb
   (`HELLO`/`SECOND` müssen vorkommen; pty-frei, daher CI-geeignet).
3. Fehlerpfade: `--help` → 0, kaputte TOML → 2, fehlendes Display → 1.
4. Artefakte (`typescript`, `tui.png`).

```sh
cd examples/26_onnx/source6
./scripts/smoke_xvfb.sh [BINARY] [OUT-DIR]
```

Hinweis: Das TUI selbst braucht ein Pty und läuft daher nicht in dieser
Umgebung (Hintergrund-Sessions suspendieren Pty-Kinder per SIGTTOU);
`--headless-frames` ist der pty-freie CI-Pfad, das TUI-Render decken
Unit-Tests ab (`08_tui.rs`).

### Bekannte UI-Varianten (im Test abgefangen)

- „Ask anything privately" ist der Platzhalter des Eingabefelds (kein
  Button) → Klick ins Feld.
- Der Absende-Button heißt je nach Session-Zustand „Ask" (Text) oder ist
  ein Icon (Pfeil) → Suche in drei Stufen: exakter Text, `aria-label`
  (`ask|send|submit`), Fallback: Button im Composer rechts unten.
- Nach der Antwort wechselt duck.ai per SPA-Navigation (iframes mit
  `about:srcdoc` im Baum) → Textauslese läuft über die DOM-Domain
  (`getDocument`/`getOuterHTML`), nicht über JS-`innerText`.
