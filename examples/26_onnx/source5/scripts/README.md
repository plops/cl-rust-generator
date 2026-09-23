# scripts/ — Test- und Beschaffungs-Skripte für source5

## fetch_assets.sh

Lädt Detektions-/Erkennungs-Modell + `inference.yml` (mit SHA256-Prüfung,
idempotent) und stellt die Unifont-Schrift sicher (per `apt-get install
fonts-unifont` als root, sonst Hinweis).

```sh
cd examples/26_onnx/source5
./scripts/fetch_assets.sh [ZIEL-VERZEICHNIS]
```

Ohne Argument landen die Dateien in `source5/` (dort erwartet sie der Build
per `include_bytes!`/`include_str!`). Die `*.onnx`-Dateien stehen bewusst
in `source5/.gitignore` und werden nie committet — frische Checkouts holen
sie per Skript nach. Die Schrift ist reine
Laufzeit-Abhängigkeit und wird an den Pfaden aus `05_overlay.rs` gesucht.

## smoke_xvfb.sh

Rauchtest unter virtuellem X-Server: startet Xvfb, zeigt Testtext in einem
xterm, fährt den Viewer hoch, simuliert Pfeiltasten + `1`/`2` + `Escape` und
sichert zwei Screenshots plus OCR-Log.

```sh
cd examples/26_onnx/source5
./scripts/smoke_xvfb.sh [BINARY] [OUT-DIR]
```

- `BINARY` (optional): Pfad zum Viewer, Default `target/debug/x11_ppocrv6`.
  Für realistische Frame-Zeiten den Release-Build nehmen:
  `cargo build --release`, dann
  `./scripts/smoke_xvfb.sh target/release/x11_ppocrv6`.
- `OUT-DIR` (optional): wohin Screenshots/Logs gehen, Default `/tmp/ocr-smoke`.
- `DISPLAY_NUM` (optional, Env): Xvfb-Display, Default `99`.

### Voraussetzungen (einmalig, als root)

```sh
apt-get install xvfb xdotool xterm x11-apps scrot fonts-unifont
cargo build   # im source5-Verzeichnis (lädt zusätzlich die
              # ONNX-Runtime-Bibliothek herunter)
```

Die PP-OCRv6-Modelle (`*.onnx`) und `inference.yml` liegen im Repo;
`fonts-unifont` liefert `/usr/share/fonts/opentype/unifont/unifont.otf`,
das der Viewer zur Laufzeit sucht (Suchliste in `05_overlay.rs`).

### Auswertung

- Exit-Code `0`: `Escape` wurde empfangen und das Programm sauber beendet.
- `before.png`/`after.png`: Viewer-Fenster mit HUD vergleichen —
  `after.png` muss eine verschobene ROI (`ROI …@200,120`) und Stufe `480`
  zeigen sowie die eingeblendete Tasten-Hilfe am unteren Rand.
- `stdout.log`: enthält `HELLO`/`SECOND`-Treffer vom xterm-Testtext
  (OCR-Nachweis; exakte Schreibweise variiert — PP-OCRv6-small lahmt bei
  Deko-Schrift, erkennt Druckschrift aber robust).
- Hinweis: Das Viewer-Fenster liegt ohne Window-Manager bei (0,0) und damit
  innerhalb seiner eigenen Capture-Region — es OCRt sich teilweise selbst
  (Feedback). Das ist ein Artefakt des fensterlosen Test-Setups, kein
  Produktfehler: Für den OCR-Nachweis zählt der erste Frame mit xterm-Text.
- Tasten-Flakiness: Synthetische Einzel-Taps (`xdotool key`) können bei
  langsamen Debug-Frames in eine Frame-Lücke fallen und verloren gehen
  (Pfeile/`1`/`2` ggf. wiederholen, `Escape` wird gehalten). Mit
  Release-Build (schnelle Frames) und echter Tastatur tritt das nicht auf:
  `is_key_pressed` ist eingerastet, Pfeiltasten sind für Halten ausgelegt.
