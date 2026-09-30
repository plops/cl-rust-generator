# scripts/ — Beschaffungs- und Test-Skripte für source9

Alle Skripte aus dem `source9/`-Verzeichnis aufrufen.

## fetch_models.sh

```sh
./scripts/fetch_models.sh [ZIEL-VERZEICHNIS]   # Default: source9/models/
```

Lädt die PaddleOCR-ONNX-Modelle (Detektion PP-OCRv6 small, Erkennung
PP-OCRv6 small universal plus PP-OCRv5-mobile für latin, eslav, el, korean,
th, arabic, devanagari, ta) von HuggingFace — gepinnter Commit, SHA256-Prüfung,
idempotent (2. Lauf: `ok (cached)`). Prüft zusätzlich, ob GNU Unifont
installiert ist (`apt-get install fonts-unifont`, als root automatisch).
~96 MB, nie committet (`models/` in `.gitignore`).

## fetch_corpus.py

```sh
uv run scripts/fetch_corpus.py [--target 150000] [--lang de,fr] [--out corpus]
```

Sammelt pro Sprache ~150 kB Einleitungen zufälliger Wikipedia-Artikel
(MediaWiki-API, reiner Text, nur Python-Stdlib, eine Sprache pro Thread,
0,5 s Pause pro Anfrage) nach `corpus/<code>.txt`. Daraus lernt das Programm
beim Start Wortlisten und Markov-Ketten. Texte sind CC BY-SA → nicht
committet. Dauer: ~4 min für alle 15 Sprachen. Fehlt der Korpus, fallen die
Generatoren `words`/`markov` auf Pangramme zurück.

## smoke_xvfb.sh

```sh
./scripts/smoke_xvfb.sh [BIN] [OUT-DIR]   # Defaults: target/debug/unicode_ocr, /tmp/unicode-smoke
```

Xvfb-Rauchtest für das interaktive Fenster: startet das Binary unter einem
virtuellen X-Server (Software-GL), schickt Tasten per `xdotool --window`
(`Right G V V M Up Space N`), macht Screenshots der Modi Text/Boxen/nichts
(`text.png`, `boxes.png`, `hidden.png`) und beendet per gehaltenem `Escape`.
Erwartet Exit 0 und den Markdown-Report auf stdout (`stdout.log`).
Braucht `xvfb`, `xdotool`, `scrot` (`apt-get install xvfb xdotool scrot`).
Der `BadWindow`-Fehler beim letzten `keyup` ist harmlos: das Fenster ist
dann bereits zu (App beendet während des gehaltenen `Escape`).
