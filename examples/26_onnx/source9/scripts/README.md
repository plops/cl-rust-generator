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
