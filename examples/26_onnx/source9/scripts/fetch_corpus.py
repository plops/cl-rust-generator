# /// script
# requires-python = ">=3.10"
# dependencies = []
# ///
"""fetch_corpus.py — lädt Trainingstexte für die Markov-Ketten.

Holt pro Sprache Einleitungen zufälliger Wikipedia-Artikel (MediaWiki-API,
`prop=extracts`, reiner Text) bis ~TARGET Bytes und schreibt sie nach
`corpus/<code>.txt` (ein Absatz pro Zeile). Nur Python-Stdlib.

Aufruf (aus source9/):
    uv run scripts/fetch_corpus.py [--target 150000] [--lang de,fr] [--out corpus]

Lizenz der Texte: CC BY-SA 4.0 (Wikipedia) — deshalb nicht committet
(`corpus/` steht in .gitignore); jeder Checkout lädt neu.
"""

import argparse
import json
import pathlib
import sys
import threading
import time
import urllib.parse
import urllib.request

LANGS = "de fr en es pl ru uk el ja zh ko th ar hi ta".split()
UA = "cl-rust-generator-unicode-ocr/0.1 (https://github.com/plops/cl-rust-generator; wolpumba@gmail.com)"
MIN_EXTRACT = 200  # Stubs (nur Ortsnamen o. ä.) überspringen


def batch(lang: str) -> list[str]:
    """Eine API-Anfrage: bis zu 20 zufällige Artikel-Einleitungen."""
    q = {
        "action": "query",
        "format": "json",
        "generator": "random",
        "grnnamespace": "0",
        "grnlimit": "20",
        "prop": "extracts",
        "explaintext": "1",
        "exintro": "1",
        "exlimit": "20",
    }
    if lang == "zh":
        q["variant"] = "zh-cn"  # vereinfachte Zeichen
    url = f"https://{lang}.wikipedia.org/w/api.php?" + urllib.parse.urlencode(q)
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as r:
        pages = json.load(r).get("query", {}).get("pages", {})
    return [p.get("extract", "") for p in pages.values()]


def fetch_lang(lang: str, target: int, out: pathlib.Path) -> None:
    dest = out / f"{lang}.txt"
    paras: list[str] = []
    size, tries = 0, 0
    while size < target and tries < 200:
        tries += 1
        try:
            extracts = batch(lang)
        except Exception as e:  # Netzfehler: kurz warten, weiter
            print(f"{lang}: {e}", file=sys.stderr)
            time.sleep(5)
            continue
        for ex in extracts:
            if len(ex) < MIN_EXTRACT:
                continue
            for para in ex.split("\n"):
                para = " ".join(para.split())
                if len(para) >= 40:
                    paras.append(para)
                    size += len(para.encode())
        time.sleep(0.5)  # höflich bleiben
    dest.write_text("\n".join(paras) + "\n", encoding="utf-8")
    print(f"{lang}: {size} bytes, {len(paras)} paragraphs, {tries} requests -> {dest}")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--target", type=int, default=150_000)
    ap.add_argument("--lang", default=",".join(LANGS))
    here = pathlib.Path(__file__).resolve().parent.parent
    ap.add_argument("--out", default=str(here / "corpus"))
    a = ap.parse_args()
    out = pathlib.Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    # Eine Sprache = ein Wikipedia-Host → parallel ohne Host zu überlasten.
    ts = [
        threading.Thread(target=fetch_lang, args=(l, a.target, out))
        for l in a.lang.split(",")
    ]
    for t in ts:
        t.start()
    for t in ts:
        t.join()


if __name__ == "__main__":
    main()
