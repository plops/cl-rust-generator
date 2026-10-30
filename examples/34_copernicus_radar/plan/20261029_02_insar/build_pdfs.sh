#!/usr/bin/env bash
# ==============================================================================
# build_pdfs.sh — PDF-Generator für 20261029_02_insar (dokumentation.md & walkthrough.md)
# ==============================================================================
#
# Dieses Skript generiert `dokumentation.pdf` und `walkthrough.pdf` im
# aktuellen Ordner (`plan/20261029_02_insar/`) unter Einbindung aller LaTeX-
# Formeln ($...$, $$...$$), Unicode-Box-Drawing-Diagramme und PNG-Abbildungen.
#
# ------------------------------------------------------------------------------
# 1. INSTALLATION DER ABHÄNGIGKEITEN IM DOCKER-CONTAINER
# ------------------------------------------------------------------------------
# Option A (Empfohlen — Primäre Pipeline via Pandoc + XeLaTeX):
#   Führen Sie im laufenden Docker-Container (als root) folgenden Befehl aus:
#
#     export DEBIAN_FRONTEND=noninteractive
#     apt-get update && apt-get install -y --no-install-recommends \
#         pandoc \
#         texlive-xetex \
#         texlive-latex-recommended \
#         texlive-latex-extra \
#         texlive-fonts-recommended \
#         texlive-pictures \
#         texlive-lang-german \
#         lmodern \
#         fontconfig \
#         fonts-dejavu-core \
#         fonts-dejavu-extra \
#         librsvg2-bin \
#         poppler-utils && \
#     fc-cache -f
#
#   Oder rufen Sie dieses Skript direkt mit `--install-deps` auf:
#     ./build_pdfs.sh --install-deps
#
# Option B (Leichtgewichtige Python-Alternative ohne TeX-Installation):
#     pip install markdown pygments pymupdf weasyprint
#
# ------------------------------------------------------------------------------
# 2. AUFRUF & VERWENDUNG
# ------------------------------------------------------------------------------
#   a) Standardaufruf (baut `dokumentation.pdf` und `walkthrough.pdf` in
#      `plan/20261029_02_insar/`):
#        ./plan/20261029_02_insar/build_pdfs.sh
#
#   b) Abhängigkeiten installieren und danach die PDFs bauen:
#        ./plan/20261029_02_insar/build_pdfs.sh --install-deps
# ==============================================================================

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_SCRIPT="${THIS_DIR}/../build_pdfs.sh"

if [ $# -eq 0 ]; then
    exec "${PARENT_SCRIPT}" "${THIS_DIR}"
else
    # Falls nur --install-deps übergeben wurde, installiere Pakete und baue diesen Ordner
    if [ $# -eq 1 ] && [ "$1" = "--install-deps" ]; then
        exec "${PARENT_SCRIPT}" --install-deps "${THIS_DIR}"
    fi
    exec "${PARENT_SCRIPT}" "$@"
fi
