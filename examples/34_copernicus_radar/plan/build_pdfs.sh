#!/usr/bin/env bash
# ==============================================================================
# build_pdfs.sh — PDF-Generator für Architektur-Dokumentationen & Walkthroughs
# ==============================================================================
#
# Dieses Skript konvertiert Markdown-Dokumente (`dokumentation.md` und
# `walkthrough.md`) inklusive LaTeX-Mathematik ($...$, $$...$$), UTF-8/Unicode-
# Box-Drawing-Diagrammen, Code-Blöcken und eingebetteten PNG-Abbildungen in
# druckfertige A4-PDF-Dokumente.
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
#   Oder rufen Sie dieses Skript direkt mit dem Flag `--install-deps` auf:
#     ./build_pdfs.sh --install-deps
#
# Option B (Leichtgewichtige Python-Alternative ohne TeX-Installation):
#   Falls kein TeX-System installiert werden soll, kann eine HTML/CSS-basierte
#   Pipeline über Python (`markdown`, `pygments`, `weasyprint` bzw. `pymupdf`)
#   installiert werden:
#
#     pip install markdown pygments pymupdf weasyprint
#
# ------------------------------------------------------------------------------
# 2. AUFRUF & VERWENDUNG
# ------------------------------------------------------------------------------
#   a) Standardaufruf (konvertiert automatisch alle `dokumentation.md` und
#      `walkthrough.md` unterhalb von `plan/`):
#        ./plan/build_pdfs.sh
#
#   b) Konvertierung eines bestimmten Unterordners (z. B. nur InSAR):
#        ./plan/build_pdfs.sh plan/20261029_02_insar
#
#   c) Konvertierung einzelner Markdown-Dateien:
#        ./plan/build_pdfs.sh plan/20261029_02_insar/dokumentation.md \
#                             plan/20261029_02_insar/walkthrough.md
#
#   d) Abhängigkeiten installieren und danach alle PDFs bauen:
#        ./plan/build_pdfs.sh --install-deps
# ==============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Lokales Arbeitsverzeichnis für temporäre Pandoc/XeLaTeX-Dateien (kein /tmp)
BUILD_TMP_DIR="${SCRIPT_DIR}/.pdf_build_tmp"
mkdir -p "${BUILD_TMP_DIR}"
export TMPDIR="${BUILD_TMP_DIR}"
export TMP="${BUILD_TMP_DIR}"
export TEMP="${BUILD_TMP_DIR}"

cleanup() {
    rm -rf "${BUILD_TMP_DIR}"
}
trap cleanup EXIT

install_deps() {
    echo "[build_pdfs] Installiere Pandoc + XeLaTeX + DejaVu-Fonts via apt-get ..."
    export DEBIAN_FRONTEND=noninteractive
    apt-get update -qq
    apt-get install -y -qq --no-install-recommends \
        pandoc \
        texlive-xetex \
        texlive-latex-recommended \
        texlive-latex-extra \
        texlive-fonts-recommended \
        texlive-pictures \
        texlive-lang-german \
        lmodern \
        fontconfig \
        fonts-dejavu-core \
        fonts-dejavu-extra \
        poppler-utils
    fc-cache -f
    echo "[build_pdfs] Paketinstallation abgeschlossen."
}

usage() {
    sed -n '2,60p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

TARGETS=()
for arg in "$@"; do
    case "$arg" in
        --install-deps)
            install_deps
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            TARGETS+=("$arg")
            ;;
    esac
done

if ! command -v pandoc >/dev/null 2>&1 || ! command -v xelatex >/dev/null 2>&1; then
    echo "[build_pdfs] FEHLER: 'pandoc' oder 'xelatex' wurde nicht gefunden." >&2
    echo "[build_pdfs] Bitte führen Sie './build_pdfs.sh --install-deps' aus oder" >&2
    echo "[build_pdfs] installieren Sie die im Header genannten apt-Pakete." >&2
    exit 1
fi

# Erzeuge LaTeX-Header für sauberes A4-Layout, Unicode-Box-Drawing, Zeilenumbruch
# in langen Codezeilen und automatische Bildskalierung (auch innerhalb Tabellen)
LATEX_HEADER="${BUILD_TMP_DIR}/header.tex"
cat > "${LATEX_HEADER}" << 'EOF'
\usepackage{fvextra}
\usepackage{graphicx}
\usepackage{float}
\usepackage{array}
\usepackage{longtable}
\usepackage{booktabs}
\usepackage{xcolor}

% Automatische Skalierung von Bildern auf Zeilen-/Spaltenbreite und max. 70% Seitenhöhe
\makeatletter
\def\maxwidth{\ifdim\Gin@nat@width>\linewidth\linewidth\else\Gin@nat@width\fi}
\def\maxheight{\ifdim\Gin@nat@height>0.68\textheight0.68\textheight\else\Gin@nat@height\fi}
\makeatother
\setkeys{Gin}{width=\maxwidth,height=\maxheight,keepaspectratio}

% Platzierung von Abbildungen möglichst direkt im Textfluss
\makeatletter
\def\fps@figure{htbp}
\makeatother

% Kompakte Schriftgröße und sanfter Zeilenumbruch für breite Code- und ASCII-Blöcke
\fvset{fontsize=\scriptsize}
\DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines=true,breakanywhere=false,commandchars=\\\{\},fontsize=\scriptsize}
\RecustomVerbatimEnvironment{Verbatim}{Verbatim}{fontsize=\scriptsize}

% Verhindere Überlauf bei langen Inline-Code-Bezeichnern und Tabellen
\emergencystretch=3em
\sloppy
EOF

# Kleiner Pandoc-Lua-Filter:
# 1) Wandelt rohes HTML `<br>` / `<br/>` (z.B. in Markdown-Tabellen) in echte
#    LaTeX-Zeilenumbrüche um.
# 2) Ersetzt selten in Monospace-Schriften fehlende Unicode-Sonderzeichen
#    (z.B. eingekreiste Ziffern ①..⑥ oder ≫ in ASCII-Diagrammen) durch
#    äquivalente darstellbare Zeichen, damit XeLaTeX ohne Font-Warnungen läuft.
LUA_FILTER="${BUILD_TMP_DIR}/filter.lua"
cat > "${LUA_FILTER}" << 'EOF'
local mono_replacements = {
  ["①"] = "(1)",
  ["②"] = "(2)",
  ["③"] = "(3)",
  ["④"] = "(4)",
  ["⑤"] = "(5)",
  ["⑥"] = "(6)",
  ["⑦"] = "(7)",
  ["⑧"] = "(8)",
  ["⑨"] = "(9)",
  ["≫"] = ">>",
  ["≪"] = "<<"
}

local function normalize_mono(s)
  for k, v in pairs(mono_replacements) do
    s = s:gsub(k, v)
  end
  return s
end

function RawInline(el)
  if el.format:match('html') and el.text:match('^<br%s*/?>$') then
    return pandoc.LineBreak()
  end
  return nil
end

function CodeBlock(el)
  el.text = normalize_mono(el.text)
  return el
end

function Code(el)
  el.text = normalize_mono(el.text)
  return el
end
EOF

# Sammle alle zu konvertierenden Markdown-Dateien
MD_FILES=()
if [ ${#TARGETS[@]} -eq 0 ]; then
    while IFS= read -r -d '' f; do
        MD_FILES+=("$f")
    done < <(find "${SCRIPT_DIR}" -type f \( -name "dokumentation.md" -o -name "walkthrough.md" \) -print0 | sort -z)
else
    for t in "${TARGETS[@]}"; do
        if [ -d "$t" ]; then
            while IFS= read -r -d '' f; do
                MD_FILES+=("$f")
            done < <(find "$t" -type f \( -name "dokumentation.md" -o -name "walkthrough.md" \) -print0 | sort -z)
        elif [ -f "$t" ]; then
            MD_FILES+=("$t")
        else
            echo "[build_pdfs] WARNUNG: Pfad '$t' existiert nicht und wird übersprungen." >&2
        fi
    done
fi

if [ ${#MD_FILES[@]} -eq 0 ]; then
    echo "[build_pdfs] Keine 'dokumentation.md' oder 'walkthrough.md' Dateien gefunden."
    exit 0
fi

echo "[build_pdfs] Konvertiere ${#MD_FILES[@]} Markdown-Datei(en) nach PDF ..."

SUCCESS_COUNT=0
for md_file in "${MD_FILES[@]}"; do
    abs_md="$(cd "$(dirname "$md_file")" && pwd)/$(basename "$md_file")"
    md_dir="$(dirname "$abs_md")"
    md_base="$(basename "$abs_md" .md)"
    pdf_out="${md_dir}/${md_base}.pdf"
    rel_md="${abs_md#${SCRIPT_DIR}/}"
    rel_pdf="${pdf_out#${SCRIPT_DIR}/}"

    echo "  -> Baue ${rel_pdf} aus ${rel_md} ..."

    # Wechsel in das Verzeichnis der Markdown-Datei, damit relative Bildpfade
    # (z.B. taiwan_insar_insar_panel.png) direkt aufgelöst werden.
    (
        cd "${md_dir}"
        pandoc "${abs_md}" \
            -o "${pdf_out}" \
            --from="markdown+tex_math_dollars+pipe_tables+raw_html+fenced_code_blocks+backtick_code_blocks+lists_without_preceding_blankline" \
            --pdf-engine=xelatex \
            --include-in-header="${LATEX_HEADER}" \
            --lua-filter="${LUA_FILTER}" \
            --resource-path=".:${md_dir}:${SCRIPT_DIR}" \
            --toc \
            --toc-depth=3 \
            -V lang=de-DE \
            -V documentclass=article \
            -V papersize=a4 \
            -V geometry:"margin=1.8cm" \
            -V fontsize=10pt \
            -V mainfont="DejaVu Serif" \
            -V sansfont="DejaVu Sans" \
            -V monofont="DejaVu Sans Mono" \
            -V monofontoptions="Scale=0.74" \
            -V colorlinks=true \
            -V linkcolor=NavyBlue \
            -V urlcolor=NavyBlue \
            -V toccolor=Black
    )

    if [ -f "${pdf_out}" ] && [ -s "${pdf_out}" ]; then
        size_kb=$(( ($(stat -c%s "${pdf_out}") + 1023) / 1024 ))
        pages="?"
        if command -v pdfinfo >/dev/null 2>&1; then
            pages="$(pdfinfo "${pdf_out}" 2>/dev/null | awk '/^Pages:/ {print $2}' || echo "?")"
        fi
        echo "     [OK] ${rel_pdf} (${size_kb} KB, ${pages} Seiten)"
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    else
        echo "     [FEHLER] ${rel_pdf} konnte nicht erstellt werden!" >&2
        exit 1
    fi
done

echo "[build_pdfs] Fertig: ${SUCCESS_COUNT}/${#MD_FILES[@]} PDF-Dateien erfolgreich generiert."
