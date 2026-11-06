Die drei prägendsten Algorithmen in BUI sind: (1) der **damage-getriebene Software-Rasterizer**, (2) der **Port von Blenders Widget-Zeichen- und Interaktionslogik** (`interface_widgets.cc` / `interface_handlers.cc`) und (3) die **Fuzzy-String-Suche mit Damerau-Levenshtein** samt Expression-Evaluator. Zusammen ergeben sie das Kernversprechen: Blender-Widgets mit Blender-Verhalten, die nur geänderte Pixel neu zeichnen. [1](#0-0) 

---

## 1. Damage-Rendering + paralleler Software-Rasterizer

Das Herzstück: Änderungen markieren nur das betroffene Rechteck, nur veränderte Tiles erreichen den Bildschirm, und ein idle Fenster verbraucht keine CPU. [2](#0-1)  Der Rasterizer zeichnet parallel in Bändern (`BUI_RASTER_THREADS`), alternativ instanced auf D3D11/OpenGL 3.3 — beide Renderer erzeugen pixelidentische Ergebnisse, was der `gpu_parity`-Test prüft. [3](#0-2)  Das macht BUI zu einer retained-mode-Engine, die trotz Software-Rendering praktisch nutzbar ist.

## 2. Port von Blenders Widget-Algorithmen

Das Zeichnen ist ein C-Port von `interface_widgets.cc` (`round_box_edges`, `widgetbase_draw`, Shape-Tabellen), die Interaktion folgt `interface_handlers.cc`. [4](#0-3)  Darin stecken die eigentlich komplexen Algorithmen: Continuous-Pointer-Grab mit Cursor-Wrapping beim Scrubben, Ctrl-Snapping-Regeln, Multi-Field-Drags und der Expression-Evaluator (`expr_pylike_eval.cc`), der `2*pi` oder `x if c else y` in Zahlenfeldern auswertet. [5](#0-4)  Das ist das Alleinstellungsmerkmal: exakt Blenders Look & Feel ohne Blender.

## 3. Fuzzy-String-Suche (Menü-Suche)

`StringSearch` in `core/include/BLI_string_search.hh` bewertet jeden Treffer mit einem Match-Score, filtert Nicht-Treffer und sortiert nach Score, Gewicht, Recency (`RecentCache`) und Heuristiken für Wortgruppen. [6](#0-5)  Die Fehlertoleranz kommt von `damerau_levenshtein_distance` — UTF-8-bewusst, mit Deletion/Insertion/Substitution/Transposition — und `get_fuzzy_match_errors`. [7](#0-6)  Das ermöglicht Blenders Menü-Suche: „ad cu" findet „Add Cube", Tippfehler werden verziehen, kürzlich gewählte Einträge ranken höher. [8](#0-7) 

## Notes

- Ehrenvolle Erwähnungen: der **Slug-Algorithmus** für GPU-Glyph-Rendering (ein Quad pro Glyph, exakte Coverage im Pixelshader) [9](#0-8)  und das CSS-Layout über `taffy` (Flexbox/Grid). [10](#0-9) 
- Die gezeigten Snippets aus `core/` (KD-Tree, SVD in `math_matrix_c.c`, `AtomicDisjointSet`) sind geerbte `blenlib`-Mathematik — komplex, aber nicht das, was BUI als UI-Toolkit definiert. [11](#0-10) 
- `kit/extern/` (libwebp, libjpeg-turbo) ist Third-Party-Code, nicht BUI-eigen. [12](#0-11) 

Wiki pages you might want to explore:
- [Overview (ppmpreetham/BUI)](/wiki/ppmpreetham/BUI#1)

