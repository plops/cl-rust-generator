❯ miss die reinen quelltext zeilen (ignoriere die tests)

◆ Ran 3 commands · Confirm single test module per file · Verify trailing tests and compute net lines · +1 ✓ · 0.4s · ctrl+o

◆ 2.156 reine Quelltextzeilen (von 2.758 brutto in src/).

  Methode: pro Datei alles ab #[cfg(test)] (jeweils genau ein Block am Dateiende, verifiziert) plus Trenn-Leerzeile abgezogen; tests/ und examples/
  (675 Zeilen) ganz ausgeschlossen.

  • common: 327 (brutto 520) — types 103, framing 109, yuv 102, lib 13
  • server: 1.214 (brutto 1.522) — config 55, capture 118, ocr 419, tiles 115, av1 59, input 147, session 209, lib 23, main 69
  • client: 615 (brutto 716) — config 13, av1 142, net 174, scene 97, app 149, lib 17, main 23

  Testanteil in src/: 589 Zeilen (~21 %). Größte Datei bleibt server/src/03_ocr.rs mit 419 Nettozeilen.
