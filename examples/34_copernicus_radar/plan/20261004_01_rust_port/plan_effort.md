# Plan-Effort: Rust-Port des Sentinel-1-Dekodierers

Token-Verbrauch der Session, die den Port von
`/workspace/src/copernicus-radar/` (C++) nach
`examples/34_copernicus_radar/` (Rust) erstellt, gegen Echtdaten
validiert und dokumentiert hat. Stand: 4. Oktober 2026, 22:01 Uhr.

## Rohwerte (Session-Ende)

```
Session usage

  Input      20,100,287
  Cached     19,431,707
  Output        149,785
  Total      20,250,072

  Turns               83
  Subagents  5 completed (11,261,874 tokens)

Model: muse-spark-1.3-contributor (max)
Session: 01a108c6-a359-7450-b1f2-4f3b12d02a9e
```

## Einordnung

| Anteil | Tokens | Wofür |
|---|---:|---|
| Subagents (5, Workflow) | ~11,3 Mio. | Erster Port: Inventur, Planung, Implementierung (~3.700 Zeilen), Verifikation |
| Haupt-Session (Rest) | ~9,0 Mio. | Echtdaten-Validierung (631 MB), `baq_mode`-Dispatch-Fix, Refactor (`python`→`header_export`, `demangle` weg), Regressionstests, README, Plan-Dokumente, Walkthrough |
| Davon Cached (Anteil) | ~19,4 Mio. (96 %) | Wiederverwendeter Kontext über 83 Turns |

Faustregel für ähnliche Ports: Grob die Hälfte der Kosten entfällt auf
den maschinellen Erst-Port (Workflow mit Subagents), die andere Hälfte
auf Validierung gegen echte Daten, Korrekturen und Dokumentation. Ohne
den 1,2-GB-Echtdatensatz wäre der `baq_mode`-Fehler (16 von 45.437
Paketen) unentdeckt geblieben — die Validierungs-Hälfte ist kein Luxus.
