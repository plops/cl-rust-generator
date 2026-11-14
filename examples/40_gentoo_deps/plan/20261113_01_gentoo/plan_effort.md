# plan_effort.md — Aufwandsschätzung `gentoo-dep-tui`

Stand: 2026-11-13. Abgerechnet wird die Implementierung auf dem Branch
`feature/gentoo-dep-tui`, inklusive aller HIL-Iterationen.

## Token- und Sitzungsbilanz

| Kennzahl | Wert |
|---|---|
| Total Tokens (Session-Abrechnung, Stichtag) | ~100,4 Mio. |
| Input / Output / Cached | nicht separat ausgewiesen |
| Turns | nicht instrumentiert (Größenordnung: mehrere Dutzend Modellrunden) |
| Subagents | 0 — strikt serielle Tasks mit gemeinsamem Checkout, keine parallelen Worker |

Die Token-Zahl stammt aus der Sitzungsabrechnung und umfasst die
gesamte Sitzung; eine phasenscharfe Aufschlüsselung wurde nicht
gemessen. Die Prozentanteile unten sind daher Schätzungen aus dem
Arbeitsverlauf, keine Messwerte.

## Phasenanteile (geschätzt)

| Phase | Anteil | Bemerkung |
|---|---|---|
| Recherche + API-Verifikation (`portage-atom` 0.12, `portage-metadata` 0.11, `ratatui` 0.30) | 15 % | Registry-Quellen gelesen, kein Raten |
| Engine (Module 01–10: Typen, Profil, Manifest, Binhost, Index, Graph, Kaskade, Delta, Plan, Export) | 40 % | Kernlogik + 44 Unit-Tests |
| TUI (Module 12–14: State, View, Runner) | 15 % | inkl. TestBackend-Rendering-Tests (80x24, 120x40) |
| HIL-Fehlersuche (falsche Annahmen aus echten Daten korrigiert) | 25 % | siehe „Echte Befunde" in `walkthrough.md`, Kapitel 2 |
| Gates, Commits, Doku (`walkthrough.md`) | 5 % | `fmt`/`clippy -D warnings`/`test` grün |

## Gelernte Aufwandstreiber

1. HIL statt Mocks war der größte Einzelposten — und jeder Aufwand
   hat sich gelohnt: 10 der 10 Architektur-Korrekturen (Kapitel 2 des
   Walkthroughs) kamen ausschließlich aus echten Daten.
2. Die Tri-State-Umstellung (`UseOverrides`) war der teuerste
   Einzel-Refactor, weil sie Plan-TOML, TUI-State und alle Tests
   gleichzeitig betraf.
3. Der HIL-Regressionstest braucht ~3 Minuten (353-MB-Snapshot);
   das begrenzt, wie oft man ihn „einfach mal" laufen lässt.


_ Worked for 59m 59s · 9:30 AM · 13/Nov

┌────────────────────────────────────────────────────────┐
│  Session usage                                         │
│                                                        │
│    Input      104,822,540                              │
│    Cached     103,327,592                              │
│    Output         355,433                              │
│    Total      105,177,973                              │
│                                                        │
│    Turns               398                             │
│    Subagents          none                             │
│    Observers  239 completed (4,306,987 tokens)         │
│                                                        │
│  Subscription · Muse Code Everyday Usage               │
│    Current        30% used · Resets at 9:36 AM         │
│    Weekly         15% used · Resets Nov 16 at 12:00 AM │
└────────────────────────────────────────────────────────┘
