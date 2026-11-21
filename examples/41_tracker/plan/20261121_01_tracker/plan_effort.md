# plan_effort.md: Aufwand `dnb-tracker` (MVP + Klang + Samples)

Stand: 2026-11-21. Gemessen aus dem Session-Transkript (Summe der
`usage`-Felder aller Modellantworten bis zum Schreiben dieses Dokuments;
die letzten Commit-Runden fehlen, nur wenige Tausend Tokens).

| Kennzahl | Wert |
|---|---|
| Input (ungecacht) | 196 |
| Cached Input (Lesen) | 15 326 165 |
| Cached Input (Schreiben) | 234 384 |
| Output | 154 231 |
| **Total** | **≈ 15,7 Mio.** |
| Modellantworten (Turns) | 96 |
| Tool-Aufrufe | 103 |
| Subagents | 0 (serielle Arbeit in einem Checkout) |
| Wanduhrzeit | ≈ 35 min (Prompt 07:06 → letzter Feature-Commit 07:33, danach Doku) |
| Nutzer-Einwürfe während der Arbeit | 6 (Audio-Frage, Klangkritik, Samples, Synth/Sample-Unterscheidung …) |

## Phasenanteile (geschätzt aus dem Verlauf)

| Phase | Anteil | Bemerkung |
|---|---|---|
| Kontext, API-Verifikation (cpal 0.18, rtrb 0.4 aus Registry-Quellen + DeepWiki) | 10 % | `deps.md` |
| Plan/Tasks | 5 % | |
| Engine, DSP, Audio-Ausgabe, erste Musik | 25 % | erste Wiedergabe auf dem Host nach ≈ 10 min |
| Song-Format, TUI, Tests | 20 % | |
| Klang-Diagnose und Umbau nach Hör-Feedback | 15 % | Spikes + Stems |
| Samples, Slicing, Badge, Mixed-Demos | 15 % | |
| Walkthrough, Abbildungen, Commits | 10 % | |

## Aufwandstreiber

1. Hör-Feedback lässt sich nicht automatisieren. Der Messaufbau (Stems +
   Bandanalyse) machte die Korrektur aber in einer Iteration möglich.
2. Kein Subagent nötig: Das Crate ist klein genug, dass der volle Kontext
   billiger war als Übergaben.
3. Cached Input dominiert (> 97 %): Die Kosten skalieren mit der Zahl der
   Turns, nicht mit der Code-Größe. Weniger, größere Schritte sparen.
