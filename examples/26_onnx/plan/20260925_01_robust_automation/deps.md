# deps.md — 20260925_01_robust_automation

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-25.

**Keine neue Abhängigkeit** in diesem Vorhaben (Matcher + Fokus sind
Handcode auf bestehendem `x11rb`; TOML-Parser bleibt hand-gerollt ohne
serde). Gültig bleibt `plan/20260924_01_automation_tui/deps.md`:

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck hier |
|---|---|---|---|
| x11rb (+ `xtest`) | psychon/x11rb | 0.14.0 | Neu genutzt: `translate_coordinates`, `set_input_focus`, `intern_atom`, `send_event`, `ClientMessageEvent`, `EventMask` (Fokus-Automatik) |
| crossterm | crossterm-rs/crossterm | 0.29.0 | unverändert (TUI/Batch) |
| ort | pykeio/ort | 2.0.0-rc.13 | unverändert (PP-OCRv6) |
| toml | toml-rs/toml | 0.9.12 | unverändert (hand-gerolltes `Value`-Parsen) |
| Xvfb / xterm | system (apt) | system | Fokus-/XTEST-Nachweis im Smoke |

DeepWiki-Abfrage-Muster: `psychon/x11rb` (Focus-/XTEST-Praxis).
Repo-Kontext: `plops/cl-rust-generator`.
NICHT eingeführt (bewusst): Regex-/Fuzzy-Crates (Matcher ist ~60 Zeilen
Handcode), serde (weiter hand-gerollt), WM-Verhandlungs-Deps.
