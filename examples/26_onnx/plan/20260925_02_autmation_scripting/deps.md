# deps.md — 20260925_02_autmation_scripting

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-25.

**Keine neue Abhängigkeit** in diesem Vorhaben (`-a`-Flag ist Handcode in
einem neuen `00_args`-Modul; die Stapel-Substitution macht `sed` im
Shell-Skript). Gültig bleibt `plan/20260925_01_robust_automation/deps.md`:

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck hier |
|---|---|---|---|
| x11rb (+ `xtest`) | psychon/x11rb | 0.14.0 | unverändert (Capture/Fokus/XTEST) |
| crossterm | crossterm-rs/crossterm | 0.29.0 | unverändert (TUI/Batch/Keymap) |
| ort | pykeio/ort | 2.0.0-rc.13 | unverändert (PP-OCRv6) |
| toml | toml-rs/toml | 0.9.12 | unverändert (hand-gerolltes `Value`-Parsen; generierte Sticker-TOMLs parsen darüber) |
| Xvfb / xterm | system (apt) | system | `-a`-Nachweis im Headless-Lauf (SCHARF vs. AUS, OCR-Kontrolle) |
| bash/sed/timeout/mktemp | system (coreutils) | system | `sticker_batch.sh` — einzige „Abhängigkeiten" des Skripts |

DeepWiki-Abfrage-Muster: keine nötig (keine neue Dep, kein Transpiler-Code
— laut Prompt direkt Rust; Repo-Kontext `plops/cl-rust-generator` daher nur
als Namensraum relevant, nicht als API).
NICHT eingeführt (bewusst): serde/clap/regex (Flag-Parsing bleibt
Handcode wie bisher, ~20 Zeilen), Template-Crates (ein `sed`-Einzeiler
ersetzt `<placeholder>`), neue OCR-/X11-Deps.
`cargo upgrade` entfällt aus demselben Grund: ohne neue Dep kein
Versionswechsel (insb. kein `ort`-RC-Sprung für einen CLI-Schalter).
