# deps.md — 20260927_01_optics

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-27 (per `cargo add` latest, Rust 1.98.1).

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck hier |
|---|---|---|---|
| serde (+ derive) | serde-rs/serde | 1.0.229 | TOML-Config + JSON-Export (`Deserialize`/`Serialize`) |
| toml | toml-rs/toml | 1.1.6 | `toml::from_str` für `[[surfaces]]`-Config |
| serde_json | serde-rs/json | 1.0.151 | `system.json`-Export (Three.js-Payload) |
| ratatui | ratatui/ratatui | 0.30.2 | TUI-Layout (Inspektor, Loss-Graph, Status) + `TestBackend` für Headless-Tests |
| crossterm | crossterm-rs/crossterm | 0.29.0 | Reales Terminal-Backend für `run_tui` |
| Xvfb | system (apt) | system | Bereits vorhanden; nur Reserve (TUI-Tests laufen über `TestBackend`, brauchen kein Display) |

Repo-Kontext: `plops/cl-rust-generator`.

DeepWiki-Abfrage-Muster: `ratatui/ratatui` (`TestBackend`-Headless-Tests),
`toml-rs/toml` (`toml::from_str` + `[[tables]]`-Arrays).

NICHT eingeführt (bewusst): `nalgebra` (Prompt-Skizze nennt es, aber
`Vector3<Dual>` bräuchte `Scalar`-Trait-Boilerplate; ~60 Zeilen
Hand-`Vec3` sind kleiner und null-Abhängigkeit — siehe
`implementation_plan.md` Entscheidung 1), `clap` (CLI ist
`std::env`-Handparse), `image`/`ndarray` (keine Bilddaten im Kern).
