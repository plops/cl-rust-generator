# deps.md — 20260924_01_automation_tui

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-24; Versionen bei Einführung per
`cargo upgrade` auf neueste heben (auch bei Warnung), danach hier pinnen.

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck |
|---|---|---|---|
| crossterm | crossterm-rs/crossterm | neueste (Prototyp: 0.28; docs.rs-Stand 0.29.0) | TUI: Raw-Mode, Events (Pfeile/`1`/`2`/`a`/`q`), Dashboard-Render, Terminal-Restore |
| x11rb (+ Feature `xtest`) | psychon/x11rb | 0.14.0 | X11-Connect + `get_image` ZPixmap (ROI-Capture) + `xtest::fake_input` (Motion/Button/Key) |
| ort | pykeio/ort | 2.0.0-rc.13 (RC — Drift einplanen) | PP-OCRv6 det-/rec-Sessions, `inputs!`, `TensorRef` |
| PP-OCRv6_small_det_onnx | PaddlePaddle/PP-OCRv6_small_det_onnx (HuggingFace, Apache-2.0) | `inference.onnx` (9,9 MB, aus source5) | DB-Detektionsmodell (`include_bytes!`) |
| PP-OCRv6_small_rec_onnx | PaddlePaddle/PP-OCRv6_small_rec_onnx (HuggingFace, Apache-2.0) | `inference.onnx` (21 MB) + `inference.yml` (`character_dict`) | CTC-Erkennung + Wörterbuch (`include_bytes!`/`include_str!`) |
| Xvfb | system (apt: `xvfb`) | system | Test-Display für Input-/E2E-Smoke (Klick-/Type-Orakel) |
| libx11 (XTEST-Laufzeit) | system (apt: `libx11-6` + X-Server mit XTEST) | system | Laufzeit-Basis für `fake_input`; Fehlen → klare Meldung + `--dry-run` |

DeepWiki-Abfrage-Muster: `psychon/x11rb` (XTEST-/GetImage-API),
`crossterm-rs/crossterm` (Raw-Mode-/Event-API), `pykeio/ort` (Session-API).
Repo-Kontext: `plops/cl-rust-generator`.
NICHT eingeführt (bewusst): `not-fl3/macroquad` (durch TUI ersetzt),
`xdotool` (durch XTEST ersetzt), Wayland-Stack, YAML-Crate (Dict-Parser
bleibt Handcode), `ratatui` (für Tabelle + Log overkill), Bild-/Netz-Crates
(kein Bild-Streaming, lokaler X11-Client).
