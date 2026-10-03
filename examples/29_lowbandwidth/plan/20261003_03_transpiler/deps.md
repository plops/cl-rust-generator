# deps.md — Abhängigkeiten von `plan/20261003_03_transpiler` (GitHub `org/projekt` für DeepWiki)

Stand: 2026-10-03. Die Laufzeit-Abhängigkeiten des erzeugten Codes sind
identisch zu `source7_mvp` (siehe `../../source7_mvp/deps.md`); hier stehen
nur die **zusätzlich für den Transpiler-Ansatz** nötigen Quellen und Werkzeuge.

| Crate/System | Version | GitHub | Wo | Wofür |
|---|---|---|---|---|
| `cl-rust-generator` | HEAD (`rs.lisp`, 181 Tests grün) | `plops/cl-rust-generator` | `gen/*.lisp` (`write-source`, `emit-rs`) | S-Expression→Rust-Transpiler; erzeugt alle `*.rs`-Dateien. DeepWiki: `ask_wiki_question(repoName="plops/cl-rust-generator", question="...")` — genutzt für Usage-Beispiele (`write-source`/`emit-rs`/`do0`/`pub_`-Idiom). |
| `alexandria`, `uiop` | via Quicklisp | `phoe/alexandria`, `fare/asdf` (uiop) | transitiv über `cl-rust-generator.asd` | Einzige Lisp-Abhängigkeiten des Transpilers. |
| SBCL | 2.6.0 (Debian) | `sbcl/sbcl` | Generator-Laufzeit | Führt `gen/gen.lisp` aus; kein Einfluss auf das erzeugte Rust. |
| `cl-cl-generator` (Beispiel 13, Polyglot) | HEAD | `plops/cl-cl-generator` | nur evaluiert, **nicht verwendet** | Alternative (eine DSL → C++/Python/Rust/CL/Go); verworfen, weil das Rust-Backend kein `unsafe`, keine Generics, keine `Result`-Propagation und keine Derive-Makros kann (Beleg: `SUPPORTED_FORMS.md` + `src/backend/rust/items.lisp` dort). |

Erzeugter Rust-Code: keine neuen Crates gegenüber `source7_mvp`
(`serde`, `bincode`, `clap`, `image`, `scrap`, `enigo`, `serde_yaml`,
`rav1e`, `rav1d`, `ort`, `macroquad`, `x11rb` — Details und Systempakete
siehe `../../source7_mvp/deps.md`).

Systempakete für diese Aufgabe (Entwicklungszeit): `sbcl` (mit Quicklisp),
`libxcb1-dev`, `libxcb-shm0-dev`, `libxcb-randr0-dev`, `xvfb`, `xterm`
(letztere vier wie `source7_mvp`).
