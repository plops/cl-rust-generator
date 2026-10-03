# Tasks: `source8_transpiled` seriell aufbauen

Jeder Schritt: implementieren → generieren (`sbcl --load gen/gen.lisp`) →
prüfen (Befehl steht dabei) → committen (Regeln in `plan.md`, Abschnitt 5).
Erst weiter, wenn der Schritt grün ist. Baseline: `source7_mvp`
(`cargo test --workspace`: 46 bestanden, 2 ignored).

## T0 — Gerüst und Helfer (kein Rust-Verhalten)

- [ ] `source8_transpiled/gen/00_util.lisp`: `pub_`, `testmod`, `doc`,
      `+key-table+`, `clap-struct`, `err-map`, `write-text-file`;
      `gen.lisp`-Einstieg mit `write-source`-Aufrufen (zunächst nur
      Workspace-`Cargo.toml` + `README.md` + `collect.sh` als Texte).
- [ ] `source8_transpiled/gen/README.md`: SBCL-Aufruf, Dateiübersicht.
- [ ] `source8_transpiled/.gitignore` (`target/`, wie `source7_mvp` … dort
      fehlt er — aus `source6` übernehmen falls vorhanden, sonst neu).
- [ ] Prüfen: `sbcl --load gen/gen.lisp` läuft; `cargo --version`;
      Determinismus: zweimal laufen lassen, `diff -r` identisch.
- [ ] Commit: `feat(source8): generator-gerüst mit util-helfern`.

## T1 — `common`-Crate (Protokoll, Framing, YUV)

- [ ] `gen/common.lisp`: `01_types.rs` (Structs, Daten-Enums als Strings,
      `Rect`-Impl mit `const fn`), `02_framing.rs` (generische
      `encode_msg`/`decode_msg`/`write_msg`, `FrameReader`, Tests),
      `03_yuv.rs` (Funktionen + Tests), `lib.rs`, `common/Cargo.toml`,
      Workspace-`Cargo.toml`.
- [ ] Prüfen: `cargo test -p lbw-common` (9 Tests grün);
      `diff` der erzeugten Dateien gegen `source7_mvp/common`
      (nur `git`-Rauschen erlaubt, sonst begründen).
- [ ] Commit: `feat(source8): common-crate transpiliert`.

## T2 — `server`-Crate, Teil 1 (Config, Capture, AV1, Input)

- [ ] `gen/server_a.lisp`: `01_config.rs` (via `clap-struct`-Helfer),
      `02_capture.rs` (Trait, `scrap`, Tests), `05_av1.rs` (rav1e,
      Tests), `06_input.rs` (Nutzt `+key-table+` für `key_code`!).
- [ ] Prüfen: `cargo test -p lbw-server --lib` für diese Module grün;
      `diff` gegen `source7_mvp` je Datei.
- [ ] Commit: `feat(source8): server-module config/capture/av1/input`.

## T3 — `server`-Crate, Teil 2 (Tiles, Session)

- [ ] `gen/server_b.lisp`: `04_tiles.rs` (BBox, Maskierung, Tests),
      `07_session.rs` (Handshake, Threads, beide `input_loop`-Tests
      vorbereiten), `lib.rs`, `main.rs`, `server/Cargo.toml`.
- [ ] Prüfen: `cargo test -p lbw-server --lib` gesamt grün (26 Tests);
      `diff` je Datei.
- [ ] Commit: `feat(source8): server-module tiles/session/main`.

## T4 — `server`-Crate, Teil 3 (OCR) + Server-Integrationstests

- [ ] `gen/server_c.lisp`: `03_ocr.rs` (Detektor, Erkenner, `sample_colors`,
      `Ocr`, Tests).
- [ ] `gen/server_tests.lisp`: `tests/loopback.rs` (Let-Chains als Strings),
      `tests/models.rs`, `tests/padding.rs`.
- [ ] Prüfen: `cargo test -p lbw-server` (26 lib + 3 loopback, 2 ignored);
      `diff` je Datei.
- [ ] Commit: `feat(source8): server-modul ocr + integrationstests`.

## T5 — `client`-Crate (ohne App-Fenster)

- [ ] `gen/client_a.lisp`: `01_config.rs` (via `clap-struct`),
      `02_av1.rs` (`unsafe`-Decoder 1:1), `03_net.rs` (Reconnect-Thread),
      `04_scene.rs` (Canvas, Tests), `lib.rs`.
- [ ] Prüfen: `cargo test -p lbw-client --lib` (6 Tests);
      `diff` je Datei.
- [ ] Commit: `feat(source8): client-module config/av1/net/scene`.

## T6 — `client`-Crate (App, Main, Probe, Tests)

- [ ] `gen/client_b.lisp`: `05_app.rs` (Nutzt `+key-table+` für
      `send_input`!), `main.rs` (`macroquad::main`), `client/Cargo.toml`,
      `examples/probe.rs`, `tests/loopback.rs`.
- [ ] Prüfen: `cargo test -p lbw-client` (6 lib + 1 main + 1 loopback);
      `diff` je Datei.
- [ ] Commit: `feat(source8): client-app, probe und loopback-test`.

## T7 — Gesamtverifikation und Smoke

- [ ] `gen/texts.lisp` finalisieren: `scripts/smoke_xvfb.sh` (identisch,
      ausführbar), `README.md` (Pfade auf `source8_transpiled` angepasst,
      Generator-Abschnitt), `collect.sh`, `deps.md`-Kopie mit
      Generator-Zeile, `Cargo.lock` via `cargo generate-lockfile`
      (oder Build).
- [ ] Prüfen, alles aus `source8_transpiled/`:
      `cargo fmt --check`, `cargo clippy --workspace -- -D warnings`,
      `cargo test --workspace` (46 + 2 ignored),
      `./scripts/smoke_xvfb.sh` (braucht `xvfb`, `xterm`, Modelle),
      `diff -r` gegen `source7_mvp` (Whitelist: `target/`, `gen/`,
      `Cargo.lock`, `README.md`-Generator-Abschnitt, `out`/`outq`).
- [ ] Commit: `feat(source8): texte, skripte, gesamtverifikation`.

## T8 — Walkthrough und Abschluss

- [ ] `plan/20261003_03_transpiler/walkthrough.md` (Regeln aus dem Prompt:
      deutsch, didaktisch, Fachbegriffe erklärt, Mermaid-Diagramme,
      Code-Beispiele; Inhalt: implementiert / Architektur-Änderungen /
      Learnings+Erweiterungen / Dockerfile-Pakete).
- [ ] `plan/20261003_03_transpiler/deps.md` final prüfen.
- [ ] Letzter Commit: `docs(plan): walkthrough transpiler-migration`.
- [ ] Abschlussmeldung mit Testübersicht und Diff-Ergebnis.
