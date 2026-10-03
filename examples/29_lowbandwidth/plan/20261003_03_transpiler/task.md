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

- [x] `gen/server.lisp` (Teil 1): `01_config.rs` (via `clap-struct`-Helfer),
      `02_capture.rs` (Trait, `scrap`, Tests), `05_av1.rs` (rav1e,
      Tests), `06_input.rs` (Nutzt `+key-table+` für `key_code`!).
- [x] Prüfen: `cargo test -p lbw-server --lib` für diese Module grün (12 Tests);
      `diff` gegen `source7_mvp` je Datei (nur Let-Blöcke, Tail-`;`, Guard-Umschreibung).
- [x] Commit: `56fd4ad feat(source8): server_a transpiliert (...)`
      (Datei hieß damals noch `server_a.lisp`, seit T3 in `server.lisp` gemergt).

## T3 — `server`-Crate, Teil 2 (Tiles, Session)

- [x] `gen/server.lisp` (Merge aus `server_a`+`server_b`, eine Datei
      pro Crate): `04_tiles.rs`, `07_session.rs` (flach umgebaut:
      `handshake`/`push_texts`/`mask_text`/`push_tile` + `TileOut`),
      `lib.rs`, `main.rs`, `server/Cargo.toml`.
- [x] Prüfen: `cargo test -p lbw-server --lib` grün (19 Tests);
      `clippy --all-targets -- -D warnings` + `fmt --check` grün;
      Merge-Neutralität: 01/02/04/05/06 + Manifest byte-identisch.
- [x] Commit: `feat(source8): server-module tiles/session/main`.

## T4 — `server`-Crate, Teil 3 (OCR) + Server-Integrationstests

- [x] `gen/server.lisp` erweitert: `03_ocr.rs` (Detektor, Erkenner, `sample_colors`,
      `Ocr`, 7 Tests). Flood-Fill als eigene `flood_component`-Funktion
      herausgezogen (in `source7` in `postprocess` eingelagert).
- [x] `gen/server.lisp` erweitert: `tests/loopback.rs` (Let-Kette als
      äquivalente Schachtelung, kein String), `tests/models.rs`,
      `tests/padding.rs`. Alle drei fast vollständig strukturiert
      (Emit-Probes + `rustfmt`-Gatter pro Idiom).
- [x] Geprüft: `cargo test -p lbw-server` (26 lib + 3 loopback, 2 ignored);
      `clippy --all-targets -- -D warnings` + `fmt --check` grün.
- [x] Commit: `feat(source8): server-modul ocr + integrationstests`.

## T5 — `client`-Crate (ohne App-Fenster)

- [x] `gen/client.lisp` (Teil 1): `01_config.rs` (via `clap-struct`),
      `02_av1.rs` (`unsafe`-Decoder 1:1), `03_net.rs` (Reconnect-Thread),
      `04_scene.rs` (Canvas, Tests), `lib.rs`, `client/Cargo.toml`
      (Manifest bereits hier, sonst baut die Crate nicht).
- [x] Prüfen: `cargo test -p lbw-client --lib` (6 Tests);
      Protokoll gegen `source7` stichprobenhaft verglichen (kein Byte-Diff).
- [ ] Commit: `feat(source8): client-module config/av1/net/scene`.

## T6 — `client`-Crate (App, Main, Probe, Tests)

- [x] `gen/client.lisp` (Teil 2): `05_app.rs` (Nutzt `+key-table+` für
      `send_input`!), `main.rs` (`macroquad::main`),
      `examples/probe.rs`, `tests/loopback.rs`, `lib.rs` um `app`
      erweitern (`client/Cargo.toml` existiert bereits aus T5).
- [x] Prüfen: `cargo test -p lbw-client` (6 lib + 1 main + 1 loopback);
      Protokoll gegen `source7` stichprobenhaft verglichen (kein Byte-Diff).
- [ ] Commit: `feat(source8): client-app, probe und loopback-test`.

## T7 — Gesamtverifikation und Smoke

- [x] `gen/texts.lisp` finalisieren: `scripts/smoke_xvfb.sh` (identisch
      bis auf Aufruf-Kommentar, ausführbar), `README.md` (Pfade auf
      `source8_transpiled` angepasst, Generator-Abschnitt), `collect.sh`
      (byte-identisch), `deps.md`-Kopie mit Generator-Zeile,
      `Cargo.lock` via `cargo generate-lockfile` (bleibt untracked:
      `*.lock` in `.gitignore`, wie `source7_mvp`).
- [x] Prüfen, alles aus `source8_transpiled/`:
      `cargo fmt --check`, `cargo clippy --workspace -- -D warnings`,
      `cargo test --workspace` (46 + 2 ignored),
      `./scripts/smoke_xvfb.sh` (braucht `xvfb`, `xterm`, Modelle).
      Kein `diff -r` als Abnahme (nur Stichproben); Abnahme =
      Protokoll + Tests + Smoke grün.
- [ ] Commit: `feat(source8): texte, skripte, gesamtverifikation`.

## T8 — Walkthrough und Abschluss

- [x] `plan/20261003_03_transpiler/walkthrough.md` (Regeln aus dem Prompt:
      deutsch, didaktisch, Fachbegriffe erklärt, Mermaid-Diagramme,
      Code-Beispiele; Inhalt: implementiert / Architektur-Änderungen /
      Learnings+Erweiterungen / Dockerfile-Pakete).
- [x] `plan/20261003_03_transpiler/deps.md` final prüfen (keine Änderung nötig:
      SBCL 2.6.0 verifiziert, alle 4 Manifeste byte-identisch, keine neuen Crates).
- [x] Letzter Commit: `docs(plan): walkthrough transpiler-migration`.
- [x] Abschlussmeldung mit Testübersicht und Verifikationsergebnis.
