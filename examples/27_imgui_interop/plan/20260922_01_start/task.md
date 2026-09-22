# Tasks: 27_imgui_interop Rust/C++ ImGui demo (20260922_01_start)

Source: `prompt.txt`. Plan: `plan.md`. Deps: `deps.md`.
Work serially: finish + verify one phase before the next. Each phase ends
with its gate — do not proceed on red.

## Phase 0 — Baseline & FFI spike

- [ ] Read `plan.md` § "Files the implementing agent must read" (at least
  `prompt.txt`, `21_mandelbrot/gen00.lisp`, `extern-block`/`attr` tests).
- [ ] Confirm toolchain: `rustc --version`, `cargo --version`, `sbcl --version`,
  `cmake --version`. Record in commit body.
- [ ] `/tmp` probe: minimal `write-source` emitting `#[repr(C)]` struct +
  `#[no_mangle] pub extern "C" fn` + null-guard; hand `cargo build` as
  `staticlib`. Locks the Lisp idiom (see `deps.md` usage example).
- [ ] Gate: probe compiles; exact `attr`/`space` idiom pasted into `plan.md`
  or `gen.lisp` comments.

## Phase 1 — `gen.lisp` → `rust_logic` staticlib

- [ ] Write `examples/27_imgui_interop/gen.lisp` (≤60-line top-level fns,
  parens checked with `parenmedic`, known-good backup before risky edits):
  `quickload` + `register-local-projects`, `in-package`, `*source-dir*`,
  `write-source` with `` `(do0 ...) `` + `*omit-redundant-parens*`.
  Emits `rust_logic/src/lib.rs` with `AppState { click_count: i32,
  slider_value: f32, checkbox_status: bool }`, `init_app_state()`,
  `process_logic(*mut AppState)` (null-guard, `unsafe` deref, +0.001 step,
  wrap at 1.0).
- [ ] Hand-write `rust_logic/Cargo.toml` (`staticlib`, edition 2021, zero
  deps) — precedent: mandelbrot `Cargo.toml` is hand-written.
- [ ] `sbcl --load gen.lisp` → exit 0.
- [ ] Gate: `cd rust_logic && cargo build && cargo fmt --check && cargo
  clippy --all-targets -- -D warnings` all green.

## Phase 2 — Headless Rust tests (no window)

- [ ] Emit `#[test]`s from `gen.lisp` (no transpiler change; `cfg(test)` via
  string hatch like the 22_summarizer slice): init defaults
  (0 / 0.5 / false), null-pointer no-op, inactive leaves slider, active
  steps +0.001, wrap 1.0 → 0.0.
- [ ] Gate: `cargo test` all green (record count in commit body).

## Phase 3 — C++ + CMake wiring

- [ ] Hand-write `src/main.cpp` from the draft, fixed: real `#include`s,
  `struct AppState { int32_t; float; bool; }` + `static_assert(sizeof == 12)`,
  correct `extern "C"` declarations, ImGui frame loop, `process_logic(&state)`
  per frame, full cleanup.
- [ ] Hand-write `CMakeLists.txt`, fixed: full FetchContent URLs
  (`corrosion-rs/corrosion`, `ocornut/imgui`), pins from `deps.md`
  (Corrosion `v0.6.1`, ImGui `v1.92.9b`), core + backend sources, include
  dirs (root + `backends/`), `corrosion_import_crate` + link
  (`rust_logic`, `glfw`, `OpenGL::GL`).
- [ ] `apt-get install -y build-essential cmake libglfw3-dev libgl1-mesa-dev`
  (plus xvfb runtime libs in Phase 4 as needed).
- [ ] Gate: `cmake -S . -B build && cmake --build build` green.

## Phase 4 — GUI smoke (xvfb) + size note

- [ ] `apt-get install -y xvfb` (+ `libxkbcommon0 libxi6 libx11-6 libgl1`
  if the smoke fails with display/library errors — cf. 02_walkthrough).
- [ ] Gate: `xvfb-run -a timeout 20 ./build/my_interop_app` → exit 124, no
  panic on stderr.
- [ ] Record `ls -lh build/my_interop_app` + `size` output for the
  walkthrough (no tuning yet — measure first).

## Phase 5 — Repo gates & docs

- [ ] `sh /workspace/src/cl-rust-generator/run-tests.sh` green; if new FFI
  forms were needed, add `transpiler-tests.lisp` cases + regenerate
  `SUPPORTED_FORMS.md` (`./generate-docs.sh`).
- [ ] `git status --short`: only intended paths, never `target/`/`build/`.
- [ ] Gate: `cargo test`, `run-tests.sh`, `cmake --build`, xvfb smoke all
  green in one session.

## Phase 6 — Commits + walkthrough

- [ ] Commit in reviewable split per `plan.md` convention
  (`feat`/`test`/`chore`/`docs`, one concern per commit, own paths only).
- [ ] Write `plan/20260922_01_start/walkthrough.md`: what was built,
  test-driven changes, spike findings, string-hatch list, transpiler-gap
  proposals with examples, size numbers, learnings, deferred extensions,
  new Docker programs.
- [ ] Gate: walkthrough exists, all commits pushed/ready, `git status` clean
  of unintended files.
