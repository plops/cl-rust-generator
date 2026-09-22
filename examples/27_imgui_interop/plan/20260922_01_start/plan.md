# Plan: Rust/C++ ImGui Interop via `cl-rust-generator` (20260922_01_start)

Goal: demonstrate Rust↔C++ interop with a small Dear ImGui GUI, following the
draft in `prompt.txt`, implemented as `examples/27_imgui_interop/`.
Source of truth for the Rust side is a Lisp generator (`gen.lisp`, pattern:
`examples/21_mandelbrot/gen00.lisp`); C++ (`src/main.cpp`) and
`CMakeLists.txt` are hand-written (C++ is outside the Rust generator's scope).
Target: Linux-first (Ubuntu 26 container), minimal deps, small binary.

Prompt draft bugs (fix during implementation, do not copy verbatim):
`FetchContent_Declare` URLs are truncated (`https://github.com` without repo
path) and `GIT_TAG`s are wrong (`v0.5.0`, `docking`). Correct values are in
`deps.md`. The C++ `extern "C"` block and `target_include_directories` lines
are also malformed in the draft (missing newlines/braces).

## Success criteria

- `examples/27_imgui_interop/gen.lisp` exists, follows the `gen00.lisp`
  pattern (`ql:quickload` + `register-local-projects`, `in-package`,
  `write-source` with `` `(do0 ...) ``), and generates
  `rust_logic/src/lib.rs` via `sbcl` with exit 0.
- Generated `lib.rs` builds as `staticlib` (`cargo build`), is
  `cargo fmt --check` clean and `cargo clippy --all-targets -- -D warnings`
  clean.
- `CMakeLists.txt` configures with Corrosion + FetchContent ImGui and builds
  `./my_interop_app` (GLFW + OpenGL on Linux).
- GUI smoke under `xvfb-run -a timeout 20 ./my_interop_app` reaches the
  event loop (exit 124 = timeout while running, no panic on stderr).
- Headless logic test proves the Rust FFI functions behave (null-pointer
  guard, checkbox auto-advance + wrap) without opening a window.
- `./run-tests.sh` (repo root) still green — no transpiler regression.
- Commits follow Conventional Commits (see § Commit convention).
- `task.md`, `deps.md` in this folder; after implementation `walkthrough.md`
  here (results, test-driven changes, learnings, extensions, new Docker
  programs).

## Context and current facts (verified this run)

### `cl-rust-generator` repo (`/workspace/src/cl-rust-generator/`)

- Shallow Lisp→Rust syntax transformer. `-` → `:`, `--` → `::`; strings pass
  through verbatim (escape hatch for attributes, generics, macros).
  `*omit-redundant-parens*` bound to `t` in generators for readable output.
- `pub` is NOT a form: write `"pub"` as a plain string in front of the item
  (`rs.lisp` errors otherwise, tells you exactly this).
- `#[...]` via `(attr "repr(C)" ...)` / `(attr "no_mangle" ...)` (test
  `attr`, `attr-no-semicolon`). `extern { ... }` block exists (test
  `extern-block`) but `#[no_mangle] pub extern "C" fn` needs the string hatch
  `(space "extern \"C\"")` or equivalent — DeepWiki answer for
  `plops/cl-rust-generator` confirms this pattern; verify with a `/tmp`
  probe before growing `gen.lisp`.
- Raw pointers: `*mut T` / `*const T` have no dedicated form; emit as string
  types, deref via `dot`/`space unsafe`. No `is_null` helper — method call
  via `(dot state (is_null))`.
- `bool` maps directly (Rust `bool` ↔ C++ `bool`, both 1 byte on Linux/x86-64)
  but layout must be guarded with `static_assert(sizeof(AppState) == ...)`
  on the C++ side (see Decisions).
- Toolchain this run: `rustc 1.98.1`, `cargo 1.98.1`, `SBCL 2.6.0.debian`,
  `cmake 4.2.3`. `glfw3` NOT installed (`/usr/include/GLFW` missing,
  `apt-cache search glfw` empty — container has no GLFW yet; `apt-get`
  install needed). `xvfb-run`/`Xvfb` NOT installed.

### Example conventions

- `examples/NN_<name>/genNN.lisp` with `*source-dir*`, `write-source`, Lisp
  helpers (e.g. `lprint`) for repetitive code; `Cargo.toml` is
  **hand-written** (precedent: `21_mandelbrot/mandelbrot/Cargo.toml`), only
  `*.rs` is generated. Keep that split: generate `rust_logic/src/lib.rs`,
  hand-write `rust_logic/Cargo.toml`, `src/main.cpp`, `CMakeLists.txt`.
- Reference generators read this run: `examples/05_imgui/gen00.lisp`
  (imgui-rs pure-Rust path — NOT what we build here, but shows `Context`,
  `Renderer` usage), `examples/08_glfw/gen00.lisp` (GLFW + imgui bindings in
  pure Rust), `examples/21_mandelbrot/gen00.lisp` (canonical `lprint` +
  `*omit-redundant-parens*` + `write-source` pattern to copy).
- Prior plan docs (style model): `plan/20260919_01_blocks/{plan,walkthrough}.md`,
  `plan/20260906_01_rs_summarizer/{implementation_plan,tasks}.md`. xvfb
  know-how: `/workspace/src/rs_disk_treemap/plan/20260918_02_review_and_xvfb_test/walkthrough.md`
  (GUI smoke = `xvfb-run -a timeout 20 <bin>`, success = exit 124;
  container needed `xvfb libxkbcommon0 libxi6 libx11-6 libgl1 libasound2t64`).

### External facts (DeepWiki, this run)

- ImGui (`ocornut/imgui`): needs core sources (`imgui.cpp`,
  `imgui_draw.cpp`, `imgui_widgets.cpp`, `imgui_tables.cpp`) + backends
  (`backends/imgui_impl_glfw.cpp`, `backends/imgui_impl_opengl3.cpp`);
  includes = repo root + `backends/`. Reference example:
  `example_glfw_opengl3`. System pkgs: `libglfw3-dev`, OpenGL dev
  (`libgl1-mesa-dev`).
- Corrosion (`corrosion-rs/corrosion`): `corrosion_import_crate(MANIFEST_PATH
  rust_logic/Cargo.toml)` creates an `INTERFACE` target named `rust_logic`
  (for `crate-type staticlib` an imported static lib); link with
  `target_link_libraries(my_interop_app PRIVATE rust_logic ...)`.
  Stable tags: `v0.6.1` newest, `v0.5.2` latest 0.5.x (draft's `v0.5.0` is
  stale — use newest, see `deps.md`).
- ImGui tags (this run): newest `v1.92.9b` / `v1.92.9b-docking`. Draft's
  `docking` branch floats — pin a tag instead (see Decisions).

## Files the implementing agent must read

| File | Why |
|---|---|
| `examples/27_imgui_interop/plan/20260922_01_start/prompt.txt` | The assignment (draft code + German process requirements). |
| `examples/27_imgui_interop/plan/20260922_01_start/{plan,task,deps}.md` | This plan, serial steps, dependency pins + usage examples. |
| `examples/21_mandelbrot/gen00.lisp` | Canonical generator pattern to copy (`quickload`, `lprint`, `write-source`, `*omit-redundant-parens*`). |
| `rs.lisp` (`emit-rs`, `parse-defun`, `parse-let`, `extern` branch) | What the transpiler can/can't emit; where the string hatch is needed. |
| `transpiler-tests.lisp` (tests `attr`, `extern-block`, `defun-string-parameter`, `defun-typed`) | Copy-pasteable FFI idioms; add new FFI tests here. |
| `SUPPORTED_FORMS.md` | Generated docs; regenerate after new tests (`./generate-docs.sh`). |
| `README.org`, `rust.md` | `pub`-as-string rule, `*const/*mut` for C interop, `write-source` hash behavior. |
| `run-tests.sh`, `generate-docs.sh` | The gates: transpiler suite + docs regeneration. |
| `examples/08_glfw/gen00.lisp` | Prior GLFW/GL setup in generator syntax (concept reference). |
| `/workspace/src/rs_disk_treemap/plan/20260918_02_review_and_xvfb_test/walkthrough.md` | xvfb GUI-smoke recipe (exit 124), Docker package list, EPIPE lesson. |
| `/workspace/src/rs_disk_treemap/plan/20260918_03_transpile_mvp/{plan,task,deps}.md` | Transpile-MVP precedent: `Cargo.toml` hand-written, `src/main.rs` generated + committed, spike-first workflow. |
| `cl-rust-generator.asd`, `package.lisp` | System name for `asdf:system-relative-pathname` in `gen.lisp`. |

## Constraints and non-goals

- Minimal code + minimal deps: no new Cargo dep (the Rust side needs zero
  deps); no `cxx`/`bindgen`/`imgui-rs` unless the spike proves raw FFI
  insufficient (it won't — 3-field POD struct). Every C++ dep must earn its
  place in `deps.md`.
- `gen.lisp` ≤60-line top-level functions, closing parens on their own lines
  where practical; run
  `/workspace/src/parenmedic/zig-out/bin/parenmedic` often; keep a
  known-good backup before risky paren edits (per prompt).
- `rustfmt` for `lib.rs`; `clang-format` (if present, else tidy-by-hand) for
  `main.cpp`; `write-source` rewrites only on hash change.
- Non-goals: pure-Rust imgui (`imgui-rs`/`imgui-wgpu`/GLFW-Rust bindings —
  `05`/`08`/`12` already cover those), macOS/Windows CI, screenshot
  golden-tests, audio, docking persistence.

## Key decisions

1. **Raw C ABI, not `cxx`/`bindgen`.** A 3-field POD struct + 2 functions
   needs no binding generator. Keeps the binary small and the demo legible.
   Rejected: `cxx` (adds build + proc-macro weight for zero benefit here).
2. **`gen.lisp` generates only `rust_logic/src/lib.rs`; `Cargo.toml`,
   `main.cpp`, `CMakeLists.txt` are hand-written and committed.** Follows
   the mandelbrot precedent; the Rust generator owns Rust, not C++/CMake.
   Generated `lib.rs` is committed (reviewable without `sbcl`).
3. **Pin versions, don't float.** ImGui: tag `v1.92.9b` (or `-docking`
   variant only if docking is explicitly wanted — default to plain tag);
   Corrosion: `v0.6.1` (newest per `git ls-remote`, CMake ≥3.15, Rust
   ≥1.46 — both satisfied). Draft's `v0.5.0`/`docking` are rejected as
   stale/floating. Newest-version rule comes from the prompt itself.
4. **Layout guard on both sides.** `(attr "repr(C)" ...)` in Lisp; C++ keeps
   `struct AppState { int32_t; float; bool; }` + `static_assert(sizeof ==
   12)` (4+4+1+3 padding on Linux/x86-64 — assert, don't assume, and
   document). `bool` ABI matches on Linux; note the platform caveat in code.
5. **Null-guard + `unsafe` kept tiny.** `process_logic(*mut AppState)`
   returns early on null (`if (dot state (is_null)) (return)`), derefs in one
   `space unsafe` block. Unit-test the guard + wrap logic headlessly.
6. **Tests without a window first, xvfb smoke last.** Rust `#[test]`s
   (emitted by the generator) cover `init` defaults + `process_logic`
   (null, inactive, active-step, wrap at 1.0). GUI liveness is only
   `xvfb-run + timeout` exit-124 — no pixel goldens.
7. **Binary size is measured, not claimed.** Record `ls -lh` + `size`
   (and `strip` comparison) of `my_interop_app` in the walkthrough;
   `Cargo.toml` gets `lto`/`strip`/`panic=abort`-candidates only if measured
   (staticlib linked into C++ limits what these do — measure first).

## Recommended approach (spike-first, then build)

1. Spike: minimal `/tmp` `write-source` probe emitting `#[repr(C)] struct` +
   `#[no_mangle] pub extern "C" fn` + null-guard; `cargo build` it as
   `staticlib` by hand. Fixes the exact Lisp idiom before `gen.lisp` grows.
2. Skeleton `gen.lisp` → generate `lib.rs` → `cargo build/test/fmt/clippy`
   loop until green.
3. Hand-write `main.cpp` (fixed draft) + `CMakeLists.txt` (fixed URLs/tags);
   `apt-get install` GLFW/GL dev pkgs; `cmake` configure + build.
4. Headless Rust tests green → xvfb GUI smoke → size measurement.
5. Docs + conventional commits in `task.md` order; `walkthrough.md` last.

## Work plan

See `task.md` (serial phases 0–6, each ends with its gate before the next
starts).

## Validation plan

- `sbcl --load gen.lisp`: exit 0, `lib.rs` written/unchanged message.
- `cd rust_logic && cargo build && cargo test && cargo fmt --check && cargo clippy --all-targets -- -D warnings`: all green.
- `sh /workspace/src/cl-rust-generator/run-tests.sh`: green (no regression).
- `cmake -S . -B build && cmake --build build`: green; `./build/my_interop_app`
  smoke under `xvfb-run -a timeout 20`: exit 124, no panic on stderr.
- `git status --short`: only intended paths, never `target/` or `build/`.
- Size note (`ls -lh`, `size`) recorded in walkthrough.

## Risks / rollback

- Transpiler gap on `extern "C"`/`pub` (string-hatch heavy output) →
  fallback is wider hatch + a `transpiler-tests.lisp` proposal in the
  walkthrough; never fork `rs.lisp` for this example.
- No network for FetchContent in CI/container → document; keep a
  `FETCHCONTENT_SOURCE_DIR` override note in `CMakeLists.txt` comments.
- GLFW/GL headers missing in fresh containers → `apt-get install`
  `libglfw3-dev libgl1-mesa-dev` (+ xvfb runtime libs from `deps.md`); record
  exact list in walkthrough as Docker candidates.
- Rollback: one commit per `task.md` phase; revert to last green phase.

## Open questions (for the implementing agent to close, not to block)

- ImGui plain `v1.92.9b` vs `v1.92.9b-docking`: default plain; switch only if
  docking is demoed.
- `docking` needs `imgui_demo.cpp`? No — demo file stays out unless used.
- Release size tuning (`strip`, `-Os`): measure first, decide in walkthrough.

## Commit convention (Conventional Commits, repo practice)

Format: `<type>(27_imgui_interop|plan): <imperative subject>` + body
(What / Why / Tests). Types: `feat` (new code), `test` (tests only),
`docs` (plan/deps/walkthrough), `chore` (deps, CMake, CI). Examples:

- `feat(27_imgui_interop): generate rust_logic staticlib via gen.lisp`
- `test(27_imgui_interop): cover process_logic null/active/wrap headlessly`
- `chore(27_imgui_interop): wire Corrosion+ImGui CMake build`
- `docs(plan): 20260922_01_start plan, tasks, deps`

One concern per commit, own paths only, never `target/`/`build/`.

## Requirements review (did the prompt miss anything?)

Missing in the draft, added by this plan: pinned (not floating) ImGui +
Corrosion versions with full URLs; `static_assert` layout guard; null-guard
test; headless Rust tests (no window needed); xvfb smoke definition
(exit 124); `Cargo.toml` hand-written vs `lib.rs` generated split;
formatting gates (`rustfmt`, `clang-format` if present); binary-size
measurement; `FETCHCONTENT` offline note; Docker package list for the
walkthrough; commit convention. Suggested-but-deferred extensions (for the
walkthrough, not this slice): slider drag test via injected state,
`docking` branch demo, Windows/macOS build notes, `strip`/`lto` tuning.
