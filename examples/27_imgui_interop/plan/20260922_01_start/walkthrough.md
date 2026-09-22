# Walkthrough: Rust/C++ ImGui Interop (20260922_01_start)

Date: 2026-09-22. Status: implemented, verified, committed.
Scope: `examples/27_imgui_interop/` — `gen.lisp` (source of truth),
generated `rust_logic/src/lib.rs`, hand-written `rust_logic/Cargo.toml`,
`src/main.cpp`, `CMakeLists.txt`, plus this plan folder.

## What was really implemented

1. **`feat(27_imgui_interop): generate rust_logic staticlib via gen.lisp`** —
   `gen.lisp` (`ql:quickload`, `in-package`, `*source-dir*`/`*code-file*`,
   `*omit-redundant-parens*`, `write-source`) emits `rust_logic/src/lib.rs`:
   `#[repr(C)] pub struct AppState` (i32/f32/bool), `#[unsafe(no_mangle)]
   pub extern "C" fn init_app_state`, `#[unsafe(no_mangle)] pub unsafe
   extern "C" fn process_logic` (null-guard, `&mut *state` deref, +0.001
   step, wrap at 1.0, `/// # Safety` docs). `Cargo.toml` hand-written:
   `staticlib`, **edition 2024** (user request), zero deps.
2. **`test(27_imgui_interop): cover process_logic headlessly`** — 5 emitted
   `#[test]`s in `cfg(test) mod tests`: init defaults, null no-op, inactive
   keeps slider, active steps once, wrap at 1.0. No window needed.
3. **`chore(27_imgui_interop): wire Corrosion+ImGui CMake build`** —
   `CMakeLists.txt` (Corrosion `v0.6.1`, ImGui `v1.92.9b`, both pinned with
   full URLs — the draft's truncated URLs and floating `docking` tag were
   fixed), `src/main.cpp` (GLFW+OpenGL3 loop, `static_assert(sizeof ==
   12)` layout guard, full ImGui cleanup).
4. **`docs(plan): 20260922_01_start plan, tasks, deps and walkthrough`**.

Results: `cargo test` 5/5, `cargo fmt --check` clean,
`cargo clippy --all-targets -- -D warnings` clean, `./run-tests.sh`
179/179 (no transpiler regression), `cmake --build` green,
`xvfb-run -a timeout 20 ./build/my_interop_app` → exit 124 (event loop ran;
only benign `XDG_RUNTIME_DIR` warning on stderr, no panic).

## Test-driven changes (deviations from the draft)

- **`unsafe fn` instead of safe fn.** Clippy `not_unsafe_ptr_arg_deref`
  (deny by default) requires it: a null check does not make a raw-pointer
  deref safe (dangling is still possible). C++ callers are unaffected
  (same symbol via `#[unsafe(no_mangle)]`); Rust test callers wrap calls in
  `unsafe` blocks.
- **Edition 2024 fallout (2×).** `#[no_mangle]` → `#[unsafe(no_mangle)]`
  (hard error in 2024); and `unsafe_op_in_unsafe_fn` requires the deref in
  an *inner* `unsafe` block even inside `unsafe fn` — so both levels exist.
- **`needless_return` / `unused_mut`.** Init body is a bare tail expression
  (no `return`); the `&mut` binding uses immutable `let`.
- **`missing_safety_doc`.** `/// # Safety` section added via plain strings
  (mandelbrot precedent for `///` passthrough).
- No transpiler change was needed: everything above is string-hatch +
  existing forms (`attr`, `space`, `ref-mut`, `deref`, `incf`), so no new
  `transpiler-tests.lisp` cases.

## Measurements

- `build/my_interop_app`: 6.5M (debug, unoptimized + debuginfo),
  `size`: text 2139342, data 36424, bss 23280. No tuning applied
  (measure-first per plan; `strip`/release left as extension).

## Learnings

- FFI idiom that works: `(attr ... (space "pub [unsafe] extern \"C\""
  (defun ...)))` — attr outside, `pub` as string head. `pub #[repr(C)]`
  (attr inside) is invalid Rust; order matters.
- Unsuffixed float literals (`1.00e-3` shortest-round-trip form) infer f32
  from context — valid, just unfamiliar-looking. Leave as-is.
- `parenmedic diagnose` flags a false positive on lines containing
  `\"C\"` (escaped quotes confuse its scanner): it reported
  unclosed/extra parens while sbcl loaded and generated fine. sbcl is the
  authority; parenmedic stays a locator, not a gate.
- `XDG_RUNTIME_DIR is invalid` under xvfb is benign GLFW noise, not a
  failure. Fresh containers need `apt-get update` before GLFW exists, plus
  `xvfb libxkbcommon0 libxi6 libx11-6 libgl1` for the smoke.

## Possible extensions (deliberately left out)

- `strip` / release-profile size tuning with before/after numbers.
- Slider-drag headless test via injected state sequences.
- `docking` ImGui variant demo; Windows/macOS build notes.
- `clang-format` gate for `main.cpp` (not installed here — hand-tidied).

## New programs for the Docker container

- Build: `libglfw3-dev libgl1-mesa-dev` (after `apt-get update`).
- Smoke: `xvfb` (+ `libxkbcommon0 libxi6 libx11-6 libgl1` if display or
  library errors appear). Everything else came from the existing
  toolchain (Rust 1.98.1, CMake 4.2.3, SBCL 2.6.0).
