# Deps: 27_imgui_interop (20260922_01_start)

New/used dependencies with GitHub orgs so later DeepWiki queries are trivial
(`deepwiki <owner>/<repo>`). Rule from prompt: newest version at introduction
time, even on compat warnings. Checked this run via `git ls-remote --tags`
and local toolchain inspection.

| Dep | Org/repo | Version / pin | DeepWiki key | Notes |
|---|---|---|---|---|
| Corrosion (CMake↔Cargo bridge) | `corrosion-rs/corrosion` | `v0.6.1` (newest; latest 0.5.x is `v0.5.2`) | `corrosion-rs/corrosion` | Draft said `v0.5.0` — stale. Needs CMake ≥3.15, Rust ≥1.46 (container: CMake 4.2.3, Rust 1.98.1 — OK). Usage: `corrosion_import_crate(MANIFEST_PATH rust_logic/Cargo.toml)` then `target_link_libraries(app PRIVATE rust_logic ...)`. |
| Dear ImGui | `ocornut/imgui` | `v1.92.9b` (newest plain tag; `-docking` variant `v1.92.9b-docking` only if docking demoed) | `ocornut/imgui` | Draft floated on `docking` branch + truncated URL — pin a tag with full URL `https://github.com/ocornut/imgui.git`. Sources: `imgui.cpp imgui_draw.cpp imgui_widgets.cpp imgui_tables.cpp` + `backends/imgui_impl_glfw.cpp backends/imgui_impl_opengl3.cpp`; includes: root + `backends/`. Reference: `example_glfw_opengl3`. |
| GLFW (system, Linux) | `glfw/glfw` | system `libglfw3-dev` (apt) | `glfw/glfw` | NOT installed in this container (`/usr/include/GLFW` missing). `apt-get install -y libglfw3-dev libgl1-mesa-dev`. CMake: `find_package(glfw3 REQUIRED)`, link `glfw`. |
| OpenGL (system, Linux) | n/a (Mesa) | system `libgl1-mesa-dev` (apt) | — | CMake: `find_package(OpenGL REQUIRED)`, link `OpenGL::GL`. |
| Rust toolchain | `rust-lang/rust` | local `1.98.1` (no pin; staticlib, zero deps) | — | `rust_logic/Cargo.toml`: `crate-type = ["staticlib"]`, edition 2024 (per user request 2026-09-22), empty `[dependencies]`. |
| SBCL (generator host) | `sbcl/sbcl` | local `2.6.0.debian` | — | Runs `gen.lisp`; needs `(ql:register-local-projects)` before `quickload` (repo precedent). |

No new Cargo deps (Rust side stays dependency-free by design). No `cxx`,
`bindgen`, or `imgui-rs` — raw C ABI is sufficient for the 3-field POD struct
(see `plan.md` Decision 1). Runtime/test extras for the container (record
exact list in walkthrough): `xvfb`, plus `libxkbcommon0 libxi6 libx11-6
libgl1` if the smoke hits display/library errors (precedent:
`rs_disk_treemap/plan/20260918_02_review_and_xvfb_test/walkthrough.md`).

## Correct FetchContent (copy-paste, fixed draft URLs/tags)

```cmake
include(FetchContent)
FetchContent_Declare(
  Corrosion
  GIT_REPOSITORY https://github.com/corrosion-rs/corrosion.git
  GIT_TAG v0.6.1
)
FetchContent_MakeAvailable(Corrosion)
FetchContent_Declare(
  imgui
  GIT_REPOSITORY https://github.com/ocornut/imgui.git
  GIT_TAG v1.92.9b
)
FetchContent_MakeAvailable(imgui)
corrosion_import_crate(MANIFEST_PATH rust_logic/Cargo.toml)
```

## Lisp FFI usage example (for `gen.lisp`, verify with `/tmp` probe first)

```lisp
;; #[repr(C)] struct: attr OUTSIDE space-pub (verified: `pub #[repr(C)]`
;; is invalid Rust, `#[repr(C)] pub struct` is correct).
(attr "repr(C)"
 (space "pub"
  (defstruct0 AppState ("pub click_count" i32))))
;; #[unsafe(no_mangle)] pub extern "C" fn: string hatch for the
;; extern prefix and the *mut type (no dedicated forms exist).
;; Edition 2024 requires the `unsafe(...)` attribute form, and the
;; fn body is a bare tail expression (no `return` — clippy
;; needless_return).
(attr "unsafe(no_mangle)"
 (space "pub extern \"C\""
  (defun init_app_state ()
   (declare (values AppState))
   (make-instance AppState :click_count 0))))
;; Unsafe fn with raw pointer: `pub unsafe extern "C"`, deref via
;; (ref-mut (deref state)) inside an inner (space unsafe ...) block
;; (edition 2024 needs unsafe inside AND outside), null-guard via
;; (if (dot state (is_null)) (return)), immutable `let` for the &mut
;; binding, `/// # Safety` docs via plain strings (missing_safety_doc).
```

Implemented exactly so in `gen.lisp` (see `walkthrough.md` for the
test-driven path there). New `transpiler-tests.lisp` cases only if a
genuinely new form is added; hatch-only usage needs no transpiler change.
