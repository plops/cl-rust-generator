# Copilot Instructions for `cl-rust-generator`

## Build, Test, and Lint Commands

### Core Common Lisp -> Rust Transpiler (Root)

- **Run full test suite** (transpilation assertions, `rustfmt` syntax checks, and `rustc` differential value tests):
  ```sh
  ./run-tests.sh
  ```
- **Run a single test or tag subset** (note: test s-expressions must be evaluated inside the `:cl-rust-generator` package with `:invert` readtable case):
  ```sh
  # Run tests matching specific tag(s), skipping rustc value tests:
  sbcl --disable-debugger --eval '(ql:register-local-projects)' --load transpiler-tests.lisp \
    --eval '(cl-rust-generator::run-transpiler-tests :tags (list :operator) :value-tests nil)' --quit

  # Run a single named test case:
  sbcl --disable-debugger --eval '(ql:register-local-projects)' --load transpiler-tests.lisp \
    --eval '(cl-rust-generator::run-transpiler-tests :tests (remove-if-not (lambda (tc) (string= (getf tc :name) "gcd-function")) cl-rust-generator::*test-cases*) :value-tests nil)' --quit
  ```
- **Regenerate `SUPPORTED_FORMS.md`** (must be run after adding or modifying entries in `*test-cases*` in `transpiler-tests.lisp`):
  ```sh
  ./generate-docs.sh
  ```
- **Regenerate a Lisp-based example** (`examples/01_gcd` through `13_vulkano`, `21`, `22`, `29/source8_transpiled`):
  ```sh
  cd examples/01_gcd && sbcl --load gen00.lisp --quit
  ```

### `examples/34_copernicus_radar` (Sentinel-1 SAR Decoder & Focusing Pipeline)

Contains three independent crates: `decoder/` (standard Cargo), `sar_focus/` (Stripmap CPU + `cuda-oxide` GPU), and `source2/` (unified SM/IW/EW TOPS CPU + `cuda-oxide` GPU).

- **`decoder/` (`copernicus-radar` crate — standard stable Rust):**
  ```sh
  cd examples/34_copernicus_radar/decoder
  cargo build --release
  cargo test                                    # Full suite (real-data tests skip if ../data/vv/*.dat is absent; override path with S1_DAT)
  cargo test <test_name>                        # Single test
  cargo clippy --all-targets -- -D warnings
  cargo fmt --check
  ```
- **`sar_focus/` and `source2/` (`cuda-oxide` GPU + CPU crates — pinned `nightly-2026-08-28`):**
  Use `cargo oxide` to compile device kernels before host code (`cargo oxide build`/`run` build the optimized release profile by default and do not take `--release`). Use plain `cargo` for clippy and rustfmt:
  ```sh
  cd examples/34_copernicus_radar/sar_focus     # or source2
  cargo oxide build
  cargo oxide test                              # Full synthetic test suite (no dataset required)
  cargo oxide test <test_name>                  # Single test
  cargo clippy --all-targets -- -D warnings
  cargo fmt --check
  ```

## High-Level Architecture

- **Lisp-to-Rust Transpiler (`package.lisp`, `rs.lisp`, `transpiler-tests.lisp`)**:
  - `rs.lisp` is a shallow s-expression-to-Rust syntax transformer (`emit-rs` and `write-source`) with no type-checking or ownership tracking.
  - `write-source` hashes emitted code (`*file-hashes*`) and only writes to disk and invokes `rustfmt` when content changes, preventing unnecessary Cargo rebuilds.
  - `transpiler-tests.lisp` is a 3-tier verification harness and documentation generator: Tier 1 checks normalized string output against `:rust` expectations; Tier 2 pipes `:item t` cases through `rustfmt`; Tier 3 (`*value-tests*`) compiles generated Rust via `rustc` and runs it both fully parenthesized and with `*omit-redundant-parens*` enabled as a differential oracle.
- **`examples/` Layout**:
  - Early examples (`01`–`13`, plus selected later dirs) are generated from `gen*.lisp` scripts calling `write-source`.
  - Later examples (`14`–`37`) are primarily standalone handwritten Rust workspaces/crates (including WebGPU, gRPC/AV1 streaming in `20_webprox_avif`, and `cuda-oxide` GPU pipelines in `32_cuda-rust` and `34_copernicus_radar`).
- **Sentinel-1 SAR Pipeline (`examples/34_copernicus_radar`)**:
  - `decoder/` (`copernicus-radar`): Memory-maps Sentinel-1 Level-0 raw `.dat` files, parses 68-byte CCSDS/SAR headers, reconstructs sub-commutated ancillary/ephemeris data, and decodes FDBAQ (modes 12/13/14), fixed BAQ (3/4/5), and bypass (0) packets into interleaved `Complex32` samples and C-style `%.3g`-formatted CSV reports.
  - `sar_focus/`: Depends on `../decoder` as a library (`copernicus-radar`) to ingest all echoes of the dominant beam, then performs range compression and azimuth focusing via Range-Doppler Algorithm (RDA) or Time-Domain Backprojection (TDBP) on either multi-core CPU (`rustfft`) or NVIDIA GPU (`cuda-oxide` kernels in Rust + minimal `extern "C"` cuFFT FFI in `08_cufft.rs` linked via `build.rs` using `CUDA_ROOT`/`CUDA_PATH`).
  - `source2/` (`sar_source2`): Self-contained processor extending CPU/GPU focusing to TOPS modes (IW/EW burst focusing, debursting, and multi-swath mosaicking) alongside Stripmap.

## Key Conventions

### Lisp Transpiler (`rs.lisp`)

- **Symbol hyphen-to-colon rule**: Any `-` in a Lisp symbol is emitted as `:`. Consequently, `--` emits `::` (e.g., `std--sync--Arc--new` -> `std::sync::Arc::new`), and Rust snake_case identifiers in Lisp source **must** use `_`, never `-`.
- **String escape hatch**: Plain Lisp strings (`"&mut self"`, `"#[derive(Clone)]"`) are emitted verbatim without symbol translation (`--` stays `--`). For Rust string literals, use `(string "...")`, `(string-r "...")`, or `(string-b "...")`.
- **Readtable case inversion & package dispatch**: Loading `rs.lisp` globally sets `(setf (readtable-case *readtable*) :invert)` so `CamelCase`, `Ok`, and `Some` preserve case while lowercase Lisp symbols map to lowercase Rust. Additionally, `emit-rs` dispatches with `CASE` on symbols in `:cl-rust-generator`—forms read in another package will silently fall through to function-call emission.
- **Lisp reader escaping**: Because `|` is the Common Lisp multiple-escape character, the bitwise OR-assign operator must be written `(\|= x mask)` in Lisp source, never `(|= x mask)`.
- **Mutability & reference declarations**: `let` binds immutably; `let*` binds mutably (`let mut`). In `defun` parameters, prefixing a parameter name with `&` in both the lambda list and `(declare (type T &x) (mutable &x))` emits `x: &mut T` (whereas `(mutable b)` without `&` emits `mut b: T`). Leading string literals in `defun`/`defun-async`/`lambda` bodies are stripped as Common Lisp docstrings unless wrapped in `(stmt ...)` or `(expr ...)`.
- **Semicolons in blocks**: `do0` terminates statements with `;` (except strings and block/item heads in `*keywords-without-semicolon*`). `progn` omits the trailing semicolon on its final form for Rust's implicit block return; `block` keeps it. Use `(stmt form)` or `(expr form)` when `(space ...)` escape hatches need explicit semicolon control.
- **No backward compatibility for removed forms**: Deprecated forms (`slice`, `cast`, `string#`, `&`, `^`) error loudly; always use `range`, `coerce`, `string-r`, `logand`, and `logxor` (see `MIGRATION.md`). Never edit `SUPPORTED_FORMS.md` by hand—edit `*test-cases*` in `transpiler-tests.lisp` and run `./generate-docs.sh`.

### Rust / CUDA SAR Crates (`examples/34_copernicus_radar`)

- **Numbered data-flow module files**: In `sar_focus/src/` and `source2/src/`, modules are numbered in pipeline execution order (`01_types.rs`, `02_meta.rs`, `03_ephem.rs`, ...) and mapped to clean module names in `lib.rs` using `#[path = "01_types.rs"] pub mod types;`. Maintain this numbering and `#[path]` pattern when adding modules.
- **Out-of-place GPU kernels**: `cuda-oxide` kernels (`09_kernel.rs` / `09_gpu.rs`) write through `DisjointSlice` and cannot borrow the same `DeviceBuffer` as `&` and `&mut` simultaneously. Elementwise GPU stages operate out-of-place (`src` -> `dst` followed by buffer swap) using 1D launch grids with 256 threads per block.
- **CPU-as-oracle verification**: Every GPU pipeline stage has a matching CPU reference implementation and is verified against it in unit/integration tests (typically bounding peak-normalized max relative error to `< 1e-3`).
- **Bilingual CLI/docs convention**: In `sar_focus` and `source2`, CLI help, error strings, code comments, and `plan/` walkthroughs are written in German, while `README.md` files are in English.
