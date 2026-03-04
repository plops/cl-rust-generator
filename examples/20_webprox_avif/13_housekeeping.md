# Housekeeping Tasks

In addition to the main architectural refactoring, we have identified several general codebase housekeeping tasks that should be addressed during this pass.

## 1. Clean Up Unused Imports & Compiler Warnings
Running `cargo check` revealed a few minor dead code and unused import warnings:
- `core-utils/src/lib.rs`: Unnecessary `mut` variables (`u_data`, `v_data`).
- `cloud-render-srv/src/main.rs`: Unused imports (`image::DynamicImage`, `ExtractedMetadata`).
- `cloud-render-srv/src/browser/mod.rs`: Unused import (`ExtractedLink`).
- `cloud-render-srv/src/session/mod.rs`: Unused import (`handle_raw_image_data`).

**Action**: Resolve all compiler warnings by removing unused imports and dead code.

## 2. Clean Up Stray Test Outputs & Artifacts
The root directory and subdirectories are cluttered with previous benchmark logs, screenshots, and test image outputs that should either be `.gitignore`d or moved to a dedicated outputs directory.
- Root directory has numerous `benchmark_*.log` files, `*.png` screenshots, and `*.avif` files.
- `screenshots/` directory has many old AV1 client test captures.
- `latency_results/` and `logs/` have test artifacts.
- `cloud-render-srv/debug_output.avif` and `test_known_good.avif`.

**Action**: 
- Create a `test_outputs/` directory inside `examples/20_webprox_avif/`.
- Move necessary benchmark/latency logs into `test_outputs/`.
- Delete stray unneeded `.png` and `.avif` screenshots from the root directory that were used for one-off manual testing.
- Update `.gitignore` to explicitly ignore `*.log`, `*.png`, `*.avif`, and `*.img` files that are generated during manual dev testing.

## 3. Formatting
Ensure all rust files conform to standard guidelines.

**Action**: Run `cargo fmt` across all packages in the workspace.

## 4. Dependencies
While the Cargo.toml looks clean and structured as a workspace, we should ensure there are no duplicate or conflicting versions between sub-crates once the refactoring separates out the modules.

**Action**: Verify `Cargo.toml` dependencies for `cloud-render-srv` and `macroquad-client` after the folder split.
