# House Keeping - Rust Build Size Reduction

The `target/` folder can grow exponentially because Rust stores extensive incremental build data and artifacts for every dependency to accelerate future compilations. If left unchecked, even small projects can occupy gigabytes of disk space.

Here is a guide on how to effectively reduce its footprint using modern tools and configuration patterns.

---

## 1. Prune Stale Artifacts
Instead of a full `cargo clean`, which forces a complete recompilation, use specialized tools to remove only what is unnecessary:

- **cargo-sweep**: A command-line tool that deletes build files older than a specified number of days (e.g., `cargo sweep --time 30`) or clears artifacts created by inactive compiler toolchains.
- **cargo-clean-all**: A powerful utility that recursively finds and cleans `target/` directories across your entire disk. It offers options to skip recent projects or those below a certain size threshold.

## 2. Optimize Cargo Profiles
Fine-tune your `Cargo.toml` to minimize the data generated during the development lifecycle:

- **Reduce Debug Information**: Setting `debug = 1` (line tables only) or `debug = 0` (no debug symbols) in your `[profile.dev]` can significantly shrink artifact size with minimal impact on basic debugging.
- **Manage Incremental Compilation**: While `incremental = true` speeds up re-compilation, it generates massive cache files. Disabling it (`incremental = false`) stops this growth at the cost of slower developer builds.
- **Strip Binary Symbols**: For production builds, add `strip = true` to your release profile to remove unnecessary symbol information from the final binaries.

## 3. Share Artifacts Across Projects
Avoid duplicating compiled dependencies by centralizing your build artifacts:

- **Centralized Target Directory**: Set the `CARGO_TARGET_DIR` environment variable to a single global folder (e.g., `~/.cargo/target`). This allows different projects to share compiled crates if they utilize the same versions.
- **sccache**: Use Mozilla's `sccache` as a shared compilation cache. It stores artifacts globally and can even use cloud storage, preventing redundant recompilation across projects.

## 4. Manage Global Cargo Caches
The hidden cache in `~/.cargo` also requires periodic maintenance:

- **cargo clean gc**: Use this native subcommand to perform "garbage collection" on your global cargo home. It removes outdated downloaded crates and metadata that are no longer referenced by any project.

---

## 5. Case Study: `19_webprox` Build Analysis
Following the build of the two main components (`cloud-proxy-server` and `minimal-tui-client`), the `target/` folder reached **1.3 GB** in total size using the `debug = 1` and `incremental = true` profile:

| Directory | Size | Description |
| :--- | :--- | :--- |
| `target/debug/deps` | **1.1 GB** | Intermediate compiled libraries and dependency objects. |
| `target/debug/build` | **61 MB** | Build scripts and their generated outputs. |
| `target/debug/.fingerprint` | **8.0 MB** | Incremental tracking data. |
| **Total `target/`** | **1.3 GB** | Total for the initial development build. |

### Binary Size Observations
Using `debug = 1` (line tables only) instead of the default `debug = 2` results in significantly smaller but still debuggable binaries:
- `cloud-proxy-server`: **118 MB**
- `minimal-tui-client`: **34 MB**

> [!TIP]
> Periodically running `cargo clean gc` along with a tool like `cargo-sweep` can keep your total Rust disk usage within reasonable bounds without sacrificing development speed.
