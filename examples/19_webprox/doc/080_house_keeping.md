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
> [!TIP]
> Periodically running `cargo clean gc` along with a tool like `cargo-sweep` can keep your total Rust disk usage within reasonable bounds without sacrificing development speed.
