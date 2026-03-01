The target/ folder grows rapidly because Rust stores extensive incremental build data and artifacts for every dependency to speed up future compilations. 
Stack Overflow
Stack Overflow
Here is how to effectively reduce its footprint:
1. Prune Stale Artifacts 
Instead of a full cargo clean, use specialized tools to remove only what is unnecessary: 
cargo-sweep: Deletes build files older than a certain number of days (e.g., cargo sweep --time 30) or those created by inactive compiler toolchains.
cargo-clean-all: A utility that recursively finds and cleans target/ directories across your entire disk, with options to keep recent or small projects. 
Crates.io
Crates.io
 +3
2. Optimize Cargo Profiles
Modify your Cargo.toml to reduce the amount of data generated during development: 
Reduce Debug Info: Set debug = 1 (line tables only) or debug = 0 (none) in your [profile.dev] to significantly shrink artifact size.
Disable Incremental Compilation: Set incremental = false. This slows down re-compilation but stops the creation of massive incremental cache files.
Strip Symbols: Add strip = true to your release profile to remove unnecessary symbol information from the final binaries. 
Reddit
Reddit
 +5
3. Share Artifacts Across Projects 
Prevent duplicating the same compiled dependencies across multiple projects: 
Centralized Target Directory: Set the CARGO_TARGET_DIR environment variable to a single global folder. This allows different projects to share compiled crates if they use the same versions.
sccache: Use a shared compilation cache that stores artifacts globally, though note it may still copy them into local target/ folders during use. 
The Rust Programming Language Forum
The Rust Programming Language Forum
 +3
4. Manage Global Cargo Caches 
Don't forget the hidden cache in ~/.cargo: 
cargo clean gc: Use this subcommand to perform "garbage collection" on your global cargo home, removing old downloaded crates and metadata. 
Rust Programming Language
Rust Programming Language
