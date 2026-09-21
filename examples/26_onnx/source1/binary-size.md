you can easily trace exactly what is bloating your executable using dedicated binary profiling tools. Given your dependencies, your large binary size is heavily driven by ort (ONNX Runtime bindings, which sometimes bundle heavy C++ runtimes or ONNX models via fetch-models), ndarray (heavy mathematical monomorphisations), and image / macroquad (graphics processing engines and decoding tables).
Here are the best ways to analyze and dissect your Rust binary:

## 1. Identify Bloat by Crate and Function (cargo-bloat)
[cargo-bloat](https://github.com/razrfalcon/cargo-bloat) is the most popular tool for this exact issue. It compiles your program in release mode and breaks down size by dependency or function. [1] 

* 
* Install it:

cargo install cargo-bloat

* See which dependencies are taking up space:

cargo bloat --release --crates

 File  .text     Size Crate
45.2%  74.8%  14.0MiB ort_sys
 9.1%  15.0%   2.8MiB [Unknown]
 1.2%   2.0% 387.0KiB std
 0.7%   1.1% 215.0KiB clap_builder
 0.5%   0.8% 159.5KiB miniquad
 0.4%   0.7% 140.4KiB macroquad
 0.4%   0.7% 129.2KiB ureq
 0.2%   0.3%  57.6KiB source1
 0.2%   0.3%  49.9KiB ort
 0.1%   0.2%  36.7KiB x11rb
 0.1%   0.2%  32.3KiB http
 0.1%   0.2%  31.2KiB x11_rb_mq_viewer
 0.1%   0.2%  31.2KiB miniz_oxide
 0.1%   0.2%  29.3KiB ttf_parser
 0.1%   0.1%  20.5KiB pkg_config
 0.1%   0.1%  19.0KiB ureq_proto
 0.1%   0.1%  17.8KiB image
 0.0%   0.1%  12.9KiB x11rb_protocol
 0.0%   0.1%  12.4KiB sha2
 0.0%   0.1%  11.5KiB anyhow
 0.3%   0.5% 104.3KiB And 39 more crates. Use -n N to show more.
60.5% 100.0%  18.7MiB .text section size, the file size is 30.9MiB


* See the top 20 largest functions/symbols:

cargo bloat --release -n 20

* 

## 2. Deep Dive Into Individual Symbols (cargo-binutils)
If you want to view a granular, sorted list of every symbol inside your executable, use [cargo-binutils](https://github.com/rust-embedded/cargo-binutils) to tap into LLVM's underlying size analysis utilities. [2] 

* 
* Install it:

cargo install cargo-binutils
rustup component add llvm-tools-preview

* List and sort all symbols by size:

cargo nm --release -- -size-sort -print-size

* 

The smoking gun is ort_sys, which is occupying 14.0 MiB (74.8%) of your compiled code size. Along with the [Unknown] entries (which are C++ symbols originating from the same place), the ONNX Runtime engine is responsible for roughly 90% of your binary's weight.
Because ort compiles and statically links the entire C++ ONNX Runtime library (onnxruntime) into your Rust binary by default, it bundles massive operators, memory layout dispatchers, and matrix multiplication routines (like the MlasGemm symbols you see in the list).
## How to Fix This## 1. Dynamic Linking (The Ultimate Fix for Size)
Instead of statically bundling the massive ONNX Runtime inside your binary, configure ort to use a shared dynamic library (.so, .dylib, or .dll) already present on the host system. This will shrink your binary down from 30MB+ to just a few megabytes.
Change your Cargo.toml dependency to opt out of the default static strategy:

[dependencies]
ort = { version = "2.0.0-rc.13", default-features = false, features = ["load-dynamic", "fetch-models"] }


* What this does: It completely strips out ort_sys's built-in heavy C++ codebase. Your binary will instead look for an external ONNX Runtime library at runtime via an environment variable (like ORT_DYLIB_PATH=/path/to/libonnxruntime.so).

## 2. Minimize ureq and Network Code
cargo-bloat shows ureq taking up noticeable room. It is pulled in by ort's fetch-models feature to download ONNX files over HTTPS.

* If you manually download your .onnx models and include them via include_bytes!() or load them via standard file paths (std::fs), you can remove the fetch-models feature entirely.
* Dropping it removes ureq, http, sha2, and internal TLS/crypto infrastructure.
