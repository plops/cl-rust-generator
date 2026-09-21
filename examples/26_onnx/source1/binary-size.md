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

