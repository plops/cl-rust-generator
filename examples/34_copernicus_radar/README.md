# copernicus-radar (Rust port)

Sentinel-1 synthetic-aperture-radar raw space-packet decoder, ported from the
C++14 project
[plops/copernicus-radar](https://github.com/plops/copernicus-radar)
(`source/*.cpp`, ~7000 lines) to idiomatic Rust.

The program memory-maps a Sentinel-1 level-0 raw `.dat` file, collects the
space-packet headers, accumulates sub-commutated ancillary data, histograms
the calibration and signal packets, selects the elevation beam with the most
data, decodes its echoes into a complex range image, and reports per-packet
details to CSV files.

Signal packets dispatch on `baq_mode`: 12/13/14 carry per-block bit-rate
codes (FDBAQ), 3/4/5 are fixed-rate BAQ, and 0 is raw bypass; calibration
packets use bypass. Validated end to end on a real S1C SM S6
`SDV` product (`data/`, VV: 44,901 echoes + 16 noise packets
+ 520 calibration packets, zero decode failures).

## Build and run

```sh
cargo build --release
./target/release/copernicus-radar input.dat
./target/release/copernicus-radar input.dat --csv-dir out --cf-dir out \
    --max-echoes 512 --export-headers
./target/release/copernicus-radar input.dat --dump-headers  # module 03
```

Without arguments the program uses the default path hard-coded in the C++
version. `cargo test` runs the unit tests, binary tests and end-to-end tests
that decode synthetic packets and verify the CSV/`.cf` outputs, plus
real-data regression tests (`tests/real_data.rs`) that decode packets from
`data/` when the dataset is present (skipped otherwise).

## Outputs

| File | Content |
| ---- | ------- |
| `o_anxillary.csv` | Decoded ancillary blocks (file name keeps the C++ typo) |
| `o_range.csv` | One row per decoded signal echo of the selected beam |
| `o_cal_range.csv` | One row per decoded calibration packet |
| `o_range<n0>_echoes<e>.cf` | Complex range image, little-endian `f32` pairs |
| `o_cal_range6000_echoes<c>.cf` | Complex calibration image |
| `o_packet_header.csv` | Full header table (`--export-headers` only) |

Floats in the CSV files use C `%.3g` formatting (3 significant digits), matching
the C++ output stream; the formatter is fuzz-checked against the C library
semantics on 200k random values in the test suite's oracle table.

## Module map

| Rust | C++ |
| ---- | --- |
| `src/main.rs` | `copernicus_00_main.cpp` (pipeline, CSV/`.cf` reports) |
| `src/mmap.rs` | `copernicus_01_mmap.cpp` (via `memmap2`) |
| `src/collect_headers.rs` | `copernicus_02_collect_packet_headers.cpp` |
| `src/process_headers.rs` | `copernicus_03_process_packet_headers.cpp` (`--dump-headers`) |
| `src/decode_packet.rs` | `copernicus_04_decode_packet.cpp` (FDBAQ, BRC/Huffman) |
| `src/decode_type_ab.rs` | `copernicus_05_decode_type_ab_packet.cpp` (bypass) |
| `src/sub_commutated.rs` | `copernicus_06_decode_sub_commutated_data.cpp` |
| `src/decode_type_c.rs` | `copernicus_07_decode_type_c_packet.cpp` (BAQ 3/4/5) |
| `src/header_export.rs` | `--export-headers` CSV (replaces the C++ embedded Python shell) |
| `src/header.rs` | the 54 header fields decoded in `main` |
| `src/tables.rs` | reconstruction tables B/NRL/SF/A/NRLA (machine-extracted) |
| `src/utils.rs` | `utils.h` + `consume_padding_bits` + `%.3g` formatting |
| `src/error.rs` | typed errors for every `assert(0)` / `out_of_range` site |
| `src/lib.rs` (`State`) | `globals.h` (owned state instead of a global) |

## Deliberate deviations from the C++

* **Decoded samples are actually stored.** The C++ decoders take an output
  pointer but never write through it, so the `.cf` files contain
  uninitialized heap memory. This port interleaves the IE/IO/QE/QO channels
  into complex samples (even: IE+i·QE, odd: IO+i·QO) and writes those.
* **No heap overflows.** The C++ caps the image at 512 echoes but keeps
  writing past the allocation when more echoes arrive; the fixed
  `brcs[205]`/`thidxs[205]` arrays overflow past 26240 quads; and an empty
  selection allocates a negative-sized array. This port bounds-checks every
  write (`--max-echoes`, default 512, limits stored echoes while still
  decoding and reporting the rest) and grows the code tables dynamically.
* **Deterministic report order.** Histograms iterate sorted keys instead of
  `unordered_map` order.
* **Dead code is reachable.** `init_process_packet_headers` and the
  header-table export are never called by C++ `main`; here they are public
  API (`--dump-headers` without the 16 ms animation delay unless `--animate`
  is given). The type-C decoders are wired into the pipeline: C++ `main`
  routes BAQ 3/4/5 signal packets through FDBAQ and fails on them, this port
  dispatches on `baq_mode` and decodes them.
* **Embedded Python → CSV export.** The pybind11/IPython shell becomes
  `--export-headers`, writing the same column table to
  `o_packet_header.csv`. The per-error table snapshot in the `catch` handler
  is preserved in `State::packet_header`.
* **Ancillary tail is zero, explicitly.** Only 130 of the 142 struct bytes
  arrive over the wire; the C++ reads whatever its zero-initialized global
  holds. This port zero-pads and parses the words as little-endian.
* **Errors instead of aborts.** Truncated files, bad sync markers and
  unsupported BAQ modes return typed errors (exit 1); per-packet decode
  failures log, snapshot the header table and continue, mirroring the
  `catch (std::out_of_range)` in C++ `main`.
* **`.cf` fallback.** When `--cf-dir` (default `/dev/shm`) is missing or not
  writable, images are written next to the CSV files with a warning.

## Dependencies

`memmap2` (mmap), `num-complex` (samples), `tempfile` (tests only).
