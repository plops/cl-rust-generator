# sar_focus — program 2: Stripmap SAR focusing (CPU reference + GPU pipeline)

> Part of `34_copernicus_radar`: start at the [overarching README](../README.md)
> (how to download a dataset, how the two programs fit together). This crate
> reuses the sibling [`decoder`](../decoder/) (`copernicus-radar`) as a
> library for packet headers, mmap and FDBAQ decoding.

`sar_focus` turns Sentinel-1 level-0 raw echoes into a focused radar image:
it decodes **all** echoes of the strongest elevation beam, aligns them on a
physical-time raster, estimates the Doppler centroid from the data
(Clutterlock on range-compressed data), range-compresses the chirps (matched
filter) and azimuth-focuses — then writes the complex image plus a quicklook
and a point-target report.

Two focusing methods, each in two backends:

| Method | Idea | Backend |
|---|---|---|
| RDA (Range-Doppler Algorithm) | FFT-based Doppler filtering + phase-only RCMC (Range-Cell-Migration Correction) | CPU (`rustfft`, multi-core) and GPU (cuFFT + `cuda-oxide` kernels) |
| TDBP (Time-Domain Backprojection) | Geometrically exact pulse-by-pulse summation (f64), no approximations | CPU (parallel) and GPU |

RDA is the workhorse (full frame in ~11 s on GPU); TDBP is the referee that
validates RDA on windows. Every GPU computation is checked against the plain
CPU reference (peak-normalized max relative deviation ~1e-7 at a 1e-3 bound),
and the decoder + CPU RDA were validated bit-near against the Python
`sentinel1decoder` and an independent NumPy RDA.

Note: the CLI prints German help and diagnostics; the walkthroughs
(`../plan/`) are German too. This README is in English.

## Build and test

All commands run from this directory (`sar_focus/`). `cargo oxide` is the
`cuda-oxide` wrapper that builds the Rust CUDA kernels before the host code
(pinned nightly in `rust-toolchain.toml`); plain `cargo` is used for
lint/format, which have no `oxide` counterparts. `cargo oxide build`/`run`
have no `--release` flag — they build the optimized release profile by
default (note the `Finished 'release' profile` line).

```sh
cargo oxide build                    # CPU code + GPU kernels
cargo oxide test                     # 48 tests, synthetic data, no dataset needed
cargo clippy --all-targets -- -D warnings
cargo fmt --check
```

Requirements for the GPU path: NVIDIA GPU + CUDA toolkit (cuFFT),
`libclang-dev` (for the build), `cargo-oxide`. Everything also runs with
`--cpu` on any machine.

## Distributable binary (copy to another machine)

The kernels are embedded in the binary (`cuda_module!`), so distribution is
one file plus its runtime libraries:

```sh
# Bake the kernels for the target GPU architecture into the binary,
# so it needs no JIT compiler (libNVVM/nvJitLink) at runtime.
cargo oxide build --arch sm_86 --materialize-cubin
ls -la target/release/sar_focus   # ~3 MB, this is the file to copy
```

Use the target machine's compute capability for `--arch` (here `sm_86` =
RTX A4000; find it via `nvidia-smi --query-gpu=compute_cap --format=csv`).
Without `--materialize-cubin` the binary embeds portable NVVM IR instead and
JIT-compiles at startup, which requires the CUDA toolkit's compiler
libraries on the target — the cubin build avoids that.

The target machine (x86_64 Linux) needs, besides the binary:

- An NVIDIA driver with a CUDA-capable GPU (the driver API is loaded at
  runtime — no toolkit install needed for that part).
- `libcufft.so.12` (CUDA toolkit library, linked by `build.rs` — the only
  non-system entry in `ldd target/release/sar_focus`). Either install the
  CUDA toolkit there, or copy `libcufft.so.12` next to the binary and set
  `LD_LIBRARY_PATH`.

Check with `ldd target/release/sar_focus` and a smoke run
(`./sar_focus meta <file.dat>`, then a small `--cpu` or GPU `focus` window)
after copying.

## Usage

```sh
DAT=../data/vv/s1c-s6-raw-s-vv-20260929t214300-20260929t214327-009667-0133f4.dat
```

**`meta` — diagnose a file without decoding.** Start here with every newly
downloaded dataset: packet/echo census, PRF, chirp parameters, slant range,
orbit blocks.

```sh
cargo oxide run -- meta "$DAT"
# Packets: 45437, imaging echoes (FDBAQ): 44901, PRF 1663.48 Hz, ...
```

**`focus` — the full run: decode → focus → image + quicklook + ship report.**

```sh
# Full frame on the GPU (reference file: ~28 s decode + ~11 s focus)
cargo oxide run -- focus "$DAT" /tmp/s1_image --compare
# Window on the CPU (2,048 echoes, seconds, ~2-3 GB RAM)
cargo oxide run -- focus "$DAT" /tmp/s1_window --cpu --az0 4000 --az1 6048
```

Flags: `--cpu` (CPU reference instead of GPU), `--az0 N --az1 M` (echo
window; < 2,048 echoes warns about aperture truncation), `--chunk C
--overlap O` (GPU tiling, default 8192/2048, overlap-save), `--compare`
(CPU↔GPU deviation on a 2,048-echo slice, ~2.6e-7), `--no-rcmc` (diagnose
without migration correction).

Writes `<prefix>.cf` (complex image, raw little-endian `f32` pairs,
row-major; full frame 44,901 × 17,634 after trimming the 2,397-column
cyclic-convolution wrap margin, 6.33 GB), `<prefix>.png` (dB quicklook with
percentile stretch, robust against RFI jammer lines), an ASCII preview and a
console report (azimuth profile, RFI-masked rows, top peaks per quarter with
FWHM in pixels/meters vs theory and contrast in dB).

**`tdbp` — exact backprojection on a window** (pulse range `--az0/--az1`,
target raster in RDA output pixels `--waz0/--waz1 --wrg0/--wrg1`), with
optional `--compare <rda.cf> <naz> <n0> <az0>` for peak-offset + registered
difference against an RDA image:

```sh
cargo oxide run -- tdbp "$DAT" /tmp/tdbp_win \
  --az0 4000 --az1 6048 --waz0 4000 --waz1 6048 --wrg0 2800 --wrg1 4500 \
  --compare /tmp/s1_image.cf 44901 17634 0
```

**`ships` — peak analysis of a focused image** (12 brightest local maxima
per azimuth quarter, FWHM + contrast; `SCHIFF` = point-like with > 10 dB
contrast on dark background; with `az0 az1` a deep ocean search):

```sh
cargo oxide run -- ships /tmp/s1_image.cf 44901 17634
```

**`ql` — re-render a quicklook** from a stored `.cf` (full frame or
`az0 az1 r0 r1` crop; RFI rows dropped from the stretch):

```sh
cargo oxide run -- ql /tmp/s1_image.cf 44901 17634 /tmp/ship.png 4195 4695 3411 3911
```

## Performance (reference file, RTX A4000, 32 CPU cores)

| Echoes | CPU focus | GPU focus | Host peak (CPU / GPU) |
|---|---|---|---|
| 2,048 | 2.8 s | 0.8 s | ~1.4 / ~1.2 GB |
| 8,192 | 11.3 s | 1.9 s | ~6 / ~4 GB |
| 44,901 (full) | 66.5 s | ~11–14 s | ~29 / ~16 GB |

Decoding (~27 s full frame, always on CPU) dominates the total runtime now
(~39 s GPU end to end). TDBP window 2,048 × 2,048×1,700: 17.3 s CPU /
5.5 s GPU. Details in
[plan/20261005_01_gpu_speed/walkthrough.md](../plan/20261005_01_gpu_speed/walkthrough.md).

## Module map

Numbered files in data-flow order, one responsibility each:

| File | Module | One sentence |
|---|---|---|
| `01_types.rs` | `types` | `Complex32`, 3D vector, constants (c, wavelength, WGS84) |
| `02_meta.rs` | `meta` | Echo metadata from headers (PRI, SWST, chirp, RGDEC→rate), slant raster |
| `03_ephem.rs` | `ephem` | Orbit from sub-commutated data (ECEF!), effective velocity, geometric f_DC |
| `04_chirp.rs` | `chirp` | Ideal chirp replica on the exact ADC raster |
| `05_range.rs` | `range` | CPU range compression (rustfft, multi-threaded) |
| `06_rda.rs` | `rda` | Staged CPU RDA + Clutterlock f_DC estimation |
| `07_tdbp.rs` | `tdbp` | CPU backprojection (f64 geometry, serial + parallel) |
| `08_cufft.rs` | `cufft` | Minimal cuFFT FFI (no heavy bindings) |
| `09_kernel.rs` | `kernel` | CUDA kernels in Rust: on-the-fly filters, shifts, TDBP sum |
| `10_gpu.rs` | `gpu` | GPU RDA/TDBP pipelines (same coefficients as CPU) |
| `11_look.rs` | `look` | dB, multilook, PNG/ASCII quicklook, peak search, FWHM |
| `12_ingest.rs` | `ingest` | Echo selection (all of best beam, no 512 cap) + alignment |
| `13_tdbp_geo.rs` | `tdbp_geo` | RDA→TDBP geometry bridge (frame, Earth rotation, sphere, time smoothing) |
| `main.rs` | CLI | `meta`, `focus`, `ships`, `ql`, `tdbp` |

Deep-dives:
[../plan/20261004_02_gpu_decode_sar/walkthrough.md](../plan/20261004_02_gpu_decode_sar/walkthrough.md)
(RDA vs TDBP, corrections, CPU↔GPU proofs, e2e narration, glossary) and
[../plan/20261005_01_gpu_speed/walkthrough.md](../plan/20261005_01_gpu_speed/walkthrough.md)
(8.5× GPU speedup). Known open issue: TDBP azimuth defocus 3–5× over
diffraction (walkthrough finding 13).

## Dependencies

`rustfft` (CPU FFT), `num-complex`, `image` (PNG), `cuda-device` /
`cuda-host` / `cuda-core` (pinned `cuda-oxide` revision), `copernicus-radar`
(sibling decoder, path dependency), cuFFT via `build.rs` (`CUDA_ROOT` or
`CUDA_PATH` override).
