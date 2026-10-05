# Sentinel-1 SAR: from Copernicus download to focused radar image

This folder holds **two Rust programs** that form a pipeline from raw
Sentinel-1 level-0 data to a focused radar image:

| Program | Crate / binary | What it does | When you need it |
|---|---|---|---|
| **Decoder** | [`decoder/`](decoder/) · `copernicus-radar` | Memory-maps a level-0 raw `.dat` file, walks the CCSDS space packets, decodes the BAQ-compressed echoes into complex range samples, and writes CSV reports + raw range images (`.cf`) | Packet census, header/ancillary diagnostics, raw (unfocused) range data |
| **Focus** | [`sar_focus/`](sar_focus/) · `sar_focus` | Takes a raw `.dat` file (reusing the decoder as a library), range-compresses the chirps and azimuth-focuses with the Range-Doppler Algorithm (RDA, CPU + GPU) or Time-Domain Backprojection (TDBP, CPU + GPU), and writes the focused image (`.cf`) + PNG quicklook | The actual radar image |

Typical flow — download once, then decode (optional) and focus:

```text
Copernicus Data Space  →  *.SAFE.zip  →  unzip  →  s1c-...-vv-....dat
                                                          ├─ decoder/focus ── meta ── packet/echo census
                                                          ├─ decoder/ ── CSV reports + raw range .cf
                                                          └─ sar_focus focus ── focused .cf + .png + ship report
```

Both programs read the **same input file** (a `*-vv-*.dat` from the
unzipped product). `sar_focus` does not need the decoder's output files —
it calls the decoder as a Rust library.

## 1. Get a dataset from the Copernicus website

1. Go to the [Copernicus Data Space Ecosystem](https://dataspace.copernicus.eu)
   and create a free account.
2. Open the Browser, search for **Sentinel-1**, sensing date of your choice,
   and product type **RAW** (level-0, unfocused echoes). For the mode tested
   here pick Stripmap (SM), e.g. beam S6, dual polarization (SDV gives you
   VV + VH channels).
3. Download the `*.SAFE.zip` product (about 1.2 GB for a ~27 s Stripmap
   slice) and unzip it. The reference product used in all walkthroughs is
   `S1C_S6_RAW__0SDV_20260929T214300_20260929T214327_009667_0133F4_5830.SAFE.zip`
   (Sentinel-1C, beam S6, 27 s over Santos/São Paulo, Brazil).
4. Inside the `*.SAFE/` folder you find 11 files; the only one both programs
   read is the VV echo file (names are lowercase with dashes):

```text
*.SAFE/
  s1c-s6-raw-s-vv-....dat        631 MB  echo packets (this is the input)
  s1c-s6-raw-s-vv-....-annot.dat  1.2 MB annotation (not read)
  s1c-s6-raw-s-vv-....-index.dat  1.7 kB packet index (not read)
  s1c-s6-raw-s-vh-....dat (+ annot/index)  cross-pol channel (unused)
  manifest.safe, *-report-*.pdf, support/*.xsd     paperwork
```

5. Copy the `*-vv-*.dat` to `data/vv/` (that folder is git-ignored and is
   where the decoder's regression tests look for it), or keep it anywhere
   and pass the path on the command line.

Scope: validated on Sentinel-1 **Stripmap VV** only. The VH channel,
IW/EW modes, geocoding (output is slant-range) and radiometric calibration
are out of scope — see the walkthroughs for details.

## 2. Decode: packets → CSV reports + raw range image

```sh
cd decoder
cargo build --release
DAT=../data/vv/s1c-s6-raw-s-vv-20260929t214300-20260929t214327-009667-0133f4.dat

./target/release/copernicus-radar "$DAT" --csv-dir out --cf-dir out
```

This walks all 45,437 packets of the reference file (~22 s), prints the
census (44,901 FDBAQ echoes + 16 BAQ5 noise + 520 bypass calibration
packets, zero decode failures), and writes `o_range.csv`,
`o_cal_range.csv`, `o_anxillary.csv` plus the raw range image
`o_range*_echoes*.cf` (default: first 512 echoes stored, all decoded and
reported — see `--max-echoes`). Details, module map and deliberate
deviations from the C++ original: [decoder/README.md](decoder/README.md).

## 3. Focus: raw echoes → radar image

`sar_focus` has two compute backends: GPU (NVIDIA, via `cargo oxide`) and a
multi-core CPU reference (`--cpu`, runs anywhere). Build and diagnose first:

```sh
cd sar_focus
cargo oxide build
DAT=../data/vv/s1c-s6-raw-s-vv-20260929t214300-20260929t214327-009667-0133f4.dat

cargo oxide run -- meta "$DAT"
```

`meta` decodes nothing — it counts packets/echoes and prints PRF, chirp and
slant-range parameters plus orbit blocks, so you see immediately whether a
newly downloaded file is readable and plausible.

Then focus. Full frame on the GPU (reference file: ~28 s decode + ~11 s
focus, needs ~16 GB host RAM and ~3 GB VRAM, writes a 6.3 GB `.cf`):

```sh
cargo oxide run -- focus "$DAT" /tmp/s1_image --compare
```

On a small machine, or without a GPU, focus a window on the CPU instead
(2,048 echoes: ~2–4 s total, ~1.4 GB RAM):

```sh
cargo oxide run -- focus "$DAT" /tmp/s1_window --cpu --az0 4000 --az1 6048
```

`focus` writes `<prefix>.cf` (the complex image: raw little-endian `f32`
pairs, row-major), `<prefix>.png` (dB-scaled, RFI-robust quicklook), an
ASCII preview and a ship/point-target report (peak positions, FWHM against
theory, contrast) to the console. Expected result on the reference file: a
44,901 × 17,634 image of bay, city and ocean with ships at 41–48 dB
contrast — compare with `plan/20261004_02_gpu_decode_sar/quicklook_full.avif`
and the full console transcript in `e2e_full.log` in the same folder.

Further subcommands (all documented in [sar_focus/README.md](sar_focus/README.md)):

| Command | Purpose |
|---|---|
| `tdbp … --az0/--az1 --waz0/--waz1 --wrg0/--wrg1` | Exact backprojection on a window (slow, no approximations) + optional comparison against an RDA image |
| `ships <img.cf> <naz> <n0>` | Peak search + FWHM/contrast report on a focused image |
| `ql <img.cf> <naz> <n0> <out.png> [window]` | Re-render a quicklook (full frame or crop) from a stored image |

Note: the `sar_focus` CLI prints German help/diagnostics (the walkthroughs
are German too); the READMEs are in English.

## Requirements

| Program | Needs |
|---|---|
| Decoder | Stable Rust (`cargo build`, `cargo test`); ~1 GB free disk for CSV + `.cf` outputs |
| `sar_focus` CPU (`--cpu`) | Stable Rust + `cargo oxide` build wrapper; 3 GB RAM for windows, ~29 GB for the full frame |
| `sar_focus` GPU | NVIDIA GPU + CUDA toolkit (cuFFT), `libclang-dev`, pinned nightly toolchain (see `sar_focus/rust-toolchain.toml`), `cargo-oxide`; ~16 GB host RAM + ~3 GB VRAM for the full frame, ~7 GB free disk for the `.cf` |

Quality gates (both crates): `cargo clippy --all-targets -- -D warnings`
and `cargo fmt --check` are green; `cargo test` (decoder, 41 tests incl. 3
real-data regressions that skip without the dataset) and `cargo oxide test`
(`sar_focus`, 48 tests on synthetic data, no dataset needed).

## Layout and further reading

```text
34_copernicus_radar/
  README.md            this file (download → decode → focus)
  decoder/             program 1: space-packet decoder (lib + binary + tests)
  sar_focus/           program 2: SAR focusing, CPU reference + GPU pipeline
  data/                your datasets (git-ignored, never committed)
  plan/                design notes and walkthroughs (German, with pictures)
```

| Document | Covers |
|---|---|
| [decoder/README.md](decoder/README.md) | Decoder usage, outputs, module map, deviations from C++ |
| [sar_focus/README.md](sar_focus/README.md) | Focus usage: five subcommands, flags, benchmarks, tests |
| [plan/20261004_01_rust_port/walkthrough.md](plan/20261004_01_rust_port/walkthrough.md) | Decoder deep-dive: packet format, FDBAQ bit-walk, pipeline, lessons |
| [plan/20261004_02_gpu_decode_sar/walkthrough.md](plan/20261004_02_gpu_decode_sar/walkthrough.md) | Focusing deep-dive: RDA vs TDBP, error corrections, CPU↔GPU proofs, e2e narration, glossary |
| [plan/20261005_01_gpu_speed/walkthrough.md](plan/20261005_01_gpu_speed/walkthrough.md) | GPU speedup 8.5×: on-the-fly filters, persistent plans, measurements |
