# Sentinel-1 SAR: from Copernicus download to focused radar image

This folder holds **three Rust programs** that form a pipeline from raw
Sentinel-1 level-0 data to focused radar images and interferograms:

| Program | Crate / binary | What it does | When you need it |
|---|---|---|---|
| **Decoder** | [`decoder/`](decoder/) · `copernicus-radar` | Memory-maps a level-0 raw `.dat` file, walks the CCSDS space packets, decodes the BAQ-compressed echoes into complex range samples, and writes CSV reports + raw range images (`.cf`) | Packet census, header/ancillary diagnostics, raw (unfocused) range data |
| **Focus** | [`sar_focus/`](sar_focus/) · `sar_focus` | Takes a raw Stripmap `.dat` file (reusing the decoder as a library), range-compresses the chirps and azimuth-focuses with the Range-Doppler Algorithm (RDA, CPU + GPU) or Time-Domain Backprojection (TDBP, CPU + GPU), and writes the focused image (`.cf`) + PNG quicklook | The Stripmap radar image + exact TDBP reference |
| **Source2** | [`source2/`](source2/) · `sar_source2` | Unified processor for Stripmap (SM) and TOPS (IW/EW) `.dat` files: own L0 ingest, per-burst TOPS focusing (CPU + GPU), debursting, multi-swath mosaic, and optional repeat-pass InSAR (master + slave) | IW/EW images, mosaics, and interferograms (coherence/phase) |

Typical flow — download once, then decode (optional) and focus:

```text
Copernicus Data Space  →  *.SAFE.zip  →  unzip  →  s1?-...-vv-....dat
                                                          ├─ sar_focus meta ── packet/echo census
                                                          ├─ decoder/ ── CSV reports + raw range .cf
                                                          ├─ sar_focus focus ── focused .cf + .png + ship report (Stripmap)
                                                          └─ source2 ── mosaic .cf/.png/.json (SM/IW/EW) or InSAR products (--slave)
```

All three programs read the **same input file** (a `*-vv-*.dat` from the
unzipped product). `sar_focus` does not need the decoder's output files —
it calls the decoder as a Rust library. `source2` is self-contained: it
has its own L0 packet ingest (`source2/src/02_l0.rs`, `05_ingest.rs`) and
does not depend on the `decoder` crate.

## 1. Get a dataset from the Copernicus website

1. Go to the [Copernicus Data Space Ecosystem](https://dataspace.copernicus.eu)
   and create a free account.
2. Open the Browser, search for **Sentinel-1**, sensing date of your choice,
   and product type **RAW** (level-0, unfocused echoes). Tested modes:
   Stripmap (SM, e.g. beam S6) for decoder/`sar_focus`, and
   Stripmap + IW/EW TOPS for `source2`. Dual polarization (SDV gives you
   VV + VH channels); all programs read the VV channel.
3. Download the `*.SAFE.zip` product (about 1.2 GB for a ~27 s Stripmap
   slice) and unzip it. Reference products used in the walkthroughs:
   - Stripmap: `S1C_S6_RAW__0SDV_20260929T214300_20260929T214327_009667_0133F4_5830.SAFE.zip`
     (Sentinel-1C, beam S6, 27 s over Santos/São Paulo, Brazil).
   - IW: `S1C_IW_RAW__0SDV_20261027T052449_20261027T052522_010066_01417A_F3DD.SAFE.zip`.
   - EW: `S1D_EW_RAW__0SDV_20261025T081255_20261025T081403_005169_009CDD_B2F9.SAFE.zip`.
   - InSAR pair (South Taiwan, 12-day repeat): `S1C_IW_RAW__0SDV_20260928T215149_20260928T215222_009653_01336F_D2EE.SAFE.zip`
     (master) + `S1C_IW_RAW__0SDV_20261010T215149_20261010T215222_009828_013950_CEB1.SAFE.zip`
     (slave), see `data/insar/`.
4. Inside the `*.SAFE/` folder you find the echo files plus paperwork; the
   only file the programs read is the VV echo file (names are lowercase
   with dashes):

```text
*.SAFE/
  s1c-s6-raw-s-vv-....dat        631 MB  echo packets (this is the input)
  s1c-s6-raw-s-vv-....-annot.dat  1.2 MB annotation (not read)
  s1c-s6-raw-s-vv-....-index.dat  1.7 kB packet index (not read)
  s1c-s6-raw-s-vh-....dat (+ annot/index)  cross-pol channel (unused)
  manifest.safe, *-report-*.pdf, support/*.xsd     paperwork
```

5. Extracted datasets live in `data/s6_vv/`, `data/iw_vv/`, `data/ew_vv/`
   and `data/insar/` (one folder per product, each with `.dat` + `-annot.dat`
   + `-index.dat` + `manifest.safe`). You can also keep a `.dat` anywhere
   and pass the path on the command line. Note: the decoder's real-data
   regression tests (`decoder/tests/real_data.rs`) still default to
   `data/vv/…` — point them at an extract via the `S1_DAT` env var, since
   that folder does not exist in this checkout.

Scope: decoder + `sar_focus` are validated on Sentinel-1 **Stripmap VV**;
`source2` additionally covers **IW/EW TOPS VV** (focus, deburst, mosaic)
and **IW repeat-pass InSAR**. The VH channel, geocoding (outputs are
slant-range) and radiometric calibration are out of scope — see the
walkthroughs for details.

## 2. Decode: packets → CSV reports + raw range image

```sh
cd decoder
cargo build --release
DAT=../data/s6_vv/s1c-s6-raw-s-vv-20260929t214300-20260929t214327-009667-0133f4.dat

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
DAT=../data/s6_vv/s1c-s6-raw-s-vv-20260929t214300-20260929t214327-009667-0133f4.dat

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

## 4. Source2: SM/IW/EW focus, mosaic, and InSAR

`source2` (`sar_source2`) is the unified follow-up: it handles Stripmap
**and** IW/EW TOPS, debursts the TOPS bursts, mosaics the sub-swaths, and
optionally runs a repeat-pass interferometry pipeline on a master/slave
pair. Same GPU stack as `sar_focus` (`cargo oxide`, cuFFT, pinned nightly
in `source2/rust-toolchain.toml`); `--cpu` forces the CPU path, and without
a GPU it falls back to CPU automatically.

```sh
cd source2
cargo oxide build
DAT=../data/iw_vv/s1c-iw-raw-s-vv-20261027t052449-20261027t052522-010066-01417a.dat

# Single scene → debursted mosaic + per-swath quicklooks in out/
cargo oxide run -- --input "$DAT" --out-dir out --prefix iw
# Small window on the CPU (also: --bursts b0,b1, --multilook laz,lrg)
cargo oxide run -- --input "$DAT" --out-dir out --prefix iw --cpu --az-window 4000,6048
```

With `--slave <slave.dat>` (plus `--goldstein-alpha` and `--coh-win` to
tune filtering) it runs the InSAR pipeline instead — coregistration,
interferogram, Goldstein filter, coherence, unwrapping — as demonstrated on
the Taiwan pair in `data/insar/` with results under `source2/out/insar_taiwan/`:

```sh
cargo oxide run -- --input data/insar/<master>.SAFE/*.dat \
  --slave data/insar/<slave>.SAFE/*.dat --out-dir out --prefix taiwan_insar
```

Single-scene outputs per run: `<prefix>_mosaic.png` (dB quicklook),
`<prefix>_mosaic.cf`/`.json` (mosaic power + metadata), and one
`<prefix>_<swath>.png` per sub-swath. Default multilook is (10,10) for
Stripmap, (6,24) for IW, (6,12) for EW. The CLI has no `meta`/`ships`/`ql`
subcommands — those belong to `sar_focus`.

## Requirements

| Program | Needs |
|---|---|
| Decoder | Stable Rust (`cargo build`, `cargo test`); ~1 GB free disk for CSV + `.cf` outputs |
| `sar_focus` CPU (`--cpu`) | Stable Rust + `cargo oxide` build wrapper; 3 GB RAM for windows, ~29 GB for the full frame |
| `sar_focus` GPU | NVIDIA GPU + CUDA toolkit (cuFFT), `libclang-dev`, pinned nightly toolchain (see `sar_focus/rust-toolchain.toml`), `cargo-oxide`; ~16 GB host RAM + ~3 GB VRAM for the full frame, ~7 GB free disk for the `.cf` |
| `source2` CPU (`--cpu`) | Stable Rust + `cargo oxide` build wrapper (pinned nightly, see `source2/rust-toolchain.toml`) |
| `source2` GPU | Same GPU stack as `sar_focus` (NVIDIA + CUDA cuFFT, `cargo-oxide`); falls back to CPU when no GPU is found |

Quality gates (all three crates): `cargo clippy --all-targets -- -D warnings`
and `cargo fmt --check` are green; `cargo test` (decoder, 41 tests incl. 3
real-data regressions that skip without the dataset), `cargo oxide test`
(`sar_focus`, 48 tests on synthetic data, no dataset needed) and
`cargo oxide test` (`source2`, 20 integration tests on synthetic data, no
dataset needed).

## Layout and further reading

```text
34_copernicus_radar/
  README.md            this file (download → decode → focus → mosaic/InSAR)
  decoder/             program 1: space-packet decoder (lib + binary + tests)
  sar_focus/           program 2: Stripmap focusing, CPU reference + GPU pipeline
  source2/             program 3: unified SM/IW/EW focuser + mosaic + InSAR
  data/                datasets (s6_vv/, iw_vv/, ew_vv/, insar/ + zips)
  plan/                design notes and walkthroughs (German, with pictures)
```

| Document | Covers |
|---|---|
| [decoder/README.md](decoder/README.md) | Decoder usage, outputs, module map, deviations from C++ |
| [sar_focus/README.md](sar_focus/README.md) | Focus usage: five subcommands, flags, benchmarks, tests |
| [plan/20261004_01_rust_port/walkthrough.md](plan/20261004_01_rust_port/walkthrough.md) | Decoder deep-dive: packet format, FDBAQ bit-walk, pipeline, lessons |
| [plan/20261004_02_gpu_decode_sar/walkthrough.md](plan/20261004_02_gpu_decode_sar/walkthrough.md) | Focusing deep-dive: RDA vs TDBP, error corrections, CPU↔GPU proofs, e2e narration, glossary |
| [plan/20261005_01_gpu_speed/walkthrough.md](plan/20261005_01_gpu_speed/walkthrough.md) | GPU speedup 8.5×: on-the-fly filters, persistent plans, measurements |
| [plan/20261028_01_architektur_ueberblick/dokumentation.md](plan/20261028_01_architektur_ueberblick/dokumentation.md) | Architecture overview + rebuild-vs-extend verdict for IW/EW |
| [plan/20261029_02_insar/walkthrough.md](plan/20261029_02_insar/walkthrough.md) | `source2` InSAR deep-dive: Taiwan repeat-pass pipeline on CPU + GPU |
