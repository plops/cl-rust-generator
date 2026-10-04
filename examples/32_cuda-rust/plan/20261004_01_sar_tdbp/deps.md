# Abhängigkeiten (`deps.md`)

Notation: `<organization>/<project>`. Alle Crates kommen von crates.io,
außer `cuda-*` (Git, Revision gepinnt, identisch zum verifizierten
`my_first_kernel`-Template).

| Crate(s) | Organisation/Projekt | Version / Rev | Zweck |
|---|---|---|---|
| `cuda-device`, `cuda-host`, `cuda-core` | `NVlabs/cuda-oxide` | git rev `a0cc6cc0bc18722ec5cdfbea136c118a7c051977` | `#[kernel]`-Makros + Device-API, Host-Loader (`#[cuda_module]`), Kontext/Streams/`DeviceBuffer`/`LaunchConfig1D` |
| `macroquad` | `not-fl3/macroquad` | `0.4.16` | Interaktive 2D-Visualisierung (Textur, Input, Overlays) |
| `image` (nur `png`) | `image-rs/image` | `0.25` | PNG-Export im Headless-Modus + Screenshot-Verifikation in Tests |

Nicht verwendet (bewusst, Details in `walkthrough.md`):
`NVlabs/cutile-rs` (zweite Kernel-Sprache überflüssig für pixelparalleles TDBP),
`rust-num/num-complex` (eigener `repr(C)`-Typ `Complex32` garantiert
Device-Kompatibilität ohne MIR-Import aus Fremd-Crates),
`rust-lang/libm` (Kernel-Math `sqrt`/`sin`/`cos` wird nativ auf CUDA-libdevice
gesenkt; Host nutzt `std`).

Dokumentationsquellen (DeepWiki MCP):
`NVlabs/cuda-oxide` (2D-Kernel, `DisjointSlice`, `LaunchConfig`, `PreparedLaunch`),
`NVIDIA/cuda-rust` (Repo-Übersicht, `cuda-core`-Host-API).
