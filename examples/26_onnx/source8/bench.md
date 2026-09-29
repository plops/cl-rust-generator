# bench.md — GPA-GUI-Detector (YOLO11m) in Rust/ort

Stand 2026-09-29. Threadripper PRO 7955WX (16C/32T), RTX A4000, ONNX Runtime
1.28.0 (ort 2.0.0-rc.13), Release-Build. Eingabe: `example_input.ppm`
(1920×1080, Windows-Desktop, 300 Boxen bei conf 0.05). Median aus 30
Iterationen nach 5 Warmups. Reproduzieren: `./scripts/bench.sh 30`.

## Inferenz (ms, Median) — Pre ≈ 1,5 ms, Post ≈ 0,3 ms kommen dazu

| Variante | MB | CPU 4T | CPU 8T | CPU Default | CUDA |
|---|---:|---:|---:|---:|---:|
| 640² fp32 | 80,4 | 150,0 | 91,1 | 109,6 | 9,4 |
| 640² fp16 | 40,3 | 183,8 | 120,2 | 155,2 | 6,0 |
| 640² int8 | 20,9 | 90,3 | 68,2 | 80,4 | 17,3 |
| 384×640 fp32 | 80,4 | 89,8 | 55,8 | 70,4 | 6,1 |
| 384×640 fp16 | 40,2 | 115,1 | 76,6 | 104,4 | **4,1** |
| 384×640 int8 | 20,9 | **53,1** | **41,1** | 51,1 | 11,3 |

Einzelthread (384×640): fp32 294,9 ms, int8 134,4 ms (2,2×).
Thread-Skalierung sättigt ab ~8 Threads; mehr Threads (16/32/Default)
bringen nichts oder schaden (Messrauschen ±10 %).

## End-to-end unter Xvfb (Grab + Pre + Infer + Post)

| Konfiguration | Grab | Infer | Total | FPS |
|---|---:|---:|---:|---:|
| CPU 8T, 384×640 int8 | 3,5 | 42,3 | 47,2 | 21 |
| CUDA, 384×640 fp16 | 3,2 | 4,6 | 9,1 | 110 |

## Genauigkeit vs. fp32 (`models/export_report.tsv`, conf ≥ 0,25, IoU ≥ 0,5)

| Variante | Recall | Präzision | Recall (Variante conf ≥ 0,15) |
|---|---:|---:|---:|
| fp16 (beide Formen) | 1,000 | 1,000 | 1,000 |
| 640² int8 | 0,934 | 0,955 | 0,986 |
| 384×640 int8 | 0,924 | 0,958 | 0,981 |
| 384×640 fp32 vs. 640² fp32 | 0,985 | 0,990 | 1,000 |

Rust-Parität zu Ultralytics (fp32): 300/300 Boxen, IoU ≥ 0,9, |Δscore| < 0,02.

## Binärgrößen

| Artefakt | Bytes |
|---|---:|
| `gui_detect` CPU (ORT statisch gelinkt) | 22 990 848 |
| `gui_detect --features embed` (+ int8-Modell) | 43 841 664 (xz: 23 039 768) |
| `gui_detect --features cuda` | 23 643 120 |
| + `libonnxruntime_providers_cuda.so` | 78 955 136 |
| int8-Modell / xz | 20 850 611 / 17 294 796 |
| fp16-Modell / xz | 40 249 355 / 36 220 400 |

## Fazit

- GPU: fp16 + 384×640 ist die schnellste Variante (4,1 ms). INT8-QDQ ist
  auf dem CUDA-EP *langsamer* als fp32 (Q/DQ-Knoten werden nicht fusioniert,
  112 Memcpy-Knoten) — echtes INT8 auf GPU bräuchte TensorRT.
- CPU: INT8 + 384×640 ist die schnellste Variante (41 ms @ 8T, 2,2× vs. fp32
  einzeln, 1,4× bei 8T). FP16 ist auf CPU langsamer als fp32 (Cast-Knoten,
  keine fp16-Kernels).
- Größe: INT8 viertelt das Modell (80 → 21 MB); eingebettet wächst das
  Binary von 23 auf 44 MB. Die ORT-Bibliothek selbst (~23 MB) ist danach
  der größere Block.
