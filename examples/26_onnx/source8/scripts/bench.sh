#!/bin/bash
# bench.sh — Benchmark-Matrix: CPU (Threads-Varianten) und CUDA × alle
# ONNX-Varianten auf dem 1920×1080-Beispielbild, plus Binärgrößen.
#
# Aufruf (aus source8/):  ./scripts/bench.sh [ITERS] > bench.tsv
# Voraussetzungen: ./scripts/export_models.sh, CUDA-13-Runtime + cuDNN 9
# für die GPU-Zeilen.
set -euo pipefail
cd "$(dirname "$0")/.."

ITERS="${1:-30}"
IMG=models/example_input.ppm
MODELS=(models/gpa_640_fp32.onnx models/gpa_640_fp16.onnx models/gpa_640_int8.onnx
        models/gpa_384x640_fp32.onnx models/gpa_384x640_fp16.onnx models/gpa_384x640_int8.onnx)

cargo build --release -q
cargo build --release -q --features cuda --target-dir target/cuda
cargo build --release -q --features embed --target-dir target/embed
CPU=target/release/gui_detect
GPU=target/cuda/release/gui_detect

# ORT-Warnungen (Memcpy-Knoten etc.) gehen nach stderr → nicht in die TSV.
"$CPU" bench "$IMG" "${MODELS[0]}" --device cpu --iters 1 --warmup 0 2>/dev/null | sed -n 1p
for t in 0 8 4; do # 0 = ORT-Default (physische Kerne)
  "$CPU" bench "$IMG" "${MODELS[@]}" --device cpu --threads "$t" --iters "$ITERS" 2>/dev/null | tail -n +2
done
"$GPU" bench "$IMG" "${MODELS[@]}" --device cuda --iters "$ITERS" 2>/dev/null | tail -n +2

echo "# Binärgrößen (Bytes)"
for f in target/release/gui_detect target/embed/release/gui_detect target/cuda/release/gui_detect \
         target/cuda/release/libonnxruntime_providers_cuda.so target/cuda/release/libonnxruntime_providers_shared.so; do
  printf '%s\t%s\n' "$f" "$(stat -c%s "$f")"
done
