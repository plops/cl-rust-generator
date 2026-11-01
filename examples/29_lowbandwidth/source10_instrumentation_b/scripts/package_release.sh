#!/bin/bash
# package_release.sh <dist-dir> — Linux-Release-Pakete für source10_b:
#   lbw10b-server-<v>-linux-x86_64.tar.gz  Server + PP-OCRv6-Gewichte + ONNX-Runtime
#   lbw10b-client-<v>-linux-x86_64.tar.gz  Desktop-Client + logstat + replay
set -euo pipefail
cd "$(dirname "$0")/.."
DIST="${1:?dist-dir}"; mkdir -p "$DIST"
V=$(awk '/^\[workspace.package\]/{p=1;next} /^\[/{p=0} p&&/^version/{gsub(/"/,"",$3);print $3;exit}' Cargo.toml)
./scripts/fetch_models.sh
cargo build --release -p lbw-server -p lbw-client -p lbw-log
T=$(mktemp -d)
mkdir -p "$T/srv/models" "$T/cli"
cp target/release/lbw-server "$T/srv/"
cp target/release/libonnxruntime*.so* "$T/srv/" 2>/dev/null || true
cp -L models/PP-OCRv6_small_det.onnx models/PP-OCRv6_small_rec.onnx models/inference.yml "$T/srv/models/"
cp README.md "$T/srv/"
cp target/release/lbw-client target/release/lbw-logstat target/release/lbw-replay README.md "$T/cli/"
strip "$T/srv/lbw-server" "$T"/cli/lbw-* || true
tar -czf "$DIST/lbw10b-server-$V-linux-x86_64.tar.gz" -C "$T/srv" .
tar -czf "$DIST/lbw10b-client-$V-linux-x86_64.tar.gz" -C "$T/cli" .
ls -la "$DIST"
