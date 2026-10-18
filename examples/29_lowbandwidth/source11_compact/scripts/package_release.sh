#!/bin/bash
# package_release.sh — baut Release-Artefakte für source11_compact.
# Aufruf: ./scripts/package_release.sh <dist-dir>
set -euo pipefail
cd "$(dirname "$0")/.."
DIST="$1"
mkdir -p "$DIST"

cargo build --release -p lbw-server -p lbw-client
strip target/release/lbw-server target/release/lbw-client || true
./scripts/fetch_models.sh

VER=$(awk '/^\[workspace.package\]/{p=1;next} /^\[/{p=0} p&&/^version/{gsub(/"/,"",$3);print $3;exit}' Cargo.toml)
S="lbw-server-$VER-linux-x86_64"
mkdir -p "/tmp/$S"
cp target/release/lbw-server "/tmp/$S/"
cp models/PP-OCRv6_small_det.onnx models/PP-OCRv6_small_rec.onnx models/inference.yml "/tmp/$S/"
[ -f models/gpa_640_int8.onnx ] && cp models/gpa_640_int8.onnx "/tmp/$S/" || true
tar -czf "$DIST/$S.tar.gz" -C /tmp "$S"

C="lbw-client-$VER-linux-x86_64"
mkdir -p "/tmp/$C"
cp target/release/lbw-client "/tmp/$C/"
tar -czf "$DIST/$C.tar.gz" -C /tmp "$C"

ls -la "$DIST"
du -sh target/release/lbw-client "$DIST"/*.tar.gz
