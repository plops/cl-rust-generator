#!/bin/bash
# package_release.sh — Linux-Release-Archive für Server und Desktop-Client.
#
#   dist/lbw-server-<v>-linux-<arch>.tar.gz   lbw-server + OCR-Modelle + README.txt
#   dist/lbw-client-<v>-linux-<arch>.tar.gz   lbw-client (nur libc; X11/GL per dlopen)
#
# <v> ist die [workspace.package]-Version aus Cargo.toml. Das GUI-Modell
# gpa_640_int8.onnx wird bewusst NICHT gepackt (Ultralytics-Export, AGPL);
# ohne es startet der Server mit `--gui none`.
#
# Aufruf (aus source6): ./scripts/package_release.sh [ausgabe-verzeichnis]
# Gleiche Schritte wie .github/workflows/release-29-lowbandwidth.yml.
set -euo pipefail
cd "$(dirname "$0")/.."
OUT="$(mkdir -p "${1:-dist}" && cd "${1:-dist}" && pwd)"
VERSION=$(awk '/^\[workspace.package\]/{p=1;next} /^\[/{p=0} p&&/^version/{gsub(/"/,"",$3);print $3;exit}' Cargo.toml)
ARCH=$(uname -m)
[ -n "$VERSION" ] || { echo "Version in Cargo.toml nicht gefunden" >&2; exit 1; }

cargo build --release -p lbw-server -p lbw-client
./scripts/fetch_models.sh

stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT

srv="lbw-server-$VERSION-linux-$ARCH"
mkdir -p "$stage/$srv/models"
cp target/release/lbw-server "$stage/$srv/"
cp models/PP-OCRv6_small_det.onnx models/PP-OCRv6_small_rec.onnx models/inference.yml "$stage/$srv/models/"
cp ../../../LICENSE "$stage/$srv/LICENSE"
cat > "$stage/$srv/README.txt" <<EOF
lbw-server $VERSION ($ARCH)

Start inside this directory (models are loaded from ./models):

    ./lbw-server --gui none --x 0 --y 0 -v

The server only listens on 127.0.0.1:7878 (the protocol has no
authentication). Reach it through SSH:

    ssh -N -L 7878:127.0.0.1:7878 user@host   # then: lbw-client --connect 127.0.0.1:7878

The Android app builds this tunnel itself (field "SSH-Host").

The GUI detector model (gpa_640_int8.onnx) is not included. If you have
it, put it into models/ and start without "--gui none".

Models: PaddlePaddle PP-OCRv6 small (Apache-2.0), unmodified from HuggingFace.
Program: see LICENSE.
EOF

cli="lbw-client-$VERSION-linux-$ARCH"
mkdir -p "$stage/$cli"
cp target/release/lbw-client "$stage/$cli/"
cp ../../../LICENSE "$stage/$cli/LICENSE"

for d in "$srv" "$cli"; do
    tar -C "$stage" -czf "$OUT/$d.tar.gz" --owner=0 --group=0 "$d"
    echo "ok: $OUT/$d.tar.gz ($(stat -c %s "$OUT/$d.tar.gz") B)"
done
glibc=$(objdump -T target/release/lbw-server target/release/lbw-client | grep -o 'GLIBC_[0-9.]*' | sort -Vu | tail -1)
echo "benötigt mindestens: $glibc"
