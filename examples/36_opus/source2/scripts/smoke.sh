#!/bin/sh
# Smoke: bauen, Demo rendern, PNG/JSON pruefen, --show unter xvfb.
# Aufruf: scripts/smoke.sh  (braucht ffprobe, python3, xvfb-run)
set -eu
cd "$(dirname "$0")/.."
BIN=target/release/opus-vis

echo "== build =="
cargo build -q --release --features show

echo "== demo =="
./scripts/mkdemo.sh /tmp/smoke_demo.opus

echo "== render =="
./$BIN /tmp/smoke_demo.opus -o /tmp/smoke.png --json /tmp/smoke.json \
  --width 800

echo "== png pruefen (ffprobe) =="
ffprobe -v error -show_entries stream=codec_name,width,height \
  -of csv=p=0 /tmp/smoke.png

echo "== report pruefen (stdout + --quiet) =="
./$BIN /tmp/smoke_demo.opus -o /tmp/smoke_report.png --width 200 \
  > /tmp/smoke_report.txt
grep -q "^opus-report:" /tmp/smoke_report.txt
grep -q "^einordnung:" /tmp/smoke_report.txt
grep -q "variable bitrate" /tmp/smoke_report.txt
./$BIN /tmp/smoke_demo.opus -o /tmp/smoke_quiet.png --width 200 --quiet \
  > /tmp/smoke_quiet.txt
test ! -s /tmp/smoke_quiet.txt

echo "== json pruefen (json.tool) =="
python3 -m json.tool /tmp/smoke.json > /dev/null
python3 -c "
import json
d = json.load(open('/tmp/smoke.json'))
assert d['schema'] == 2, d.get('schema')
assert len(d['frames']) == 300, len(d['frames'])
assert d['summary']['silk_frac'] > 0.4
assert d['summary']['celt_frac'] > 0.4
assert 'head' in d and 'tags' in d and 'encoder' in d and 'pcm' in d
assert d['frames'][0]['mode'] == 'silk' and d['frames'][0]['celt'] is None
assert d['frames'][-1]['celt'] is not None
print('json ok:', d['summary']['frames'], 'frames')
"

echo "== silk/hybrid-fixtures (alle ansichten) =="
./$BIN tests/data/silk_sample.opus -o /tmp/smoke_silk.png --width 400 --quiet
./$BIN tests/data/hybrid_sample.opus -o /tmp/smoke_hyb2.png \
  --view bitstrom --width 400 --quiet
./$BIN tests/data/hybrid_sample.opus -o /tmp/smoke_hyb3.png \
  --view divergenz --width 400 --quiet
./$BIN tests/data/hybrid_sample.opus -o /tmp/smoke_hyb4.png \
  --view dichte --width 400 --quiet
./$BIN tests/data/hybrid_sample.opus --json /tmp/smoke_hyb.json \
  -o /tmp/smoke_hyb.png --width 400 --quiet
python3 -c "
import json
d = json.load(open('/tmp/smoke_hyb.json'))
assert d['summary']['hybrid_frac'] == 1.0, d['summary']
assert all(f['mode'] == 'hybrid' for f in d['frames'])
assert all(f['silk'] is not None for f in d['frames']), 'F0-Lane leer'
assert all(f['celt'] is not None for f in d['frames']), 'Ansicht 2-4 leer'
print('hybrid ok:', d['summary']['frames'], 'frames mit SILK+CELT')
"
./$BIN tests/data/silk_sample.opus --json /tmp/smoke_silk.json \
  -o /tmp/smoke_silk2.png --width 400 --quiet
python3 -c "
import json
d = json.load(open('/tmp/smoke_silk.json'))
assert d['summary']['silk_frac'] == 1.0, d['summary']
assert all(f['mode'] == 'silk' and f['silk'] is not None for f in d['frames'])
print('silk ok:', d['summary']['frames'], 'frames mit SILK')
"

echo "== stdin/stdout-pfad =="
./$BIN - -o - --width 320 < /tmp/smoke_demo.opus > /tmp/smoke_stdout.png
ffprobe -v error -show_entries stream=codec_name -of csv=p=0 \
  /tmp/smoke_stdout.png

echo "== show unter xvfb (30 frames) =="
xvfb-run -a ./$BIN /tmp/smoke_demo.opus --show --frames 30 \
  -o /tmp/smoke_show.png

echo "== interaktion (zoom/pan/hover) =="
if command -v xdotool >/dev/null && command -v scrot >/dev/null; then
  ./scripts/interact.sh
else
  echo "(kein xdotool/scrot, uebersprungen)"
fi

echo "== echte datei (falls vorhanden) =="
if [ -f ../rpQs3m5jtIM.webm ]; then
  ffmpeg -y -v error -i ../rpQs3m5jtIM.webm -t 30 -c:a copy -vn \
    /tmp/smoke_yt.opus
  ./$BIN /tmp/smoke_yt.opus -o /tmp/smoke_yt.png --width 1200
  ffprobe -v error -show_entries stream=codec_name -of csv=p=0 \
    /tmp/smoke_yt.png
else
  echo "(kein rpQs3m5jtIM.webm, uebersprungen)"
fi

echo "SMOKE OK"
