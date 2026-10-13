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
