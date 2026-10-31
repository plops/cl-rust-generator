#!/usr/bin/env bash
# Erzeugt die Real-Daten-Fixtures fuer tests/oracle.rs aus den
# ungecommitteten Quell-Dateien neben source2 (silk.webm, rpQs3m5jtIM.webm).
# Jede Datei treibt einen anderen Decoder-Pfad (siehe Kommentar je Zeile).
# Braucht: ffmpeg (mit libopus), opus-tools (opusenc).
set -euo pipefail
cd "$(dirname "$0")/.."
SRC=..
OUT=tests/data/gen
mkdir -p "$OUT"
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
# Rohmaterial: 8 s Sprache (silk.webm ab 1:00), 8 s Musik/Sprache (YouTube ab 2:00).
ffmpeg -v error -y -ss 60 -t 8 -i "$SRC/silk.webm" -vn -ac 2 -ar 48000 "$T/speech.wav"
ffmpeg -v error -y -ss 120 -t 8 -i "$SRC/rpQs3m5jtIM.webm" -vn -ac 2 -ar 48000 "$T/music.wav"
# Tonale Synthese-„Musik" (Akkordwechsel + Anschlag): libopus waehlt CELT.
ffmpeg -v error -y -f lavfi -i "aevalsrc='0.2*sin(2*PI*(220+110*floor(mod(t,4)))*t)+0.15*sin(2*PI*(330+55*floor(mod(t*2,3)))*t)+0.1*sin(2*PI*660*t)*exp(-3*mod(t,0.5)):s=48000:c=stereo'" -t 6 "$T/tones.wav"
# Wechsel Sprache→Musik→Sprache (Moduswechsel + Redundanz-Frames).
ffmpeg -v error -y -i "$T/speech.wav" -i "$T/tones.wav" -i "$T/speech.wav" \
  -filter_complex "[0:a]atrim=0:3[a];[1:a]atrim=0:6[b];[2:a]atrim=3:6,asetpts=PTS-STARTPTS[c];[a][b][c]concat=n=3:v=0:a=1" "$T/mix.wav"
enc() { ffmpeg -v error -y -i "$1" -c:a libopus "${@:3}" "$OUT/$2.opus"; }
enc "$T/speech.wav" silk_stereo_wb   -ac 2 -ar 16000 -application voip -b:a 32k   # SILK stereo (Mid/Side, Mid-only)
enc "$T/speech.wav" silk_fec         -ac 1 -ar 16000 -application voip -b:a 20k -fec 1 -packet_loss 20  # LBRR
enc "$T/speech.wav" silk_fec_stereo  -ac 2 -ar 16000 -application voip -b:a 32k -fec 1 -packet_loss 20  # LBRR stereo
enc "$T/speech.wav" silk_nb_60ms     -ac 1 -ar 8000  -application voip -b:a 8k -frame_duration 60   # 3 SILK-Frames/Opus-Frame
enc "$T/speech.wav" silk_mb_40ms     -ac 1 -ar 12000 -application voip -b:a 12k -frame_duration 40  # MB, 2 SILK-Frames
enc "$T/speech.wav" silk_10ms        -ac 1 -ar 16000 -application voip -b:a 16k -frame_duration 10  # nb_subfr = 2
enc "$T/speech.wav" silk_120ms       -ac 1 -ar 16000 -application voip -b:a 16k -frame_duration 120 # Code 3, mehrere Opus-Frames
enc "$T/speech.wav" hybrid_60ms      -ac 1 -application voip -b:a 24k -frame_duration 60           # Hybrid Code 3
enc "$T/speech.wav" hybrid_stereo    -ac 2 -application voip -b:a 40k                              # Hybrid stereo
enc "$T/speech.wav" hybrid_swb_10ms  -ac 1 -application voip -b:a 20k -frame_duration 10 -cutoff 12000 # Hybrid SWB 10 ms
enc "$T/mix.wav"    switch_voip      -ac 2 -application voip -b:a 32k                              # Hybrid↔CELT + Redundanz
enc "$T/mix.wav"    switch_audio     -ac 1 -application audio -b:a 16k                             # Hybrid↔CELT bei niedriger Rate
enc "$T/mix.wav"    switch_silk_wb   -ac 1 -ar 16000 -application audio -b:a 16k                   # SILK↔CELT (16 kHz)
enc "$T/mix.wav"    switch_fec       -ac 1 -application voip -b:a 24k -fec 1 -packet_loss 15       # Wechsel + LBRR
enc "$T/music.wav"  celt_2_5ms       -t 2 -ac 2 -application lowdelay -b:a 96k -frame_duration 2.5     # lm 0
enc "$T/music.wav"  celt_5ms_mono    -ac 1 -application lowdelay -b:a 48k -frame_duration 5       # lm 1
enc "$T/music.wav"  celt_10ms_nb     -ac 1 -application lowdelay -b:a 12k -frame_duration 10 -cutoff 4000 # CELT NB
enc "$T/music.wav"  celt_stereo_lo   -ac 2 -application audio -b:a 24k                             # Intensity/Dual-Stereo
enc "$T/music.wav"  celt_stereo_hi   -t 2 -ac 2 -application audio -b:a 256k                            # viele Pulse, Splits
enc "$T/music.wav"  celt_cbr_40ms    -ac 2 -application audio -b:a 64k -vbr off -frame_duration 40 # Code 3 CBR
opusenc --quiet --set-ctl-int 4016=1 --bitrate 12 "$T/speech.wav" "$OUT/dtx.opus"            # DTX (OPUS_SET_DTX)
opusenc --quiet --framesize 60 --bitrate 16 --downmix-mono --save-range "$OUT/opusenc_60ms.rng" "$T/mix.wav" "$OUT/opusenc_60ms.opus" # + Encoder-Final-Range
ls -la "$OUT"
