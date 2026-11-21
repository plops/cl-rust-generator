# /// script
# requires-python = ">=3.11"
# dependencies = ["numpy>=2"]
# ///
"""Grundfrequenz (Autokorrelation, 30–1200 Hz) eines WAV-Samples → Notenname.
Aufruf: uv run pitch.py [--max HZ] a.wav [...]  (--max begrenzt die Suche, z. B. 300 fuer 808s)"""
import sys, wave
import numpy as np

NAMES = ["C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-", "G#", "A-", "A#", "B-"]

def f0(path, fmax):
    with wave.open(path) as w:
        sr = w.getframerate()
        x = np.frombuffer(w.readframes(w.getnframes()), "<i2").astype(float)
        x = x.reshape(-1, w.getnchannels()).mean(axis=1)
    seg = x[int(0.05 * sr): int(0.05 * sr) + 8192]  # skip the attack transient
    seg = seg - seg.mean()
    ac = np.correlate(seg, seg, "full")[len(seg) - 1:]
    lo, hi = int(sr / fmax), int(sr / 30)
    lag = lo + np.argmax(ac[lo:hi])
    return sr / lag

args = sys.argv[1:]
fmax = 1200.0
if args[:1] == ["--max"]:
    fmax, args = float(args[1]), args[2:]
for p in args:
    f = f0(p, fmax)
    semi = round(12 * np.log2(f / 440) + 57)  # 0 = C-0, matches the tracker
    print(f"{f:8.1f} Hz  {NAMES[semi % 12]}{semi // 12}  {p.split('/')[-1]}")
