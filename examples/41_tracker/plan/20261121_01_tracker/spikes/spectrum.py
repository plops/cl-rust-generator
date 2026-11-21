# /// script
# requires-python = ">=3.11"
# dependencies = ["numpy>=2"]
# ///
"""Klang-Kennzahlen eines WAV-Renders: Energieanteil je Frequenzband,
spektraler Schwerpunkt und Crest-Faktor. Aufruf: uv run spectrum.py a.wav [b.wav ...]"""
import sys, wave
import numpy as np

BANDS = [(20, 60, "sub"), (60, 120, "bass"), (120, 500, "lowmid"),
         (500, 2000, "mid"), (2000, 6000, "presence"), (6000, 20000, "air")]

def load(path):
    with wave.open(path) as w:
        x = np.frombuffer(w.readframes(w.getnframes()), "<i2").astype(np.float64) / 32768
        return x.reshape(-1, w.getnchannels()).mean(axis=1), w.getframerate()

def analyse(path):
    x, sr = load(path)
    n = 8192
    frames = np.lib.stride_tricks.sliding_window_view(x, n)[:: n // 2] * np.hanning(n)
    psd = (np.abs(np.fft.rfft(frames, axis=1)) ** 2).mean(axis=0)
    f = np.fft.rfftfreq(n, 1 / sr)
    total = psd[(f >= 20) & (f < 20000)].sum()
    row = {name: 10 * np.log10(psd[(f >= lo) & (f < hi)].sum() / total) for lo, hi, name in BANDS}
    row["centroid_hz"] = (f * psd).sum() / psd.sum()
    row["crest_db"] = 20 * np.log10(np.abs(x).max() / np.sqrt((x ** 2).mean()))
    return row

if __name__ == "__main__":
    print(f"{'file':<14}" + "".join(f"{b[2]:>10}" for b in BANDS) + f"{'centroid':>10}{'crest':>8}")
    for p in sys.argv[1:]:
        r = analyse(p)
        print(f"{p.split('/')[-1]:<14}" + "".join(f"{r[b[2]]:>9.1f} " for b in BANDS)
              + f"{r['centroid_hz']:>9.0f} {r['crest_db']:>7.1f}")
