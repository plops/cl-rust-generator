# /// script
# requires-python = ">=3.11"
# dependencies = ["numpy>=2"]
# ///
"""Erzeugt die SVG-Abbildungen des Walkthroughs aus WAV-Renders.
Aufruf: uv run figures.py <wav-ordner> <img-ordner>
Erwartet: dnb_before/after.wav, phonk_before/after.wav, mixed_amen_jungle.wav"""
import sys
from pathlib import Path
import numpy as np
from spectrum import BANDS, analyse, load

src, out = Path(sys.argv[1]), Path(sys.argv[2])

def bands_svg(_):
    rows = [("DnB vorher", "dnb_before"), ("DnB nachher", "dnb_after"),
            ("Phonk vorher", "phonk_before"), ("Phonk nachher", "phonk_after")]
    data = [(label, analyse(str(src / f"{f}.wav"))) for label, f in rows]
    W, H, left, top, bh = 760, 330, 110, 40, 16
    cols = ["#9ecae1", "#3182bd", "#fdae6b", "#e6550d"]
    g = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" font-family="sans-serif" font-size="12">',
         '<rect width="100%" height="100%" fill="white"/>',
         f'<text x="{left}" y="20" font-weight="bold">Energieanteil je Band (dB relativ zur Gesamtenergie; länger = mehr Energie)</text>']
    scale = (W - left - 30) / 30  # 0 dB … −30 dB
    for b, (_, _, name) in enumerate(BANDS):
        y0 = top + b * (4 * bh + 12)
        g.append(f'<text x="{left - 8}" y="{y0 + 2 * bh + 4}" text-anchor="end">{name}</text>')
        for k, (label, r) in enumerate(data):
            w = max(0.0, 30 + r[name]) * scale
            y = y0 + k * bh
            g.append(f'<rect x="{left}" y="{y}" width="{w:.1f}" height="{bh - 2}" fill="{cols[k]}"/>')
            g.append(f'<text x="{left + w + 4:.1f}" y="{y + bh - 5}" font-size="10">{r[name]:.1f}</text>')
    for k, (label, _) in enumerate(data):
        g.append(f'<rect x="{left + k * 150}" y="{H - 18}" width="12" height="12" fill="{cols[k]}"/>'
                 f'<text x="{left + k * 150 + 16}" y="{H - 8}">{label}</text>')
    g.append("</svg>")
    (out / "band_energy.svg").write_text("\n".join(g))

def envelope_svg(name, bpm, title):
    x, sr = load(str(src / f"{name}.wav"))
    hop = sr // 100
    env = np.abs(x[: len(x) // hop * hop]).reshape(-1, hop).max(axis=1)
    W, H, pad = 760, 190, 40
    sx = (W - 2 * pad) / len(env)
    base, h = H - 30, H - 70
    g = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" font-family="sans-serif" font-size="12">',
         '<rect width="100%" height="100%" fill="white"/>',
         f'<text x="{pad}" y="20" font-weight="bold">{title}</text>']
    pat = 64 * 60 / (bpm * 4)
    t = pat
    while t < len(env) / 100:
        xx = pad + t * 100 * sx
        g.append(f'<line x1="{xx:.1f}" x2="{xx:.1f}" y1="30" y2="{base}" stroke="#999" stroke-dasharray="4 3"/>')
        t += pat
    pts = " ".join(f"{pad + i * sx:.1f},{base - v * h:.1f}" for i, v in enumerate(env))
    g.append(f'<polyline points="{pts}" fill="none" stroke="#3182bd" stroke-width="1"/>')
    g.append(f'<line x1="{pad}" x2="{W - pad}" y1="{base}" y2="{base}" stroke="black"/>')
    for s in range(0, int(len(env) / 100) + 1, 2):
        g.append(f'<text x="{pad + s * 100 * sx - 4:.1f}" y="{base + 14}" font-size="10">{s}s</text>')
    g.append("</svg>")
    (out / f"envelope_{name}.svg").write_text("\n".join(g))

bands_svg(None)
envelope_svg("mixed_amen_jungle", 175,
             "Amen Jungle: Spitzenpegel je 10 ms, gestrichelt = Pattern-Grenzen (Pattern 0 Loop, Pattern 1 Chops)")
envelope_svg("dnb_after", 174,
             "Two-Step Reese: Pattern 0 nur Drums, ab Pattern 1 Reese-Bass (Pegelsprung)")
print("ok")
