# Spikes und Werkzeuge zu 20261027_01_silk

Kleine Experimente, deren Ergebnisse in `plan.md` §1 und
`walkthrough.md` §2 stehen. Key immer aus
`/workspace/src/cl-rust-generator/api_key.txt` (nie committen).

| Datei | Was sie zeigt | Aufruf |
|---|---|---|
| `spike/src/main.rs` | opus-pure: 24/16 kHz, 60/20 ms, 12 kbit/s, Wideband-Cap → 100 % `SilkOnly`, ~10,7 kbit/s, Decoder direkt 16 kHz | `cd spike && cargo run --release` |
| `spike/examples/ws.rs` | tokio-tungstenite + rustls nur mit `ring` → `setupComplete` von Gemini (ohne Provider: Panic) | `cd spike && cargo run --example ws` |
| `spike/probe.py` | Gemini-TTS-Satz → Live-Translate mit `translationConfig`, Transkription auf `setup`-Ebene, misst Audio + Transkripte | `cd spike && uv run --with websockets python probe.py` |
| `tools/package.json` | feste Versionen der Browser-Test-Treiber (playwright-core, ws) | wird von `source2/scripts/browser-setup.sh` mit denselben Versionen installiert |

Nicht im Repo (zu groß bzw. reproduzierbar per
`source2/scripts/browser-setup.sh` nach `/tmp/live-hil`): Chrome for
Testing 156.0.8078.47 (~403 MB), `node_modules` (~14 MB), Fake-Mikro-WAVs
(aus `chinese/…webm` bzw. per Gemini-TTS erzeugt), Screenshots und Logs.

Stimmen-Spikes (2026-10-27):

| Datei | Was sie zeigt | Aufruf |
|---|---|---|
| `spike/voice_probe.py` | Translate-Modell mit `speechConfig`: Stimme wird akzeptiert, aber die Ausgabe folgt der Stimme des Sprechers (F0 + Gemini-Urteil) | `uv run --with websockets --with numpy python voice_probe.py Charon` (bzw. `Kore`) |
| `spike/voice_probe_live.py` | `gemini-3.8-live` + Dolmetsch-Anweisung: gewählte Stimme wird befolgt | `uv run --with websockets --with numpy python voice_probe_live.py Charon` oder mit Roh-PCM-Datei `x.raw` (spart TTS-Kontingent) |
| `spike/vad_probe.py` | `gemini-3.8-live` als Dolmetscher bei DURCHGEHENDER Sprache: wie viel übersetztes Audio bei welcher `realtimeInputConfig`-Variante (Server-VAD liefert 0,0 s; eigene Segmentierung ~8–11 s). Beleg für die Segmentierung in `walkthrough.md` 2.14 | `uv run --with websockets --with numpy python vad_probe.py [SEG_S] translate\|default\|no_interrupt\|no_interrupt+fast_end\|manual3s` |

Achtung: Die TTS-Referenz verbraucht Free-Tier-Kontingent (429 nach ~10
Aufrufen); mit einer `.raw`-Datei (PCM 24 kHz) als Argument entfällt TTS.
