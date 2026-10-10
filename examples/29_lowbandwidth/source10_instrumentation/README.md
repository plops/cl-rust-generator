# source10_instrumentation — Low-Bandwidth-Remote-Desktop mit Recording (1280×720)

Wie [`../source9_gpu`](../source9_gpu) (GPU-Hybrid-OCR, eine AV1-Box pro
Frame), aber Protokoll **v3** (stabile Text-IDs + Text-Delta statt
Komplett-Resend — inkompatibel zu v2, alte `.lbwlog`-Dateien bleiben lesbar).
Server und Client können per `--record` jede übertragene Nachricht plus
Timing-/Performance-Metriken in eine `.lbwlog`-Datei schreiben. `lbw-logstat`
wertet Aufzeichnungen offline aus (Durchsatz, Gaps, Dedup, Latenzen, `--deep`),
`lbw-replay` spielt sie headless wieder ab.

Plan, Tasks, Walkthrough:
[`../plan/20261010_01_instrumentation/`](../plan/20261010_01_instrumentation/),
Abhängigkeiten: [deps.md](deps.md).

```sh
# Modelle: Symlink models/ -> ../source7_mvp/models (nicht im Git)
ls models/PP-OCRv6_small_det.onnx
cargo build --release

# Server mit Aufzeichnung (lauscht nur an localhost — kein Auth!)
DISPLAY=:0 ./target/release/lbw-server --record /tmp/srv.lbwlog
# Client mit Aufzeichnung (Tunnel wie in source9)
./target/release/lbw-client --connect 127.0.0.1:7878 --record /tmp/cli.lbwlog

# Offline-Analyse + Replay
./target/release/lbw-logstat /tmp/srv.lbwlog /tmp/cli.lbwlog
./target/release/lbw-logstat --deep /tmp/srv.lbwlog /tmp/cli.lbwlog
./target/release/lbw-logstat --json /tmp/srv.lbwlog > auswertung.json
./target/release/lbw-replay /tmp/srv.lbwlog

# Tests
cargo test --workspace                        # Unit + Loopback (ohne X11/Modelle)
cargo test --release -p lbw-server --test models -- --ignored  # echte Modelle + GPU
./scripts/smoke_xvfb.sh                       # E2E ohne Recording
./scripts/smoke_record.sh                     # E2E mit Recording + logstat + replay
./scripts/render_check.sh                     # Text-Lage per Pixel (Xvfb + GL)
./wireshark/check.sh                          # C-Dissector bauen + per tshark prüfen
```

## Was gegenüber source9 neu ist

- Crate `lbw-log`: `.lbwlog`-Format (Magic + längenpräfixierte
  `bincode`-Records), `Writer`/`Reader`, Offline-Statistik, `lbw-logstat`.
- Server: `--record <pfad>`, Pipeline-Timings pro Frame (Capture/det/rec/
  Mask+Diff/Encode/Send), Input→Injektion-Latenz, Connect/Disconnect-Marker.
- Client: `--record <pfad>`, Gap-Events mit Down-Dauer, AV1-Decode-ms,
  Eingabe-Sendezeiten (→ Input→Photon-Latenz offline).
- `lbw-replay`: Headless-Replay durch echten Decoder + Szene (Canvas-Hash,
  `--ppm`, `--realtime`).
- `probe`-Example: `--record`-Option für aufgezeichnete Smokes.
- `wireshark/`: C-Dissector für (t)shark (alle v3-Varianten, Reassembly,
  `sample.pcap`, `check.sh`) — kein Lua.
- Protokoll v3: stabile Text-IDs, `RemoveText`, Delta statt Resend (Server
  `09_textids.rs`); Erkennungs-Cache (statische Zeilen ~0 ms); Stale-Flush
  bei Reconnect; `logstat --deep` (Tiefenanalyse); Text-Baseline-Fix
  (`scripts/render_check.sh`).

## Hinweise aus source9 (gelten weiter)

Xorg erforderlich (kein Wayland); erfasst wird der primäre Monitor
(RandR-Ursprung im Log). Details: [`../source9_gpu/README.md`](../source9_gpu/README.md).
