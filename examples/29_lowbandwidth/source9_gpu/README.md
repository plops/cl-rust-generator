# source9_gpu — Low-Bandwidth-Remote-Desktop mit GPU (1280×720)

Wie [`../source7_mvp`](../source7_mvp), aber: Server captured einen festen
1280×720-Ausschnitt des X11-Desktops, Text läuft als Vektordaten (PP-OCRv6),
der Rest als eine AV1-Box (`rav1e`) pro Frame, direkt über TCP. Der
`macroquad`-Client (`rav1d`) setzt Bild + Text zusammen und leitet
Maus/Tastatur zurück (`enigo`). Protokoll v2 (inkompatibel zu v1/640²).

KI-Ausführung (Hybrid, gemessen optimal): Der Detektor (DBNet, compute-gebunden)
läuft per ONNX-CUDA-EP auf der NVIDIA-GPU, der Erkenner (viele kleine
Zeilen-Inferenzen, eine Breite pro Zeile) auf CPU. Das 720p-Bild wird für die
Detektion auf 1280×736 gepaddet (Kanten-Replikation, keine Kachel-Stückelung),
Boxen werden auf 720 zurückgeschnitten.

Plan, Tasks, Walkthrough:
[`../plan/20261009_01_gpu_bigger/`](../plan/20261009_01_gpu_bigger/),
Abhängigkeiten: [deps.md](deps.md).

```sh
# Modelle: Symlink models/ -> ../source7_mvp/models (nicht im Git)
ls models/PP-OCRv6_small_det.onnx
cargo build --release

# entfernter Rechner mit NVIDIA-GPU (bindet nur an localhost — kein Auth!)
DISPLAY=:0 ./target/release/lbw-server
# CPU-Vergleich: ./target/release/lbw-server --cpu
# lokaler Rechner: Tunnel + Client
ssh -N -L 7878:127.0.0.1:7878 user@remote &
./target/release/lbw-client --connect 127.0.0.1:7878
# Client-Tasten: F1 HUD an/aus

# Tests
cargo test --workspace                        # Unit + Loopback (ohne X11/Modelle)
cargo test --release -p lbw-server --test models -- --ignored  # echte Modelle + GPU
LBW_CPU=1 cargo test --release -p lbw-server --test models -- --ignored  # CPU-Vergleich
cargo test --release -p lbw-server --test padding -- --ignored --nocapture  # Sweep
./scripts/smoke_xvfb.sh                       # E2E: Xvfb + xterm + Server + Probe
```

Messwerte (RTX A4000, 720p-Testbild, 40 Zeilen): Detektor warm 15 ms (CPU:
83 ms), Erkenner 385 ms auf CPU (CUDA: 994 ms — Formwechsel pro Zeile). Details im
Walkthrough.

## Was gegenüber source6 fehlt (bewusst)

Wie `source7_mvp`: Android-Client, YOLO-GUI-Detektor, Scheduler/Drosselung,
stabile Text-IDs, Resume/Ack/Heartbeat, Zwischenablage, Zoom, Unifont.

## Hinweis: Xorg erforderlich (kein Wayland)

Capture (`scrap`) und Eingabe (`enigo`/XTEST) sprechen reines X11. In einer
Wayland-Sitzung sieht der Server nur den schwarzen XWayland-Root — das Bild
bleibt stehen, obwohl Eingaben fehlerfrei ankommen. Der Server warnt beim
Start, wenn er eine Wayland-Sitzung erkennt. Ob er Bildinhalt sieht, zeigt
die Headless-Probe im Smoke (`probe: OK (… Texte, … Kacheln …)`); ein
Standbild-Capture schreibt der Padding-Test nach `/tmp/padding_frame.png`.

## Hinweis: mehrere Monitore

Erfasst wird der primäre Monitor (`scrap`). Der Server fragt dessen Ursprung
per RandR ab und rechnet ihn auf die Mauskoordinaten (`[input]
Monitor-Ursprung …` im Log); sonst landen Klicks auf dem falschen Monitor.
Schlägt die Abfrage fehl, gilt +0+0 (Ein-Monitor-Verhalten).
