# source7_mvp — Low-Bandwidth-Remote-Desktop als MVP

Minimalversion von `../source6`: Server captured einen festen
640×640-Ausschnitt des X11-Desktops, schickt Text als Vektordaten
(PP-OCRv6) und den Rest als AV1-Kacheln im exakten 10×10-Raster
(64×64, `rav1e`), direkt über TCP. Der `macroquad`-Client (`rav1d`)
setzt Bild + Text zusammen und leitet Maus/Tastatur zurück (`enigo`).

Plan, Tasks, Walkthrough:
[`../plan/20261003_01_simplify/`](../plan/20261003_01_simplify/),
Abhängigkeiten: [deps.md](deps.md).

```sh
# Modelle aus source6 wiederverwenden (nicht im Git)
ls ../source6/models/PP-OCRv6_small_det.onnx
cargo build --release

# entfernter Rechner (X11, bindet nur an localhost — kein Auth im Protokoll!)
DISPLAY=:0 ./target/release/lbw-server --models ../source6/models
# lokaler Rechner: Tunnel + Client
ssh -N -L 7878:127.0.0.1:7878 user@remote &
./target/release/lbw-client --connect 127.0.0.1:7878
# Client-Tasten: F1 HUD an/aus

# Tests
cargo test --workspace                        # Unit + Loopback (ohne X11/Modelle)
cargo test --release -p lbw-server --test models -- --ignored  # echte Modelle
./scripts/smoke_xvfb.sh                       # E2E: Xvfb + xterm + Server + Probe
```

## Was gegenüber source6 fehlt (bewusst)

Android-Client, YOLO-GUI-Detektor, Scheduler/Drosselung, stabile Text-IDs,
Resume/Ack/Heartbeat, Zwischenablage, Zoom, Unifont. Das Protokoll ist
nicht kompatibel zu source6. Details: Walkthrough im Plan-Ordner.

## Hinweis: Xorg erforderlich (kein Wayland)

Capture (`scrap`) und Eingabe (`enigo`/XTEST) sprechen reines X11. In einer
Wayland-Sitzung sieht der Server nur den schwarzen XWayland-Root — das Bild
bleibt stehen, obwohl Eingaben fehlerfrei ankommen. Der Server warnt beim
Start, wenn er eine Wayland-Sitzung erkennt. Mit `--dump frame.png` lässt
sich prüfen, was der Server tatsächlich sieht.

## Hinweis: mehrere Monitore

Erfasst wird der primäre Monitor (`scrap`). Der Server fragt dessen Ursprung
per RandR ab und rechnet ihn auf die Mauskoordinaten (`[input]
Monitor-Ursprung …` im Log); sonst landen Klicks auf dem falschen Monitor.
Schlägt die Abfrage fehl, gilt +0+0 (Ein-Monitor-Verhalten).
