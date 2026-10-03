# source8_transpiled — Low-Bandwidth-Remote-Desktop, transpiliert

Dieselbe MVP-Funktionalität wie [`../source7_mvp`](../source7_mvp/):
Server captured einen festen 640×640-Ausschnitt des X11-Desktops,
schickt Text als Vektordaten (PP-OCRv6) und den Rest als AV1-Kacheln
im exakten 10×10-Raster (64×64, `rav1e`), direkt über TCP. Der
`macroquad`-Client (`rav1d`) setzt Bild + Text zusammen und leitet
Maus/Tastatur zurück (`enigo`). Der gesamte Rust-Code ist aus
Lisp-Quellen in [`gen/`](gen/) mit
[`cl-rust-generator`](../../) erzeugt — `gen/` ist die Quelle der
Wahrheit, die Crates sind generiert.

Plan, Tasks, Walkthrough:
[`../plan/20261003_03_transpiler/`](../plan/20261003_03_transpiler/),
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

## Generator (`gen/`)

Alle `.rs`-Dateien, Manifeste, Skripte und dieser README werden aus
`gen/` erzeugt (`client.lisp`, `common.lisp`, `server.lisp`,
`texts.lisp`, `gen.lisp`, geteilte Helfer in `00_util.lisp`).
Generierte Dateien nie von Hand ändern — immer den Generator anpassen
und neu erzeugen:

```sh
# aus dem Repo-Root (cl-rust-generator):
sbcl --eval '(ql:register-local-projects)' \
  --load examples/29_lowbandwidth/source8_transpiled/gen/gen.lisp --quit
```

Der Generator nutzt Lisp-Funktionen und `,@`-Splices zur Faktorisierung
(z. B. eine Tastentabelle für Client und Server); Details stehen im
Walkthrough im Plan-Ordner.

## Was gegenüber source6 fehlt (bewusst)

Android-Client, YOLO-GUI-Detektor, Scheduler/Drosselung, stabile Text-IDs,
Resume/Ack/Heartbeat, Zwischenablage, Zoom, Unifont. Das Protokoll ist
nicht kompatibel zu source6. Details: Walkthrough im Plan-Ordner.

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
