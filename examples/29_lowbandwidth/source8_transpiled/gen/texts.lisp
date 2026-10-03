(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; texts.lisp --- non-Rust outputs (Cargo manifests, scripts, docs).
;;;; Grows with every task; T0 only carries the workspace manifest.

(defun workspace-cargo-toml ()
  "[workspace]
resolver = \"3\"
members = [\"common\", \"server\", \"client\"]

[workspace.package]
version = \"0.1.0\"
edition = \"2024\"
authors = [\"Wol Pumba <wolpumba@gmail.com>\"]
license = \"MIT\"

[profile.release]
opt-level = 3

# Debug-Builds: Abhängigkeiten optimieren (Encoder/Decoder sonst ~20x langsamer).
[profile.dev.package.\"*\"]
opt-level = 3
")

(defun smoke-xvfb-sh ()
  "#!/bin/sh
# E2E-Smoke: Xvfb + xterm + lbw-server (echte Modelle) + Headless-Probe.
# Aufruf aus source8_transpiled/: ./scripts/smoke_xvfb.sh
# Env: DISPLAY_NR (Default 99), PORT (Default 17878),
#      MODELS (Default ../source6/models).
set -eu

DISPLAY_NR=\"${DISPLAY_NR:-99}\"
PORT=\"${PORT:-17878}\"
MODELS=\"${MODELS:-../source6/models}\"
DISP=\":$DISPLAY_NR\"

for f in PP-OCRv6_small_det.onnx PP-OCRv6_small_rec.onnx inference.yml; do
  if [ ! -f \"$MODELS/$f\" ]; then
    echo \"smoke: Modell fehlt: $MODELS/$f (vgl. source6/scripts/fetch_models.sh)\" >&2
    exit 2
  fi
done

cleanup() {
  kill \"$SERVER_PID\" \"$XTERM_PID\" \"$XEV_PID\" 2>/dev/null || true
  # Xvfb erst nach den Clients beenden.
  sleep 0.5
  kill \"$XVFB_PID\" 2>/dev/null || true
}
trap cleanup EXIT

Xvfb \"$DISP\" -screen 0 1280x1024x24 &
XVFB_PID=$!
sleep 1
# xev oben (fängt den Probe-Klick bei 100,100), xterm mit Text darunter.
stdbuf -o0 -e0 env DISPLAY=\"$DISP\" xev -geometry 640x160+0+0 > /tmp/smoke_xev.log 2>&1 &
XEV_PID=$!
DISPLAY=\"$DISP\" xterm -geometry 100x30+0+200 -e sh -c 'echo SMOKE-TEST-640; exec sleep 300' &
XTERM_PID=$!
sleep 1

cargo build --release -p lbw-server -p lbw-client 2>&1 | tail -n 2
DISPLAY=\"$DISP\" ./target/release/lbw-server \\
  --listen \"127.0.0.1:$PORT\" --models \"$MODELS\" -v > /tmp/smoke_server.log 2>&1 &
SERVER_PID=$!
sleep 2

cargo run --release -p lbw-client --example probe -- \"127.0.0.1:$PORT\" 2>&1 | tail -n 12

if grep -q \"Session-Fehler\" /tmp/smoke_server.log; then
  echo \"smoke: FEHLER — Server meldet Session-Fehler:\" >&2
  grep \"Session-Fehler\" /tmp/smoke_server.log >&2
  exit 1
fi
if ! grep -q \"ButtonPress\" /tmp/smoke_xev.log; then
  echo \"smoke: FEHLER — kein Mausklick im xev-Fenster angekommen\" >&2
  exit 1
fi
if ! grep -q \"\\[input\\] Button\" /tmp/smoke_server.log; then
  echo \"smoke: FEHLER — Server loggt keine Eingabe-Events (-v)\" >&2
  exit 1
fi
if ! grep -q \"Monitor-Ursprung +0+0\" /tmp/smoke_server.log; then
  echo \"smoke: FEHLER — Server loggt keinen RandR-Monitor-Ursprung\" >&2
  exit 1
fi
echo \"smoke: OK (Server-Log: /tmp/smoke_server.log)\"
")

(defun collect-sh ()
  "for i in ./Cargo.toml \\
	     ./client/Cargo.toml client/src/*.rs \\
	     ./common/Cargo.toml common/src/*.rs \\
	     ./server/Cargo.toml server/src/*.rs
do
    echo \"// start of \"$i
    cat $i
done

echo \"schaue den code an. das ziel ist mit moeglichst wenig code eine remote control loesung fuer 6kB/s verbindungen zu bauen. ist der code so minimal wie es geht (wir haben aufloesung auf 640x640 fixiert). vielleicht koennte man noch code sparen indem wir keinen fallback fuer fehlende modelle erlauben. ausserdem schaue dir an wie wir av1 codieren. frueher setzten wir mal die tiles zum groesstmoeglichen rechteck zusammen um datenrate zu minimieren, ist es wie es jetzt ist okay oder was ist der beste weg?\"
")

(defun readme-md ()
  "# source8_transpiled — Low-Bandwidth-Remote-Desktop, transpiliert

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
sbcl --eval '(ql:register-local-projects)' \\
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
")

(defun deps-md ()
  "# deps.md — Abhängigkeiten von `source8_transpiled` (GitHub `org/projekt` für DeepWiki)

Stand: 2026-10-03, alle auf neuester lauffähiger Version (`cargo upgrade`).
Generiert aus `gen/texts.lisp`; Versionen und Manifeste sind mit `source7_mvp` identisch (bytegleich geprüft).

| Crate | Version | GitHub | Wo | Wofür (ersetzt in `source6`) |
|---|---|---|---|---|
| `serde` | 1.0.229 | `serde-rs/serde` | common, server | Derive für Protokoll-Typen (ersetzt Hand-Codec) |
| `bincode` | 2.0.1 | `bincode-org/bincode` | common | `bincode::serde::{encode_to_vec, decode_from_slice}` mit `config::standard()` (ersetzt `common/02_codec.rs`) |
| `clap` | 4.6.7 | `clap-rs/clap` | server, client | `#[derive(Parser)]` CLI (ersetzt `01_config.rs` handgeparst) |
| `image` | 0.24.9 | `image-rs/image` | server | `RgbImage`, Crop (ersetzt `server/03_image.rs`). Bewusst 0.24 wie `macroquad` (statt 0.25): eine Version im Baum. `default-features = false` — Produktion nutzt nur `Rgb`/`RgbImage` ohne jedes Format; `png`/`pnm` nur als Dev-Dep für Tests (PPM-Testbild öffnen, PNG-Capture speichern). |
| `scrap` | 0.5.0 | `quadrupleslap/scrap` | server | MIT-SHM-Capture, BGRX→RGB (ersetzt `x11rb`-`GetImage` in `02_capture.rs`) |
| `enigo` | 0.6.1 | `enigo-rs/enigo` | server | Maus/Tastatur-Injektion (ersetzt `x11rb`-XTEST in `13_input.rs`) |
| `serde_yaml` | 0.9.34 | `dtolnay/serde-yaml` | server | `character_dict` aus `inference.yml` (ersetzt `load_dict`-Handparser) |
| `rav1e` | 0.8.1 | `xiph/rav1e` | server | AV1-Still-Picture-Encoder, **ohne** `asm` (kein `nasm` nötig) |
| `rav1d` | 1.1.0 | `memorysafety/rav1d` | client | AV1-Decoder (`bitdepth_8`), wie bisher |
| `ort` | 2.0.0-rc.13 | `pykeio/ort` | server | ONNX Runtime für PP-OCRv6 (nur Detektion+Erkennung, kein YOLO mehr) |
| `macroquad` | 0.4.16 | `not-fl3/macroquad` | client | Fenster, Textur, Default-Font-Text, Eingabe (ohne Unifont, ohne Clipboard) |
| `x11rb` | 0.13.2 | `psychon/x11rb` | server | RandR-`GetMonitors` für den Maus-Ursprung (war schon transitiv via `enigo` dabei; kein neues Crate, keine neuen Systemlibs) |

Bekannte Auffälligkeiten:

- `bincode` 3.0.0 existiert auf crates.io, ist aber ein kaputter Stub
  (einzige Zeile: `compile_error!(\"https://xkcd.com/2347/\")`). Wir bleiben
  bewusst auf 2.0.1, bis ein lauffähiges 3.x erscheint.
- `image` ist seit 2026-10-03 einheitlich 0.24.9 (direkt + via
  `macroquad` — davor 0.25.10/0.24.9 doppelt) und ohne Default-Features:
  Server-Produktion ohne Formate (147 statt 181 Crates im Graphen),
  `exr`/`half`/`tiff`/`jpeg`/`gif`/… entfallen ersatzlos. Es werden
  weiterhin keine `image`-Typen über die macroquad-Grenze gereicht (nur
  `Vec<u8>`).
- Dadurch aufgelöst: `zerocopy` 0.7/0.8 (0.8 kam nur via `half` ← `exr`
  ← `image`-Defaults; übrig ist 0.7 via `rav1d`).
- Verbleibende Doppelversionen sind rein transitiv und von uns nicht
  behebbar (`cargo tree -d`): `bitflags` 1 (via `png` ← `macroquad`) / 2
  (via `rav1d`/`x11rb`/…), `miniz_oxide` 0.8/0.9 (beide via `png` 0.17
  selbst), `hashbrown` 0.15 (via `fontdue` ← `macroquad`) / 0.17 (via
  `indexmap` ← `serde_yaml`), `cfg-if` 0.1 (via `scrap`, stale) / 1,
  `syn` 2 (Derive-Helfer von `rav1e`/`rav1d`) / 3 (nur Build-Zeit,
  kein Binary-Anteil). Keine dieser Spreizungen betrifft ein direktes Dep.
- `ort` ist weiterhin nur als Release-Candidate aktuell (rc.13).
- `x11rb` 0.14.0 evaluiert (2026-10-03) und verworfen: `protocol::randr`
  löst dort nicht mehr auf — wir bleiben auf 0.13.2, bis der Importpfad
  geklärt ist (eine Zeile in `06_input.rs`).
- `block` 0.1.6 (via `scrap`): Future-Incompat-Warnung bei jedem Build
  (`static of uninhabited type`). Nicht per Upgrade behebbar — `block`
  0.1.6 und `scrap` 0.5.0 sind jeweils final/aktuell (Upstream stale),
  `scrap` zieht `block` unbedingt herein, obwohl nur macOS-Code es nutzt.
  Harmlos (Build erfolgreich, Pfad auf Linux ungenutzt); Entfernung
  verworfen: Vendor-Patch (~700 Zeilen), Hand-SHM-Capture (~100 Zeilen +
  unsafe) oder `--cap-lints allow` (maskiert alle Dep-Warnungen).

Bewusst **nicht** übernommen (evaluiert, verworfen):

| Crate | GitHub | Grund |
|---|---|---|
| `governor` | `antifuchs/governor` | Kein Rate-Limit im MVP nötig; direktes TCP-Schreiben genügt |
| `xcap` | `nashaofu/xcap` | Verworfene Alternative zu `scrap`: 0.9 braucht Wayland+EGL-Systemlibs ohne Feature-Gate; `scrap` braucht nur libxcb |
| `imageproc` | `image-rs/imageproc` | Maskieren sind 10 Zeilen mit `image` allein; keine extra Dep |
| `miniquad` (direkt) | `not-fl3/miniquad` | Nur transitiv via `macroquad`; kein Clipboard im MVP |

Modelle (Laufzeit, nicht im Git — aus `../source6/models` wiederverwendet):

| Datei | Quelle |
|---|---|
| `PP-OCRv6_small_det.onnx`, `PP-OCRv6_small_rec.onnx`, `inference.yml` | HuggingFace `PaddlePaddle/PP-OCRv6_small_{det,rec}_onnx` (GitHub `PaddlePaddle/PaddleOCR`) |

Gestrichen: `gpa_640_int8.onnx` (YOLO/GUI-Detektor entfällt), GNU Unifont
(Client nutzt den eingebauten `macroquad`-Font).

Systempakete für Build/Laufzeit: `libxcb1-dev`, `libxcb-shm0-dev`,
`libxcb-randr0-dev` (nur Link-Symlinks für `scrap`; kein `nasm`, keine
X11-Header). Für Tests/Smoke: `xvfb`, `xterm`.

DeepWiki-Beispiel: `ask_wiki_question(repoName=\"enigo-rs/enigo\", question=\"...\")`.
")
