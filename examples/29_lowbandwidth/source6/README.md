# source6 — Low-Bandwidth-Remote-Desktop (6 kB/s)

Server nimmt einen 640×640-Ausschnitt eines X11-Bildschirms auf, schickt
Text als Vektordaten (PP-OCRv6) und den Rest als AV1-Still-Picture-Kacheln
(rav1e), priorisiert und gedrosselt. Der Macroquad-Client (rav1d, GNU
Unifont) setzt das Bild zusammen und leitet Maus/Tastatur per XTEST zurück.
Plan, Tasks, Walkthrough: [`../plan/20260929_01_lowbandwidth/`](../plan/20260929_01_lowbandwidth/).
Abhängigkeiten: [deps.md](deps.md).

```sh
./scripts/fetch_models.sh                       # Modelle nach models/ (nicht im Git)
cargo build --release                           # Server, Client, Drossel
cargo build --profile min -p lbw-client         # kleinstes Client-Binary

# entfernter Rechner (X11, bindet nur an localhost — kein Auth im Protokoll!)
target/release/lbw-server --x 0 --y 0 -v
# lokaler Rechner: Tunnel + Client
autossh -M 0 -N -o ServerAliveInterval=15 -o ServerAliveCountMax=8 -L 7878:127.0.0.1:7878 user@remote &
target/release/lbw-client --connect 127.0.0.1:7878
# Client startet mit 2:1-Zoom (1280×1280 bei --size 640).
# Für 1:1-Darstellung: lbw-client --no-zoom
# Client-Tasten: F1 HUD, F2 Text auswählen → Zwischenablage, F3 Zwischenablage tippen

# Tests
cargo test --workspace                                           # Unit + Loopback (ohne X11/Modelle)
cargo test --release -p lbw-server --test models -- --ignored    # echte Modelle
./scripts/smoke_xvfb.sh     # E2E: 2× Xvfb, 6 kB/s, 60 s Blackout, Abriss (BLACKOUT=10 für kurz)
./scripts/ssh_tunnel.sh     # E2E über lokalen sshd + ssh -L, Tunnel-Neuaufbau
```

## Android-Client

Hybrid: Rust-Kern `android_client/rust-core` (lbw-client ohne macroquad →
`liblbw_core.so`, JNI) + Kotlin-Oberfläche `android_client/android-app`
(ohne AndroidX; JSch-SSH-Tunnel mit Host-Key-Pinning, Touch: Trackpad/Direkt/
Auswahl, Pinch-Zoom, Tastenleiste Esc/Tab/Ctrl/Alt/F1–F12). Das APK baut die
GitHub Action [`android-lbw.yml`](../../../.github/workflows/android-lbw.yml)
(Artefakt `lbw-client-debug-apk`). Plan/Walkthrough:
[`../plan/20260929_03_android/`](../plan/20260929_03_android/), Tasks:
[android_client/task.md](android_client/task.md), Abhängigkeiten:
[android_client/deps.md](android_client/deps.md).

```sh
cd android_client
scripts/build_android.sh     # Rust → .so (arm64-v8a, x86_64), Unifont, JVM-Tests, Lint, APK
scripts/ci_local.sh          # alle Schritte der Action lokal (inkl. SSH-Tunnel-Tests gegen sshd)
scripts/emulator_e2e.sh      # HIL: Emulator (KVM) ↔ lbw-server auf Xvfb, direkt + SSH, 19 Nachweise

# App per Intent starten (Emulator erreicht den Host als 10.0.2.2)
adb shell am start -n de.lbw.client/.MainActivity --es addr 10.0.2.2:7878 --ez autoconnect true
```

Serverseite wie oben; das Handy verbindet sich per SSH-Tunnel (in der App)
mit `127.0.0.1:7878` auf dem SSH-Host. Debug-APK 6,9 MB (Unifont 5,3 MB,
`liblbw_core.so` 1,8 MB arm64).

## Messwerte (2026-09-29, Threadripper PRO 7955WX, CPU, Release)

| Größe | Wert |
|---|---|
| Client-Binary (`--profile min` / release) | 2,0 MB / 2,4 MB, nur libc/libm/libgcc |
| Server-Binary (ORT statisch) | 24,9 MB (+ Modelle 52 MB) |
| DBNet-Detektion 640² | 55–65 ms |
| Erkennung je Zeile / mit Cache (unverändert) | ~11 ms / ~0 ms |
| GUI-Detektor 640² int8 | 80–100 ms |
| AV1 640² Speed 10 | 80–140 ms |
| HN-Seite 640²: AV1 roh → maskiert | 37 610 B → 1 966 B; Text 2 606 B (37 Zeilen) |
| xterm, Tippen im Client → Text zurück (6 kB/s, 50 ms) | ~0,1 s nach letzter Taste |
| Loopback: Textänderung (6 kB/s, 30 ms) | 62 ms |
| Webseite öffnen: Text / Bild komplett | Text zuerst, AV1 (2,2 kB, 1 + 3 Icon-Kacheln) +0,4 s |
| 60 s Blackout | keine Trennung, gestaute Eingaben kommen danach an |
