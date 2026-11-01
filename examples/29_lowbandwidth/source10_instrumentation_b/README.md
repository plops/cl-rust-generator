# source10_instrumentation_b — Latenz unter Last, CPU-only (1280×720)

Wie [`../source10_instrumentation`](../source10_instrumentation) (Protokoll
**v3**, `.lbwlog`-Recording, `lbw-logstat`, `lbw-replay` — Client, Server
und Wireshark-Dissector bleiben untereinander kompatibel), aber mit
Fokus auf **Text-Echo und Eingaben während großer Bildänderungen**:

- **Sender-Thread mit zwei Spuren** (`server/src/12_send.rs`): Texte gehen
  sofort, Kacheln gepacet — die Capture/OCR-Schleife wartet nie auf den Kanal.
- **Kachel-Deckel** `--max-tile` (Default 2400 B ≈ 0,4 s bei 6 kB/s):
  große Änderungen werden in Streifen zerlegt (`encode_capped`).
- **Schattenbild**: kodiert wird nur bei freier Kachel-Spur, als Differenz zu
  dem, was der Client schon hat — nichts Veraltetes, nichts geht verloren.
- **CPU-only** (kein `cuda`-Feature, kein `--cpu`).
- Client fasst Mausbewegungen zusammen.
- `lbw-logstat`: Erst-Echo-Latenz (statt verfälschender 1:1-FIFO),
  Kanal-Simulation je Nachricht, Logs der Version 2 wieder lesbar.
- **Mobil-Clients** (`mobile/`): gemeinsamer Rust-Kern (`mobile/core`:
  SSH-Jump-Kette mit Ed25519-Keys + TOFU-Pins, Protokoll v3, AV1, Szene),
  Android-App (Kotlin, JNI) und iOS-App (SwiftUI, C-ABI `lbw_core.h`);
  gebaut per GitHub Actions (`.github/workflows/release-29-source10b.yml`).
- **Prüfstand mit echten Daten** (`client/examples/bench.rs`, A/B gegen
  source10 über `tools/bench10`) und HIL-Smoke hinter einem Drossel-Proxy.

Review, Plan, Tasks, Handbuch, Walkthrough:
[`../plan/20261031_01_review_b/`](../plan/20261031_01_review_b/),
Abhängigkeiten: [deps.md](deps.md).

```sh
ls models/PP-OCRv6_small_det.onnx           # Symlink -> ../source7_mvp/models
cargo build --release

DISPLAY=:0 ./target/release/lbw-server --budget 6000 --max-tile 2400 --record /tmp/srv.lbwlog
./target/release/lbw-client --connect 127.0.0.1:7878 --record /tmp/cli.lbwlog
./target/release/lbw-logstat /tmp/srv.lbwlog /tmp/cli.lbwlog

# Prüfstand: echtes Log → Session → 6-kB/s-Drosselleser (A/B)
cargo run --release -p lbw-client --example bench -- \
  ../source10_instrumentation/20261018_0839.lbwlog --from 10 --secs 60
cargo run --release --manifest-path tools/bench10/Cargo.toml -- \
  ../source10_instrumentation/20261018_0839.lbwlog --from 10 --secs 60

cargo test --workspace                       # Unit + Loopback + Realdaten (falls Log da)
cargo test --release -p lbw-server --test models -- --ignored   # echte Modelle, CPU
./scripts/smoke_hil.sh                       # Xvfb + Modelle + 6-kB/s-Proxy + Tippen

mobile/android/build_android.sh              # .so + APK + JVM-Tests (SDK/NDK/JDK 21)
mobile/android/emulator_e2e.sh               # App im Emulator durch Jump-Kette (KVM)
mobile/ios/build_ios.sh                      # nur macOS: XCFramework + App + .ipa
```
