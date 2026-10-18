# source11_compact — minimale, SSH-getunnelte Neuauflage (Protokoll v4)

Ferner Desktop über SSH: Linux- und Android-Clients bauen den Tunnel
selbst (Jump-Host-Kette für Server hinter CGNAT), der Server bindet nur
an `127.0.0.1:7878`. Scrollen sendet keine Texte neu (`MoveText`),
OCR-Flackern schluckt der Stabilisator. Plan und Tasks:
[`../plan/20261018_01_encrypt/`](../plan/20261018_01_encrypt/).

## Struktur

- `common/` — Protokoll v4 (Framing, `NewText`/`MoveText`/`RemoveText`
  mit content-verankerten IDs, Challenge/Auth mit Nonce + Token-MAC).
- `tunnel/` — wiederverwendbare SSH-Kette (`russh`, `direct-tcpip`,
  verschachtelte Sessions, TOFU-Pins pro Hop, Retry-Politik).
- `server/` — X11-Grab + OCR (CPU/GPU), Stabilisator (Quantisierung,
  Levenshtein, Debounce, Move-Deadband), Pacer, AV1-Tiles.
- `client/` — Desktop-Client (winit, AV1, Glyphen-Fallback inkl. CJK,
  Metriken, `--tap` für den Dissector).
- `android/` — `rust-core` (Engine + JNI, Tunnel in Rust, kein Java-Crypto)
  und Kotlin-App (Profile, TOFU-Pins, Touch/IME).
- `wireshark/` — C-Dissector + `check.sh` + Sample-pcap.
- `docker/` — `netem`-Szenarien + Xvfb-Tests. `scripts/` — Modelle,
  Release-Paket, `smoke_e2e.sh`.

Modelle: Symlink `models/` → `../source7_mvp/models` (nicht im Git).

## Schnellstart

```sh
cargo test --workspace          # ohne X11 und ohne Modelle grün
cargo fmt --check               # Tor 0
cargo clippy --workspace --all-targets -- -D warnings
./wireshark/check.sh            # Dissector: 14 Varianten + Baum
./scripts/smoke_e2e.sh          # Jump 2322 → Ziel 2323, Tap-Dissektion
```

Server (hinter CGNAT: Reverse-Tunnel zu tinyeu, s. [DEPLOY.md](DEPLOY.md)):

```sh
./target/release/lbw-server --token "$(cat ~/.lbw_token)" --budget 6000
```

Desktop-Client durch die Kette tinyeu → hpz6o:

```sh
./target/release/lbw-client --token "$(cat ~/.lbw_token)" \
  --jump kiel@51.107.16.14:22 --jump-identity ~/.ssh/tinyeu_key.pem \
  --target-sshd 127.0.0.1:2332 --lbw-port 7878 \
  --via kiel@127.0.0.1 --via-identity ~/.ssh/id_lenovo_to_hpz6o
```

Android: Felder pro Hop (Host, Port, User, Key), Ziel-sshd aus
Jump-Sicht, LBW-Port, Token — alles Profile in der App, IPs/Ports
frei konfigurierbar. Details: [WALKTHROUGH.md](WALKTHROUGH.md).

## Plan-Aspekte: übernommen vs. gestrichen

| Aspekt aus Review/Plan | Stand |
|---|---|
| Kein Volltext pro Frame (Scroll via `MoveText`/IDs) | drin (v4, T3b-Soak-Test) |
| SSH-Tunnel baut der Client selbst (Linux + Android) | drin (`tunnel/`, russh) |
| Jump-Host-Kette (CGNAT: tinyeu → hpz6o) | drin, E2E-geprüft |
| localhost-Klartext + Nonce/Auth für Wireshark | drin (`--tap`, Dissector) |
| OCR-Flackern verhindern | drin (Stabilisator `09_stabilize`) |
| Font-Fallback inkl. CJK | drin (Noto CJK → Noto → Unifont) |
| Android ohne Java-Crypto | drin (Tunnel ganz in Rust) |
| mosh-tcp als Kryptomodell | gestrichen (Klartext, schwacher Key, GPL) |
| Noise/TLS statt SSH | gestrichen (SSH reicht, Server kann sshd fordern) |
| JSch auf Android | gestrichen (veraltet, russh in Rust) |
| R8/Minify im APK-Build | gestrichen (JNI-Namen müssen leben) |
| Client-Größenlimit (3 MB) | gestrichen (SSH darf wachsen, Größe nur berichtet) |
| `gpa`-Modell aus dem Netz | gestrichen (keine HF-URL, optional/lokal) |
| Latenz-p50-Automatisierung | vertagt (HUD/Tap vorhanden, Follow-up) |
