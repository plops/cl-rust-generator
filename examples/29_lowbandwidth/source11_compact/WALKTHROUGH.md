# Walkthrough — vom Server bis zur Android-App

Setup: `lbw-server` auf hpz6o (hinter CGNAT), Jump über tinyeu
(`51.107.16.14:22`), Reverse-Tunnel `2332 → 127.0.0.1:22` (s. DEPLOY.md).
Token liegt auf beiden Seiten in `~/.lbw_token` (0600).

## 1. Server starten

```sh
export DISPLAY=:0
./target/release/lbw-server --token "$(cat ~/.lbw_token)" --budget 6000
# „lauscht 127.0.0.1:7878" — mehr sieht man nicht; Rest läuft im Tunnel.
```

## 2. Desktop-Client (Linux)

```sh
./target/release/lbw-client --token "$(cat ~/.lbw_token)" \
  --jump kiel@51.107.16.14:22 --jump-identity ~/.ssh/tinyeu_key.pem \
  --target-sshd 127.0.0.1:2332 --lbw-port 7878 \
  --via kiel@127.0.0.1 --via-identity ~/.ssh/id_lenovo_to_hpz6o
# F1: HUD (FPS, Bytes, Texte). Scrollen im Browser bewegt nur IDs.
```

Erster Start lernt die Hostkeys (TOFU) und zeigt die Fingerprints;
mit `--pin SHA256:… --pin SHA256:…` (Hop-Reihenfolge) wird geprüft.

## 3. Mitschneiden (Wireshark)

```sh
./target/release/lbw-client --tap 17878 ...   # wie oben, Rest gleich
tshark -i lo -f 'tcp port 17878' -w tap.pcap  # Klartext nur auf lo
./wireshark/check.sh                          # Dissector-Verifikation
```

## 4. Android-App

1. APK installieren (`android/scripts/build_android.sh` baut `.so` + APK).
2. Profil anlegen: Jump-Host `51.107.16.14:22` + User + Key (PEM einfügen),
   Ziel-Host (aus Jump-Sicht `127.0.0.1`, Port `2332`) + User + Key,
   Ziel-sshd `127.0.0.1:2332`, LBW-Port `7878`, Token.
3. „Verbinden“ — beim ersten Mal werden beide Hostkeys gelernt und unter
   `pinKey()` gespeichert („Host-Keys vergessen“ löscht sie).
4. Trackpad-Modus: eine Finger-Berührung = Maus, Tastatur-Icon = IME.

## 5. Tests fahren

```sh
cargo test --workspace
cd android/android-app && ./gradlew :app:testDebugUnitTest
./scripts/smoke_e2e.sh            # Kette + Tap-Dissektion, alles lokal
bash android/scripts/emulator_e2e.sh   # echte App im Emulator (A2)
```
