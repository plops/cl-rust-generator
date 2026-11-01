# Handbuch: Mobil-Clients (Android + iPhone) für `source10_instrumentation_b`

Die Mobil-Clients zeigen denselben entfernten Bildschirm wie der
Desktop-Client und bauen die Verbindung **selbst** über eine SSH-Kette
auf — mit Jump-Host und Ed25519-Keys, ohne Termux, ohne ssh-App, ohne
Java- oder Swift-Krypto. Bedienung des Servers und das Protokoll erklärt
das [Handbuch](handbuch.md).

## 1. Architektur: ein Rust-Kern, zwei dünne Apps

```mermaid
flowchart LR
    subgraph App [Android: Kotlin / iOS: SwiftUI]
        UI[Formular · Bildschirm · Touch · Tastatur]
    end
    subgraph Kern [mobile/core — Rust]
        E[Engine] --> N[Net-Thread v3\nReconnect · AV1 · Maus-Bündelung]
        N -->|127.0.0.1:x| F[Forward\nlbw-tunnel]
    end
    UI -- JNI / C-ABI --> E
    F -->|SSH Ed25519| J[Jump-Host sshd]
    J -->|direct-tcpip → genestete SSH-Session| Z[Ziel-sshd]
    Z -->|direct-tcpip 127.0.0.1:7878| S[lbw-server]
```

- **Forward statt eigenem Protokoll-Pfad:** Der Kern öffnet einen lokalen
  Port wie `ssh -L` und lässt den *unveränderten* Netz-Thread des
  Desktop-Clients dorthin verbinden. So gibt es genau eine
  Protokoll-Implementierung für alle drei Plattformen.
- **Genestete SSH-Sitzung:** Der Jump-Host sieht nur verschlüsselte
  SSH-Bytes zum Ziel; der Hostkey des Ziels wird Ende-zu-Ende geprüft.
- **TOFU-Pins** (*Trust On First Use*): Beim ersten Verbinden merkt sich
  die App den Hostkey-Fingerprint (`SHA256:…`, wie `ssh-keygen -lf`)
  jedes Hops; ändert er sich später, bricht die Kette **vor** der
  Anmeldung ab.

| Datei | Inhalt |
|---|---|
| `tunnel/src/02_chain.rs` | SSH-Kette (aus source11 übernommen) |
| `tunnel/src/03_forward.rs` | lokaler Forward, Fehler je Versuch in die Statuszeile |
| `mobile/core/src/01_engine.rs` | Konfig (JSON), Forward + Net + Szene, `poll` |
| `mobile/core/src/04_ffi.rs` + `include/lbw_core.h` | C-ABI (iOS) |
| `mobile/core/src/05_jni.rs` | JNI (Android, Signaturen wie source11) |
| `mobile/android/` | Android-App (aus source11, angepasst) |
| `mobile/ios/` | iOS-App (SwiftUI + UIKit), XcodeGen-Projekt |

## 2. Server-Seite vorbereiten

1. `lbw-server` lauscht nur an localhost (Default `127.0.0.1:7878`).
2. Auf dem Ziel läuft `sshd` mit `AllowTcpForwarding yes`; der
   öffentliche Ed25519-Key des Telefons steht in `~/.ssh/authorized_keys`.
3. Jump-Host: ebenfalls Key-Login; vom Jump aus muss der Ziel-sshd
   erreichbar sein (direkt, oder per `autossh -R 2332:127.0.0.1:22` vom
   Ziel zum Jump — dann ist „Ziel-sshd aus Jump-Sicht" `127.0.0.1:2332`).

Key fürs Telefon erzeugen (am PC, dann in die App kopieren):

```sh
ssh-keygen -t ed25519 -N '' -f lbw_phone   # lbw_phone = privat, lbw_phone.pub = öffentlich
ssh-copy-id -i lbw_phone.pub user@jump; ssh-copy-id -i lbw_phone.pub user@ziel
```

## 3. Android

Installation: APK aus dem GitHub-Release (`lbw10b-android-<v>.apk`)
herunterladen und installieren („Unbekannte Quellen" erlauben).

Formular: pro Hop Host/IP, Port, User und privater Key (OpenSSH-Text
einfügen), dazu „Ziel-sshd aus Jump-Sicht" und LBW-Port; Profile lassen
sich speichern. Keys liegen in den app-privaten SharedPreferences.
Bedienung: Trackpad-/Direkt-/Auswahl-Modus, Sondertastenleiste (Esc, Tab,
Ctrl, Alt, Pfeile), Softwaretastatur, Pinch-Zoom.

Selbst bauen und testen (Linux, SDK 36 + NDK 27 + JDK 21):

```sh
mobile/android/build_android.sh          # .so (arm64-v8a, x86_64) + APK + 29 JVM-Tests
mobile/android/emulator_e2e.sh           # App im Emulator gegen echten Server durch Jump-Kette
```

## 4. iPhone

Gebaut wird auf einem macOS-Runner (GitHub Actions, Job `ios`):
`LbwCore.xcframework` (Gerät + Simulator) und ein **unsigniertes**
`lbw10b-ios-<v>-unsigned.ipa`. Apple lässt nur signierte Apps aufs Gerät:

- *AltStore/SideStore* (kostenlose Apple-ID, 7 Tage gültig) signiert das
  `.ipa` beim Installieren, oder
- eigenes Xcode: `mobile/ios/build_ios.sh` lokal auf dem Mac, dann
  `LbwClient.xcodeproj` öffnen, Team wählen, auf das Gerät spielen.

Formular wie Android (Jump an/aus, Hops, Keys, Ziel-sshd, LBW-Port).
Keys liegen im iOS-Schlüsselbund (`WhenUnlockedThisDeviceOnly`), Pins in
den App-Einstellungen; „Hostkey-Pins vergessen" setzt TOFU zurück.
Bedienung: Tippen = Klick, Ziehen = Maus, langes Drücken = Rechtsklick,
zwei Finger = Bild auf/ab, Tastatur-Knopf = Softwaretastatur.

## 5. Konfiguration (für eigene Apps)

```json
{"hops": ["kiel@jump.example:22", "kiel@ziel"],
 "identities": ["-----BEGIN OPENSSH PRIVATE KEY-----\n…", "…"],
 "pins": ["", "SHA256:…"],
 "target_sshd_host": "127.0.0.1", "target_sshd_port": 2332,
 "lbw_port": 7878}
```

`pins` ist positionsgenau ("" = lernen); ohne SSH genügt
`{"direct": "host:port"}`. C-Funktionen: `lbw_new`, `lbw_poll`,
`lbw_texts`, `lbw_status`, `lbw_pins`, `lbw_key`, `lbw_text`,
`lbw_mouse`, `lbw_button`, `lbw_wheel`, `lbw_free` (`include/lbw_core.h`).

## 6. Sicherheit

- Ohne gültigen Ed25519-Key und passenden Hostkey kommt niemand an den
  Server; das LBW-Protokoll selbst hat **keine** Authentifizierung —
  `lbw-server` deshalb nie an eine öffentliche Adresse binden.
- Private Keys liegen auf dem Telefon: Android app-privat (kein Backup
  dank `data_extraction_rules`), iOS im Schlüsselbund. Für Keys mit
  Passphrase gibt es das Feld `passphrases` in der Konfiguration
  (die Formulare setzen es noch nicht).
