# deps.md — Abhängigkeiten des Android-Clients (GitHub `org/projekt` für DeepWiki)

DeepWiki-Beispiel: `ask_wiki_question(repoName="mwiede/jsch", question="...")`.

## Rust-Core (`rust-core`, Crate `lbw-core`, Bibliothek `liblbw_core.so`)

| Crate | Version | GitHub | Wofür |
|---|---|---|---|
| `jni-sys` | 0.4.1 | `jni-rs/jni-sys` | Rohe `jni.h`-Definitionen (keine Laufzeit-Abhängigkeiten) |
| `lbw-client` | Pfad, ohne `desktop` | `plops/cl-rust-generator` | Netz, Reconnect, Assembler, Szene, Auswahl |
| `rav1d` | 1.1.0 (via `lbw-client`) | `memorysafety/rav1d` | AV1-Dekodierung in Rust (Port von `videolan/dav1d`) |
| `lbw-common` | Pfad | `plops/cl-rust-generator` | Protokoll, Framing, Keysyms |

## Android/Kotlin (`android-app`)

| Artefakt | Version | GitHub | Wofür |
|---|---|---|---|
| `com.github.mwiede:jsch` | 2.28.7 | `mwiede/jsch` | SSH2-Tunnel (Local Port Forwarding), reines Java |
| Kotlin-Stdlib/Compiler | 2.4.20 | `JetBrains/kotlin` | Sprache |
| Android Gradle Plugin | 9.4.1 | (Google, `android/...` nicht auf GitHub) | Build, Kotlin eingebaut |
| Gradle | 9.8.0 | `gradle/gradle` | Build-Werkzeug (Wrapper) |
| `junit:junit` | 4.13.2 (Test) | `junit-team/junit4` | JVM-Unit-Tests |

Bewusst **nicht** verwendet: `androidx/androidx` (Compose, AppCompat),
`mozilla/uniffi-rs`, `jni-rs/jni-rs` (höhere Ebene), `apache/mina-sshd`.

## Build & Tooling

| Werkzeug | Version | GitHub | Wofür |
|---|---|---|---|
| `cargo-ndk` | 4.1.2 | `bbqsrc/cargo-ndk` | `.so` je ABI direkt nach `jniLibs` |
| Android NDK | 30.0.16248370 | `android/ndk` | Clang-Toolchain für Android |
| `actions/checkout` | v7 | `actions/checkout` | CI |
| `actions/setup-java` | v6 | `actions/setup-java` | CI (JDK 21, Gradle-Cache) |
| `android-actions/setup-android` | v4 | `android-actions/setup-android` | CI (sdkmanager) |
| `actions/upload-artifact` | v7 | `actions/upload-artifact` | CI (APK) |
| `Swatinem/rust-cache` | v2 | `Swatinem/rust-cache` | CI (Cargo-Cache inkl. `cargo-ndk`) |
| `actionlint` | 1.7.12 (lokal) | `rhysd/actionlint` | Workflow-Prüfung vor dem Push |
| OpenSSH-Server | Ubuntu-Paket | `openssh/openssh-portable` | Test-`sshd` für Tunnel-Tests (`scripts/test_sshd.sh`) |

## Schrift

GNU Unifont (`fonts-unifont`, Lizenz GPLv2+ mit Font-Exception) —
Quelle `https://unifoundry.com/unifont/`, im CI aus dem Ubuntu-Paket nach
`android-app/app/src/main/assets/fonts/unifont.otf` kopiert
(`scripts/fetch_font.sh`), nicht im Git.
