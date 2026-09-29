# task.md — serielle Tasks Android-Client (jeder Schritt getestet)

Konventionen: Rust-Befehle in `examples/29_lowbandwidth/source6`,
Gradle in `source6/android_client/android-app`. Nach **jedem** Task:
`cargo fmt --all`, `cargo clippy --workspace --all-targets -- -D warnings`,
`cargo test --workspace` (bzw. `./gradlew testDebugUnitTest`), dann ein
Commit nach `plan/20260929_03_android/plan.md` §7.

Übertragung des Prompt-Schemas: *Modus* = Teilsystem (Core, Bridge,
UI, Tunnel), *Host-Tests* = `cargo test` / JVM-Unit-Tests,
*HIL-Nachweis* = KVM-Emulator gegen echten `lbw-server` unter Xvfb
(kein Telefon am USB), *TUI* = Touch-Oberfläche (zuletzt).

## A0 Umgebung
- [ ] A0.1 JDK 21, Android SDK (`/opt/android-sdk`: platform-tools, build-tools 37.0.0, platforms android-36, NDK 30.0.16248370, emulator, system-image android-36 default x86_64), Gradle 9.8.0, `cargo-ndk`, Rust-Targets `aarch64-linux-android x86_64-linux-android`, `nasm` (rav1e), `fonts-unifont`, `xvfb`, `openssh-server`.
  Validierung: `cargo ndk --version`, `sdkmanager --list_installed`, Baseline `cargo test --workspace` grün.

## A1 Desktop-Client aufteilen (Refactor, ohne Verhaltensänderung)
- [ ] A1.1 `05_input.rs` → `05_input.rs` (Modifier, `char_input`, `MouseThrottle`) + `06_keycode.rs` (Macroquad-KeyCode); `06_select→07`, `07_render→08`, `08_app→09`.
- [ ] A1.2 Feature `desktop` (Default) = macroquad; `keycode/render/app` und `[[bin]]` nur damit.
  Validierung: `cargo test --workspace` (vorher/nachher gleiche Anzahl), `cargo build -p lbw-client --no-default-features`.
- [ ] A1.3 `Net` Stop-Flag bei `Drop`. Test: Fake-Listener sieht EOF < 1 s nach `drop(net)`.

## A2 Rust-Core (Modus „Core“)
- [ ] A2.1 Crate `android_client/rust-core` (`lbw-core`, `cdylib`+`rlib`), Workspace-Mitglied.
- [ ] A2.2 `01_engine.rs`: `Engine{net, scene}`, `poll(buf) → Flags`, Größe aus `Hello`, Eingaben.
- [ ] A2.3 `02_blob.rs`: Text-Blob, HUD-Zeile. `03_keymap.rs`: Android-KeyCode/Meta → `Input`.
- [ ] A2.4 Host-Tests: Blob-Format, Keymap, `tests/engine.rs` mit Fake-Server (Hello 320², Text, AV1-Kachel aus `rav1e` des Servers per Dev-Dependency, Input kommt an, Reconnect).

## A3 JNI-Bridge (Modus „Bridge“)
- [ ] A3.1 `04_jni.rs`: `Java_de_lbw_client_CoreBridge_native*` mit `jni-sys`; Strings als UTF-8-`byte[]`, Frame via Direct-`ByteBuffer`.
- [ ] A3.2 Cross-Build: `cargo ndk -t arm64-v8a -t x86_64 -P 26 build --release -p lbw-core`; `.so`-Größe notieren.

## A4 Android-App Grundgerüst (Modus „App“)
- [x] A4.1 Gradle-Projekt `android-app` (AGP 9.4.1, Wrapper 9.8.0, Kotlin eingebaut), Manifest mit `INTERNET`.
- [x] A4.2 `01_CoreBridge.kt`, `03_TextItems.kt`, `04_Viewport.kt` + JVM-Tests; `CoreBridgeHostTest` lädt die Host-`.so`.
  Validierung: `./gradlew testDebugUnitTest`.
- [x] A4.3 `scripts/build_android.sh` (Font kopieren, cargo-ndk, `assembleDebug`). Validierung: APK enthält `lib/*/liblbw_core.so` und `assets/fonts/unifont.otf`.

## A5 SSH-Tunnel (Modus „Tunnel“)
- [x] A5.1 `02_SshTunnel.kt` (JSch, Passwort oder Schlüssel, Keepalive, Watchdog mit festem lokalem Port).
- [x] A5.2 Host-Test: JVM-Test gegen lokalen `sshd` (übersprungen, wenn keiner läuft).
  `eval "$(LBW_SSHD_PASSWORD=pw scripts/test_sshd.sh start)"; LBW_SSHD_PASSWORD=pw ./gradlew testDebugUnitTest`
- [x] A5.3 (ergänzt) Host-Key-Pinning „Trust on first use“: SHA256-Fingerprint wird im KEX geprüft, vor der Authentisierung.

## A6 CI
- [x] A6.1 `.github/workflows/android-lbw.yml` (Pfadfilter, feste NDK-Version, Rust-Host-Tests, cargo-ndk, Gradle-Tests, APK-Artefakt).
  Validierung: YAML parsen; alle Schritte lokal mit denselben Befehlen (`scripts/ci_local.sh`).

## A7 TUI (Touch-Oberfläche)
- [x] A7.1 `08_ScreenView.kt`: Bitmap (Nearest), Unifont-Text in Boxen, Cursor, Auswahlrahmen, HUD; `07_KeyInput.kt`: IME-`InputConnection` (ohne Vorschläge), Hardware-Tasten.
- [x] A7.2 `05_TouchInput.kt`: Trackpad (Tap=Links, 2-Finger-Tap=Rechts, 2-Finger-Scroll=Rad, Lang-Drücken=Ziehen), Direkt (Lang-Drücken=Rechts), Auswahl; Pinch-Zoom/Pan. `TouchInputTest` (11 Fälle).
- [x] A7.3 `06_VirtualKeybar.kt`: ⌨ Modus Esc Tab Ctrl Alt ⇧ ❖ ← ↑ ↓ → Rad Fn(F1..F12) Pos1 Ende Bild↑/↓ Einf Entf, Einfügen, Einpassen, HUD; Sticky-Modifier (`StickyModsTest`).
- [x] A7.4 `09_ConnectForm.kt` (Formular direkt/SSH, Prefs ohne Passwort, Intent-Extras) + `10_MainActivity.kt` (Sitzung, Verbindung, `onStart/onStop`, Zurück = trennen (OnBackInvokedDispatcher ab API 33)). In A9 geteilt, weil über 300 Zeilen.
- [x] A7.5 `lintDebug` im Build (0 Fehler); Backup schließt Prefs (Host-Key-Pins) aus.

## A8 HIL-Nachweis (Emulator)
- [x] A8.1 `scripts/emulator_e2e.sh`: Xvfb :99 + xterm + `lbw-server`, Emulator (KVM, headless, AVD `lbw36` wird angelegt), APK installieren, per Intent-Extras verbinden (direkt `10.0.2.2`). 19 Nachweise, ~55 s ab Kaltstart:
  (1) „link up“ + OCR-Text in der App, (2) Tap (Direkt) auf Szene (200,150) → Zeiger auf :99 exakt dort,
  (3) `adb input text` und Bildschirmtastatur (commitText) → xterm → OCR → App, (4) Trackpad-Tap/-Wischen,
  (6) HOME → onStop trennt, Neustart → onStart verbindet neu, (7) Tastatur startet verborgen, ein Zurück → Formular.
- [x] A8.2 Gleiches über SSH-Tunnel (5): JSch auf Android handelt ECDSA aus (Ed25519 fehlt dort), Fingerabdruck = `ssh-keygen -lf`, Pin in den Prefs, Passwort nicht.
- [x] A8.3 Befunde behoben: Modifier-Tasten weiß (Theme-Tint geht bei `backgroundTintList` verloren → eigene Drawables); IME öffnete sich beim Start und verschluckte das erste Zurück (`stateHidden`); Emulator-Hardware-Tastatur unterdrückt IME (`show_ime_with_hard_keyboard=1` im Skript).

## A9 Abschluss
- [x] A9.1 `cargo upgrade` (alles aktuell; AGP 9.4.1, Kotlin 2.4.20, JSch 2.28.7, Gradle 9.8.0 = neueste stabile), fmt, clippy, alle Tests; README-Abschnitt Android; `09_MainActivity` (319 Zeilen) in `09_ConnectForm` + `10_MainActivity` geteilt, danach JVM-Tests, Lint und `emulator_e2e.sh` grün.
- [x] A9.2 `plan/20260929_03_android/walkthrough.md` (Regeln plan.md §8), Commit.
