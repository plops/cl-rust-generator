# plan.md — Android-Client für den Low-Bandwidth-Remote-Desktop

Ziel: Der Desktop-Client `source6/client` (Macroquad) bekommt eine
Android-Variante. Protokoll, Netz-Schleife, AV1-Dekodierung und Szene
bleiben **Rust** (wiederverwendet, nicht kopiert); Oberfläche, Gesten,
Tastatur und SSH-Tunnel sind **Kotlin**. Gebaut wird per GitHub Action
(`.github/workflows/android-lbw.yml`), das APK ist ein Artefakt.

Arbeitsordner: `examples/29_lowbandwidth/source6/android_client`.
Serielle Tasks: `source6/android_client/task.md`.
Abhängigkeiten (GitHub `org/projekt`): `source6/android_client/deps.md`.

## 1. Kontext für einen neuen Agenten (zuerst lesen)

| Datei | Warum |
|---|---|
| `plan/20260929_03_android/prompt.txt` | Auftrag, Vorschläge, Regeln (Dateiaufteilung, Commits, Walkthrough). |
| `plan/20260929_01_lowbandwidth/walkthrough.md` | Gesamtarchitektur Server/Client, Protokoll-Ideen (Text als Vektordaten, AV1-Kacheln, Acks). |
| `source6/README.md` | Bedienung, Messwerte, Skripte. |
| `source6/common/src/01_types.rs` | `Rect`, `TextItem`, `Input`, `ServerMsg`, `ClientMsg` — die Protokolltypen. |
| `source6/common/src/03_frame.rs` | Längen-Präfix-Framing, `FrameReader` (Teil-Reads, Timeouts). |
| `source6/common/src/04_keys.rs` | X11-Keysyms, Modifier-Bits, `char_to_keysym`. |
| `source6/client/src/02_av1.rs` | rav1d-Wrapper: Still-Picture-OBU → RGBA. |
| `source6/client/src/03_net.rs` | Netz-Thread: Reconnect/Backoff, Resume, Assembler, Acks, `Event`s. |
| `source6/client/src/04_scene.rs` | Canvas (RGBA) + Text-Map, `apply(Event)`. |
| `source6/client/src/05_input.rs` | Modifier-Bits, `char_input`, `MouseThrottle` (plattformneutral). |
| `source6/client/src/06_keycode.rs` | Macroquad-`KeyCode` → Keysym (nur Desktop). |
| `source6/client/src/07_select.rs` | Auswahlrechteck → Text in Lesereihenfolge, Paste in Stücken. |
| `source6/client/src/08_render.rs` | Unifont-Einpassung (`font_size_for`, `font_scale_aspect`) — Vorbild für Kotlin. |
| `source6/client/src/09_app.rs` | Desktop-Hauptschleife (F1 HUD, F2 Auswahl, F3 Einfügen). |
| `source6/server/tests/loopback.rs` | Wie man einen Server im Test startet (synthetische Quelle). |
| `source6/scripts/smoke_xvfb.sh` | E2E unter Xvfb; Vorlage für den Emulator-Nachweis. |

## 2. Architektur

```text
Kotlin (android-app, ohne AndroidX/Compose)          Rust (rust-core → liblbw_core.so)
┌──────────────────────────────────────────┐          ┌─────────────────────────────────────┐
│ 08_MainActivity  Formular, Lebenszyklus   │          │ 01_engine  Net + Scene, Größe, Flags │
│ 02_SshTunnel     JSch -L 0:127.0.0.1:7878 │  JNI     │ 02_blob    Texte → Binärblob, HUD    │
│ 07_ScreenView    Bitmap+Unifont, IME      │◄───────► │ 03_keymap  Android-KeyCode → Keysym  │
│ 05_TouchInput    Trackpad/Direkt/Auswahl  │ jni-sys  │ 04_jni     extern "system"-Exporte   │
│ 06_VirtualKeybar Esc Tab Ctrl Alt F1..F12 │          │   ↳ lbw-client (ohne macroquad):     │
│ 04_Viewport      Zoom/Pan (rein, getestet)│          │     net, av1(rav1d), scene, select   │
│ 03_TextItems     Blob-Parser, Schriftgröße│          │   ↳ lbw-common: Protokoll, Framing   │
└──────────────────────────────────────────┘          └─────────────────────────────────────┘
```

Entscheidungen (gegenüber den Vorschlägen im Prompt):

1. **Wiederverwendung statt Kopie.** `lbw-client` bekommt ein Feature
   `desktop` (Default, zieht macroquad). `rust-core` hängt mit
   `default-features = false` davon ab und nutzt `net`, `av1`, `scene`,
   `input`, `select` unverändert → keine Protokoll-Drift. Dafür wird
   `05_input.rs` in plattformneutral (`05_input`) und Macroquad-Tasten
   (`06_keycode`) geteilt; `select/render/app` rücken auf 07/08/09.
2. **`jni-sys` statt UniFFI/`jni`.** Nur ~12 Funktionen mit `long`,
   `int`, `byte[]` und einem Direct-`ByteBuffer`. UniFFI würde Frames
   kopieren und Codegen einführen; `jni-sys` ist nur `jni.h` in Rust.
3. **Pull statt Callback.** Kotlin ruft pro Vsync (`postInvalidateOnAnimation`)
   `nativePoll(h, frameBuffer)`; Rust leert die Ereignis-Queue, kopiert
   den Canvas nur wenn geändert in den Direct-Buffer und liefert Flags.
   Kein `AttachCurrentThread`, keine JVM-Referenzen in Rust-Threads.
   Kotlin übernimmt mit `Bitmap.copyPixelsFromBuffer` (ein memcpy, kein GC).
4. **Strings als UTF-8-`byte[]`.** JNI-`NewStringUTF` erwartet
   „modified UTF-8“ (falsch für Zeichen > U+FFFF); Bytes sind eindeutig.
5. **Kein Compose/AndroidX.** Eine eigene `View` mit `Canvas`
   (`isFilterBitmap=false` = Nearest-Neighbor), `GestureDetector`,
   `ScaleGestureDetector`, `InputConnection` genügt. Abhängigkeiten der
   App: Kotlin-Stdlib und JSch. Kleines APK, schneller CI-Build.
6. **Szenegröße vom Server.** `ServerMsg::Hello{w,h}` bestimmt die
   Canvasgröße (Desktop nutzt `--size`); Kotlin fragt `nativeSize`.
7. **Sauberes Beenden.** `Net` bekommt ein Stop-Flag (`Drop`), damit
   `onStop()` die TCP-Verbindung sofort schließt (bisher lief der
   Thread bis zum nächsten Ereignis weiter).

### Datenfluss

```text
TCP ─► net-Thread (Frame → ServerMsg → Assembler → rav1d → Event) ─► mpsc
UI-Thread: nativePoll ─► Scene.apply ─► RGBA → Direct-ByteBuffer ─► Bitmap
                                    └► Text geändert? → nativeTexts (Blob)
Touch/IME ─► nativeMouse/Button/Wheel/AndroidKey/Char/Text ─► mpsc ─► Writer-Thread ─► TCP
```

### Text-Blob (Little Endian)

```text
u32 anzahl, dann je Element:
u32 id | u16 x y w h | u8 fg[3] | u8 bg[3] | u16 len | len Byte UTF-8
```

## 3. Requirements (Prompt) und Ergänzungen

Aus dem Prompt: Hybrid Rust/Kotlin, AV1 per rav1d, Framebuffer ohne
GC-Last, Trackpad- und Direkt-Modus, virtuelle Tastenleiste, Auswahl →
Zwischenablage, Einfügen, JSch-Tunnel, Lebenszyklus, `INTERNET`-Recht,
Pinch-Zoom/Pan mit Nearest-Neighbor, Unifont als Asset mit Fallback,
feste NDK-Version im CI, Profile speichern.

Ergänzt (vorgeschlagen und umgesetzt, sofern nicht anders markiert):

- **Softwaretastatur als `InputConnection`** (`commitText` →
  `Char`, `deleteSurroundingText` → Backspace), Typ „visible password“
  gegen Autokorrektur/Komposition.
- **Hardware-Tastatur** (Bluetooth/USB): `onKeyDown` → Rust-Keymap,
  sonst `getUnicodeChar` → `Char`.
- **Sticky-Modifier**: Ctrl/Alt der Leiste gelten für die nächste Taste.
- **Tastenwiederholung** kommt bei Android vom System (`repeatCount`).
- **HUD** (Status, kB/s, Backlog) wie F1 am Desktop, aus Rust formatiert.
- **Szenegröße dynamisch** (s. o.), Seitenverhältnis bleibt erhalten.
- **Tunnel-Watchdog**: Tunnel neu aufbauen, lokaler Port bleibt gleich,
  die Rust-Reconnect-Schleife verbindet dann von selbst neu.
- **Passwort wird nicht gespeichert** (nur Host/Port/User); privater
  Schlüssel optional als Text (app-privat). EncryptedSharedPreferences
  ist in AndroidX deprecated und würde eine Abhängigkeit bringen.
- **abiFilters** `arm64-v8a`, `x86_64` (Geräte + Emulator);
  32-Bit (`armeabi-v7a`, `x86`) optional, spart CI-Zeit und APK-Größe.
- **Host-Key-Pinning (TOFU)**: SHA256-Fingerprint (Format wie
  `ssh-keygen -lf`) wird beim ersten Verbinden gespeichert und danach im
  Schlüsseltausch geprüft — vor dem Senden von Passwort/Schlüssel.
- Nicht umgesetzt (Erweiterungen): Foreground-Service für Hintergrund,
  MediaCodec-Hardware-Dekodierung, Profile-Liste.

## 4. Werkzeuge und Versionen (neueste stabile, 2026-09)

| Werkzeug | Version |
|---|---|
| Rust | 1.98 (Edition 2024), Targets `aarch64-linux-android`, `x86_64-linux-android` |
| cargo-ndk | 4.1.2 |
| Android NDK | 30.0.16248370 |
| Android Gradle Plugin | 9.4.1 (Kotlin eingebaut, kein `kotlin-android`-Plugin) |
| Gradle | 9.8.0 (Wrapper im Repo) |
| Kotlin | 2.4.20 |
| compileSdk / targetSdk / minSdk | 36 / 36 / 26 |
| JDK | 21 |
| JSch (`com.github.mwiede:jsch`) | 2.28.7 |
| JUnit | 4.13.2 (nur Tests) |
| jni-sys | 0.4.1 |

## 5. Usage-Beispiele der neuen Abhängigkeiten

`jni-sys` — die Funktionstabelle ist eine Union je JNI-Version:

```rust
use jni_sys::{JNIEnv, jobject};
unsafe fn direct_buffer<'a>(env: *mut JNIEnv, buf: jobject) -> Option<&'a mut [u8]> {
    let f = unsafe { (**env).v1_4 };
    let p = unsafe { (f.GetDirectBufferAddress)(env, buf) } as *mut u8;
    let n = unsafe { (f.GetDirectBufferCapacity)(env, buf) };
    (!p.is_null() && n > 0).then(|| unsafe { std::slice::from_raw_parts_mut(p, n as usize) })
}
```

JSch — lokales Port-Forwarding (Port 0 = frei wählen):

```kotlin
val s = JSch().getSession(user, host, 22)
s.setPassword(pw); s.setConfig("StrictHostKeyChecking", "no")
s.connect(10_000)
val local = s.setPortForwardingL("127.0.0.1", 0, "127.0.0.1", 7878)
```

cargo-ndk — direkt in `jniLibs` bauen:

```sh
cargo ndk -t arm64-v8a -t x86_64 -P 26 -o android-app/app/src/main/jniLibs build --release -p lbw-core
```

## 6. Tests

| Ebene | Was | Befehl |
|---|---|---|
| Rust-Host | Engine gegen Fake-Server (Hello/Text/Kachel/Input), Blob, Keymap, Net-Stop | `cargo test --workspace` |
| JVM-Unit | Blob-Parser, Schriftgröße, Viewport-Mathe | `./gradlew testDebugUnitTest` |
| JNI-Host | Kotlin `CoreBridge` lädt die Host-`.so` (Signaturen stimmen) | dito, nach `cargo build -p lbw-core` |
| Android-Build | APK für arm64 + x86_64 | `./gradlew assembleDebug` |
| HIL (Emulator) | echter `lbw-server` unter Xvfb, App im KVM-Emulator, direkt und über SSH-Tunnel; Text im Screenshot, Tap/Tippen kommt am Server an | `android_client/scripts/emulator_e2e.sh` |

## 7. Commits (Conventional Commits)

```text
<typ>(<scope>): <kurze beschreibung im präsens>

<ausführliche begründung: warum nötig, was gelöst>

- Detail 1
- Detail 2
```

Typen: `feat`, `fix`, `refactor`, `test`, `ci`, `docs`, `build`.
Scopes: `client`, `rust-core`, `android`, `ci`, `plan`.
Jeder Task in `task.md` endet mit fmt/clippy/Tests grün und einem Commit
(Trailer `Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>`).

## 8. Regeln für `walkthrough.md`

- **Zwingend Deutsch**, didaktisch, flüssig; Fachbegriffe kurz erklären.
- Reichlich Code-Beispiele und **Mermaid-Diagramme** (Architektur,
  Datenfluss, Sequenzen).
- Struktur: 1. Was exakt implementiert wurde. 2. Welche Architektur-
  Entscheidungen aufgrund von Tests geändert wurden. 3. Learnings und
  Erweiterungen. 4. Neue Programme/Pakete für das Dockerfile.
- Ablage: `plan/20260929_03_android/walkthrough.md`.
