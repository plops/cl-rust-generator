# Plan: `source11_compact` — minimale, verschlüsselte Neuauflage (`plan/20261018_01_encrypt`)

Stand: 2026-10-18. Basis: `source10_instrumentation` (Protokoll v3,
1280×720, GPU-Hybrid-OCR aus PP-OCRv6, Pacer-Leaky-Bucket `--budget 6000`,
`TCP_NODELAY`, Text-vor-Tile-Priorität, 1 Tile/Frame, ~6700 Zeilen inkl.
`lbw-log`), Android-Client aus `source6/android_client`
(Rust-Core + Kotlin-UI, JSch-SSH-Tunnel 2.28.7) und die geforderte
Neuentwicklung in `source11_compact/`.

## 0. Wichtigster Befund vorab: `/workspace/src/mosh-tcp` ist KEIN Crypto-Vorbild

Verifiziert per `grep` über `Cargo.toml` + `src/*.rs` (0 Treffer für
`ed25519|x25519|chacha|noise|snow|russh|rustls|dalek`):

- `mosh-tcp` erklärt Verschlüsselung/Auth explizit für **out of scope**
  (`README.md:19-20,41,64-84`) und fährt über SSH-Port-Forwarding.
- `server.rs:40-53`: Session-Key = Timestamp-Nanos + PID als Hex-String
  (kein CSPRNG, kein Ed25519/X25519, kein AEAD); leere Keys werden
  akzeptiert (`server.rs:72-89,143-155`).
- Transport nur TCP (`tokio TcpListener/TcpStream + Framed/PacketCodec`),
  `ServerFrame.seq` wird vom Client ignoriert (`client.rs:297-298`) —
  kein Replay-Schutz, kein Roaming, ein Client, dann Exit.
- Lizenz **GPL-3.0-or-later** — Copyleft; höchstens Ideen (Tag-Codec,
  Framing, Token-Bucket-Drossel, `puff.c`-Inflate) wiederverwenden,
  kein Crypto zu holen.

Konsequenz: Das „moderne Encryption (z. B. Ed25519)" kommt **nicht** aus
mosh-tcp, sondern aus SSH — serverseitig Pflicht, clientseitig per
eingebautem `russh`-Tunnel mit Ed25519-Keys (s. §2.1). mosh-tcp liefert
nur Negativ-Lehren (schwacher Key, kein Replay-Schutz) und nützliche
Drossel-Muster (500-ms-Batching, Token-Bucket, 16-KB-Pty-Cap mit Drop +
Refresh).

## 1. Ziele und Nicht-Ziele

Ziele (aus `prompt.txt`):

1. Minimale Codebasis für genau diese Features (Protokoll darf brechen).
2. SSH ist Pflicht auf dem Server (Ed25519-Keys); Linux- und
   Android-Client bauen den Tunnel **selbst** auf (kein externer
   `ssh`-Prozess, keine alte Java-Crypto) — inkl. Jump-Host-Kette.
3. Garantierte Input-Latenz bei limitierter Bandbreite (Learnings aus
   source10 anwenden).
4. Linux-Client mit Unifont/Noto (CJK/J/K) + Fallback-Kette.
5. Wireshark-Dissector mit Decryption (ggf. Rust-Modul im Dissector).
6. Android-Client ohne die alte Java-Crypto (moderne Keys funktionieren).
7. GitHub Actions für Linux-Server/Client, Android, Dissector;
   Server-Release enthält Gewichte; SSH-Client darf größer werden
   (Größe wird nur gemessen), schlanker Direkt-Client ohne Tunnel bleibt
   separates Artefakt.
8. Gewichte nur per Link auf existierende neuronale Gewichte.
9. Docker-Tests mit limitierter Bandbreite + Xvfb/Host-X.
10. Umsetzung in Schritten mit Subagenten, kleinem Kontext,
    Wonder-Planung + Spark-Umsetzung.

Nicht-Ziele: Protokoll-Kompatibilität (v3 bricht bewusst),
`.lbwlog`-Recording-Format weiterführen, Browser-/iPhone-Clients,
Wayland, Multi-Monitor, Clipboard, Motion Compensation, adaptiver
Quantizer (Follow-ups, keine Baustellen in source11).

## 2. Architektur-Entscheidungen

### 2.1 Transport: SSH-Pflicht auf dem Server, Tunnel bauen die Clients selbst

- Der Server bindet ausschließlich an **127.0.0.1:7878** und verlangt SSH
  auf dem Server-Host (OpenSSH, `AllowTcpForwarding yes`, Ed25519-Hostkey).
  Verschlüsselung = SSH; **kein** eigenes Noise/TLS-Protokoll mehr (der
  Noise-Entwurf aus der Vorversion ist gestrichen — eine Verschlüsselung
  reicht, zwei wären nur Code).
- Linux- **und** Android-Client bauen den Tunnel **selbst** auf: `russh`
  (Tokio-SSH2-Client, moderne Primitive inkl. Ed25519 lt.
  `plan/misc/russh.md`) als `direct-tcpip`-Forward
  `127.0.0.1:<lokal> → 127.0.0.1:7878`, mit Keepalive, Watchdog und
  Reconnect — kein externer `ssh -L`-Prozess, kein JSch (der
  source6-Fehler `task.md:62`, ECDSA-statt-Ed25519 auf Android, ist damit
  weg, weil nie eine Java-Crypto angefasst wird).
- **Jump-Host-Pflicht** (Server hinter Carrier-Grade-NAT). Verbindliche
  Topologie = Ihr realer Weg (SSH-Config, Stand 2026-10-18):
  `tinyeu` = `51.107.16.14:22`, `User kiel`,
  `IdentityFile ~/.ssh/tinyeu_key.pem`, `ServerAliveInterval 15`,
  `LocalForward 2332 127.0.0.1:2332`; `hpz6o` = `HostName 127.0.0.1`,
  `Port 2332`, `User kiel`, `IdentityFile ~/.ssh/id_lenovo_to_hpz6o`.
  Lesart: Auf `tinyeu` hört etwas auf `127.0.0.1:2332`, das zum sshd von
  `hpz6o` führt (Reverse-Tunnel des CGNAT-Servers, von `hpz6o` selbst
  gehalten, z. B. `ssh -R 2332:127.0.0.1:22 tinyeu` — serverseitige
  Deployment-Pflicht, im Server-Task zu dokumentieren/automatisieren).
  In russh exakt nachgebaut: Session zu `tinyeu` → `direct-tcpip` auf
  `127.0.0.1:2332` (= sshd von `hpz6o`) → **genestete zweite SSH-Session
  zu `hpz6o`** (Ziel-Hostkey Ende-zu-Ende geprüft; `tinyeu` sieht nur
  SSH-Chiffrat, keinen LBW-Klartext) → darin `direct-tcpip` auf
  `127.0.0.1:7878` (LBW). CLI: `--jump kiel@tinyeu:22` (Host/IP und
  Port je Hop frei konfigurierbar, verkettbar),
  `--identity` pro Hop (OpenSSH-/PEM-Keys RSA/ECDSA/**Ed25519**,
  Passwort nur Fallback); Hostkey-TOFU-Pinning (`SHA256`-Fingerprint
  wie `ssh-keygen -lf`, Muster `02_SshTunnel.kt:141-150`) gilt pro Hop.
  Kein externer LocalForward-Prozess mehr: Die `2332`-Strecke ist ein
  Channel in der `tinyeu`-Session, kein lokaler Listener nötig.
- Fallback ohne Jump: Direktverbindung; `lbw-server` lehnt alles ab, was
  nicht von localhost kommt (Warnung bei `--listen` ≠ localhost wie bisher).

### 2.2 Protokoll v4 (minimal, priorisiert)

- 4 Frame-Arten im SSH-Tunnel (Klartext, SSH verschlüsselt):
  `NewText{id,rect,fg,bg,text}` (nur wirklich neuer Inhalt),
  `MoveText{id,rect}` (bekannter Inhalt an neuer Position — **kein**
  String-Retransmit; das ist die Scroll-Optimierung: scrollender
  Browser verschiebt nur IDs, statt identischen Text neu zu senden),
  `RemoveText{id}`, `Tile` (eine coalescierte Dirty-Box,
  AV1-Still-Picture), `Input` (Mouse/Button/Text/Key wie bisher) plus
  `Hello`/`ClearText` (nur 1. Frame je Verbindung).
- ID-Vergabe **inhaltsverankert** (`09_textids.rs`-Logik behalten und
  verschärfen): Matching primär über Inhalts-Hash (Text + Farben + Größe),
  Position nur als Zweitkriterium mit Toleranz. Beim Scrollen behalten
  Zeilen ihre IDs → nur `MoveText`-Rechtecke (~10 B) gehen über die
  Leitung, keine Text-Bytes. Erster Frame je Verbindung `ClearText` +
  volle `NewText`-Liste (Reconnect beginnt bei Vollbild, wie bisher).
- Autorisierung trotz localhost: Nach dem SSH-Tunnel beweist der Client
  per Challenge/Response, dass er das LBW-Token kennt (schützt den
  Klartext-Port `7878` gegen fremde lokale Prozesse): Server sendet
  `Challenge{nonce: [u8;16]}` (Zufall pro Verbindung), Client antwortet
  `Auth{mac: [u8;32]}` = HMAC-SHA256(Token, `nonce || "lbw11"`),
  konstantzeitig verglichen. Das Token liegt als Datei (0600) auf dem
  Server und wird einmalig im Client-Profil hinterlegt (nie im Log,
  nie in der Connect-Form im Klartext angezeigt).
- Priorität ist Sender-Disziplin (kein Wire-Bit, wie im Bunching-Plan):
  `NewText`/`MoveText`/`RemoveText` + `Hello` gehen sofort, `Tile` nur
  durch den Pacer.
- Framing: `[u32LE len][bincode-Body]` (Klartext **innerhalb** des
  SSH-Tunnels, wie in source10); `MAX_MSG` 8 MB bleibt; bincode + serde
  bleiben (kein Hand-Codec).

### 2.2a Anti-Flacker-Verfahren (OCR-Stabilisierung, neu `09_stabilize.rs`)

Problem: PP-OCR liefert pro Frame leicht andere Ergebnisse — Box ±wenige
Pixel, Farben ±wenige Stufen, gelegentlich ein falsch gelesenes Zeichen,
kurzes Aussetzen einer Zeile. Ohne Dämpfung erzeugt das `NewText`-/
`RemoveText`-Spam und frisst genau die Bandbreite, die §2.2 spart.
Verfahren (Server, rein funktional über Frame-Folgen, 10 fps):

1. **Normalisieren vor jedem Vergleich:** Text trimmen + Whitespace
   kollabieren; `fg`/`bg` pro Kanal auf 16er-Stufen quantisieren
   (`v & 0xF0`, nach dem bestehenden Median-`sample_colors`); Rechteck
   auf ein 2-px-Raster snappen. Kleinere Abweichungen existieren danach
   gar nicht mehr.
2. **Ähnlichkeits-Matching statt Gleichheit:** Zeilen werden per
   Kostenfunktion zugeordnet (greedy, `used`-Flags wie bisher — damit
   funktionieren auch doppelte identische Zeilen als Multimenge):
   Kosten = Text-Distanz (normalisierte Levenshtein-Distanz, ~30 Zeilen,
   Texte sind kurz) + Mittelpunkt-Distanz + Größen-Differenz. Match bei
   Text-Ähnlichkeit ≥ 0,8 **oder** exakt gleichem Text; Positionsfenster
   großzügig (Scroll!), weil der Text die Identität trägt.
3. **Hysterese über die Konfidenz:** Bestehende Zeile bleibt schon ab
   niedrigerer Konfidenz gültig, als eine neue entstehen darf
   (zwei Schwellen um das bestehende `MIN_TEXT_CONF`).
4. **Zeitliche Entprellung (Debounce):** Neuer/geänderter Text wird erst
   gesendet, wenn er in **2 aufeinanderfolgenden Frames** gleich
   erkannt wurde; eine verschwundene Zeile wird erst nach **5 Frames**
   (500 ms) per `RemoveText` gelöscht. Ein einzelnes Flacker-Frame
   erzeugt so exakt null Nachrichten.
5. **Move-Totband:** `MoveText` nur, wenn der Mittelpunkt ≥ 6 px
   gewandert oder die Größe ≥ 4 px anders ist; darunter gilt die Zeile
   als stillstehend (der bestehende `TextCache` macht statische Zeilen
   ohnehin ~0 ms billig).
6. **Masken-Kohärenz:** Die Tile-Maske nutzt die **gesendeten**
   (stabilisierten) Rechtecke, nicht die Roh-OCR-Boxen — sonst blitzt
   alter Text in der Kachel auf, wenn sich eine Box nur stabilisiert
   verschiebt. Alte + neue Position einer verschobenen Zeile kommen
   beide in die Dirty-Box.

Prüfbar per synthetischer Frame-Folgen (Unit-Tests, kein X11 nötig):
Jitter (±3 px, ±10 Farben, abwechselnd 1 falsches Zeichen) → null
Nachrichten nach dem Erst-Frame; echtes Tippen → Update nach 2 Frames;
Scroll (alle Zeilen −40 px) → nur `MoveText`; Zeile fehlt 2 Frames →
kein `RemoveText`, fehlt 6 Frames → genau eins.

### 2.3 Latenz-Garantie (alle source10-Learnings übernommen)

Aus `plan/20261017_01_bunching` + Walkthrough §2.5/§6 (Messwerte: Queue
~2 MB ≈ 329 s Kanalzeit, Input→Kachel p50 614 ms, Tile-Max 13 628 B):

- Leaky-Bucket-Pacer (`11_pace.rs`-Logik, `--budget`, Default 6000 B/s,
  Guthaben-Cap 1 s) nur für `Tile`; `TCP_NODELAY` überall;
  Client-Lese-Timeout 10 ms (0–10 ms statt 0–50 ms Eingabe-Wartezeit).
- Genau 1 coalescierte `dirty_bbox` pro Frame, `FRAME_GAP` 100 ms mit
  `sleep(GAP − elapsed)`; überholte Tiles werden beim Pacing **verworfen**
  (neueste Box gewinnt) statt aufgestaut — kein Burst-Pause-Verhalten.
- Eingabe-Pfad: eigener Reader-Thread + bounded `mpsc` (statt unbegrenzt),
  Stale-Flush bei Reconnect (aus v3 übernommen), Maus-Spam zählt nicht als
  Latenz-Referenz (nur diskrete Key/Text/Button, FIFO je Echo-Art).
- Budget: Input→Text p50 ≤ 250 ms, Input→Tile p50 ≤ 1 Tile-Kanalzeit +
  Encode, `queue_peak_b` < 30 kB im Animations-Soak, ~0 s über Budget.
- `lbw-log`-Crate (ca. 2900 Zeilen) wird **gestrichen**: keine `.lbwlog`s,
  dafür Live-Metriken (B/s, letzte Latenzen) + pcap-basierte Prüfung
  (Dissector + `tshark`-Feld-Export). Das ist die größte Einzel-Ersparnis.

### 2.4 Modul-Layout (Ziel ~3500–4000 Zeilen)

```text
source11_compact/
  Cargo.toml                 # Workspace: common, server, client, xtask?
  common/src/ 01_types.rs 02_framing.rs 03_yuv.rs
  tunnel/src/ 01_config.rs 02_tunnel.rs 03_jump.rs   # russh-Client, beide Clients
  server/src/ 01_config.rs 02_capture.rs 03_ocr.rs 04_tiles.rs 05_av1.rs
              06_input.rs 07_session.rs 08_pace.rs
  client/src/ 01_config.rs 02_av1.rs 03_net.rs 04_scene.rs 05_app.rs 06_font.rs
  android/ rust-core/ + kotlin-app/  (eigener Gradle-Root, s. §2.7)
  wireshark/ packet-lbw11.c decrypt-helper/ check.sh sample*.pcap
  docker/ Dockerfile netem.sh smoke_latency.sh
  scripts/ fetch_models.sh (Link, keine Blobs)
```

Gestrichen: `lbw-log`, `lbw-replay`/`lbw-logstat`, Noise-`04_crypto`,
OCR-Cache-Sonderwege (nur Beibehaltung, wenn Messung es erzwingt),
`replay/xz/zst`-Fixtures, `cuelo/`, alte Skripte. `03_ocr.rs` bleibt eine
Datei (kein Detect/Recognize-Split), GPU-Hybrid (det CUDA, rec CPU) wie
source10.

### 2.5 Fonts: Glyphen-Fallback statt Font-Kette

Zur Verständnisfrage: Die bisherige Kette lädt **einen** Font
(z. B. Noto) und zeichnet damit alles. Fehlt ein Zeichen in genau diesem
Font (z. B. ein seltener Han- oder Hangul-Block in Noto Sans), malt
macroquad eine leere Box (Tofu) — es gibt kein automatisches Ausweichen
auf den nächsten Font pro Zeichen. Deshalb: **Font-Stapel mit
Glyphen-Fallback** in `06_font.rs`:

- Bis zu drei Fonts gleichzeitig laden: Noto Sans CJK (Qualität) →
  Noto Sans → Unifont (fast vollständige BMP-Abdeckung, grob gerastert)
  → macroquad-Default als letzte Rettung.
- Abdeckung pro Zeichen per `ttf-parser` (winziges Cmap-Lesen aus
  denselben Font-Dateien) bestimmen, `char → Font`-Cache (HashMap).
- Text beim Zeichnen in Läufe gleicher Font zerlegen, jeden Lauf mit
  seinem Font messen/zeichnen, x läuft kumulativ weiter — Chinesisch,
  Japanisch und Koreanisch im selben Text-Item funktionieren gemischt.
- Fehlt ein Zeichen überall: `?` aus dem Default-Font (sichtbar statt
  stiller Box). `--font PATH` ersetzt nur die erste Stufe.

### 2.5a Client-Profile und Größe
`--font PATH` → Noto Sans CJK → Noto Sans → Unifont (`fonts-unifont`) →
macroquad-Default (Fallback, kein Absturz). **Keine** Font-Blobs im Git.
Größe: Der SSH-fähige Client (Linux wie Android) **darf wachsen** —
Tokio/russh schlagen mit ≈2–3 MB zu Buche
(`plan/20261002_01_russh/migration-proposal.md:92`); SSH-Fähigkeit ist
wichtiger als das alte 2–3-MB-Ziel. CI misst und meldet die Größe nur
noch (kein hartes Gate). Wer einen Minimal-Download braucht, bekommt
weiter einen schlanken Direkt-Client ohne Tunnel-Feature
(`--connect`, System-Fonts, ohne mitgelieferte Fonts) — als separates,
klar benanntes Artefakt, nicht als heimliches Gate.

### 2.6 Wireshark-Dissector: Klartext auf localhost, wie bisher

So wird er gebaut (unverändert das Verfahren aus
`source10_instrumentation/wireshark/README.md`):

```sh
sudo apt install tshark libwireshark-dev cmake
cd source11_compact/wireshark
cmake -S . -B build && cmake --build build   # → build/lbw.so
V="$(pkg-config --modversion wireshark | cut -d. -f1,2)"
mkdir -p ~/.local/lib/wireshark/plugins/"$V"/epan
cp build/lbw.so ~/.local/lib/wireshark/plugins/"$V"/epan/
./check.sh   # baut, zerlegt sample.pcap, behauptet alle Varianten
```

Warum keine Entschlüsselung nötig ist: Die LBW-Daten laufen an beiden
Tunnel-Enden **unverschlüsselt über localhost** (SSH verschlüsselt nur
die Strecke dazwischen). Mitschnitt auf dem Server-Host, Interface
`lo`, Port 7878 → der C-Dissector (Basis `packet-lbw.c`, Reassembly,
alle v4-Varianten inkl. `MoveText`, `Challenge`, `Auth`) zeigt alles.
Auf dem Client läuft der russh-Strom prozessintern (kein lokaler
TCP-Port); für Live-Analyse dort bekommt der Client einen optionalen
Debug-Schalter `--tap 127.0.0.1:<port>`, der den Klartext-Strom
zusätzlich auf einen lokalen Port spiegelt. SSH selbst wird **nicht**
entschlüsselt (OpenSSH/russh geben keine Session-Keys heraus — der
frühere Keylog-Satz im Plan war falsch und ist gestrichen). Die
Autorisierung auf dem Klartext-Port regelt das Token-Verfahren aus
§2.2 (`Challenge`/`Auth` mit Nonce); der Dissector zeigt davon nur
Längen, nie das Token. Ein Rust-`cdylib`-Helper im Dissector ist
**nicht** vorgesehen — C pur wie bisher; nur wenn `check.sh` einen
konkreten Bedarf beweist, kommt er als eigener Task zurück.

### 2.7 Android ohne alte Java-Crypto

Der Fehler aus source6 ist belegt (`task.md:62`): `mwiede/jsch` 2.28.7
handelt auf Android ECDSA statt Ed25519 aus; nur Fingerprint-Test mit
ed25519-Blob existiert, keine Ed25519-Auth-E2E. Fix in source11:
**gesamter Tunnel inkl. SSH wandert in den Rust-Core** (`russh` + `ssh-key`,
gemeinsame `tunnel/`-Crate mit dem Linux-Client), Kotlin macht nur
UI/Touch/Lifecycle — kein JSch, keine JCE-Abhängigkeit. JNI-Muster aus
source6 bleiben (`01_CoreBridge.kt:36-50` ↔ `04_jni.rs`-Exporte, `jni-sys`
roh, Direct-ByteBuffer, `isMinifyEnabled=false`). Key-Handling:
Ed25519-Client-Keys und Hostkey-TOFU-Pin in `SharedPreferences` (Format wie
bisher `user@host:port`, pro Hop bei Jump); moderne OpenSSH-Keys
funktionieren, weil sie nie eine Java-Lib sehen. Jump-Host auch in der
Android-Connect-Form (Feld + Key-Auswahl pro Hop minimal).
**Alle IPs/Hostnamen und Ports sind in der App konfigurierbar** — nichts
ist hartcodiert: Connect-Form mit Profilen (SharedPreferences), Feldern
pro Hop für Host/IP, Port (Default 22), User, Key-Import/Passphrase,
dazu Ziel-LBW-Port (Default 7878) und Jump-Zwischenport (Ihr `2332` ist
nur Ihr gespeicherter Profilwert, keine Konstante im Code). Dieselben
Felder existieren im Linux-Client als CLI-Flags (`--jump
user@host:port`, `--port`, `--identity`).

### 2.8 Gewichte per Link (verifiziert, nicht erfunden)

`source6/scripts/fetch_models.sh:24,40-42` (SHA256-geprüft, auch Fallback
`../../26_onnx/source5`):

- `https://huggingface.co/PaddlePaddle/PP-OCRv6_small_det_onnx/resolve/main/inference.onnx`
- `https://huggingface.co/PaddlePaddle/PP-OCRv6_small_rec_onnx/resolve/main/inference.onnx`
- `https://huggingface.co/PaddlePaddle/PP-OCRv6_small_rec_onnx/resolve/main/inference.yml`
- `gpa_640_int8.onnx`: nur lokal (`26_onnx/source8`, kein URL) → optional,
  Server läuft mit `--gui none` ohne dieses Modell.

`source10/models → ../source7_mvp/models → ../source6/models/` bleibt
Symlink-Kette; CI cached `models/` per `fetch_models.sh`-Hash; das
Server-Release-Tar enthält Server-Binary + Modelle.

### 2.9 CI/Release + Docker-Tests

CI (`../../.github/workflows/`, Muster `release-29-lowbandwidth.yml` +
`android-lbw.yml`): neuer Workflow `release-29-source11.yml`
(Tag `lbw11-v*`, Cargo-Versions-Gate, `ubuntu-24.04`, `nasm`, rust-cache,
Modell-Cache) mit Jobs Linux (fmt/clippy/tests, `package_release.sh`,
Client-Größe nur messen/berichten, kein Gate), Android (`cargo-ndk` + Gradle, beide ABIs), Dissector
(C-Build + `tshark`-Check), Publish (Server+Modelle-, Client-full-,
SSH-Client-, Direkt-Client-, APK-, Dissector-Assets + `SHA256SUMS`; `workflow_dispatch`
= Dry-Run). Docker: neuer `source11_compact/docker/`-Ordner
(`Dockerfile`, `tc qdisc netem rate 6kbit`-Harness, Xvfb `:99` 1280×720 +
xterm/xev, alternativ Host-X via übergebenem `DISPLAY`), weil heute
weder Dockerfile noch `tc`-Aufruf existieren. Start wie vorgegeben:

```sh
cl-cl-generator/example/05_dockerfile_meta/source01/examples/03_ai_env $ setup02_run.sh --gpu --host-kmsg --usb --source-isolation --docker-sock --x11 --graphics --host-network
```
(Dockerfile: `…/03_ai_env/Dockerfile`; der Container, in dem diese Planung
lief, wurde aus ebendiesem Image gebaut und mit genau diesem Kommando
gestartet.)

## 3. Risiken und offene Fragen

- `russh` auf Android-NDK (Tokio-Runtime im `cdylib`, minSdk 26) früh per
  Prototyp beweisen (Task A0); Tokio auf Android ist die größte
  Unbekannte des Plans.
- Jump-Host-Ketten (2 Hops, CGNAT) brauchen einen sshd-Jump im
  Docker-Harness — Task D1 deckt das ab, HIL gegen echten Jump folgt.
- Dissector-Entscheidung C-pur vs. Rust-`cdylib`-Helper erst nach
  Review-Prototyp fällen (Task W0, Größen-/Build-Kosten).
- Client-Größe ist **kein** Gate mehr (SSH-Client darf wachsen, §2.5);
  CI meldet die Größe nur. Restrisiko: Tokio/russh-Blähung der
  Android-`.so` — in A0 messen und berichten, nicht blockieren.
- `gpa`-Exportpfad (`26_onnx/source8/scripts/export_models.sh`) und
  HF-Erreichbarkeit sind im Plan nur als Datei-Links verifiziert, nicht
  per Download — holt der Fetch-Task nach.

## Sources

- [Wireshark-Entwickleranleitung: Dissector hinzufügen](https://www.wireshark.org/docs/wsdg_html_chunked/ChDissectAdd.html)
- `plan/misc/russh.md` (Repo-Notiz: russh = Tokio-SSH2-Client/Server, moderne Primitive inkl. Post-Quantum-Hybrid-KEX, Terrapin-/Strict-KEX-Mitigation)
