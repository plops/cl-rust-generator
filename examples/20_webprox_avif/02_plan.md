### 1. Architektur-Philosophie & Entkopplung (Clean Architecture)

Um die Wartbarkeit zu maximieren und das Projekt testbar zu machen, trennen wir strikt zwischen **Netzwerk**, **Logik** und **Darstellung**. Wir nutzen ein Cargo Workspace mit separaten Crates.

#### Workspace-Struktur:
*   `proto-definitions`: Beinhaltet die `browser_stream.proto` und generiert die Rust-Structs (`prost`/`tonic`).
*   `cloud-render-server`: Der Server. Steuert Chrome, greift Frames ab, kodiert mit `rav1e` und streamt sie.
*   `macroquad-client`: Der grafische TUI/GUI-Client.
*   `core-logic`: Beinhaltet geteilte Logik wie das Event-Handling (Maus, Tastatur) und Bildpuffer-Verwaltung.

**Entkopplung im Client:**
Die Netzwerklogik (`tonic` gRPC) läuft in einem eigenen asynchronen `tokio`-Task. Die GUI (`macroquad`) läuft im Main-Thread (da viele Grafik-APIs Thread-gebunden sind). Beide kommunizieren über asynchrone Channels (`tokio::sync::mpsc`). Das bedeutet: Die GUI blockiert nie, wenn das Netzwerk laggt, und das Netzwerk ist unabhängig von der Framerate der GUI.

---

### 2. Technologie-Stack & Dependency-Minimierung (Anti-Bloat)

*   **Kommunikation:** `tonic` (gRPC über HTTP/2). Erlaubt Multiplexing und ist extrem bandbreiteneffizient. Optionale TLS-Unterstützung per Feature-Flag (`features = ["tls"]`).
*   **Server-Browser:** `chromiumoxide` zur Steuerung eines Headless Chrome via CDP (Chrome DevTools Protocol).
*   **Kompression (Server):** `rav1e` (reines Rust). Wir konfigurieren es auf hohe Geschwindigkeit (z.B. Speed-Level 8-10) und "Low Latency / Zero Latency", da wir Echtzeit-Interaktion brauchen.
*   **GUI (Client):** `macroquad`. Um Bloat zu vermeiden, binden wir es strikt in der `Cargo.toml` so ein:
    ```toml
    [dependencies.macroquad]
    version = "0.4"
    default-features = false
    # Keine Audio-Engines, keine schweren Extra-Features.
    ```
    Für die UI (Adressleiste, Status) nutzen wir Macroquads einfache `draw_text` und `draw_rectangle` Funktionen. Ein minimaler Font wird als Byte-Array in die Binary kompiliert (`include_bytes!`), um externe Abhängigkeiten zu vermeiden.
*   **Dekompression (Client):** Da `rav1e` nur encodiert, brauchen wir clientseitig einen schnellen Decoder. Eine gute Wahl ist `dav1d` (via `dav1d-rs`), da er extrem performant und leichtgewichtig ist.

---

### 3. Protokoll-Design (gRPC)

Wir nutzen einen bidirektionalen Stream.

```protobuf
syntax = "proto3";
package graphical_proxy;

service RemoteBrowser {
  rpc StreamSession(stream ClientEvent) returns (stream ServerUpdate);
}

message ClientEvent {
  oneof event {
    Navigate navigate = 1;
    MouseClick click = 2;
    MouseMove move = 3;
    ScrollEvent scroll = 4;
    KeyPress key = 5;
    ResizeViewport resize = 6;
  }
}

message ServerUpdate {
  oneof update {
    Av1Frame av1_frame = 1;      // Ein AV1-kodierter Frame (Intra oder Inter/Delta)
    StatusMessage status = 2;    // z.B. "Lädt..." oder "Verbindung getrennt"
  }
}
```

---

### 4. Implementierungsdetails: Inkrementelle Updates & Scrolling

Der Schlüssel zur Effizienz ist die Chrome-API `Page.startScreencast`.
1.  Der Server weist Chrome an, einen Screencast zu starten. Chrome sendet automatisch **nur dann** einen neuen Frame an unseren Rust-Server, wenn sich der DOM visuell ändert (Animationen, Scrolling).
2.  Chrome liefert uns Dirty-Rectangles und den Bildinhalt.
3.  Der Server pflegt einen internen Raw-Framebuffer (RGBA). Er updatet diesen Buffer mit den Chrome-Daten und pusht den Buffer in den `rav1e` Encoder.
4.  `rav1e` ist auf Video-Encoding eingestellt. Wenn gescrollt wird, berechnet `rav1e` einen extrem kleinen Delta-Frame (Inter-Frame) dank Motion Estimation.
5.  Der komprimierte Frame geht via gRPC an den Client, der ihn mit `dav1d` dekodiert und als `Texture2D` in `macroquad` auf den Bildschirm zeichnet (`draw_texture`).

---

### 5. Testinfrastruktur & AI-Agent-Kompatibilität

Da KI-Agenten das System mitentwickeln und testen sollen, ist "CLI-First" für die Testbarkeit entscheidend.

**Server CLI (`clap`):**
```bash
cloud-render-server --listen 0.0.0.0:50051 \
                    --headed false \           # True für Debugging (manuelles Zusehen)
                    --tls-cert cert.pem \
                    --tls-key key.pem
```

**Client CLI (`clap`) - Der Agenten-Traum:**
Wir statten den Client mit einem "Headless Test Mode" aus. In diesem Modus initiiert der Client gar kein GUI-Fenster (`macroquad` bleibt inaktiv). Stattdessen nutzt er nur den Netzwerk-Core, verbindet sich, navigiert, empfängt den ersten dekodierten AV1-Frame und speichert ihn als PNG auf die Festplatte.

```bash
macroquad-client --url "https://news.ycombinator.com" \
                 --headless-test \
                 --output-file "test_result.png" \
                 --timeout 5000  # Beenden nach 5 Sekunden
```
*Warum das für den KI-Agenten genial ist:*
Der Agent kann nach Code-Änderungen das Kommando ausführen. Er prüft den Exit-Code (0 = Erfolg). Anschließend kann er mit seinem Vision-Modell (falls vorhanden) das `test_result.png` ansehen, oder einfach per Hash/Dateigröße verifizieren, dass das Bild korrekt empfangen und dekomprimiert wurde. Dies ermöglicht vollautomatisierte Integrationstests (`cargo test --test integration`), bei denen Server und Client lokal gestartet werden und die visuelle Pipeline verifiziert wird.

---

### 6. Meilensteine & Roadmap für die Umsetzung

1.  **Phase 1: Fundament & Protokoll (Mocking)**
    *   Aufsetzen des Workspaces, `tonic` gRPC-Definitionen und `clap` CLI.
    *   Erstellung des Clients im "Test-Modus", der einen Dummy-Frame empfängt.
2.  **Phase 2: Server Browser-Integration**
    *   `chromiumoxide` einbinden.
    *   `Page.startScreencast` implementieren und rohe Bilddaten extrahieren.
    *   *Headless/Headed* Modus-Toggle für lokales Debugging implementieren.
3.  **Phase 3: Die AV1-Pipeline (Das Herzstück)**
    *   Server: `rav1e` konfigurieren (Low Latency, Zero-Delay Tuning). RGB-Frames in YUV konvertieren (benötigt von AV1) und encodieren.
    *   Client: `dav1d-rs` einbinden, AV1-Bitstream empfangen, in RGB decodieren.
4.  **Phase 4: Client GUI & Interaktion**
    *   `macroquad` initialisieren.
    *   Dekodierten Buffer in eine `macroquad::texture::Texture2D` laden und zeichnen.
    *   Mauskoordinaten, Klicks und Tastaturanschläge abfangen und via gRPC an den Server senden (wo sie via CDP `Input.dispatchMouseEvent` etc. in Chrome injiziert werden).
5.  **Phase 5: Optimierung & Verschlüsselung**
    *   Zertifikatsbasierte TLS-Verschlüsselung aktivieren.
    *   Latenz-Feintuning (z.B. Debouncing von Mausbewegungen).

### Nächste Schritte
Soll ich als Erstes das Cargo-Workspace Setup und die detaillierte `proto`-Datei generieren, oder wollen wir uns direkt das Setup des `macroquad` Clients mit entkoppeltem Netzwerk-Task ansehen?




Das ist ein hervorragender architektonischer Einwurf. Als Systemarchitekt schätze ich solche Weitsicht. Gehen wir die Punkte systematisch durch, denn sie verfeinern unseren Plan maßgeblich.

### 1. Dekoder: `rav1d` statt C-Bindings
Die Wahl von `rav1d` (dem reinen Rust-Port von dav1d) ist ein massiver Gewinn für unsere CI/CD-Pipeline und Cross-Platform-Ziele. Wir umgehen damit das berüchtigte "C-Toolchain-Hell" (kein Meson, kein Ninja, kein clang notwendig). Ein reines Rust-Ökosystem (`rav1e` auf dem Server, `rav1d` auf dem Client) bedeutet, dass wir per GitHub Actions mühelos für Linux, Windows und macOS kompilieren können. Das passt perfekt zur "Anti-Bloat"-Philosophie.

### 2. Die Video-Pipeline: PNG als saubere Quelle
Dein Gedankengang zur Bildqualität ist absolut korrekt (Vermeidung von *Generation Loss*). Wenn Chrome JPEG liefert, enthält das Bild bereits Kompressionsartefakte. Der AV1-Encoder (`rav1e`) würde dann Bandbreite verschwenden, um diese JPEG-Artefakte exakt nachzubilden. 

Die optimierte Server-Pipeline sieht daher wie folgt aus:
1.  **Screencast-Config:** Wir weisen Chrome über CDP an: `Page.startScreencast { format: "png" }`.
2.  **Base64 Decode:** Wir dekodieren den String (sehr billig per `base64` Crate).
3.  **PNG Decode:** Wir nutzen z.B. das `image` oder `png` Crate (strikt ohne Default-Features, nur PNG-Support kompiliert), um an die unkomprimierten RGBA-Pixel zu kommen.
4.  **Farbraum-Konvertierung:** AV1 arbeitet effizientesten im YUV-Farbraum (meist YUV420p). Wir konvertieren das RGBA-Array in YUV.
5.  **AV1 Encoding:** Übergabe an `rav1e`.
6.  **Transport:** Ab in den gRPC Stream.

### 3. Volltextsuche (Architectural Design)
Da wir im Kern einen *visuellen* Proxy bauen, gibt es für die Suche zwei elegante Architekturebenen, die wir im Protokoll vorsehen sollten:

*   **Ebene 1: Visuelle Suche (Server-Side):** Der Client sendet einen `SearchRequest` mit dem Suchbegriff. Der Server instruiert Chrome (via CDP `Input.dispatchKeyEvent` mit `Ctrl+F` oder der nativen CDP-Such-API). Chrome markiert die Treffer gelb auf der Webseite. Da wir den Bildschirm streamen, sieht der Nutzer das Highlighting sofort nativ im Videostream, inklusive automatischem Scrollen zum Treffer!
*   **Ebene 2: Semantischer Text (Client-Side Overlay):** Wir erweitern das gRPC-Protokoll um eine Metadaten-Nachricht. Der Server kann per CDP (`Runtime.evaluate("document.body.innerText")`) blitzschnell den sichtbaren Text extrahieren und an den Client senden. Der Client kann diesen Text im Hintergrund halten. Wenn der Nutzer lokal sucht, weiß der Client zumindest sofort, *ob* das Wort existiert und wie oft.

---

### Der aktualisierte Masterplan: Workspace & gRPC-Protokoll

Um das Fundament richtig zu gießen, schlage ich vor, wir definieren zuerst den Workspace und den gRPC-Vertrag (`browser_stream.proto`). Das ist die Schnittstelle, die unsere Teams (oder KI-Agenten) strikt entkoppelt arbeiten lässt.

#### 1. Die Workspace Struktur (`Cargo.toml` im Root)
```toml
[workspace]
members =[
    "proto-def",        # Das gRPC Schema (Kompiliert mit tonic-build)
    "cloud-render-srv", # Der Server (chromiumoxide + rav1e)
    "macroquad-client", # Die GUI (macroquad + rav1d)
]
resolver = "2"

[profile.release]
opt-level = "z"     # Optimierung auf Dateigröße (Anti-Bloat)
lto = true          # Link Time Optimization (Entfernt ungenutzten Code)
codegen-units = 1
panic = "abort"
strip = true        # Entfernt Debug-Symbole aus der Release-Binary
```

#### 2. Der gRPC-Vertrag (`proto-def/proto/browser_stream.proto`)
Dieses Protokoll spiegelt nun auch deine Anforderungen für Suche und Text-Metadaten wider:

```protobuf
syntax = "proto3";
package graphical_proxy;

// Der Haupt-Service für den Remote-Browser
service RemoteBrowser {
  // Bidirektionaler Stream für Low-Latency Interaktion
  rpc StreamSession(stream ClientEvent) returns (stream ServerUpdate);
}

// --- Nachrichten vom CLIENT zum SERVER ---
message ClientEvent {
  oneof event {
    NavigateRequest navigate = 1;
    MouseInput mouse = 2;
    KeyboardInput keyboard = 3;
    ScrollInput scroll = 4;
    ViewportResize resize = 5;
    SearchRequest search = 6;      // NEU: Triggert die Suche im Server-Browser
    RequestTextData request_text = 7; // NEU: Fordert den reinen Seitentext an
  }
}

message NavigateRequest { string url = 1; }
message MouseInput { 
  int32 x = 1; 
  int32 y = 2; 
  bool is_click = 3; 
  string button = 4; // "left", "right", "middle"
}
message KeyboardInput { 
  string key_code = 1; 
  bool is_pressed = 2; 
}
message ScrollInput { int32 delta_x = 1; int32 delta_y = 2; }
message ViewportResize { int32 width = 1; int32 height = 2; }
message SearchRequest { string query = 1; bool next = 2; }
message RequestTextData {}

// --- Nachrichten vom SERVER zum CLIENT ---
message ServerUpdate {
  oneof update {
    VideoFrame frame = 1;          // Der encodierte AV1 Frame
    PageMetadata metadata = 2;     // Meta-Infos (Titel, URL)
    TextContent text_content = 3;  // NEU: Extrahierter Text für lokale Indexierung
    SystemStatus status = 4;       // Fehler, Ladezustände
  }
}

message VideoFrame {
  bytes data = 1;             // rav1e Output (AV1 Bitstream)
  bool is_keyframe = 2;       // Wichtig für den rav1d Decoder
  uint64 timestamp_ms = 3;
}

message PageMetadata {
  string current_url = 1;
  string title = 2;
}

message TextContent {
  string inner_text = 1; // Der sichtbare Text der Seite
}

message SystemStatus {
  string message = 1;
  enum Level { INFO = 0; WARNING = 1; ERROR = 2; }
  Level level = 2;
}
```

### Wie wir die KI-Agenten-Testbarkeit hiermit maximieren

Da die Logik durch gRPC komplett vom UI entkoppelt ist, können wir im `macroquad-client` Crate ein Modul `agent_tester.rs` anlegen.

Wenn der Client mit `--test-mode --url "https://news.ycombinator.com"` gestartet wird, passiert Folgendes:
1. Das UI (macroquad) startet gar nicht erst (spart Ressourcen im CI-Container).
2. Der gRPC-Task verbindet sich mit dem Server.
3. Er sendet `NavigateRequest`.
4. Er wartet auf das erste `VideoFrame`, wo `is_keyframe == true`.
5. Er wirft die Bytes in `rav1d`, bekommt ein YUV-Bild, wandelt es in RGB.
6. Er speichert es per `image` Crate als `test_output.png` und beendet sich mit Exit Code 0.

Der KI-Agent (oder ein automatisiertes Skript) kann dann den Server starten, diesen Client-Befehl absetzen und verifizieren, dass `test_output.png` generiert wurde und korrekten Inhalt hat.

### Nächster Schritt

Sollen wir als Nächstes die Implementierungs-Architektur des **Servers** (das Zusammenspiel von `chromiumoxide` Screencast $\rightarrow$ PNG Decode $\rightarrow$ `rav1e` Encoder $\rightarrow$ gRPC Stream) im Detail skizzieren, oder bevorzugst du, dass wir mit dem Minimal-Setup des **Clients** (`macroquad` + `rav1d` + gRPC) beginnen?