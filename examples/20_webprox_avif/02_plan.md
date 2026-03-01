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