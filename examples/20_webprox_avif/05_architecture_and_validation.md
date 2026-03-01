# Architektur-Refinement & Early Validation Plan
**Datei:** `05_architecture_and_validation.md`
**Priorität:** Hoch (Vor Beginn der Implementierung zu beachten)

Dieses Dokument adressiert zwei essenzielle Anforderungen an das Projekt:
1. Die Code-Basis muss hochgradig wartbar sein und darf keine monolithischen "God-Files" (z.B. Dateien mit 4000 Zeilen) enthalten.
2. Die Implementierung soll frühzeitige, testbare Zwischenergebnisse (Early Validation) liefern, um Systemgrenzen und Performance-Probleme vorab zu erkennen.

---

## 1. Modulare Dateistruktur (Anti-Monolith Richtlinie)

Um zu verhindern, dass die Hauptkomponenten unüberschaubar werden, wird pro Crate eine strikte Modul-Aufteilung verlangt. Eine Datei sollte initial selten mehr als 300-400 Zeilen Code umfassen. Wenn sie wächst, muss fachlich ausgelagert werden.

### `cloud-render-srv` (Server)
Anstatt alle Browser- und Video-Logik in eine `browser.rs` oder `main.rs` zu stecken, wird folgende Aufteilung vorgegeben:

*   `src/main.rs`: Nur CLI Parsing (clap) und Setup (Logging etc.).
*   `src/server.rs`: Die gRPC `tonic` Server-Implementation und das Spawning von Client-Sessions.
*   `src/session/`
    *   `manager.rs`: Verwaltung von verbundenen Clients und deren State.
*   `src/browser/` (Interaktion mit Chromium / CDP)
    *   `chrome_runner.rs`: Lifecycle Management (Start/Stop von headless Chrome).
    *   `cdp_stream.rs`: Die Extraction des Bild-Streams (`Page.startScreencast` oder Raw Screenshots).
    *   `dom_injection.rs`: Die Injektion von JavaScript für Link-Positionen und seitenweite Volltextsuche.
*   `src/video/` (Die Encoding Pipeline)
    *   `encoder.rs`: Integration von `rav1e` und das Threading / Pacing.
    *   `color.rs`: Die RGB zu YUV Konvertierung mittels des `yuv` Crates (`rgb_to_yuv420`).

### `macroquad-client` (Client)
Die strikte Trennung von asynchronem Netzwerk (Tokio) und synchronem Rendering (Macroquad) muss sich in den Dateien widerspiegeln:

*   `src/main.rs`: CLI (clap), Start des Async-Tasks und Übergabe an das Macroquad Window-Makro.
*   `src/state.rs`: Definition der geteilten Thread-sicheren Zustandsstrukturen (z.B. `Arc<Mutex<PageState>>`).
*   `src/network/`
    *   `grpc_client.rs`: Der Tokio Background-Task, der gRPC Updates empfängt und an den `state` pusht.
*   `src/render/`
    *   `main_loop.rs`: Die 60 FPS `macroquad` Schleife.
    *   `camera.rs`: Logik des "virtuellen Framebuffers" und lokaler Scrollvorhersage.
    *   `ui.rs`: Das Zeichnen des Skeleton-UIs (Rechtecke für Links, Text Rendering).
*   `src/video/`
    *   `decoder.rs`: `rav1d` Decoding Pipeline.
    *   `color.rs`: YUV zu RGB Konvertierung (`yuv420_to_rgb`).

---

## 2. Early Validation (Milestone Iterationen)

Um das komplexe Zusammenspiel aus gRPC, Video-Codierung und Browser-Automatisierung sicher in den Griff zu bekommen, implementieren wir nicht blind alle Features von Phase 1 bis 5, sondern fokussieren uns auf End-to-End validierbare Zwischenschritte.

### Milestone 1: The Mock Stream (Video & Network Validation)
*Was: Wir validieren die pure Infrastruktur ohne Chromium.*
1.  **Server:** Ein Mock-Worker generiert künstliche Bilder (z.B. ein farbiges Testbild oder rotierendes Rechteck mit dem `image` Crate).
2.  Das Bild wird mittels `yuvutils-rs` nach YUV konvertiert und durch `rav1e` gejagt.
3.  Der gRPC-Server schickt das fertige AV1 Frame an den Client.
4.  **Client:** Verbindet sich, dekodiert per `rav1d` und zeichnet das Video mit `macroquad` auf den Bildschirm.
*   **Warum:** So beweisen wir frühzeitig, dass das Thread-Modell im Client funktioniert (Tokio-Netzwerk blockiert Rendering nicht) und wir evaluieren die Performance + Latency von `rav1e` / `rav1d` in Echtzeit.

### Milestone 2: Headless Chrome Extraction (Browser Validation)
*Was: Wir isolieren die Chromium-Komplexität.*
1.  Wir implementieren in `cloud-render-srv` ein CLI Flag (z.B. `--test-screencast=test.png --url=...`).
2.  Der Server startet Chromium, navigiert zur URL und extrahiert per CDP ein einzelnes PNG.
3.  Zudem führt er das JavaScript für die Link-Metadaten einmalig aus und dumpt die JSON-Ergebnisse.
*   **Warum:** Da CDP Screencasts und DOM-Injektion asynchron und teils anfällig für Timing-Bugs sind, validieren wir dies in einem puren Konsolenbefehl, auf den die KI/wir schnell Test Cases schreiben können.

### Milestone 3: Die Integration & Virtual Framebuffer
*Was: Zusammenführen von Milestone 1 und 2.*
1.  Wir binden die echten Browser-Bilder an die Video-Pipeline an.
2.  Der Client empfängt erste Metadaten und zeichnet "Blaue Kasten" (Links) über die Video-Textur.
3.  Das Konzept des "lokalen Scrollens" über empfangene alte Video-Frames (`viewport_y`) wird validiert. Es muss sich smooth anfühlen.

### Milestone 4: Optimierung & Search
*Was: Finaler Feinschliff.*
1.  Implementierung der Volltextsuche und Search-Snippets.
2.  Optimierung der Parameter (Bitraten, Keyframe-Intervalle, Latenzen).
3.  Sicherstellen der CLI Test-Modi für den Test-Agenten.
