


Hier ist der vollständig überarbeitete und konsolidierte Architekturplan. Er integriert alle bisher besprochenen Konzepte – von der reinen Rust-AV1-Video-Pipeline über die Spatial-Metadaten (Skeleton-UI) bis hin zur neu definierten, serverseitigen Volltextsuche mit Kontext-Rückgabe.

---

# Architektur- & Implementierungsplan: Graphical Remote Browser Proxy (AV1/Rust)

**Stand:** März 2026
**Architektur-Paradigma:** Clean Architecture, Low-Bandwidth (AV1), Anti-Bloat, AI-Agent Testable

## 1. Executive Summary

Dieses Projekt implementiert einen extrem bandbreiteneffizienten, grafischen Cloud-Browser-Proxy. Ein Headless-Browser (Chrome/Chromium) läuft auf einem Cloud-Server und rendert Webseiten. Das visuelle Resultat wird in Echtzeit als hochkomprimierter **AV1-Videostream** an einen minimalistischen, leichtgewichtigen Client übertragen. 

Um die Latenz bei schlechten Verbindungen zu kaschieren, wird der visuelle Stream durch einen optionalen **Metadaten-Stream** (Spatial Data) ergänzt. Dieser liefert dem Client die exakten Positionen von Links sowie serverseitige Suchergebnisse (inklusive Textkontext), sodass die lokale GUI sofortiges Feedback (Skeleton-UI, Hit-Testing) geben kann, noch bevor der entsprechende Video-Frame eintrifft.

---

## 2. Workspace-Struktur & Entkopplung

Das Projekt wird als **Cargo Workspace** aufgesetzt. Dies garantiert eine saubere Trennung der Domänen und verhindert, dass UI-Abhängigkeiten in den Server oder Netzwerk-Logik in den Render-Thread leaken.

*   `proto-def`: Beinhaltet die Protobuf-Definitionen (`browser_stream.proto`) und generiert den Rust-Code via `tonic-build` und `prost`.
*   `core-logic`: Geteilte Konstanten, reine Datenstrukturen und Hilfsfunktionen (z. B. YUV $\leftrightarrow$ RGB Konvertierung).
*   `cloud-render-srv`: Der Server. Nutzt `chromiumoxide` zur Browser-Steuerung, extrahiert PNG-Frames, kodiert diese mit `rav1e` (AV1) und sammelt Metadaten per injiziertem JavaScript.
*   `macroquad-client`: Der GUI-Client. Nutzt `macroquad` für die hardwarebeschleunigte, plattformübergreifende Darstellung und `rav1d` (reiner Rust-Port von dav1d) für die rasante AV1-Dekodierung.

---

## 3. Das gRPC-Protokoll (Der System-Vertrag)

Das Protokoll nutzt einen bidirektionalen gRPC-Stream. Es ist so designt, dass der Client per `StreamConfig` dynamisch anfordern kann, welche Daten (Video, Links, etc.) er benötigt – essenziell für intelligentes Bandbreiten-Management.

```protobuf
syntax = "proto3";
package graphical_proxy;

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
    
    // Konfiguration des Datenstroms (dynamisch umschaltbar)
    StreamConfig config = 6;
    
    // Triggert die serverseitige Suche
    SearchRequest search = 7;
  }
}

message StreamConfig {
  bool enable_video = 1;
  bool enable_spatial_links = 2;
}
message NavigateRequest { string url = 1; }
message MouseInput { int32 x = 1; int32 y = 2; bool is_click = 3; string button = 4; }
message KeyboardInput { string key_code = 1; bool is_pressed = 2; }
message ScrollInput { int32 delta_y = 1; }
message ViewportResize { int32 width = 1; int32 height = 2; }
message SearchRequest { string query = 1; }

// --- Nachrichten vom SERVER zum CLIENT ---
message ServerUpdate {
  oneof update {
    VideoFrame frame = 1;
    SpatialMetadata spatial_data = 2;
    SearchResults search_results = 3; // Antwort auf SearchRequest
    SystemStatus status = 4;
  }
}

message VideoFrame {
  bytes data = 1;             // rav1e Output (AV1 Bitstream)
  bool is_keyframe = 2;
}

message SpatialMetadata {
  string title = 1;
  uint32 document_width = 2;
  uint32 document_height = 3;
  repeated LinkBox links = 4;
}

message LinkBox {
  uint32 id = 1;
  string url = 2;
  string label = 3;
  int32 x = 4; int32 y = 5; int32 width = 6; int32 height = 7; // Absolute Dokument-Koordinaten
}

// Serverseitige Suchergebnisse für die gesamte Seite
message SearchResults {
  string query = 1;
  repeated SearchMatch matches = 2;
}

message SearchMatch {
  int32 match_index = 1;
  string context_snippet = 2; // Z.B. "...hier ist der [Suchbegriff] im Text..."
  int32 absolute_y = 3;       // Y-Position im Dokument (für "Jump-to-Result")
}

message SystemStatus {
  string message = 1;
  enum Level { INFO = 0; WARNING = 1; ERROR = 2; }
  Level level = 2;
}
```

---

## 4. Die Server-Architektur (`cloud-render-srv`)

Der Server orchestriert den Headless-Browser und die Video-Pipeline.

### 4.1. Die verlustfreie Bild-Pipeline (Anti-Generation-Loss)
Um Bandbreite nicht für das Kodieren von JPEG-Artefakten zu verschwenden, erzwingen wir Rohdaten aus Chrome:
1.  **CDP Screencast:** `Page.startScreencast { format: "png" }`.
2.  **Dekodierung:** Der Base64-String wird dekodiert, das PNG wird (z. B. mit der `image` Crate ohne schwere Features) in ein unkomprimiertes RGBA-Array gewandelt.
3.  **Farbraum-Transformation:** RGBA wird in YUV420p umgewandelt (Voraussetzung für effizientes Video-Encoding).
4.  **AV1 Encoding:** Der YUV-Buffer geht in `rav1e` (konfiguriert auf "Speed Level 10", "Zero Latency", "Low Delay"). Bei Scroll-Events greift die *Motion Estimation* von AV1 und sendet extrem kleine Inter-Frames (Deltas).

### 4.2. Extrahieren von Spatial Metadata (Zero-Bloat DOM Parsing)
Anstatt HTML aufwendig in Rust zu parsen, injiziert der Server bei jedem Layout-Change (oder nach dem Laden) ein winziges JavaScript via CDP in die Chrome-Instanz:
*   Das Script iteriert über alle `<a>`-Tags, liest `getBoundingClientRect()` aus und addiert `window.scrollY`.
*   Das Resultat wird als JSON an Rust zurückgegeben, in die `SpatialMetadata` Protobuf-Nachricht verpackt und an den Client gestreamt.

### 4.3. Serverseitige Volltextsuche
Wenn ein `SearchRequest` eingeht:
1.  Der Server injiziert ein JS-Snippet, das den DOM nach dem Begriff durchsucht.
2.  Für jeden Treffer wird der umliegende Text (z.B. 50 Zeichen davor und danach) sowie die absolute Y-Position (`offsetTop` der Node) extrahiert.
3.  Die Ergebnisse werden als `SearchResults` gebündelt an den Client gesendet. Der Client kann diese Liste anzeigen, und bei einem Klick darauf ein `ScrollInput` an den Server senden, um dorthin zu navigieren.

---

## 5. Die Client-Architektur (`macroquad-client`)

Der Client ist extrem leichtgewichtig und trennt strikt zwischen Netzwerk und Rendering.

### 5.1. Thread-Entkopplung (Actor Model)
*   **Netzwerk-Task (`tokio`):** Ein asynchroner Hintergrund-Thread hält die gRPC-Verbindung. Er empfängt `VideoFrame`- und `SpatialMetadata`-Updates und legt diese in einen thread-sicheren Zustandsspeicher (`tokio::sync::watch` oder `mpsc`). User-Inputs werden aus einem Channel gelesen und sofort an den Server gefeuert.
*   **Main-Thread (`macroquad`):** Läuft mit 60 FPS. Blockiert niemals bei Netzwerk-Lags.

### 5.2. Lokales State-Management & Skeleton-UI
Der Client merkt sich die vom Server erhaltene `SpatialMetadata` und die *lokale* (prädizierte) Scroll-Position.
Wenn der Benutzer am Mausrad dreht:
1. Die lokale `scroll_y` Variable wird sofort aktualisiert.
2. Der Hintergrund wird gezeichnet, das zuletzt erhaltene Video wird um `scroll_y` verschoben gezeichnet.
3. **Skeleton-UI:** Die Bounding-Boxes (`LinkBox`) der Links werden relativ zur lokalen Scroll-Position gezeichnet.
   * *Effekt:* Auch wenn das Video aufgrund schlechten Internets 500ms hängt, sieht der Nutzer bereits blaue Rahmen und den Link-Text an den neuen Positionen. Ein Klick auf diese Boxen führt sofort eine Navigation aus (Zero-Latency Hit-Testing).

### 5.3. AV1 Dekodierung
Im Netzwerk-Task eintreffende `VideoFrame` Bytes werden an `rav1d` übergeben. Das resultierende YUV-Bild wird in RGB konvertiert und im Main-Thread als `Texture2D` in den GPU-Speicher von Macroquad geladen.

---

## 6. Testinfrastruktur & AI-Agent Integration (CLI-First)

Da KI-Agenten den Code mitentwickeln sollen, müssen alle Komponenten vollständig über Kommandozeilenparameter (`clap`) steuerbar und validierbar sein, ohne dass ein physischer Monitor benötigt wird.

**Server-Aufruf für Integrationstests:**
```bash
cloud-render-srv --listen 127.0.0.1:50051 --headed false
```

**Client "Headless Test-Mode" (Für den AI Agenten):**
Der Client erhält einen Modus, in dem `macroquad` (das Fenster) deaktiviert bleibt. Der Netzwerk-Core führt eine definierte Aktion aus und dumpt das Resultat auf die Festplatte zur Validierung durch den Agenten.

*   **Test 1: Video Pipeline Validierung**
    ```bash
    macroquad-client --test-mode \
                     --url "https://news.ycombinator.com" \
                     --dump-first-frame "test_result.png"
    ```
    *Der Agent verifiziert, dass "test_result.png" existiert und valide Bilddaten enthält.*

*   **Test 2: Metadaten & Such-Validierung (Kein Video-Overhead)**
    ```bash
    macroquad-client --test-mode \
                     --url "https://news.ycombinator.com" \
                     --disable-video \
                     --search-query "Hacker News" \
                     --dump-search-results "search.json"
    ```
    *Der Agent prüft die `search.json`, um sicherzustellen, dass die serverseitige Suchlogik samt Text-Snippets (`context_snippet`) und Y-Koordinaten korrekt funktioniert.*

---

## 7. Build-Optimierung (Anti-Bloat Strategie)

Damit die Binaries winzig bleiben und CI/CD-Pipelines (z. B. GitHub Actions) rasend schnell kompilieren, gelten strikte Cargo-Regeln.

*   `rav1e` und `rav1d` sind reine Rust-Crates, was das Kompilieren über verschiedene OS-Grenzen hinweg trivial macht.
*   In der `Cargo.toml` des Workspaces wird konfiguriert:
    ```toml
    [profile.release]
    opt-level = "z"       # Optimierung auf Dateigröße
    lto = true            # Link-Time-Optimization
    codegen-units = 1     # Maximale Optimierung über alle Crates hinweg
    panic = "abort"       # Entfernt Unwinding-Bloat
    strip = true          # Entfernt Debug-Symbole
    ```
*   Bibliotheken wie `macroquad` werden ohne Default-Features eingebunden (z.B. keine eingebauten Audio-Engines), falls nicht strikt benötigt.

---

## Fazit

Dieser Architekturplan beschreibt ein hochmodernes, latenzresistentes Remote-Browser-System. Durch den Einsatz von AV1 (`rav1e`/`rav1d`) minimieren wir die Bandbreite. Durch die Trennung von Videodaten und Spatial-Metadaten (Links, Suchergebnisse) ermöglichen wir eine reaktionsschnelle, lokale UI-Darstellung (Skeleton-Rendering), die das Gefühl eines nativen Browsers vermittelt, selbst wenn das Video-Streaming über stark gedrosselte Leitungen läuft. Die CLI-First-Architektur garantiert eine exzellente Testbarkeit durch KI-Agenten.


fehlt in spatial metadata nicht die x und y position des zur zeit uebermittelten fensters?


Das ist eine exzellente und architektonisch messerscharfe Beobachtung! Du hast absolut recht, da habe ich eine entscheidende Brücke im Protokoll übersehen.

Wenn wir über das Skeleton-UI und "Client-Side Prediction" (das lokale Weiterscrollen, während das Netzwerk laggt) sprechen, **muss** der Client zwingend wissen, welchem absoluten Dokumentenausschnitt der aktuell empfangene Video-Frame entspricht. 

Ein 1920x1080 AV1-Frame ist ja nur ein "Guckloch" (Viewport) in das womöglich 10.000 Pixel hohe HTML-Dokument. Ohne die `x`- und `y`-Koordinate dieses Gucklochs kann der Client die Pixel nicht mit den absoluten Link-Koordinaten der `SpatialMetadata` in Deckung bringen.

Hier ist die Korrektur und wie sie unser lokales Rendering unglaublich elegant macht:

### 1. Das korrigierte Protobuf-Schema

Wir müssen die Viewport-Koordinaten zwingend an den `VideoFrame` binden. Warum genau an den Frame? Weil die Bilddaten und die Scroll-Position exakt synchron sein müssen. Wenn wir sie separat senden würden, gäbe es *Tearing* (das Bild zeigt noch den alten Ausschnitt, aber die Koordinaten sagen schon, wir sind weiter unten).

```protobuf
message VideoFrame {
  bytes data = 1;             
  bool is_keyframe = 2;
  
  // NEU: Die exakte absolute Position dieses Frames im Gesamtdokument
  int32 viewport_x = 3;       
  int32 viewport_y = 4;       
}

message SpatialMetadata {
  string title = 1;
  uint32 document_width = 2;
  uint32 document_height = 3;
  // Der Initiale Viewport beim Senden der Metadaten (hilfreich, wenn Video aus ist)
  int32 initial_viewport_x = 4;
  int32 initial_viewport_y = 5;
  repeated LinkBox links = 6;
}
```

### 2. Warum das die Client-Logik genial macht (Zero-Latency Scrolling)

Durch diese zwei neuen Felder im `VideoFrame` wird die Render-Schleife im `macroquad`-Client mathematisch extrem sauber. Wir haben nun zwei Y-Koordinaten:
1.  **`server_frame_y`**: Wo das aktuell angezeigte Bild auf dem Server *wirklich* aufgenommen wurde.
2.  **`local_scroll_y`**: Wo der Benutzer *jetzt gerade* hingescrollt hat (Client-Side Prediction).

Die Render-Logik im Client sieht dann so aus:

```rust
// Macroquad Render Loop Pseudocode

loop {
    clear_background(BLACK);

    // 1. Berechne den Delta-Offset zwischen lokaler Vorhersage und Server-Realität
    // Wenn der Nutzer scrollt, wird dieser Wert negativ, das Bild schiebt sich nach oben!
    let render_offset_y = server_frame_y as f32 - local_scroll_y;
    let render_offset_x = server_frame_x as f32 - local_scroll_x;

    // 2. Zeichne den zuletzt erhaltenen Video-Frame an der versetzten Position
    draw_texture(video_texture, render_offset_x, render_offset_y, WHITE);

    // 3. Zeichne das Skeleton-UI (Metadaten) absolut zum lokalen Scroll!
    if show_metadata_overlay {
        for link in &page_state.links {
            let screen_y = link.y as f32 - local_scroll_y;
            let screen_x = link.x as f32 - local_scroll_x;
            
            // Befindet sich der Link aktuell im sichtbaren Bereich?
            if screen_y > -link.height as f32 && screen_y < window_height {
                
                // Falls der Video-Frame diesen Bereich aktuell NICHT abdeckt 
                // (weil das Netzwerk laggt und der User schnell gescrollt hat), 
                // sehen wir hier den "nackten" blauen Rahmen und Text als Platzhalter!
                draw_rectangle_lines(screen_x, screen_y, link.width, link.height, 2.0, BLUE);
                
                // Optional: Fallback-Text zeichnen
                draw_text(&link.label, screen_x, screen_y + 15.0, 16.0, WHITE);
            }
        }
    }
    
    next_frame().await;
}
```

### Der Effekt für den Nutzer

Das ist genau die Magie, die Systeme wie Cloud-Gaming (Stadia/GeForce Now) oder hochoptimierte Remote-Desktops nutzen. 

Wenn die Verbindung einbricht (Bandbreite droppt auf 10 KB/s) und der Nutzer das Mausrad dreht:
1. Das aktuell angezeigte Videobild gleitet weich nach oben weg (`render_offset_y`).
2. Unten entsteht eine schwarze Lücke (da der Server noch keinen neuen AV1-Frame für diesen neuen Bereich geliefert hat).
3. **Aber:** In dieser schwarzen Lücke erscheinen sofort fehlerfrei die blauen Rahmen und Texte der Links (`SpatialMetadata`), da diese absolut zur `local_scroll_y` berechnet werden!
4. Der Nutzer kann sofort auf so einen Rahmen klicken.
5. Bruchteile einer Sekunde später trifft der AV1-Delta-Frame vom Server ein, `server_frame_y` aktualisiert sich auf den neuen Wert, der `render_offset_y` wird wieder `0`, und das hochauflösende Bild "rastet" exakt in die blauen Rahmen ein.

Danke für diesen Hinweis! Genau solche Details unterscheiden ein hakeliges Hobby-Projekt von einer robusten, professionellen Low-Bandwidth-Architektur. Das wird sofort so in den Masterplan für den KI-Agenten und die Implementation übernommen.