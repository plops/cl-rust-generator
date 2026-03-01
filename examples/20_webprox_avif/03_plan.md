# Architektur- & Implementierungsplan: Graphical Remote Browser Proxy (AV1)
**Datei:** `03_plan.md`
**Status:** In Planung
**Kern-Technologien:** Rust, gRPC (`tonic`), `chromiumoxide`, `rav1e` (Encoder), `rav1d` (Decoder), `macroquad` / `miniquad` (GUI), `yuv` (Farbkonvertierung)

## 1. Executive Summary
Dieses Projekt implementiert einen bandbreiteneffizienten, grafischen Cloud-Browser-Proxy. Ein auf einem Cloud-Server laufender Headless-Browser (Chromium) rendert Webseiten und extrahiert Rohbilder (PNG). Diese werden mittels AV1 (`rav1e`) hochkomprimiert und per gRPC an einen extrem leichtgewichtigen, hardwarebeschleunigten Client (`macroquad`) gestreamt. 

Zusätzlich zum Videostream liefert der Server separate "Spatial Metadata" (Link-Positionen) und Volltextsuchergebnisse für die gesamte Seite. Der Client verwaltet einen "virtuellen Framebuffer", in den die empfangenen Video-Updates an ihren exakten X/Y-Koordinaten hineingezeichnet werden. So kann der Nutzer lokal flüssig scrollen und sieht sofort ein Skeleton-UI (Links, Text-Boxen), noch bevor der nächste AV1-Frame über das Netzwerk eintrifft.

---

## 2. Workspace & Modularisierung (Clean Architecture)
Das Projekt wird als Cargo Workspace strukturiert, um Domänen strikt zu trennen und schnelles Kompilieren zu garantieren. 

*   **`proto-def`**: Enthält die `.proto`-Dateien und generiert den Rust-Code via `tonic-build`. Wird von Server und Client geteilt.
*   **`core-utils`**: Geteilte Logik ohne Abhängigkeiten zu Netz oder UI (z.B. YUV <-> RGB Farbraumkonvertierung, geometrische Primitiven).
*   **`cloud-render-srv`**: Der Server. Steuert `chromiumoxide`, verwaltet Sessions, betreibt die AV1-Encoding-Pipeline und das DOM-Scraping.
*   **`macroquad-client`**: Der TUI/GUI-Client. Beinhaltet die `rav1d` Decoding-Pipeline und das `macroquad`-Rendering.

### Anti-Bloat & Cross-Platform
*   `macroquad` wird mit minimalen Features kompiliert (z.B. Audio-Engines deaktiviert). 
*   `rav1e` und `rav1d` sind reine Rust-Implementierungen – das eliminiert komplexe C-Abhängigkeiten (wie `libaom` oder `dav1d`) und macht das Projekt nativ via GitHub Actions für Linux (zunächst Fokus), Windows und macOS kompilierbar.

---

## 3. Das gRPC Protokoll (Der System-Vertrag)
Die Kommunikation erfolgt über einen bidirektionalen gRPC-Stream, wahlweise mit TLS verschlüsselt. Der Client kann über `StreamConfig` dynamisch anpassen, welche Daten er empfangen möchte (z.B. Bilddaten abschalten, um nur Metadaten/Suchergebnisse zu testen).

```protobuf
syntax = "proto3";
package graphical_proxy;

service RemoteBrowser {
  rpc StreamSession(stream ClientEvent) returns (stream ServerUpdate);
}

// --- CLIENT -> SERVER ---
message ClientEvent {
  oneof event {
    NavigateRequest navigate = 1;
    MouseInput mouse = 2;
    KeyboardInput keyboard = 3;
    ScrollInput scroll = 4;
    ViewportResize resize = 5;
    StreamConfig config = 6;
    SearchRequest search = 7;
  }
}

message StreamConfig {
  bool enable_video = 1;
  bool enable_spatial_links = 2;
}

message SearchRequest { string query = 1; }

// --- SERVER -> CLIENT ---
message ServerUpdate {
  oneof update {
    VideoFrame frame = 1;
    SpatialMetadata spatial_data = 2;
    SearchResults search_results = 3;
    SystemStatus status = 4;
  }
}

message VideoFrame {
  bytes av1_data = 1;         // rav1e Output
  bool is_keyframe = 2;
  int32 viewport_x = 3;       // X-Position des aktuellen Bildausschnitts im Gesamtdokument
  int32 viewport_y = 4;       // Y-Position des aktuellen Bildausschnitts im Gesamtdokument
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
  int32 x = 4; int32 y = 5; int32 width = 6; int32 height = 7;
}

message SearchResults {
  string query = 1;
  repeated SearchMatch matches = 2;
}

message SearchMatch {
  int32 absolute_y = 1;       // Position im Dokument für "Jump to"
  string context_snippet = 2; // Z.B. "...der[Suchbegriff] ist hier..."
}
```

---

## 4. Server-Architektur (`cloud-render-srv`)

### 4.1. Browser-Steuerung & Konfiguration
Wie in `19_webprox` wird die Konfiguration via TOML und CLI-Argumenten (`clap`) gelöst.
*   `--headed` oder `headless = false` im TOML startet Chrome im sichtbaren Modus (für Debugging).
*   Das Starten von `chromiumoxide` nutzt eine Session-Verwaltung pro Client-Verbindung.

### 4.2. Die Raw-to-AV1 Pipeline
Um Artefakte zu vermeiden, liefert CDP (Chrome DevTools Protocol) keine verlustbehafteten JPEGs.
1.  **Capture:** `Page.startScreencast` wird auf `png` oder direktes Raw-Format (falls über Shared Memory oder Extensions möglich) konfiguriert.
2.  **Color Space:** Serverseitige Umwandlung von RGB/RGBA nach YUV420.
3.  **Encoding:** `rav1e` ist auf "Low Latency" / "Speed Level 10" konfiguriert. Es erzeugt Intra-Frames bei großen Sprüngen und extrem kleine Inter-Frames (Deltas) beim Scrollen.

### 4.3. DOM-Metadaten & Serverseitige Suche
*   **Links:** Bei Seiten-Ladevorgängen oder Layout-Änderungen injiziert der Server JavaScript, das `getBoundingClientRect()` + `window.scrollY` aller `<a>`-Tags liest und als `SpatialMetadata` an Rust zurückgibt.
*   **Suche:** Bei einem `SearchRequest` sucht injiziertes JS im DOM nach dem Text, extrahiert die umliegenden 50-100 Zeichen als Kontext und liefert die absolute `offsetTop`-Koordinate zurück. Die Suche iteriert über den kompletten DOM, unabhängig davon, welcher Bereich gerade im AV1-Viewport sichtbar ist.

---

## 5. Client-Architektur (`macroquad-client`)

Der Client trennt Netzwerk (`tokio`) und Rendering (`macroquad` Main-Thread) vollständig über Channels (`mpsc` / `watch`).

### 5.1. Der virtuelle Framebuffer & Scrolling
Die zentrale Innovation der UX.
1.  Der Client allokiert serverseitig gemeldete Dokument-Dimensionen (z.B. 1920x10000) als logischen Raum.
2.  Macroquad nutzt eine `RenderTexture` (oder verwaltet ein Gitter aus Texturkacheln), die den "virtuellen Framebuffer" darstellt.
3.  Empfängt der Client einen `VideoFrame`, wird dieser durch `rav1d` dekodiert und an den Koordinaten `(viewport_x, viewport_y)` in diesen virtuellen Framebuffer gezeichnet.
4.  **Lokales Scrollen:** Wenn der Nutzer scrollt, bewegt sich die `Camera2D` von Macroquad *sofort* über diesen virtuellen Framebuffer. Der Nutzer sieht sofort die "alte" Version der Seite, falls er nach oben scrollt.
5.  Der Client sendet das Scroll-Event an den Server. Wenig später trifft der aktualisierte VideoFrame für den neuen Bereich ein und überschreibt den Framebuffer an der neuen `viewport_y`-Position.

### 5.2. Skeleton UI
Befindet sich die lokale Kamera über einem Bereich des Framebuffers, der schwarz ist (noch kein Video empfangen), greift die `SpatialMetadata`.
Der Client iteriert über alle `LinkBox`es. Liegen diese im sichtbaren Bereich der Kamera, werden sie mit `macroquad::draw_rectangle_lines` als blaue Rahmen gezeichnet. Dies ermöglicht "Zero-Latency Hit-Testing": Der Nutzer kann auf einen Link im noch nicht geladenen Bildbereich klicken.

---

## 6. KI-Agent Testinfrastruktur & CLI-Design

Um die Software vollständig durch KI-Agenten oder CI/CD-Pipelines testbar zu machen, muss das GUI-System umgangen werden können. Beide Binaries erhalten umfangreiche CLI-Flags.

### 6.1. Client Headless / Dump-Modus
Der Client erhält einen `--test-mode`. In diesem Modus öffnet Miniquad kein Fenster. Der Client führt eine im CLI definierte Aktion aus, wartet auf das Server-Resultat und speichert es auf die Festplatte.

**Beispiel-Szenario für den Agenten (Integrationstest "Full-Text Search"):**
```bash
# Agent startet den Client im Test-Modus, ohne Video anzufordern
macroquad-client --test-mode \
                 --url "https://en.wikipedia.org/wiki/Rust_(programming_language)" \
                 --disable-video \
                 --search-query "memory safety" \
                 --dump-search "search_results.json" \
                 --timeout 5000
```
Der Agent kann danach die `search_results.json` einlesen, parsen und validieren, dass die `context_snippet` und `absolute_y` Werte korrekt vom Server extrahiert wurden.

**Beispiel-Szenario für den Agenten (Integrationstest "Render Frame"):**
```bash
# Agent testet die AV1 Pipeline
macroquad-client --test-mode \
                 --url "https://example.com" \
                 --dump-frame "frame_output.png" \
                 --timeout 5000
```
Der Agent liest `frame_output.png` ein oder nutzt ein CLI-Image-Tool (wie ImageMagick), um zu prüfen, ob Pixel geschrieben wurden.
Zusätzlich soll ein Tooling implementiert werden (oder externe Tools wie `maim` oder `import` genutzt werden), um Screenshots vom tatsächlichen X-Window des Clients zu erstellen. Diese "Final Render" Bilder werden gegen die vom Server gelieferten Referenz-Frames verglichen, um sicherzustellen, dass das Macroquad-Rendering (Scrolling-Offsets, Texturen) exakt arbeitet.

### 6.2. Server Debugging & Metriken
```bash
cloud-render-srv --config server.toml --headless=false --log-level=debug
```
Der Server sollte eine Status-Ausgabe besitzen, die misst, wie lange die PNG-Extraktion und das AV1-Encoding dauern. Diese Metriken können im Log landen, sodass der Agent Performance-Regressionen erkennen kann.

---

## 7. Phasen der Implementierung

*   **Phase 1: Foundation & gRPC**
    *   Setup des Cargo Workspaces.
    *   Definition der Protobufs und Generierung.
    *   Einrichten des Clients mit CLI-Flags und `--test-mode` Skelett.
*   **Phase 2: Chrome & Metadaten (No Video)**
    *   Anbindung von `chromiumoxide`.
    *   Implementierung der DOM-Injektion für Links und Volltextsuche.
    *   Agent testet den Fluss durch `--disable-video` und `--dump-search`.
*   **Phase 3: AV1 Video Pipeline**
    *   Server extrahiert PNGs, konvertiert zu YUV, sendet via `rav1e`.
    *   Client empfängt Bytes, dekodiert via `rav1d`, konvertiert zu RGB.
*   **Phase 4: Framebuffer & Macroquad Rendering**
    *   Verheiraten der dekodierten AV1-Frames mit `macroquad` Texturen.
    *   Implementierung der `Camera2D` für das lokale Scrollen über den virtuellen Framebuffer.
    *   Zeichnen des Skeleton-UIs (Blaue Link-Boxen).
*   **Phase 5: Optimierung & Verschlüsselung**
    *   Optionales TLS zuschalten.
    *   Feintuning der AV1-Bitraten und des Frame-Pacings.
