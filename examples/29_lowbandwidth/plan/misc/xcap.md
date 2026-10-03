# nashaofu/xcap

## GitHub & DeepWiki
- GitHub: https://github.com/nashaofu/xcap
- DeepWiki: https://deepwiki.com/nashaofu/xcap

## Kurze Einführung
XCap ist eine plattformübergreifende Bildschirmaufnahmebibliothek, die in Rust geschrieben ist. Sie unterstützt Linux (X11, Wayland), macOS und Windows. XCap ermöglicht sowohl Screenshots von Bildschirmen und Fenstern als auch die Videoaufzeichnung, wobei letzteres sich noch in der Entwicklung befindet.   Die Bibliothek richtet sich an Entwickler, die eine flexible und leistungsstarke Lösung für die Bildschirmaufnahme in ihren Anwendungen benötigen. 

## Die 3 wichtigsten (oder komplexesten) Algorithmen

### 1. Windows Graphics Capture (WGC) Backend
1.  **Name & Verortung im Code**: `wgc_capture` Funktion in `src/windows/wgc.rs` , `capture_monitor`  und `capture_window`  Funktionen in `src/windows/wgc.rs`.
2.  **Detaillierte technische Funktionsweise**: Das WGC-Backend nutzt die moderne `Windows.Graphics.Capture` WinRT API in Kombination mit Direct3D11 für GPU-beschleunigte Texturoperationen.  Es initialisiert ein `ID3D11DEVICE`  und `ID3D11DEVICE_CONTEXT`  und erstellt einen `Direct3D11CaptureFramePool` . Ein `FrameArrived`-Handler  verarbeitet eingehende Frames, indem er `get_next_frame`  aufruft, um Texturdaten zu extrahieren und in ein `Frame`-Objekt umzuwandeln.  Für die Videoaufzeichnung wird ein `WgcRuntime`  verwendet, das den Lebenszyklus der Aufnahmesitzung verwaltet und Frames über einen synchronen Kanal streamt. 
3.  **"Warum prägend"**: Dieses Backend bietet überlegene Leistung und Funktionen im Vergleich zum älteren GDI-Backend, insbesondere durch GPU-Beschleunigung und Unterstützung für Videoaufzeichnung.  Die Verwendung von `GraphicsCaptureItem`  und das Caching dieser Elemente verbessert die Leistung bei wiederholten Aufnahmen. 

### 2. Wayland Video Recording mit PipeWire
1.  **Name & Verortung im Code**: `WaylandVideoRecorder` Struktur und `pipewire_capturer` Methode in `src/linux/wayland_video_recorder.rs`.  
2.  **Detaillierte technische Funktionsweise**: Die Videoaufzeichnung unter Wayland erfolgt über PipeWire.  Die `WaylandVideoRecorder::new` Methode  initialisiert eine `ScreenCast`-Sitzung über das XDG Desktop Portal . Anschließend wird ein Stream über PipeWire eingerichtet, der Frames empfängt.  Die `pipewire_capturer` Methode  läuft in einem separaten Thread und verarbeitet die empfangenen Puffer. Sie konvertiert verschiedene Videoformate (z.B. RGB, RGBA, BGRx) in das standardisierte RGBA-Format  und sendet die fertigen `Frame`-Objekte über einen Kanal. 
3.  **"Warum prägend"**: Wayland ist aufgrund seiner Sicherheitsbeschränkungen komplex für die Bildschirmaufnahme.  Die Integration mit PipeWire und dem XDG Desktop Portal ermöglicht es XCap, diese Herausforderungen zu überwinden und eine funktionierende Videoaufzeichnung unter Wayland bereitzustellen.  Die Formatkonvertierung stellt sicher, dass die Frames in einem konsistenten Format für die weitere Verarbeitung vorliegen. 

### 3. macOS Video Recording mit AVFoundation
1.  **Name & Verortung im Code**: `ImplVideoRecorder` Struktur und die zugehörigen Methoden in `src/macos/impl_video_recorder.rs`.  Insbesondere die `capture` Methode der `DataOutputSampleBufferDelegateVars`  und die YUV-zu-RGB-Konvertierungsfunktionen wie `yuv_to_rgb_video_range`  und `nv12_to_rgba` .
2.  **Detaillierte technische Funktionsweise**: Die macOS-Videoaufzeichnung verwendet das AVFoundation-Framework.  Ein `AVCaptureSession`  wird mit einem `AVCaptureScreenInput`  (für den Bildschirm) und einem `AVCaptureVideoDataOutput`  konfiguriert. Ein `DataOutputSampleBufferDelegate`  implementiert das `AVCaptureVideoDataOutputSampleBufferDelegate`-Protokoll, um Frames zu empfangen.  Die `capture`-Methode  innerhalb des Delegates verarbeitet die `CMSampleBuffer`-Objekte, sperrt den Pixelpuffer  und konvertiert verschiedene Pixelformate (z.B. BGRA, ARGB, NV12, YUV422) in RGBA.  Die konvertierten Frames werden dann über einen synchronen Kanal gesendet. 
3.  **"Warum prägend"**: Die direkte Nutzung des AVFoundation-Frameworks ermöglicht eine effiziente und native Bildschirmaufnahme unter macOS.  Die umfangreichen Konvertierungsfunktionen für verschiedene Pixelformate  sind entscheidend, um die Kompatibilität mit den von macOS bereitgestellten Formaten zu gewährleisten und eine konsistente Ausgabe zu liefern. 

## Architektur & Zusammenspiel

```mermaid
graph TD
    subgraph "XCap Library (src/lib.rs)"
        A[Monitor] --> B{Platform Abstraction}
        C[Window] --> B
        D[VideoRecorder] --> B
    end

    subgraph "Platform Implementations"
        subgraph "Windows"
            B -- "Windows OS" --> E[src/windows/mod.rs]
            E --> F[WGC Backend (src/windows/wgc.rs)]
            E --> G[DXGI Video Recorder (src/windows/dxgi_video_recorder.rs)]
            E --> H[WGC Video Recorder (src/windows/wgc_video_recorder.rs)]
            F -- "Captures Frames" --> I[Frame]
            G -- "Records Video" --> I
            H -- "Records Video" --> I
        end

        subgraph "Linux"
            B -- "Linux OS" --> J[src/linux/mod.rs]
            J --> K[X11 Capture (src/linux/xorg_capture.rs)]
            J --> L[Wayland Capture (src/linux/wayland_capture.rs)]
            J --> M[Xorg Video Recorder (src/linux/xorg_video_recorder.rs)]
            J --> N[Wayland Video Recorder (src/linux/wayland_video_recorder.rs)]
            K -- "Captures Frames" --> I
            L -- "Captures Frames" --> I
            M -- "Records Video" --> I
            N -- "Records Video" --> I
        end

        subgraph "macOS"
            B -- "macOS" --> O[src/macos/mod.rs]
            O --> P[Core Graphics Capture (src/macos/capture.rs)]
            O --> Q[AVFoundation Video Recorder (src/macos/impl_video_recorder.rs)]
            P -- "Captures Frames" --> I
            Q -- "Records Video" --> I
        end
    end

    subgraph "Core Data Structures"
        I[Frame]
    end

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style C fill:#f9f,stroke:#333,stroke-width:2px
    style D fill:#f9f,stroke:#333,stroke-width:2px
    style I fill:#ccf,stroke:#333,stroke-width:2px
```
Die Bibliothek `xcap`  bietet eine plattformübergreifende Abstraktionsschicht für die Bildschirmaufnahme.  Die Kernstrukturen `Monitor`  und `Window`  kapseln plattformspezifische Implementierungen, die über das `platform`-Modul  dynamisch geladen werden. Jede Plattform (Windows, Linux, macOS) verfügt über eigene Module, die die spezifischen APIs und Techniken für die Bildschirm- und Fensteraufnahme sowie die Videoaufzeichnung implementieren.  Beispielsweise verwendet Windows das WGC-Backend  oder DXGI , Linux nutzt X11 (XCB)  oder Wayland (PipeWire) , und macOS greift auf Core Graphics  und AVFoundation  zurück. Die erfassten Daten werden in `Frame`-Objekten  standardisiert, die Rohbilddaten und Metadaten enthalten. 

## Notes
Eine bemerkenswerte Strategie ist die Fallback-Logik für die Wayland-Aufnahme unter Linux. <cite repo="nashaofu/xcap" path="Glossary" start="8

Wiki pages you might want to explore:
- [WGC Backend (nashaofu/xcap)](/wiki/nashaofu/xcap#4.1.2)
- [Glossary (nashaofu/xcap)](/wiki/nashaofu/xcap#7)

View this search on DeepWiki: https://deepwiki.com/search/zielrepository-nashaofuxcap-er_9975cab7-022e-40f5-915e-85186e99a13b
