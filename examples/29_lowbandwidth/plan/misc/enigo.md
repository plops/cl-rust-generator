# enigo-rs/enigo

## GitHub & DeepWiki
- GitHub: https://github.com/enigo-rs/enigo
- DeepWiki: https://deepwiki.com/enigo-rs/enigo

## Kurze Einführung
Enigo ist eine plattformübergreifende Rust-Bibliothek zur Simulation von Maus- und Tastaturereignissen . Sie ermöglicht die Automatisierung von UI-Interaktionen auf Linux (X11, Wayland, libei, xdg_desktop), macOS und Windows . Die Bibliothek richtet sich an Entwickler, die UI-Tests durchführen, Fernsteuerungsanwendungen erstellen oder Aufgaben in Benutzeroberflächen automatisieren möchten, die keine öffentliche API bieten .

## Die 3 wichtigsten (oder komplexesten) Algorithmen

### 1. Plattformübergreifende Eingabesimulation mit Backend-Abstraktion
*   **Name & Verortung im Code**: Die Kernlogik befindet sich im `Enigo`-Struktur  und den plattformspezifischen Modulen wie `src/linux/mod.rs` , `src/macos/macos_impl.rs`  und `src/win/win_impl.rs` . Die `Mouse`- und `Keyboard`-Traits definieren die Schnittstelle .
*   **Detaillierte technische Funktionsweise**: Enigo verwendet eine Abstraktionsschicht, um plattformspezifische Eingabe-APIs zu kapseln . Die `Enigo`-Struktur enthält optionale Verbindungen zu verschiedenen Backends (z.B. `wayland`, `x11`, `libei`, `xdg_desktop` für Linux) . Bei der Initialisierung versucht `Enigo::new` , alle aktivierten Backends zu initialisieren und verwendet das erste erfolgreiche . Wenn eine Eingabeaktion (z.B. `button`, `move_mouse`, `key`) aufgerufen wird, iteriert Enigo durch die verfügbaren Backends und versucht, die Aktion über das erste Backend auszuführen, das die Simulation erfolgreich durchführt .
*   **Warum prägend**: Dieser Ansatz ermöglicht es Enigo, eine konsistente API über verschiedene Betriebssysteme und Linux-Display-Server-Protokolle hinweg anzubieten . Die modulare Backend-Auswahl über Feature-Flags in `Cargo.toml`  minimiert die Binärgröße und Abhängigkeiten, da nur die benötigten Backends kompiliert werden . Die Fallback-Logik bei der Ausführung erhöht die Robustheit, indem sie versucht, Eingaben über alternative Protokolle zu simulieren, falls ein bevorzugtes Backend fehlschlägt .

### 2. Behandlung von Tastatur- und Mausereignissen auf macOS
*   **Name & Verortung im Code**: Die Implementierung befindet sich in `src/macos/macos_impl.rs` , insbesondere in den Methoden der `Enigo`-Struktur für `Keyboard`  und `Mouse` .
*   **Detaillierte technische Funktionsweise**: Auf macOS nutzt Enigo die Core Graphics Event-APIs, um Eingabeereignisse zu simulieren . Für die Texteingabe (`fast_text`) werden Unicode-Strings in `CGEvent`s umgewandelt und über `CGEvent::keyboard_set_unicode_string` gesendet . Eine Besonderheit ist die Handhabung von `CGEventKeyboardSetUnicodeString`, die Strings auf 20 Zeichen kürzt, was durch eine Chunking-Logik umgangen wird . Zudem werden führende Steuerzeichen wie Tabs und Newlines speziell behandelt, da `set_string` bei diesen Zeichen fehlschlägt . Mausbewegungen und Klicks verwenden `CGEvent::new_mouse_event` und `CGEvent::post` . Die `smooth_scroll` Funktion nutzt `CGScrollEventUnit::Pixel` für pixelbasiertes Scrollen .
*   **Warum prägend**: Diese Implementierung ist entscheidend für die präzise und zuverlässige Eingabesimulation auf macOS. Die Workarounds für API-Einschränkungen (z.B. String-Kürzung, Steuerzeichen) zeigen die Notwendigkeit einer tiefen Kenntnis der plattformspezifischen APIs und deren Eigenheiten, um eine robuste Funktionalität zu gewährleisten . Die `platform_specific`-Funktion für `smooth_scroll`  bietet eine verbesserte Benutzererfahrung, die über die Standardfunktionen hinausgeht .

### 3. Windows Mausbewegungs- und Beschleunigungssteuerung
*   **Name & Verortung im Code**: Die Logik ist in `src/win/win_impl.rs`  innerhalb der `move_mouse`-Methode der `Enigo`-Struktur implementiert.
*   **Detaillierte technische Funktionsweise**: Auf Windows kann die Mausbewegung entweder absolut oder relativ erfolgen . Ein kritischer Aspekt ist die `windows_subject_to_mouse_speed_and_acceleration_level`-Einstellung . Wenn diese auf `true` gesetzt ist, unterliegen relative Mausbewegungen den Windows-Systemeinstellungen für Mausgeschwindigkeit und -beschleunigung, was zu unvorhersehbaren Bewegungen führen kann (bis zu vierfache Distanz) . Um präzise, vorhersagbare Bewegungen zu gewährleisten, setzt Enigo diese Einstellung standardmäßig auf `false` . In diesem Fall werden relative Bewegungen intern in absolute Zielkoordinaten umgerechnet und dann als absolute Bewegung gesendet, wodurch die Systembeschleunigung umgangen wird .
*   **Warum prägend**: Diese Implementierung ist entscheidend für die Zuverlässigkeit der Maussteuerung auf Windows. Die Möglichkeit, die Systembeschleunigung zu umgehen, ist für Automatisierungs- und Testzwecke unerlässlich, da sie präzise und reproduzierbare Mausbewegungen ermöglicht . Die `set_dpi_awareness()`-Funktion  ist ebenfalls wichtig, um die korrekte Skalierung von Koordinaten auf High-DPI-Displays sicherzustellen .

## Architektur & Zusammenspiel

```mermaid
graph TD
    subgraph "Enigo Crate"
        A[Enigo::new()] --> B{Platform-Specific Enigo Impl}
        B --> C[Mouse Trait]
        B --> D[Keyboard Trait]
    end

    subgraph "Linux Backends (src/linux/mod.rs)"
        B_Linux[Linux Enigo] --> B_Linux_Init[Enigo::new() logic]
        B_Linux_Init --> F{Try Backends}
        F --> F1[xdg_desktop::Con]
        F --> F2[wayland::Con]
        F --> F3[x11::Con (x11rb/xdo)]
        F --> F4[libei::Con]
        F1 -- Success --> G[Use xdg_desktop]
        F2 -- Success --> H[Use Wayland]
        F3 -- Success --> I[Use X11]
        F4 -- Success --> J[Use libei]
        C_Linux[Mouse Trait Impl] --> F
        D_Linux[Keyboard Trait Impl] --> F
    end

    subgraph "macOS Backend (src/macos/macos_impl.rs)"
        B_macOS[macOS Enigo] --> K[Core Graphics API]
        C_macOS[Mouse Trait Impl] --> K
        D_macOS[Keyboard Trait Impl] --> K
        K --> K1[CGEvent::new_keyboard_event]
        K --> K2[CGEvent::new_mouse_event]
        K --> K3[CGEvent::new_scroll_event]
        K3 -- Pixel-based --> L[smooth_scroll (platform_specific)]
    end

    subgraph "Windows Backend (src/win/win_impl.rs)"
        B_Windows[Windows Enigo] --> M[Win32 API]
        C_Windows[Mouse Trait Impl] --> M
        D_Windows[Keyboard Trait Impl] --> M
        M --> M1[SendInput()]
        M --> M2[mouse_event()]
        M --> M3[keybd_event()]
        M2 -- Coordinate Conversion --> N[Mouse Speed/Acceleration Logic]
        M -- DPI Awareness --> O[set_dpi_awareness()]
    end

    style B_Linux fill:#f9f,stroke:#333,stroke-width:2px
    style B_macOS fill:#f9f,stroke:#333,stroke-width:2px
    style B_Windows fill:#f9f,stroke:#333,stroke-width:2px
    style F1 fill:#ccf,stroke:#333,stroke-width:2px
    style F2 fill:#ccf,stroke:#333,stroke-width:2px
    style F3 fill:#ccf,stroke:#333,stroke-width:2px
    style F4 fill:#ccf,stroke:#333,stroke-width:2px
    style K fill:#ccf,stroke:#333,stroke-width:2px
    style M fill:#ccf,stroke:#333,stroke-width:2px
```
Die `Enigo`-Struktur  dient als zentrale Schnittstelle für die Eingabesimulation. Sie implementiert die `Mouse`-  und `Keyboard`-Traits , die die plattformübergreifenden Funktionen definieren.

Auf **Linux**  versucht `Enigo::new` , Verbindungen zu mehreren Backends herzustellen, darunter `xdg_desktop`, `wayland`, `x11` (entweder `x11rb` oder `xdo`) und `libei` . Die Reihenfolge, in der diese Backends ausprobiert werden, kann variieren . Wenn eine Eingabeaktion angefordert wird (z.B. `button` , `move_mouse` , `key` ), versucht Enigo, die Aktion über jedes verfügbare Backend auszuführen, bis eines erfolgreich ist .

Auf **macOS**  werden Eingabeereignisse direkt über die Core Graphics API simuliert . Funktionen wie `smooth_scroll`  sind plattformspezifisch und nutzen pixelbasiertes Scrollen <cite repo="enigo-rs/enigo" path

Wiki pages you might want to explore:
- [Feature Flags and Compilation (enigo-rs/enigo)](/wiki/enigo-rs/enigo#5)
- [Platform-Specific Features (enigo-rs/enigo)](/wiki/enigo-rs/enigo#6.2)

View this search on DeepWiki: https://deepwiki.com/search/zielrepository-enigorsenigo-er_e6136105-f170-489e-a2ff-872a93a5c0a5
