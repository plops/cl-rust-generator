# image-rs/image

## GitHub & DeepWiki
- GitHub: https://github.com/image-rs/image
- DeepWiki: https://deepwiki.com/image-rs/image

## Kurze Einführung
`image-rs/image` ist eine Rust-Bibliothek, die grundlegende Bildverarbeitungsfunktionen und Methoden zum Konvertieren von und in verschiedene Bildformate bereitstellt. Sie bietet eine einheitliche Schnittstelle für Bildkodierungen und generische Puffer für deren Inhalte, wobei der Fokus auf einer kleinen, stabilen Menge gängiger Operationen liegt. Die Bibliothek richtet sich an Entwickler, die eine performante und sichere Bildverarbeitung in Rust benötigen, und unterstützt eine Vielzahl von Bildformaten durch Feature-Flags.  

## Die 3 wichtigsten (oder komplexesten) Algorithmen

### 1. Bilddekodierung mit `ImageDecoder`
Die Bilddekodierung wird durch das Trait `ImageDecoder` im Modul `src/io/decoder.rs`  abstrahiert. Konkrete Implementierungen für verschiedene Formate wie HDR finden sich beispielsweise in `src/codecs/hdr/decoder.rs` .

Die `ImageDecoder`-Implementierung für HDR-Bilder, `HdrDecoder`, liest die Bilddaten zeilenweise ein. Dabei wird zwischen zwei RLE-Kompressionsmethoden (Run-Length Encoding) unterschieden: einer neueren pro-Komponenten-RLE-Methode und einer älteren RLE-Methode. Die Funktion `read_scanline`  entscheidet basierend auf den ersten vier Bytes der Scanline, welche Dekodierungsmethode angewendet wird. Die dekodierten 8-Bit-RGBE-Pixel (`Rgbe8Pixel`) werden anschließend in 32-Bit-Float-RGB-Pixel (`Rgb<f32>`) umgewandelt. 

Dieser Algorithmus ist prägend, da er die Grundlage für das Laden und Interpretieren von Bilddaten aus verschiedenen Dateiformaten bildet. Die Abstraktion durch das `ImageDecoder`-Trait ermöglicht es, neue Formate einfach zu integrieren und gleichzeitig eine konsistente Schnittstelle für die Bildverarbeitung bereitzustellen. Die effiziente Handhabung von Kompression, wie im HDR-Decoder, ist entscheidend für die Performance beim Laden großer Bilddateien. 

### 2. Bildskalierung mit `resize_impl`
Die Bildskalierung wird durch die Funktion `resize_impl` im Modul `src/imageops/resize.rs`  implementiert. Diese Funktion delegiert die eigentliche Skalierungslogik an die externe `pic-scale-safe`-Kiste.

`resize_impl` nimmt ein `DynamicImage` , Zielbreite und -höhe sowie einen `FilterType`  entgegen. Bevor die Skalierung durchgeführt wird, prüft die Funktion, ob eine Alpha-Vormultiplikation notwendig ist, um Farbausblutungen bei transparenten Pixeln zu vermeiden, es sei denn, der `FilterType` ist `Nearest` . Anschließend wird je nach `DynamicImage`-Variante (z.B. `ImageLuma8`, `ImageRgb8`, `ImageRgba32F`) die entsprechende Skalierungsfunktion von `pic-scale-safe` aufgerufen, wie `resize_plane8` oder `resize_rgba_f32` . Nach der Skalierung wird bei Bedarf die Alpha-Vormultiplikation rückgängig gemacht. 

Dieser Algorithmus ist prägend, da er eine performante und qualitativ hochwertige Bildskalierung ermöglicht, die für viele Bildbearbeitungsanwendungen unerlässlich ist. Die Verwendung einer externen, optimierten Bibliothek wie `pic-scale-safe`  und die Berücksichtigung von Alpha-Kanälen für korrekte Farbmischung sind entscheidend für die Bildqualität und die Effizienz der Operation.

### 3. Dynamische Bildrepräsentation mit `DynamicImage`
Die `DynamicImage`-Enumeration, definiert in `src/images/dynimage.rs` , ist eine zentrale Datenstruktur, die verschiedene statisch typisierte `ImageBuffer`-Formate zur Laufzeit kapselt.

`DynamicImage` ist ein Enum, das Varianten für verschiedene Pixeltiefen (8-Bit, 16-Bit, 32-Bit Float) und Farbtypen (Luma, LumaA, Rgb, Rgba) bereitstellt.  Es implementiert die Traits `GenericImageView` und `GenericImage` , wodurch es eine einheitliche Schnittstelle für den Zugriff und die Manipulation von Pixeln bietet, unabhängig vom zugrunde liegenden konkreten Bildtyp. Operationen wie `dimensions()` und `get_pixel()` werden über ein internes Makro (`dynamic_map!`) an die spezifische `ImageBuffer`-Variante delegiert. 

Diese Datenstruktur ist prägend, da sie die Flexibilität der Bibliothek erheblich steigert. Sie ermöglicht es, Bilddaten zu verarbeiten, deren exaktes Format zur Kompilierzeit unbekannt ist, was für Anwendungen, die mit einer Vielzahl von Bildformaten arbeiten müssen, unerlässlich ist. Die Kapselung verschiedener `ImageBuffer`-Typen unter einer einzigen Schnittstelle vereinfacht die API und reduziert die Notwendigkeit von generischem Code oder Box-Typen für den Benutzer. 

## Architektur & Zusammenspiel

```mermaid
graph TD
    A[Raw Bytes] --> B{ImageReader};
    B --> C{guess_format()};
    C --> D[ImageFormat];
    D --> E{ImageDecoder Selection};
    E --> F(ImageDecoder Trait);
    F --> G{prepare_image()};
    G --> H{read_image()};
    H --> I[DynamicImage Enum];
    I --> J{ImageBuffer<P, Vec<S>>};
    J --> K[Image Processing Functions (imageops)];
    K --> L{resize_impl};
    K --> M{gaussian_blur_dyn_image};
    I -- "put_pixel" --> J;
    J -- "get_pixel" --> I;
    I -- "save" --> N(ImageEncoder Trait);
    N --> O[Encoded Bytes];

    subgraph "Decoding Pipeline"
        B -- "open()" --> I;
    end

    subgraph "Image Representation"
        I -- "ImageLuma8" --> J;
        I -- "ImageRgb8" --> J;
        I -- "ImageRgba32F" --> J;
    end

    subgraph "Image Operations"
        K -- "blur" --> M;
        K -- "resize" --> L;
    end
```
Die Architektur der `image`-Kiste basiert auf einem modularen Design, das die Trennung von Verantwortlichkeiten für Dekodierung, Bildrepräsentation und Bildverarbeitung fördert.

Der Prozess beginnt mit `Raw Bytes` , die von einem `ImageReader`  gelesen werden. Der `ImageReader` kann das `ImageFormat`  der Eingabedaten erraten  und wählt basierend darauf eine spezifische `ImageDecoder`-Implementierung  aus. Das `ImageDecoder`-Trait  definiert die Schnittstelle für die Dekodierung, mit Kernmethoden wie `prepare_image()`  zur Vorbereitung der Bildlayout-Informationen und `read_image()`  zum Lesen der Pixeldaten in einen Puffer.

Die dekodierten Pixeldaten werden in einer `DynamicImage`-Enumeration  gespeichert. `DynamicImage` ist ein Wrapper für verschiedene `ImageBuffer`-Typen , die die eigentlichen Bilddaten in einem `Vec<Subpixel>`  halten. Diese Struktur ermöglicht die Laufzeit-Polymorphie und vereinfacht die Handhabung unterschiedlicher Bildformate und Farbtiefen.

Bildverarbeitungsfunktionen sind im `imageops`-Modul  zusammengefasst. Funktionen wie `resize`  (implementiert durch `resize_impl` ) und `blur`  (implementiert durch `gaussian_blur_dyn_image` ) operieren auf `DynamicImage`-Instanzen. Die `GenericImageView` und `GenericImage` Traits  bieten eine gemeinsame Schnittstelle für diese Operationen.

Schließlich können `DynamicImage`-Instanzen mit Hilfe des `ImageEncoder`-Traits  in verschiedene Formate zurückgeschrieben werden, was zu `Encoded Bytes`  führt.

## Notes
Die Bibliothek nutzt Feature-Flags in `Cargo.toml` , um die Unterstützung für verschiedene Bildformate zu steuern. Dies ermöglicht es Benutzern, nur die benötigten Codecs zu kompilieren, was die Abhängigkeitsbäume reduziert und die Kompilierzeiten verkürzt.  Das `rayon`-Feature  ermöglicht Multithreading in einigen Abhängigkeiten, was die Performance bei rechenintensiven Operationen wie der Bildskalierung verbessern kann.  Die `imageops`-Modul  enthält eine Vielzahl weiterer Bildverarbeitungsfunktionen, darunter affine Transformationen  und Farboperationen .

Wiki pages you might want to explore:
- [Glossary (image-rs/image)](/wiki/image-rs/image#9)

View this search on DeepWiki: https://deepwiki.com/search/zielrepository-imagersimage-er_44b7a8b3-d204-4549-823f-09d61ce8c9d3
