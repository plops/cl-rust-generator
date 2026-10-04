# dm6718/RITSAR

## GitHub & DeepWiki
- GitHub: https://github.com/dm6718/RITSAR
- DeepWiki: https://deepwiki.com/dm6718/RITSAR

## Kurze Einführung
RITSAR (Rochester Institute of Technology Synthetic Aperture Radar) ist eine Python-Toolbox zur Verarbeitung von Synthetic Aperture Radar (SAR)-Daten. Sie ermöglicht die Simulation von Phasenhistorien, die Anwendung verschiedener Bildgebungsalgorithmen und bietet Dienstprogramme für die Datenverarbeitung und -visualisierung. Die Toolbox ist flexibel konzipiert, um sowohl simulierte als auch reale SAR-Datensätze verarbeiten zu können.  

## Die 3 wichtigsten (oder komplexesten) Algorithmen

### 1. Polar Format Algorithm (PFA)
1.  **Name & Verortung im Code**: `polar_format` Funktion im Modul `ritsar/imgTools.py` .
2.  **Detaillierte technische Funktionsweise**: Der PFA verarbeitet Phasenhistoriedaten, die auf einer zweidimensionalen Oberfläche im k-Raum gesammelt wurden. Für jeden Puls wird ein Streifen dieser Oberfläche erfasst. Der Algorithmus projiziert jeden Streifen auf die (ku,kv)-Ebene, die durch den Normalenvektor im `img_plane`-Wörterbuch definiert ist.  Dies führt zu ungleichmäßig verteilten Daten in (ku,kv), die anschließend auf ein gleichmäßig verteiltes (ku,kv)-Gitter interpoliert werden.  Die Interpolation erfolgt zuerst in radialer Richtung und dann in der Along-Track-Richtung.  Nach der Interpolation wird eine 2D-Inverse-FFT angewendet, um das SAR-Bild zu rekonstruieren. 
3.  **Warum prägend**: Der PFA ist ein klassischer und effizienter Bildgebungsalgorithmus für SAR, insbesondere für Spotlight-SAR-Modi.  Seine Effizienz beruht auf der Nutzung der Fast Fourier Transformation (FFT) nach der Interpolation, was zu einer besseren Performance im Vergleich zu zeitaufwändigeren Algorithmen wie der Backprojection führen kann. 

### 2. Omega-K Algorithm
1.  **Name & Verortung im Code**: `omega_k` Funktion im Modul `ritsar/imgTools.py` .
2.  **Detaillierte technische Funktionsweise**: Dieser Algorithmus basiert auf der Formulierung im Carrera-Text und setzt voraus, dass die Phasenhistorie auf einen festen Referenzpunkt demoduliert wurde.  Der erste Schritt ist eine 1D-FFT entlang des Azimuts.  Anschließend wird ein angepasster Filter angewendet, um die Reichweitenkrümmung aller Streuer mit minimaler Reichweite `R_s` zu kompensieren.  Die Daten werden dann auf ein neues Gitter abgebildet, um die Reichweitenkrümmung für andere Streuer zu korrigieren, was als Stolt-Interpolation bekannt ist. 
3.  **Warum prägend**: Der Omega-K-Algorithmus ist bekannt für seine hohe Genauigkeit und Effizienz bei der Verarbeitung von SAR-Daten mit geraden Flugbahnen.  Er korrigiert die Reichweitenkrümmung effektiv und ist besonders leistungsfähig für große Datensätze, da er FFT-Operationen nutzt. 

### 3. Fast Factorized Backprojection (FFBP)
1.  **Name & Verortung im Code**: `FFBP` Funktion im Modul `ritsar/imgTools.py` . Eine Variante mit Multiprocessing ist `FFBPmp` .
2.  **Detaillierte technische Funktionsweise**: Der FFBP-Algorithmus ist eine optimierte Version des Backprojection-Algorithmus, der die Bildgebung durch Faktorisierung auf verschiedenen Rekursionsebenen beschleunigt.  Jede Faktorisierungsebene wird durch den Digital Spotlight (DS)-Algorithmus (`DS` Funktion)  gehandhabt, der die Phasenhistorie und die Plattformparameter für Unterbilder anpasst.  Der Algorithmus zerlegt das Gesamtbild in kleinere Unterbilder, die dann separat verarbeitet und am Ende wieder zusammengesetzt werden.  Die `FFBPmp`-Funktion nutzt Multiprocessing, um die Verarbeitung dieser Unterbilder parallel durchzuführen. 
3.  **Warum prägend**: FFBP ist entscheidend für die Skalierbarkeit der Backprojection-Methode, die sonst sehr rechenintensiv sein kann.  Durch die Faktorisierung und optionale Parallelisierung (`FFBPmp`)  ermöglicht FFBP die Verarbeitung großer SAR-Datensätze und die Erzeugung hochauflösender Bilder in einer praktikablen Zeit. 

## Architektur & Zusammenspiel

```mermaid
graph TD
    A[Phasenhistorie (phs)] --> B{phsTools.simulate_phs};
    B --> C[phsTools.RVP_correct];
    C --> D[phsTools.phs_to_const_ref];
    D --> E[phsTools.reMoComp];
    E --> F{imgTools.polar_format};
    E --> G{imgTools.omega_k};
    E --> H{imgTools.backprojection};
    H --> I{imgTools.DSBP};
    I --> J{imgTools.FFBP};
    J --> K{imgTools.FFBPmp};
    F --> L[SAR Bild];
    G --> L;
    I --> L;
    J --> L;
    K --> L;

    subgraph phsTools Module
        B; C; D; E;
    end

    subgraph imgTools Module
        F; G; H; I; J; K;
    end

    classDef default fill:#fff,stroke:#333,stroke-width:2px;
    classDef module fill:#add8e6,stroke:#333,stroke-width:2px;
    class phsTools Module, imgTools Module module;
```

Die Architektur von RITSAR ist modular aufgebaut und gliedert sich hauptsächlich in die Module `phsTools` und `imgTools`.  

1.  **`phsTools`**: Dieses Modul ist für die Vorverarbeitung der Phasenhistorie zuständig. 
    *   `simulate_phs`: Simuliert eine Phasenhistorie basierend auf Zielorten und Amplituden. 
    *   `RVP_correct`: Korrigiert die Residual Video Phase (RVP) in der Phasenhistorie. 
    *   `phs_to_const_ref`: Konvertiert eine Phasenhistorie, die mit einem pulsabhängigen Bereich demoduliert wurde, in eine, die mit einer festen Referenz demoduliert wurde. 
    *   `reMoComp`: Führt eine Re-Motion-Kompensation durch, um den effektiven Szenenmittelpunkt der Phasenhistorie zu verschieben. 

2.  **`imgTools`**: Dieses Modul enthält die verschiedenen Bildgebungsalgorithmen. 
    *   `polar_format`: Implementiert den Polar Format Algorithm. 
    *   `omega_k`: Implementiert den Omega-K-Algorithmus. 
    *   `backprojection`: Implementiert den Backprojection-Algorithmus. 
    *   `DSBP` (Digital Spotlight Backprojection): Eine Variante des Backprojection-Algorithmus, die Digital Spotlight verwendet. 
    *   `FFBP` (Fast Factorized Backprojection): Eine optimierte Version des Backprojection-Algorithmus, die Faktorisierung nutzt. 
    *   `FFBPmp`: Die Multiprocessing-Variante des FFBP-Algorithmus. 

Das Zusammenspiel beginnt typischerweise mit der Erzeugung oder dem Einlesen einer Phasenhistorie, die dann durch Funktionen in `phsTools` korrigiert und für die Bildgebung vorbereitet wird.  Anschließend wird die vorbereitete Phasenhistorie an einen der Bildgebungsalgorithmen in `imgTools` übergeben, um das endgültige SAR-Bild zu erzeugen. 

## Notes
Die Toolbox enthält auch eine separate Suite von konvertierten Soumekh Spotlight-Algorithmen (`converted_Soumekh_spotlight_algorithm/`), die ursprünglich in MATLAB entwickelt und nach Python portiert wurden.  Diese Implementierungen dienen als Referenz für die räumliche Frequenzinterpolation, sind jedoch nicht in die Haupt-API von RITSAR integriert und können bei hohen Abtastanforderungen zu Speicherfehlern führen.   


Wiki pages you might want to explore:
- [Overview (dm6718/RITSAR)](/wiki/dm6718/RITSAR#1)
- [Corrections and Motion Compensation (dm6718/RITSAR)](/wiki/dm6718/RITSAR#3.2)
- [Converted Soumekh Spotlight Algorithms (dm6718/RITSAR)](/wiki/dm6718/RITSAR#7.2)

View this search on DeepWiki: https://deepwiki.com/search/zielrepository-dm6718ritsar-er_d8cf1a57-f041-4aaf-ba30-078f6e920c03

---

# sirbastiano/SSFocus

## GitHub & DeepWiki
- GitHub: https://github.com/sirbastiano/SSFocus
- DeepWiki: https://deepwiki.com/sirbastiano/SSFocus

## Kurze Einführung
SSFocus ist eine Python-Bibliothek, die für die Dekodierung und Fokussierung von Rohdaten von Sentinel-1 Synthetic Aperture Radar (SAR)-Bildern entwickelt wurde, wobei der Range-Doppler-Algorithmus zum Einsatz kommt. Sie zielt darauf ab, Fernerkundungsexperten und Forschern ein Werkzeug zur Verfügung zu stellen, um SAR-Bilder für verschiedene Anwendungen zu verarbeiten und für maschinelles Lernen vorzubereiten. Die Bibliothek unterstützt sowohl NumPy- als auch PyTorch-Backends für die Berechnungen, was CPU- und GPU-Beschleunigung ermöglicht.

## Die 3 wichtigsten (oder komplexesten) Algorithmen

### 1. Range-Doppler-Fokussierung
Die Range-Doppler-Fokussierung ist der zentrale Algorithmus in SSFocus, der Roh-SAR-Daten in fokussierte Bilder umwandelt. Dieser Prozess ist in der Datei `SARProcessor/focus.py` implementiert und wird auch in der älteren Implementierung `SARProcessor/focus_old.py` detailliert beschrieben.

#### Detaillierte technische Funktionsweise
Der Algorithmus umfasst mehrere Schritte, die hauptsächlich im Frequenzbereich durchgeführt werden:
1.  **2D Fast Fourier Transformation (FFT)**: Die Rohdaten werden mittels 2D-FFT in den Frequenzbereich (Range- und Azimut-Frequenz) transformiert. Dies geschieht durch die Funktion `fft2D` .
2.  **Range-Filterung**: Ein angepasster Filter wird im Range-Frequenzbereich angewendet, um den Puls zu komprimieren. Die Funktion `get_range_filter`  berechnet diesen Filter unter Verwendung von Metadaten wie `Range Decimation`, `PRI`, `SWST` und Ephemeridendaten.
3.  **Range Cell Migration Correction (RCMC)**: Dieser Schritt korrigiert die Migration der Echos über verschiedene Range-Zellen hinweg, die durch die Bewegung der SAR-Plattform verursacht wird. Der RCMC-Filter wird von `get_RDMC`  berechnet und angewendet.
4.  **Azimut-Kompression**: Ein Azimut-Filter wird angewendet, um das Signal entlang der Azimut-Dimension zu komprimieren und das Ziel zu fokussieren. Die Funktion `get_azimuth_filter`  ist dafür zuständig.
5.  **Inverse 2D FFT**: Die Daten werden zurück in den räumlichen Bereich transformiert, um das fokussierte SAR-Bild zu erzeugen.

#### Warum prägend
Dieser Algorithmus ist prägend, da er die Kernfunktionalität von SSFocus darstellt: die Umwandlung von rohen, unbrauchbaren SAR-Daten in kohärente, fokussierte Bilder. Die Unterstützung von NumPy und PyTorch in `fft2D`  ermöglicht eine flexible Nutzung von CPU- oder GPU-Ressourcen, was entscheidend für die Performance und Skalierbarkeit bei der Verarbeitung großer Datensätze ist. Die mathematische Präzision der Filterberechnungen ist entscheidend für die Qualität der Ausgabe.

### 2. Level-0 Dekodierung und Datenaufteilung
Die Dekodierung von Level-0-Daten ist der erste Schritt in der SAR-Verarbeitungspipeline und wird hauptsächlich in `SARProcessor/decode.py` gehandhabt.

#### Detaillierte technische Funktionsweise
Die Funktion `sentinel1decoder.Level0File`  liest das proprietäre Sentinel-1-Datenformat. Dabei werden Ephemeridendaten, Burst-Metadaten und Radardaten extrahiert . Um Speicherüberlastungen zu vermeiden, werden die Radardaten in kleinere Blöcke (Chunks) aufgeteilt und als Pickle-Dateien gespeichert, was durch die Funktion `split_radar_data`  realisiert wird.

#### Warum prägend
Dieser Algorithmus ist prägend, da er die Grundlage für alle weiteren Verarbeitungsschritte bildet. Die effiziente Dekodierung und Aufteilung der Rohdaten in handhabbare Chunks ist entscheidend für die Speicherverwaltung und die Skalierbarkeit der gesamten Pipeline, insbesondere bei sehr großen SAR-Datensätzen. Ohne diesen Schritt könnten die nachfolgenden Fokussierungsalgorithmen nicht effektiv ausgeführt werden.

### 3. PhiNet Deep Learning Modell
Das `PhiNet`-Modell ist ein Deep-Learning-Modell, das in `Models/PhiNet.py` implementiert ist und für die verbesserte Bildfokussierung und Rauschunterdrückung eingesetzt wird.

#### Detaillierte technische Funktionsweise
`PhiNet`  ist eine Multi-Branch-Architektur, die einen `SpectrumAttentionBlock`  verwendet. Dieser Block normalisiert das Eingangsspektrum. Das Modell besteht aus zwei Hauptzweigen: einem Pooling-Zweig und einem Strided Convolution-Zweig. Beide Zweige verarbeiten die Daten parallel und ihre Ausgaben werden addiert, um das endgültige Ergebnis zu liefern .

#### Warum prägend
`PhiNet` ist prägend, da es die Integration von Deep Learning in die SAR-Bildfokussierung ermöglicht. Dies kann zu einer verbesserten Bildqualität, Rauschunterdrückung und möglicherweise zu einer effizienteren Fokussierung führen, die über traditionelle Signalverarbeitungsansätze hinausgeht. Die modulare Architektur mit dem `SpectrumAttentionBlock` und den parallelen Verarbeitungszweigen ermöglicht es, komplexe Muster in den SAR-Daten zu lernen und die Fokussierung zu optimieren.

## Architektur & Zusammenspiel

Die SAR-Verarbeitungspipeline in SSFocus beginnt mit Rohdaten (Level-0), die dekodiert und in Chunks aufgeteilt werden. Diese Chunks durchlaufen dann die Range-Doppler-Fokussierung, um Level-1-Bilder zu erzeugen. Optional können die dekodierten Daten oder die fokussierten Bilder zur Patch-Generierung für ML-Datensätze verwendet werden.

```mermaid
graph TD
    A[Rohdaten Level-0] --> B{Level-0 Dekodierung};
    B --> C[Dekodierte Radardaten & Metadaten];
    C --> D{Range-Doppler Fokussierung};
    D --> E[Fokussierte Level-1 Bilder];
    E --> F{Patch-Generierung für ML-Datensätze};
    F --> G[ML-Bereite Patches];
    B -- Optional --> F;

    subgraph "Range-Doppler Fokussierung (SARProcessor/focus.py)"
        D -- "Verwendet" --> D1[fft2D];
        D1 -- "Backend-Auswahl" --> D2{NumPy Backend};
        D1 -- "Backend-Auswahl" --> D3{PyTorch Backend};
        D -- "Verwendet" --> D4[get_range_filter];
        D -- "Verwendet" --> D5[get_RDMC];
        D -- "Verwendet" --> D6[get_azimuth_filter];
    end

    subgraph "Level-0 Dekodierung (SARProcessor/decode.py)"
        B -- "Verwendet" --> B1[sentinel1decoder.Level0File];
        B1 -- "Extrahiert" --> B2[Ephemeridendaten];
        B1 -- "Extrahiert" --> B3[Burst-Metadaten];
        B1 -- "Extrahiert" --> B4[Radardaten];
        B4 --> B5[split_radar_data];
        B5 -- "Speichert als" --> B6[Radardaten-Chunks (.pkl)];
    end

    subgraph "Deep Learning Modell (Models/PhiNet.py)"
        F -- "Kann nutzen" --> DL1[PhiNet];
        DL1 --> DL2[SpectrumAttentionBlock];
        DL1 --> DL3[Pooling Branch];
        DL1 --> DL4[Convolution Branch];
    end
```

## Notes
Ein weiteres technisches Highlight ist die Fähigkeit zur **Chunk-Partitionierung** . Dies ist entscheidend für die Verarbeitung großer SAR-Datensätze, die die Speicherkapazität überschreiten könnten. Durch das Aufteilen der Daten in kleinere, verwaltbare Chunks kann die Pipeline effizienter arbeiten und den Speicherverbrauch optimieren. Die Verwendung von `pickle`  zum Speichern und Laden dieser Chunks sowie die Überwachung des Speicherverbrauchs mit `psutil`  unterstreichen die Bedeutung des Speichermanagements in diesem Projekt.

Wiki pages you might want to explore:
- [SAR Processing Pipeline (sirbastiano/SSFocus)](/wiki/sirbastiano/SSFocus#2)
- [Range-Doppler Focusing (sirbastiano/SSFocus)](/wiki/sirbastiano/SSFocus#2.2)
- [Glossary (sirbastiano/SSFocus)](/wiki/sirbastiano/SSFocus#6)

View this search on DeepWiki: https://deepwiki.com/search/zielrepository-sirbastianossfo_cf4dd9cb-3e2a-4bba-8256-55e5b9c260b1
