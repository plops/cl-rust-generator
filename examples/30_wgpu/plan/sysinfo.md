https://deepwiki.com/GuillaumeGomez/sysinfo

Statt alle /proc-Dateien selbst von Hand zu parsen, können Sie auf robuste, bestehende Bibliotheken zurückgreifen:
    Das sysinfo-Crate bietet eine fertige, plattformübergreifende Abstraktion.


was sind die drei wichtigsten (oder komplexesten) algorithmen in diesem repo? erklaere wie sie die software zu dem machen was sie ist

Du fragst nach den drei wichtigsten/komplexesten Algorithmen in **sysinfo** – einer plattformübergreifenden Bibliothek zum Auslesen von Systeminformationen (CPU, Speicher, Prozesse). Kurz gesagt: Es sind (1) die Delta-basierte CPU-Auslastungsberechnung, (2) das Scannen und Aktualisieren von Prozessen über `/proc`, und (3) die plattformspezifische Datenbeschaffung (Windows-PDH-Counter, `/proc`-Parsing inkl. Fallbacks).

---

## 1. CPU-Auslastung über Messdifferenzen

Der zentrale Algorithmus der Bibliothek: CPU-Usage kann nicht direkt gelesen werden, sondern wird als Differenz zweier Messungen berechnet. Auf Linux holt `update_procs_cpu` die globalen Rohzeiten, teilt die verstrichene Gesamtzeit durch die CPU-Anzahl und ruft `compute_cpu_usage` für jeden Prozess auf, begrenzt durch `cpus.len() * 100`. [1](#0-0) 

Auf macOS existieren sogar zwei Varianten in `compute_cpu_usage`: der bevorzugte Weg über ein übergebenes `time_interval` und ein "backup way" über `mach_absolute_time()` mit saturierenden Subtraktionen gegen Zeit-Rückwärtssprünge. [2](#0-1) 

Das erklärt auch die API-Form der Bibliothek: `MINIMUM_CPU_UPDATE_INTERVAL` und die `refresh_*`-Methoden existieren nur, weil jede Messung eine Baseline braucht.

## 2. Prozess-Discovery und selektives Refresh über `/proc`

`refresh_processes_specifics` scannt `/proc` nach PID-Verzeichnissen, liest pro Prozess `stat`, `status`, `cmdline`, `environ`, `io` usw. und aktualisiert die `process_list`-HashMap selektiv nach `ProcessesToUpdate` und `ProcessRefreshKind`. [3](#0-2) 

Die Komplexität steckt in der Verwaltung: File-Descriptor-Limits, Erkennen verschwundener Prozesse, optionales Multithreading via rayon. Das ist das Herzstück dessen, was sysinfo zu einem "Prozess-Monitor-Baukasten" macht.

## 3. Plattformspezifische Parsing- und Counter-Machinerie

- **Linux**: `read_table("/proc/meminfo", ...)` parst zeilenweise Speicherfelder mit KiB→Byte-Konvertierung; fehlt `MemAvailable` (Kernel < 3.14), greift ein Schätz-Fallback aus `MemFree + Buffers + Cached + SReclaimable - Shmem`. [4](#0-3) 
- **Linux CPU-Infos**: Ein verschachtelter Parser extrahiert `vendor_id`, `model name`, `CPU implementer`, `CPU part` aus `/proc/cpuinfo` pro Prozessor-Block – mit Hex-Parsing für ARM. [5](#0-4) 
- **Windows**: Die `Query`-Klasse wrappt die Performance-Data-Helper-API (`PdhOpenQueryA`, `PdhAddEnglishCounterW`, `PdhCollectQueryData`, `PdhGetFormattedCounterValue`), ergänzt um Registry-Zugriffe via `RegKey`. [6](#0-5) [7](#0-6) 

## Warum das die Software ausmacht

sysinfo ist im Kern ein **Übersetzer heterogener OS-Schnittstellen in eine einheitliche API**. Genau diese drei Algorithmen definieren es: die zustandsbehaftete Delta-Berechnung (erklärt das Refresh-Design), das inkrementelle Prozess-Tracking (erklärt `ProcessRefreshKind`/`ProcessesToUpdate`) und die plattformspezifischen Adapter (erklären die `src/unix/...` vs. `src/windows/...`-Struktur).

## Notes

- Weitere erwähnenswerte Komplexität außerhalb der gezeigten Snippets: cgroup v1/v2-Limits (`cgroup_limits`) und die Physical-Core-Erkennung über core/physical IDs – beide nur indirekt im Kontext sichtbar.
- Die Auswahl basiert auf den bereitgestellten Snippets und Wiki-Seiten; der Index enthält nicht den vollständigen Quelltext aller Dateien.
