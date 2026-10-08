for i in plan/20261006_01_init/walkthrough.md \
	     *.toml \
	     src/*.rs \
	     tests/*.rs
do
    echo "// start of "$i
    cat $i
done

cat <<EOF
mache einen review des rust codes unter beruecksichtigung der folgenden informationen. welche ansaetze lassen beste optimierungsmoeglichkeiten erwarten?

Um die Performance deines SPH-Solvers (Smoothed Particle Hydrodynamics) zu messen und das Maximum aus deiner GPU herauszuholen, musst du den Flaschenhals zwischen CPU-Host, GPU-Compute und Speicherbandbreite isolieren. Da du vermutlich cuda-oxide (für native NVIDIA-PTX-Generierung in Rust) oder eine wgpu/Vulkan-Infrastruktur nutzt, unterscheidet sich die GPU-Profilierung grundlegend vom klassischen CPU-Profiling. [1] 
Hier ist der strategische Fahrplan, um die Performance präzise zu messen und zu optimieren:
------------------------------
## 1. High-Level-Metriken etablieren (Die Baseline)
Bevor du tief in Profiler eintauchst, implementiere Anwendungsmetriken direkt in deinem Rust-Code:

* MIPS / MIUPS (Million Iteration Updates per Second): Das ist der Standard in der Strömungssimulation. Berechne:
$$\text{MIUPS} = \frac{\text{Anzahl Partikel} \times \text{Substeps}}{\text{Berechnungszeit in Sekunden} \times 10^6}$$ 
* Reine Kernel-Zeit messen: Nutze asynchrone GPU-Timestamps (z.B. cudaEventRecord via cuda-oxide oder Timestamp-Queries in wgpu). Miss niemals die GPU-Zeit einfach mit std::time::Instant::now() auf der CPU, da du sonst die CPU-Device-Synchronisationslatenz mitmisst. [2] 

------------------------------
## 2. Die richtigen Profiling-Tools nutzen
Reine Rust-Profiler (wie samply oder flamegraph) zeigen dir nur die CPU-Seite. Für die GPU benötigst du hardwarenahe Tools: [3] 

* NVIDIA Nsight Systems (System-Level):
Zeigt dir die Timeline. Du siehst sofort: Wartet die GPU auf die CPU? Wie lange dauern die Speicherübertragungen (HtoD / DtoH) im Vergleich zur Rechenzeit?
* NVIDIA Nsight Compute (Kernel-Level):
Das wichtigste Tool für deinen SPH-Kernel. Es analysiert die Roofline-Execution und sagt dir exakt, ob dein Kernel Memory Bound (wartet auf VRAM) oder Compute Bound (wartet auf Rechenwerke) ist.
* Unter Linux (nvidia-smi / rocm-smi):
Lass während des Solves nvidia-smi dmon im Terminal laufen. Bleibt die sm (Streaming Multiprocessor Utilization) bei unter 90%, fütterst du die GPU nicht schnell genug mit Arbeit. [4] 

------------------------------
## 3. Typische SPH-Flaschenhälse auf der GPU aufdecken
Da ein SPH-Solver extrem interaktiv und rechenintensiv ist, solltest du bei der Analyse gezielt nach folgenden typischen GPU-Schwachstellen suchen:
## A. Die Nachbarschaftssuche (Neighbor Particle Search - NPS)

* Das Problem: SPH benötigt Interaktionen mit umliegenden Partikeln. Eine naive O(N²)-Suche killt die GPU-Performance.
* Die Lösung: Nutze ein Uniform Grid (Spatial Hashing). Sortiere die Partikel im VRAM (z.B. mittels GPU-Radix-Sort), sodass Partikel, die räumlich nah beieinanderliegen, auch kontinuierlich im Speicher hinterlegt sind. [5] 

## B. Memory Coalescing (Speicherzugriffe)

* Das Problem: Wenn ein GPU-Thread-Block auf den globalen Speicher zugreift und die Threads "wild verstreut" lesen (unkoaleszierter Zugriff), bricht die Bandbreite ein.
* Die Lösung: Stelle sicher, dass benachbarte Threads (ein Warp von 32 Threads) auf aufeinanderfolgende Speicheradressen zugreifen. Nutze Arrays of Structures (AoS) vs. Structures of Arrays (SoA). Auf GPUs ist SoA (z.B. ein separater Puffer für x, einer für y, einer für z) fast immer drastisch schneller als ein Array aus struct Particle { pos: Vector3, vel: Vector3 }.

## C. Shared Memory ausnutzen

* Nsight Compute wird dir vermutlich eine hohe Latenz beim Lesen der Nachbarpartikel anzeigen.
* Optimierung: Lade die Partikeldaten einer Grid-Zelle kollaborativ in den ultraschnellen Shared Memory (#[shared]-Memory-Bereiche), bevor die Threads die Interaktionskräfte (Druck/Viskosität) berechnen.

## D. Divergenz innerhalb von Warps (Branch Divergence)

* SPH-Kernels haben oft Abfragen wie if distance < smoothing_length. Wenn innerhalb eines 32er-Warps einige Threads true und andere false ergeben, muss die GPU beide Pfade nacheinander ausführen.
* Versuche, solche Bedingungen durch mathematische Kniffe (z.B. step()-Funktionen oder Multiplikation mit Masken) zu eliminieren, um den Kontrollfluss linear zu halten.

## E. Host-Device Transfer minimieren

* Der größte Performance-Killer ist das Hin- und Herkopieren von Daten zwischen CPU und GPU pro Frame.
* Goldene Regel: Lass die komplette Simulationsschleife (Grid-Erstellung, NPS, Dichte-Update, Kraft-Update, Positions-Update) auf der GPU. Kopiere Daten nur dann zur CPU zurück, wenn du sie für die Visualisierung auf die Festplatte schreiben musst.

------------------------------
Wenn du mir verrätst, wie deine Datenstrukturen aktuell aufgebaut sind (z.B. SoA vs. AoS) oder welche API (cuda-oxide, wgpu, vulkan) das Fundament bildet, können wir gezielt deinen Kernel zerlegen.

[1] [https://www.iampedro.com](https://www.iampedro.com/blog/cuda-oxide-rust-meets-cuda/)
[2] [https://blog.cadfem.net](https://blog.cadfem.net/de/ansys-fluent-gpu-performance-testing-use-case)
[3] [https://www.reddit.com](https://www.reddit.com/r/rust/comments/1kzcewt/how_can_i_best_test_my_performance/)
[4] [https://shattered.io](https://shattered.io/at/gpu-benchmark-tutorial-2026/)
[5] [https://www.sciencedirect.com](https://www.sciencedirect.com/science/article/abs/pii/S0032591022008968)

EOF


