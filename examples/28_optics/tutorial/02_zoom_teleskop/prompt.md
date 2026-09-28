# Prompt für Tutorial 02 — Herstellbarkeit eines Zoom-Teleskops (Toleranz-/Sensitivitätsanalyse)

> Dieses Dokument ist die **Aufgabenbeschreibung (das Prompt)** für das nächste
> Tutorial. Es enthält bewusst noch **keine** Implementierung — nur das Ziel, den
> nötigen fachlichen Hintergrund aus der Recherche, eine geprüfte
> Machbarkeitsanalyse des Solvers und die konkreten Liefergegenstände. Wer
> dieses Tutorial baut, soll von hier aus starten.

---

## 1. Ziel in einem Satz

Entwirf mit dem `optics`-Ray-Tracer (aus `../../source6`) ein **Teleskop mit
variabler Vergrößerung (Zoom)**, dessen Bild über den gesamten
Vergrößerungsbereich scharf auf der Kamera bleibt (parfokal), und nutze die
statistische Versuchsplanung (Design of Experiments, DoE) diesmal, um die
**Herstellbarkeit** zu bewerten und zu verbessern: die **Empfindlichkeit des
Designs gegen Fertigungsfehler** (Toleranzen) soll minimiert werden. **Wo immer
möglich, soll dabei die exakte Gradienten­information des Solvers den DoE-Sweep
ersetzen.**

---

## 2. Was „Herstellbarkeit" hier bedeutet

Ein Design ist auf dem Papier vielleicht optisch perfekt, aber **nicht
herstellbar**, wenn schon winzige Fertigungsabweichungen die Bildqualität
zusammenbrechen lassen. Reale Fehlerquellen sind zum Beispiel:

- **Radius-Fehler** — die geschliffene Krümmung weicht vom Sollwert ab.
- **Dicken-/Abstands-Fehler** — Linsendicken und Luftspalte (`thickness`)
  stimmen nicht exakt (Montage- und Fertigungstoleranz).
- **Material-/Brechzahl-Fehler** — die Glascharge hat eine leicht andere
  Brechzahl (`material`) als spezifiziert.

**Sensitivität** ist das Maß dafür, wie stark die Bildschärfe (der `loss`,
d. h. die RMS-Spotgröße) auf eine kleine Abweichung eines Parameters reagiert.
Formal ist die Sensitivität eines Parameters `p` die Ableitung

```
s_p = ∂Loss / ∂p
```

Ein **großes** `|s_p|` bedeutet: kleiner Fehler → große Unschärfe → enge
Toleranz nötig → teuer/schwer herstellbar. Ein **kleines** `|s_p|` bedeutet:
robust, gutmütig, günstig herstellbar. **Herstellbarkeit optimieren heißt also,
die Sensitivitäten klein zu halten** — nicht (nur) den Nennschärfewert.

---

## 3. Machbarkeitsanalyse des Solvers (geprüft, nicht geraten)

Ich habe den Solver-Code gelesen (`06_optimize.rs`, `01_dual.rs`,
`05_trace.rs`), um festzustellen, was per Gradient geht und wo die Grenze liegt.
Ergebnis:

### 3.1 Was der Solver per Autodiff direkt liefert

- Der Solver nutzt **Vorwärts-Automatische-Differenziation** (forward-mode
  autodiff) über Dualzahlen. Die Funktion `gradient(setup, vars)` gibt für jeden
  Parameter `p ∈ {radius, thickness, material}` **exakt** `∂Loss/∂p` zurück —
  in einem einzigen Trace pro Parameter, ohne numerisches Herumprobieren.
- **Das ist genau die gesuchte Sensitivität.** `s_p = ∂Loss/∂p` fällt direkt aus
  dem `.d`-Feld der Dualzahl. Wir können also die Fehlerempfindlichkeit jedes
  Toleranzparameters **gradientenbasiert und billig** auslesen — **kein Raster,
  kein Sweep nötig.** Das erfüllt den Wunsch „Gradientensuche statt DoE-Sweep"
  für den Sensitivitäts-Teil vollständig.

### 3.2 Wo die reine Gradientenmethode an eine Grenze stößt (ehrlich benannt)

- Der Forward-Mode-Autodiff trägt **einen Seed pro Trace** — er liefert erste
  Ableitungen, aber **keine zweiten Ableitungen** direkt.
- Die **Robustheit** zu *optimieren* (die Sensitivität `s_p` durch Verstellen
  der Designvariablen kleiner machen) würde die Ableitung der Sensitivität nach
  den Designvariablen erfordern, also `∂/∂design (∂Loss/∂p)` — eine **zweite
  Ableitung / Hesse-Diagonale**. Die gibt der Solver nicht unmittelbar her.

### 3.3 Empfohlene hybride Strategie (Gradient-first)

Daraus folgt ein klarer, ehrlicher Aufbau:

1. **Sensitivität messen: rein per Gradient (kein Sweep).**
   Für jede Zoom-Stellung die exakten Sensitivitäten `s_p = ∂Loss/∂p` aller
   Toleranzparameter per Autodiff auslesen. Das ist der Ersatz des DoE-Sweeps
   durch Gradienten. Ergebnis: ein **Sensitivitäts-/Toleranzprofil** pro
   Zoom-Stellung.

2. **Robustheit verbessern: dort DoE/Suche, wo Gradient nicht reicht.**
   Da die zweite Ableitung fehlt, wird die *Verbesserung* der Robustheit über
   die Designfreiheitsgrade behandelt als:
   - entweder ein **kleiner DoE-Sweep** über die freien Designvariablen, wobei
     an **jedem** Punkt die Robustheitskennzahl *gradientenbasiert* (per
     Autodiff-Sensitivität) berechnet wird — also DoE nur über den Rest, die
     Kennzahl selbst per Gradient;
   - oder ein **Finite-Differenzen-Gradientenabstieg auf der Autodiff-
     Sensitivität** (die Autodiff-Sensitivität ist die billige, exakte innere
     Größe; außen ein numerischer Schritt). Beides im Tutorial vergleichen und
     die Wahl begründen.

3. **Nennschärfe/Parfokalität** kann weiterhin mit dem eingebauten
   Gradienten­abstieg (`optics optimize`) sichergestellt werden (wie in
   Tutorial 01), bevor/parallel zur Toleranzbetrachtung.

> **Kernbotschaft der Strategie:** Die *Messung* der Fehlerempfindlichkeit ist
> vollständig gradientenbasiert (Wunsch erfüllt); nur die *Optimierung der
> Robustheit* braucht dort, wo zweite Ableitungen nötig wären, eine
> Rastersuche oder einen äußeren numerischen Schritt.

---

## 4. Fachlicher Hintergrund: Aufbau eines Zoom-Teleskops (Recherche)

Diese Erkenntnisse müssen im Bericht **für Laien erklärt** werden.

### 4.1 Wie viele Linsengruppen müssen sich bewegen?

Die klassische mechanisch kompensierte Zoom-Architektur hat **vier Gruppen**,
von denen sich typischerweise **zwei bewegen**:

1. **Frontgruppe / Objektiv** — fest. Sammelt das Licht.
2. **Variator** — *bewegt*. Ändert die Vergrößerung durch Verschieben entlang
   der optischen Achse.
3. **Kompensator** — *bewegt*. Wird nachgeführt, damit die Bildlage trotz der
   Variator-Bewegung konstant bleibt.
4. **Relais- / hintere Gruppe** — fest. Bildet auf die Kamera ab.

Man braucht **mindestens zwei bewegliche Gruppen**, weil zwei Größen
gleichzeitig kontrolliert werden: die Vergrößerung *und* die Bildlage.

Quellen (sinngemäß wiedergegeben, für Lizenzkonformität umformuliert):
- Konventionelle Zoom-Objektive bestehen im Allgemeinen aus drei beweglichen
  optischen Gruppen plus einer festen Gruppe; jede Gruppe enthält meist zwei
  oder mehr Elemente. [adaptall-2.com](http://adaptall-2.com/articles/InsideZoomLens/InsideZoomLens.html)
- Bewegen sich Variator und Kompensator **gleich**, spricht man von *optischer
  Kompensation*; erfordern sie **unterschiedliche** Bewegungen, von
  *mechanischer Kompensation*. [Cambridge, Introduction to Lens Design](https://www.cambridge.org/core/books/introduction-to-lens-design/zoom-lenses/C7741006BE26F36D2810C78F4379060B)
- Ein echtes Zoom hält den Fokus über den gesamten Bereich (**parfokal**); ein
  Varifokal muss danach neu fokussiert werden. [Apollo Optical](https://www.apollooptical.com/feeds/blog/zoom-lens-assembly)

### 4.2 Bezug zur Herstellbarkeit

Bei einem Zoom ist Herstellbarkeit besonders heikel: Die Sensitivitäten ändern
sich **über den Zoombereich**. Ein Design kann bei Weitwinkel robust, bei Tele
aber extrem fehlerempfindlich sein. Genau das soll die gradientenbasierte
Analyse pro Zoom-Stellung sichtbar machen.

---

## 5. Konkrete Aufgabenstellung

### 5.1 Was zu bauen ist

1. **Prescription eines vierteiligen Zoom-Teleskops** als TOML-Datei(en):
   Front (fest), Variator (beweglich), Kompensator (beweglich), Relais (fest),
   Bildebene = Kamera. Bewegung über die `thickness`-Luftspalte abgebildet.
   Mehrere Zoom-Stellungen (z. B. Weitwinkel / Mitte / Tele).

2. **Python-Treiber** (analog `doe_sweep.py`, reine Standardbibliothek), der:
   - je Zoom-Stellung die **exakten Sensitivitäten** `∂Loss/∂p` aller
     Toleranzparameter (Radien, Dicken, Brechzahlen) per Gradient ermittelt.
     Falls die CLI keine direkte Gradienten-Ausgabe hat: prüfen, ob der
     Solver-Code eine dünne CLI-Erweiterung oder ein kleines Rust-`example`
     zum Ausgeben von `gradient(...)` braucht — **erst prüfen, dann
     entscheiden**; sonst als dokumentierten Fallback Finite-Differenzen der
     `loss`-Ausgabe verwenden und den Unterschied im Bericht offenlegen.
   - eine **Robustheitskennzahl** definiert (z. B. gewichtete Summe der
     quadrierten Sensitivitäten, oder die worst-case-Sensitivität über die
     Zoom-Stellungen) und diese über die freien Designvariablen verbessert —
     per kleinem DoE-Sweep *oder* Finite-Differenzen-Abstieg auf der
     Autodiff-Sensitivität (siehe Strategie in Abschnitt 3.3).

3. **Analyse**:
   - **Toleranz-Ranking**: welche Parameter sind die kritischsten (größte
     `|s_p|`) — und wie ändert sich das Ranking über den Zoombereich?
   - **Vorher/Nachher**: Robustheitskennzahl des Ausgangsdesigns vs. des
     verbesserten Designs; möglichst ohne die Nennschärfe/Parfokalität zu
     verschlechtern.
   - Optional: aus den Sensitivitäten ein einfaches **Toleranzbudget**
     ableiten (welche Fertigungsgenauigkeit pro Parameter nötig ist, um eine
     Schärfevorgabe einzuhalten).

4. **Validierung**: Gegenprobe der Autodiff-Sensitivitäten mit Finite-
   Differenzen (der Solver hat dafür bereits Tests im Optimize-Modul — Muster
   übernehmen), damit die Gradienten belastbar sind.

### 5.2 Der Bericht (`Bericht.md`, auf Deutsch)

Dieselben Qualitätskriterien wie Tutorial 01:

- **Geltungsbereich (Scope)** klar definieren — was abgedeckt ist und was nicht.
- Den **Leser abholen und mitnehmen**: ohne Optik-Vorwissen verständlich,
  motivierendes Einstiegsbild (z. B. „Warum ein perfektes Design in der
  Fertigung durchfallen kann").
- **Abkürzungen und Jargon erklären**: Zoom, parfokal/varifokal, Variator,
  Kompensator, Relais, Toleranz, Sensitivität, RMS, EFL, Defokus, Autodiff,
  Gradient, erste/zweite Ableitung, DoE, Finite Differenzen.
- **Beispiele** mit echten Zahlen, echten CLI-Ausgaben, echten Codeausschnitten.
- **Diagramme (z. B. Mermaid)**:
  - Aufbau der vier Gruppen (fest/beweglich),
  - Datenfluss Python ↔ Solver,
  - Sensitivitäts-/Toleranzprofil über die Zoom-Stellungen (als Konzeptskizze),
  - Ablaufdiagramm: Gradient misst Sensitivität → Kennzahl → Verbesserung.
- Den Punkt aus Abschnitt 3 **transparent machen**: warum die Messung rein
  gradientenbasiert ist, aber die Robustheits-*Optimierung* an der fehlenden
  zweiten Ableitung eine Rastersuche/äußere Schleife braucht.

### 5.3 Liefergegenstände (Struktur wie Tutorial 01)

```
tutorial/02_zoom_teleskop/
├── prompt.md                 # dieses Dokument
├── tolerance_analysis.py     # Python-Treiber (reine Standardbibliothek)
├── Bericht.md                # deutscher Bericht
├── assets/                   # Zoom-Teleskop-Prescriptions (TOML)
│   └── zoom_*.toml
└── results/                  # generierte Artefakte (gitignore)
    ├── sensitivity_profile.csv
    ├── tolerance_ranking.csv
    ├── robustness_before_after.json
    └── summary.json
```

---

## 6. Randbedingungen und Hinweise

- **Gradient bevorzugen**: Die Sensitivitätsmessung MUSS gradientenbasiert sein
  (Autodiff), nicht per DoE-Sweep. DoE/äußere Suche nur dort, wo zweite
  Ableitungen fehlen (Robustheits-*Optimierung*).
- **Reine Standardbibliothek** für den Python-Treiber (kein numpy/matplotlib).
- Solver als **Black Box** über die CLI; falls für die Gradienten-Ausgabe eine
  minimale, saubere Solver-Erweiterung nötig ist (kleiner Unterbefehl oder
  `example`), zuerst den Code prüfen und die Erweiterung begründet und
  getestet vornehmen.
- **Autodiff gegen Finite Differenzen absichern** (Muster: die vorhandenen
  Tests `gradient_matches_finite_differences` im Optimize-Modul).
- **Reproduzierbarkeit**: kompletter Lauf per einem Python-Aufruf wiederholbar;
  Zahlen im Bericht müssen mit der tatsächlichen Ausgabe übereinstimmen.
- **Verifizieren vor Abschluss**: `cargo build --release`, Treiber ausführen,
  Artefakte prüfen, Autodiff-Gegenprobe bestehen.

---

## 7. Erwartete Kernaussage des Tutorials

> Herstellbarkeit ist eine Frage der **Fehlerempfindlichkeit**: Ein gutes Design
> hält nicht nur die Nennschärfe hoch, sondern die **Sensitivitäten** gegen
> Fertigungstoleranzen klein. Der differenzierbare Solver liefert diese
> Sensitivitäten **exakt und gradientenbasiert** (`∂Loss/∂p`), ganz ohne
> Parameter-Sweep — pro Zoom-Stellung, sodass man das kritischste Bauteil sofort
> erkennt. Nur die *Verbesserung* der Robustheit stößt an die Grenze der zweiten
> Ableitung und wird dort durch eine schlanke Rastersuche bzw. einen äußeren
> numerischen Schritt ergänzt. Damit erweitert dieses Tutorial den Werkzeugkasten
> aus Tutorial 01 von „bestes Nennergebnis" zu „bestes **herstellbares**
> Ergebnis".
