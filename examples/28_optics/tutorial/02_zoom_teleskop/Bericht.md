# Herstellbarkeit eines Zoom-Teleskops — Toleranz- und Sensitivitätsanalyse per Autodiff

*Ein Tutorial: Wie man mit dem differenzierbaren Rust-Solver `optics` nicht nur ein optisch scharfes, sondern ein **herstellbares** Zoom-Teleskop bewertet — indem man die Fehlerempfindlichkeit jedes Bauteils exakt und gradientenbasiert misst, ganz ohne Parameter-Sweep.*

---

## 0. Für wen ist dieses Dokument — und was Sie davon haben

Dieses Tutorial richtet sich an Leserinnen und Leser, die **programmieren können**, aber **keine Optik-Fachleute** sein müssen. Vorwissen über Linsen ist nicht nötig; alle Fachbegriffe werden bei ihrer ersten Verwendung erklärt und alle Abkürzungen ausgeschrieben.

Es baut auf Tutorial 01 (statistische Versuchsplanung für ein Cooke-Triplet) auf, ist aber eigenständig lesbar. Während Tutorial 01 fragte „Welches Design ist am **schärfsten**?", fragt dieses Tutorial: „Welches Design ist am **besten herstellbar**?" — und das ist eine andere, oft wichtigere Frage.

Am Ende verstehen Sie:

1. **Warum** ein auf dem Papier perfektes Design in der Fertigung durchfallen kann.
2. **Wie** ein differenzierbarer Solver die **Fehlerempfindlichkeit** (Sensitivität) jedes Fertigungsparameters *exakt* liefert — als Ableitung, nicht als Rastersuche.
3. **Wo** diese Gradientenmethode an eine Grenze stößt (zweite Ableitungen) und wie man sie dort mit einer schlanken Rastersuche ergänzt.

> **Ein Bild vorweg.** Zwei Brücken tragen dieselbe Last. Die eine kippt schon, wenn ein Niet einen Millimeter falsch sitzt; die andere verzeiht solche Fehler. Auf dem Reißbrett sehen beide gleich gut aus — der Unterschied zeigt sich erst in der Werkstatt. Bei Optik ist es genauso: Herstellbarkeit heißt **Fehler verzeihen**.

---

## 1. Geltungsbereich (Scope)

**Was dieses Tutorial abdeckt:**

- Ein konkretes, reproduzierbares Beispiel: ein **vierteiliges Zoom-Teleskop** mit variabler Brennweite, das über den gesamten Zoombereich scharf bleibt (**parfokal**).
- Die **gradientenbasierte** Messung der Fehlerempfindlichkeit `∂Loss/∂p` aller Toleranzparameter (Radien, Dicken, Brechzahlen) — pro Zoom-Stellung, ohne Sweep.
- Ein **Toleranz-Ranking** (welches Bauteil ist am kritischsten) und wie es über den Zoombereich wandert.
- Eine **Robustheits-Optimierung** dort, wo der Gradient nicht reicht: eine kleine Rastersuche (DoE) über die freien Montagevariablen, deren Kennzahl an jedem Punkt gradientenbasiert berechnet wird.
- Die **Absicherung** der Autodiff-Gradienten gegen Finite Differenzen.

**Was dieses Tutorial *nicht* abdeckt:**

- Die Herleitung der optischen Physik (Snellius, automatische Differenziation). Sie steckt im Rust-Code und wird hier nur *benutzt*.
- Vollständige kommerzielle Toleranzrechnung (Monte-Carlo über Fertigungsstatistik, RSS-Budgets mit Kopplungen, Zentrier-/Kippfehler). Wir bleiben bei den vier Parametertypen, die der Solver differenziert.
- Ein real gerechnetes, korrigiertes Seriendesign. Unser Zoom ist ein *didaktisches* System (Zoomfaktor ~1,17×), bewusst einfach gehalten.

**Voraussetzungen:**

- Python 3.9 oder neuer (reine Standardbibliothek, kein numpy/matplotlib).
- Das kompilierte `optics`-Programm. Falls es noch nicht existiert:
  ```sh
  cd ../../source6 && cargo build --release
  ```

---

## 2. Das große Ganze: Wer redet mit wem?

Wie in Tutorial 01 kennt Python **keine Optik**. Es schreibt Konfigurationsdateien, ruft den Rust-Solver als *Black Box* auf und liest dessen Textausgabe zurück. Neu ist: Der Solver liefert jetzt nicht nur eine Gütezahl, sondern auf Wunsch auch deren **exakte Ableitungen**.

```mermaid
flowchart LR
    A["Python-Treiber<br/>tolerance_analysis.py"] -->|"schreibt *.toml"| B["Prescription<br/>je Zoom-Stellung"]
    B --> C["Rust-Solver<br/>optics (Binary)"]
    C -->|"optics sensitivity<br/>dLoss/dp je Parameter"| D["Sensitivitäten<br/>(exakte Gradienten)"]
    C -->|"optics trace / efl<br/>loss / EFL / defocus"| E["Nennschärfe<br/>+ Fokuslage"]
    D -->|"Parsing"| A
    E -->|"Parsing"| A
    A -->|"schreibt"| F["Ergebnisse<br/>CSV, JSON, robuste TOML"]
```

### 2.1 Glossar der Abkürzungen und Fachbegriffe

| Begriff | Ausgeschrieben / Bedeutung |
|---|---|
| **Zoom** | Objektiv mit *stufenlos veränderlicher* Brennweite (Vergrößerung). |
| **parfokal** | Der Fokus bleibt über den ganzen Zoombereich erhalten — man muss beim Zoomen *nicht* nachfokussieren. |
| **varifokal** | Gegenteil: nach dem Zoomen muss neu fokussiert werden. |
| **Variator** | Die *bewegliche* Gruppe, die die Vergrößerung ändert. |
| **Kompensator** | Die *bewegliche* Gruppe, die die Bildlage konstant hält (kompensiert den Variator). |
| **Relais** | Feste hintere Gruppe, die auf die Kamera/den Sensor abbildet. |
| **EFL** | Effective Focal Length (effektive Brennweite) in mm — wie stark gebündelt wird. |
| **RMS** | Root Mean Square (quadratischer Mittelwert) — Maß für die Spotgröße. |
| **loss** | Unsere Gütezahl: Summe der quadrierten Strahlabstände vom Bildzentrum. **Klein = scharf.** |
| **defocus** | Abstand (mm) zwischen tatsächlichem Fokus und Bildebene. |
| **Toleranz** | Erlaubte Fertigungsabweichung eines Parameters. |
| **Sensitivität** | Wie stark der `loss` auf eine kleine Parameterabweichung reagiert: `s_p = ∂Loss/∂p`. |
| **Autodiff** | Automatische Differenziation — der Solver berechnet Ableitungen exakt mit, nicht per Nährung. |
| **Gradient** | Vektor aller ersten Ableitungen `∂Loss/∂p`. |
| **erste/zweite Ableitung** | Steigung bzw. Krümmung einer Funktion. |
| **DoE** | Design of Experiments (statistische Versuchsplanung) — systematisches Raster von Versuchen. |
| **Finite Differenzen** | Ableitung durch kleine Zahlen-Differenzen genähert: `(f(x+e)−f(x−e))/2e`. |

---

## 3. Das reale Problem: ein Design, das die Fertigung überstehen muss

### 3.1 Warum ein „perfektes" Design durchfallen kann

Ein Optikdesign ist eine Liste von Sollwerten: Krümmungsradien, Linsendicken, Luftspalte, Brechzahlen. In der Werkstatt trifft keiner davon exakt zu. Gläser werden geschliffen (Radius-Fehler), Linsen montiert (Dicken-/Abstands-Fehler), Glaschargen schwanken (Brechzahl-Fehler). **Herstellbarkeit** heißt: Das Bild bleibt trotz dieser Abweichungen scharf.

Das Maß dafür ist die **Sensitivität**:

```
s_p = ∂Loss / ∂p
```

- **Großes** `|s_p|` → kleiner Fehler bewirkt große Unschärfe → **enge** Toleranz nötig → teuer.
- **Kleines** `|s_p|` → das Design *verzeiht* Fehler → **günstig** herstellbar.

Herstellbarkeit optimieren heißt also: die **Sensitivitäten klein halten** — nicht nur den Nennschärfewert.

### 3.2 Aufbau eines Zoom-Teleskops (Recherche für Laien)

Die klassische, mechanisch kompensierte Zoom-Architektur hat **vier Gruppen**, von denen sich **zwei bewegen**:

```mermaid
flowchart LR
    L["Licht<br/>(parallel)"] --> F["Front<br/>(fest, positiv)"]
    F --> V["Variator<br/>(beweglich, negativ)"]
    V --> K["Kompensator<br/>(beweglich, positiv, Blende)"]
    K --> R["Relais<br/>(fest, positiv)"]
    R --> I["Bildebene<br/>(Kamera)"]
    V -. "verschiebt sich → ändert Vergrößerung" .-> V
    K -. "wird nachgeführt → hält Bildlage" .-> K
```

- **Front** (fest): sammelt das Licht.
- **Variator** (beweglich): ändert durch Verschieben die **Vergrößerung**.
- **Kompensator** (beweglich): wird nachgeführt, damit die **Bildlage** trotz Variator-Bewegung konstant bleibt.
- **Relais** (fest): bildet auf die Kamera ab.

Man braucht **mindestens zwei bewegliche Gruppen**, weil zwei Größen gleichzeitig kontrolliert werden: Vergrößerung *und* Bildlage. Bewegen sich beide *gleich*, spricht man von *optischer* Kompensation; brauchen sie *unterschiedliche* Bewegungen, von *mechanischer* Kompensation. Ein echtes Zoom hält den Fokus über den ganzen Bereich (**parfokal**); ein Varifokal müsste man nachfokussieren.

*(Sinngemäß nach: [adaptall-2.com – Inside a Zoom Lens](http://adaptall-2.com/articles/InsideZoomLens/InsideZoomLens.html); [Cambridge – Introduction to Lens Design, Zoom Lenses](https://www.cambridge.org/core/books/introduction-to-lens-design/zoom-lenses/C7741006BE26F36D2810C78F4379060B); [Apollo Optical – Zoom Lens Assembly](https://www.apollooptical.com/feeds/blog/zoom-lens-assembly). Inhalte wurden für die Lizenzkonformität umformuliert.)*

### 3.3 Wie das im Solver abgebildet ist

Der `optics`-Solver sieht ein System als Folge von **Flächen** (jede Linse hat Vorder- und Rückseite), beschrieben durch `radius`, `thickness` (Abstand zur nächsten Fläche) und `material` (Brechzahl danach). Die **Bewegung** einer Gruppe bilden wir über die **Luftspalte** ab. Drei Spalte sind zoom-abhängig:

- **g1** = `Front Rückseite.thickness` — der Zoom-Stellhebel (bewegt den Variator),
- **g2** = `Variator Rückseite.thickness` — Vorderspalt des Kompensators,
- **g3** = `Kompensator Rückseite.thickness` — Hinterspalt des Kompensators.

Der **letzte** Spalt (Relais → Bildebene) bleibt **fest** — die Kamera bewegt sich nicht. Genau das macht das System **parfokal**. Die drei Prescriptions `zoom_wide.toml`, `zoom_mid.toml`, `zoom_tele.toml` unterscheiden sich *nur* in g1/g2/g3; Radien, Dicken und Gläser sind identisch.

Die g2/g3-Werte je Stellung wurden mit dem **eingebauten Gradientenabstieg** des Solvers (`optics optimize`) so gesucht, dass jede Stellung bei fester Kamera scharf abbildet. Das Ergebnis ist ein echtes, parfokales Zoom:

| Stellung | g1 (mm) | g2 (mm) | g3 (mm) | EFL (mm) | loss | defocus (mm) |
|---|---|---|---|---|---|---|
| wide | 8,0 | 20,519 | 25,743 | 41,72 | 0,0171 | −0,48 |
| mid | 17,0 | 19,991 | 17,705 | 45,11 | 0,0133 | −0,40 |
| tele | 26,0 | 20,062 | 7,843 | 48,69 | 0,0278 | −0,47 |

Der Zoombereich EFL 41,7 … 48,7 mm entspricht einem **Zoomfaktor ~1,17×**. Der letzte Spalt ist überall **18,774 mm** — Beleg für die Parfokalität.

---

## 4. Machbarkeit des Solvers — was per Gradient geht (und was nicht)

Ich habe den Solver-Code (`06_optimize.rs`, `01_dual.rs`, `05_trace.rs`) gelesen, um zu prüfen, was der Gradient hergibt.

### 4.1 Was der Solver exakt liefert

Der Solver nutzt **Vorwärts-Automatische-Differenziation** über Dualzahlen. Die Bibliotheksfunktion `gradient(setup, vars)` gibt für jeden Parameter `p ∈ {radius, thickness, material}` **exakt** `∂Loss/∂p` zurück — in *einem* Trace pro Parameter, ohne numerisches Herumprobieren. **Das ist genau die gesuchte Sensitivität.** Die Fehlerempfindlichkeit fällt direkt aus dem `.d`-Feld der Dualzahl.

Diese Größe war bisher nur intern verfügbar. Für dieses Tutorial habe ich die CLI um einen schlanken Unterbefehl erweitert (siehe Abschnitt 5), sodass Python die Gradienten direkt abgreifen kann.

### 4.2 Wo die reine Gradientenmethode an eine Grenze stößt (ehrlich benannt)

Der Forward-Mode trägt **einen Seed pro Trace** — er liefert **erste** Ableitungen, aber keine zweiten. Die **Robustheit** zu *optimieren* (die Sensitivität `s_p` durch Verstellen der Designvariablen kleiner machen) bräuchte `∂/∂design (∂Loss/∂p)` — eine **zweite Ableitung**. Die gibt der Solver nicht unmittelbar her.

### 4.3 Die hybride Strategie (Gradient-first)

```mermaid
flowchart TD
    A["Sensitivität MESSEN"] -->|"rein per Gradient<br/>(Autodiff, kein Sweep)"| B["Sensitivitätsprofil<br/>je Zoom-Stellung"]
    B --> C["Toleranz-Ranking<br/>kritischste Bauteile"]
    B --> D["Robustheitskennzahl<br/>(RSS der skalierten s_p)"]
    D -->|"2. Ableitung fehlt →<br/>äußere Rastersuche (DoE)"| E["Robustheit VERBESSERN<br/>DoE über g2/g3,<br/>Kennzahl je Punkt per Gradient"]
    E --> F["robustes, weiterhin<br/>scharfes & parfokales Design"]
```

> **Kernbotschaft:** Die *Messung* der Fehlerempfindlichkeit ist vollständig gradientenbasiert (Wunsch erfüllt). Nur die *Optimierung* der Robustheit braucht dort, wo zweite Ableitungen nötig wären, eine Rastersuche mit gradientenbasierter Kennzahl.

---

## 5. Die Solver-Erweiterung: `optics sensitivity`

Die CLI hatte bisher `trace`, `efl`, `optimize`, `export`, `tui` — aber keinen Weg, die Gradienten auszugeben. Statt in Python Finite Differenzen zu bauen (ungenau, teuer), habe ich den bereits vorhandenen, getesteten `gradient()`-Kern über einen neuen Unterbefehl zugänglich gemacht:

```rust
"sensitivity" => {
    let setup = load_config(args)?;
    let vars = variables(&setup)?;
    let grad = gradient(&setup, &vars);   // exakte ∂Loss/∂p, ein Trace je Var
    let loss = loss_for(&setup);
    println!("loss = {loss:.12e}");        // hohe Präzision für FD-Gegenprobe
    for (w, s) in vars.iter().zip(grad.iter()) {
        println!("sensitivity {} {} value={:.6} dloss_dp={:.9e}",
                 setup.surfaces[w.surface].name, w.key.key(),
                 get_var(&setup.surfaces, *w), s);
    }
    Ok(())
}
```

Die Ausgabe für die Tele-Stellung (gekürzt) sieht so aus:

```
$ optics sensitivity --config assets/zoom_tele.toml
loss = 2.776104844903e-2
sensitivity Front Vorderseite radius value=62.000000 dloss_dp=-2.408954184e-4
sensitivity Front Vorderseite thickness value=6.000000 dloss_dp=-4.526195455e-4
sensitivity Front Vorderseite material value=1.617000 dloss_dp=-2.512587103e-2
...
```

Die Erweiterung ist getestet: `tests/pipeline.rs::sensitivity_gradient_matches_finite_differences_all_kinds` prüft alle drei Parametertypen (Radius/Dicke/Material) an einem mehrgruppigen, polychromatischen System gegen zentrale Finite Differenzen.

---

## 6. Schritt 1 — Sensitivität messen (rein per Gradient)

Der Python-Treiber ruft je Zoom-Stellung **einmal** `optics sensitivity` auf und erhält alle 20 Sensitivitäten. Das ersetzt den DoE-Sweep aus Tutorial 01 vollständig: **kein Raster, keine Wiederholungen.**

### 6.1 Das Einheiten-Problem und seine Lösung

Die rohe Ableitung `∂Loss/∂p` hat je nach Parameter eine **andere Einheit**: pro mm (Radius, Dicke) oder pro Brechzahl-Einheit (Material). Eine Materialsensitivität von `−0,025` und eine Radiussensitivität von `−0,00024` lassen sich so nicht direkt vergleichen.

Die Lösung: **Skalierung mit realistischen Fertigungstoleranzen.** Wir multiplizieren jede Sensitivität mit der typischen Abweichung ihres Typs:

```
s_p(skaliert) = |∂Loss/∂p| · tol_p
```

Das ergibt die **erwartete Loss-Verschlechterung bei einer typischen Fertigungsabweichung** — für alle Parametertypen dieselbe Einheit, fair vergleichbar. Die verwendeten Toleranzen (bewusst konservativ):

| Typ | Toleranz `tol_p` | Bedeutung |
|---|---|---|
| radius | 0,10 mm | Radius-Fehler beim Schleifen |
| thickness | 0,02 mm | Dicken-/Abstandstoleranz bei Montage |
| material | 0,0010 | Brechzahl-Streuung der Glascharge |

---

## 7. Schritt 2 — Toleranz-Ranking (und wie es über den Zoom wandert)

Sortiert man die skalierten Sensitivitäten nach ihrem **worst-case** über die Zoom-Stellungen, ergibt sich das Ranking der kritischsten Bauteile (Auszug aus `tolerance_ranking.csv`):

| Rang | Bauteil [Parameter] | wide | mid | tele | worst @ |
|---|---|---|---|---|---|
| 1 | Front Vorderseite [material] | **4,31e-5** | 3,11e-5 | 2,51e-5 | wide |
| 2 | Variator Vorderseite [material] | **3,75e-5** | 2,49e-5 | 1,56e-5 | wide |
| 3 | Kompensator Vorderseite [radius] | **3,59e-5** | 1,42e-5 | 2,77e-5 | wide |
| 4 | Relais Rückseite [thickness] | 1,36e-5 | 0,98e-5 | **2,94e-5** | tele |
| 5 | Kompensator Rückseite [radius] | **2,94e-5** | 1,23e-5 | 2,65e-5 | wide |
| 6 | Front Vorderseite [radius] | 1,22e-5 | 0,24e-5 | **2,41e-5** | tele |

Zwei Beobachtungen, die der Prompt vorhersagt:

1. **Die Gläser sind am kritischsten.** Die größten Empfindlichkeiten sind Brechzahl-Fehler der vorderen Gruppen (Front, Variator). Für die Fertigung heißt das: bei diesen Elementen ist die Glasauswahl / Chargenkontrolle am wichtigsten.

2. **Das Ranking wandert über den Zoombereich.** Manche Parameter (Materialien) sind bei **wide** am empfindlichsten, andere (Relais-Dicke, Front-Radius) klettern bei **tele** nach oben. Ein Design kann in einer Stellung robust und in einer anderen heikel sein — genau deshalb misst man **pro Zoom-Stellung**. Herstellbarkeit richtet sich nach der **schlechtesten** Stellung.

Das vollständige, maschinenlesbare Profil steht in `results/sensitivity_profile.csv` (roh **und** skaliert, je Parameter je Stellung).

---

## 8. Validierung — Autodiff gegen Finite Differenzen

Bevor wir den Gradienten trauen, prüfen wir ihn. Für die (nach Betrag) größten Sensitivitäten jeder Stellung berechnet der Treiber eine **zentrale Finite Differenz** der `loss`-Ausgabe und vergleicht:

```
numerisch = (loss(p+e) − loss(p−e)) / (2e)
```

Wichtig ist hier die **Präzision**: die menschenlesbare `trace`-Ausgabe rundet den `loss` auf 6 Nachkommastellen — viel zu grob, die Differenz zweier gerundeter Werte verschwindet. Deshalb liest der Treiber den `loss` aus der `sensitivity`-Ausgabe in `%.12e`-Genauigkeit.

Ergebnis:

```
Validierung — Autodiff gegen zentrale Finite Differenzen:
  wide: max. rel. Fehler 2.17e-07 über 6 Parameter
  mid : max. rel. Fehler 1.34e-06 über 6 Parameter
  tele: max. rel. Fehler 4.16e-07 über 6 Parameter
  -> größter relativer Fehler gesamt: 1.34e-06  [BESTANDEN]
```

Der relative Fehler liegt bei rund **10⁻⁶** — die Autodiff-Sensitivitäten sind belastbar. (Das entspricht dem Rust-Test `gradient_matches_finite_differences`.)

---

## 9. Schritt 3 — Robustheit verbessern (DoE außen, Gradient innen)

### 9.1 Die Kennzahl

Die **Robustheit einer Zoom-Stellung** fassen wir als Wurzel der Summe der quadrierten skalierten Sensitivitäten (RSS = Root Sum of Squares):

```
Kennzahl = sqrt( Σ_p ( |∂Loss/∂p| · tol_p )² )
```

Das ist die erwartete Loss-Verschlechterung, wenn alle Toleranzen unabhängig zuschlagen. **Klein = robust.** Die **Gesamtkennzahl** ist der worst-case über die drei Stellungen.

Der Clou: Diese Kennzahl wird an **jedem** untersuchten Punkt **gradientenbasiert** (per Autodiff) berechnet — der DoE-Sweep steuert nur, *welche* Designs untersucht werden.

### 9.2 Warum hier eine Rastersuche und kein Gradientenabstieg?

Die Kennzahl ist selbst schon ein Gradient (eine Sensitivität). Sie *nach den Designvariablen* abzuleiten, um bergab zu laufen, wäre eine **zweite Ableitung** — die der Forward-Mode-Solver nicht direkt liefert (Abschnitt 4.2). Also: **äußere Rastersuche** über die freien Variablen, **innere Kennzahl per Gradient**.

### 9.3 Die freien Variablen und der „naive Entwurf"

Als freie Variablen wählen wir die beiden **Kompensator-Luftspalte g2 und g3** — sie sind in der Fertigung tatsächlich justierbar (Montage-/Nachführlage) und verschieben die Kamera nicht. Als **Ausgangszustand („vorher")** nehmen wir einen *naiven Entwurf*: g2/g3 gegenläufig um 2,5 mm aus der guten Lage verstellt — so, wie ein Konstrukteur die Nachführung zunächst grob ansetzt.

Der Treiber legt dann je Stellung ein **13×13-Raster** über (g2, g3), berechnet an jedem Punkt die Kennzahl per Gradient und wählt das Minimum. Ein **Vignettierungs-Schutz** verwirft Kandidaten, die mehr Strahlen abschatten als das Referenzdesign — sonst könnte die Summen-Kennzahl fälschlich durch Abschattung „besser" werden statt durch echte Unempfindlichkeit.

### 9.4 Ergebnis

```
  wide: Entwurf g2=23.019 g3=23.243 Kennzahl=0.064688  ->  best g2=20.519 g3=25.743 Kennzahl=0.000079
  mid : Entwurf g2=22.491 g3=15.205 Kennzahl=0.054620  ->  best g2=19.991 g3=17.705 Kennzahl=0.000046
  tele: Entwurf g2=22.562 g3= 5.343 Kennzahl=0.073298  ->  best g2=20.062 g3= 7.843 Kennzahl=0.000072

  worst-case Kennzahl vorher (naiver Entwurf): 0.073298
  worst-case Kennzahl nachher (DoE-optimiert): 0.000079
  Verbesserung: Faktor 923.769x
```

Und — entscheidend — **ohne die Nennschärfe zu opfern**:

```
  Kontrolle: Nennschärfe/Parfokalität des verbesserten Designs:
    wide: loss 0.01709 -> 0.01709  defocus -0.480 -> -0.480 mm  (24/30 Strahlen)
    mid : loss 0.01326 -> 0.01326  defocus -0.396 -> -0.396 mm  (24/30 Strahlen)
    tele: loss 0.02776 -> 0.02776  defocus -0.472 -> -0.472 mm  (30/30 Strahlen)
```

**Interpretation:** Die gradientengeführte Rastersuche findet aus dem naiven Entwurf heraus die Montagelage, die *gleichzeitig* am scharfsten **und** am unempfindlichsten ist — hier fallen beide zusammen, weil die Empfindlichkeit nahe des scharfen Optimums minimal wird. Das robuste Design (`results/zoom_*_robust.toml`) ist damit sauber begründet, nicht geraten. Die worst-case-Kennzahl fällt um mehr als drei Größenordnungen; der große Faktor spiegelt vor allem wider, wie schlecht der bewusst detunte Entwurf war.

### 9.5 Toleranzbudget — und eine ehrliche Grenze

Aus den Sensitivitäten lässt sich ein einfaches **Toleranzbudget** ableiten: Wie viel darf ein einzelner Parameter abweichen, bevor er den `loss` um mehr als eine Vorgabe (hier 0,02) verschlechtert? Erste-Ordnung-Schätzung: `erlaubte Abweichung = budget / |s_p|`.

Für die kritischsten Materialien ergibt das erlaubte Brechzahl-Abweichungen von ~0,46 … 0,84 — **physikalisch unrealistisch groß** (echtes Glas streut um ~0,001). Das ist **kein Fehler, sondern eine ehrliche Grenze der ersten Ableitung**: Nahe einem gut korrigierten Optimum ist die *Steigung* klein, die *Krümmung* (zweite Ableitung) übernimmt. Ein belastbares Budget bräuchte hier den quadratischen Term — genau die zweite Ableitung, die der Forward-Mode nicht liefert. Wir weisen die Budgetzahlen deshalb nur als **optimistische Orientierung** aus (`summary.json → tolerance_budget_sample`) und benennen die Grenze offen.

---

## 10. Reproduzieren

```sh
# 1) Solver bauen (inkl. neuem Unterbefehl `sensitivity`)
cd ../../source6 && cargo build --release && cargo test --release

# 2) Analyse fahren (reine Python-Standardbibliothek)
cd ../tutorial/02_zoom_teleskop && python3 tolerance_analysis.py
```

Erzeugte Artefakte in `results/` (per `.gitignore` ausgeschlossen, weil reproduzierbar):

| Datei | Inhalt |
|---|---|
| `sensitivity_profile.csv` | `s_p` je Parameter je Zoom-Stellung (roh + skaliert). |
| `tolerance_ranking.csv` | Parameter nach worst-case-Empfindlichkeit sortiert, je Stellung. |
| `robustness_before_after.json` | Kennzahl vorher/nachher + verbesserte g2/g3-Werte. |
| `summary.json` | Maschinenlesbare Gesamt-Zusammenfassung inkl. FD-Gegenprobe und Budget. |
| `zoom_{wide,mid,tele}_robust.toml` | Die robusten, weiterhin scharfen Prescriptions. |

Alle Zahlen in diesem Bericht stammen aus genau diesem Lauf.

---

## 11. Kernaussage des Tutorials

> Herstellbarkeit ist eine Frage der **Fehlerempfindlichkeit**: Ein gutes Design hält nicht nur die Nennschärfe hoch, sondern die **Sensitivitäten** gegen Fertigungstoleranzen klein. Der differenzierbare Solver liefert diese Sensitivitäten **exakt und gradientenbasiert** (`∂Loss/∂p`), ganz ohne Parameter-Sweep — pro Zoom-Stellung, sodass man das kritischste Bauteil (hier: die Gläser der vorderen Gruppen) sofort erkennt und das Ranking über den Zoombereich wandern sieht. Nur die *Verbesserung* der Robustheit stößt an die Grenze der zweiten Ableitung und wird dort durch eine schlanke, gradientengeführte Rastersuche ergänzt. Damit erweitert dieses Tutorial den Werkzeugkasten aus Tutorial 01 von „bestes Nennergebnis" zu „bestes **herstellbares** Ergebnis".
