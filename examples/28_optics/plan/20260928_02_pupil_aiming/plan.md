# Plan: Pupil-Aiming im `optics`-Tracer + Tutorial 02 auf einem echten Patent-Zoom

> **Auftrag an den ausführenden Agenten.** Dieses Dokument ist eine
> Arbeitsanweisung. Es beschreibt *was* zu tun ist und *warum*, mit den bereits
> experimentell verifizierten Fakten, damit du nicht bei null anfängst. Setze es
> von einem sauberen Stand aus um (der Tracer-Code ist aktuell **unverändert**
> gegenüber dem letzten Commit — frühere Prototyp-Änderungen wurden bewusst
> zurückgesetzt).

---

## 0. Kontext in drei Sätzen

Tutorial 02 (`tutorial/02_zoom_teleskop/`) demonstriert eine gradientenbasierte
Toleranz-/Sensitivitätsanalyse an einem Zoom-Teleskop. Das bisherige
Eigendesign erreicht nur einen mageren Zoomfaktor (~1,17×). Ein Recherche-Agent
hat ein reales, abgelaufenes Patent gefunden — **Canon US 5,146,366 A**, ein
vierteiliges, rein sphärisches Zoom mit exakt unserer Architektur (Front fest /
Variator / Kompensator / Relais) und **~5,7× Zoomfaktor** — und die
Prescription bereits als TOML abgelegt. Das Weitwinkelende dieses Designs
tracet in unserem vereinfachten Solver aber **nicht sauber**, weil dem Tracer
das **Pupil-Aiming** fehlt.

---

## 1. Ziel

1. Den `optics`-Ray-Tracer um **Feldwinkel** und **Pupil-Aiming** erweitern,
   sodass schräge Feldbündel die Aperturblende durchlaufen statt zu
   vignettieren/total zu reflektieren.
2. Damit das **Canon-Patent-Zoom (US 5,146,366)** über den vollen Bereich
   (wide/mid/tele) sauber und parfokal tracen lassen.
3. Tutorial 02 auf dieses Patent-Design umstellen (Prescriptions, Treiberlauf,
   Bericht) — die gradientenbasierte Toleranzanalyse-Methodik bleibt dieselbe.

---

## 2. Verifizierte Ausgangslage (nicht geraten — gemessen)

Alle folgenden Zahlen wurden bereits mit dem gebauten Binary geprüft:

- **Patent-Design tracet mid/tele sauber:**
  - `mid`: EFL 2,50; back focus z 9,58; defocus −0,22; 27/75 Strahlen an.
  - `tele`: EFL 5,71; back focus z 9,58; defocus −0,22; 75/75 Strahlen an.
  - Verhältnis EFL(tele)/EFL(mid) = 2,28×; die Patent-Nominalwerte F = 1,0 /
    2,5 / 5,7 skalieren 1:1 mit der gemessenen EFL → voller Bereich **5,7×**.
  - back focus mid ≈ tele (9,58) → **parfokal**.
- **Weitwinkelende (`wide`) versagt** mit dem aktuellen Tracer:
  - on-axis-Bündel: nur **3/75** Strahlen an, EFL nicht berechenbar.
  - Ursache-Nachweis: Bei **abgeschalteter Blende** (`stop = false`) liefert
    `wide` **EFL 0,988** (= Patent-F 1,0) und 39/75 Strahlen → das Design ist
    korrekt, die **Blende** vignettiert das axiale Bündel.
  - Mit korrektem Fokus (letzter Luftspalt ~0,70 statt Platzhalter 0,5) und
    kleiner Arbeitspupille tracet `wide` on-axis **loss 4e-6, 75/75** — also
    perfekt, sobald richtig fokussiert und die Blende nicht überfüllt wird.
- **Der Platzhalter-Backfocus (0,5)** in den wide/mid/tele-TOMLs ist **kein
  Patentwert**. mid/tele lösen per Gradientenabstieg zu ~0,70/0,71; `wide` muss
  ebenso gelöst werden (geht erst nach Pupil-Aiming zuverlässig).

**Schlussfolgerung:** Das Weitwinkelproblem ist eine **Modell-Limitierung** des
Tracers (starres axiales Bündel, keine Blenden-Anzielung), kein Design-Fehler.
Reale Codes (Zemax/Code V) lösen das per **Pupil-Aiming** pro Feld.

### Relevante Dateien
- Tracer: `source6/src/05_trace.rs` (Bündelerzeugung `trace_dual`, Einzelstrahl
  `trace_ray`, Blenden-Vignettierung über `s.stop`), `source6/src/03_ray.rs`
  (Schnitt/Refraktion, alle `Dual`), `source6/src/04_system.rs` (`SourceCfg`,
  `bundle`, `pupil_radius`), `source6/src/06_optimize.rs` (`spot_loss`,
  `gradient`).
- Patent-Prescription (bereits vorhanden, **ungetestet lauffähig**):
  `source6/assets/zoom_us5146366_{wide,mid,tele}.toml`, Test-Stub
  `source6/tests/zoom_us5146366.rs`.
- Recherche-Agent: `.kiro/agents/patent-scout.md` (für Rückfragen zum Patent).

---

## 3. Aufgabe A — Feldwinkel-Unterstützung (Voraussetzung fürs Aiming)

1. In `SourceCfg` (`04_system.rs`) ein optionales Feld
   `field_angles_deg: Vec<f64>` mit Default `[0.0]` ergänzen (serde-Default, wie
   `wavelengths`). Default `[0.0]` **muss** das heutige Verhalten exakt
   reproduzieren, damit alle Bestandstests grün bleiben.
2. `IntersectionResult` (`05_trace.rs`) um `field_deg: f64` erweitern; in
   `trace_ray` überall mit `0.0` initialisieren, in `trace_dual` den echten
   Feldwinkel einstempeln.
3. In `trace_dual` über alle Feldwinkel iterieren: Bündelrichtung um den Winkel
   in der y-z-Ebene kippen (`dir = (0, sinα, cosα)`), Bündelstart entsprechend
   zurückversetzen.
4. `spot_loss` (`06_optimize.rs`) auf **gruppenweise Zentrierung** umstellen:
   Loss = Summe der quadrierten Abstände jedes angekommenen Strahls vom
   **Schwerpunkt seiner (Wellenlänge, Feldwinkel)-Gruppe**. Begründung: ein
   off-axis-Feld landet erwartungsgemäß neben der Achse; gewertet wird die
   Spot-**Größe** (Aberration), nicht die Feldlage. Bei einem einzelnen
   on-axis-Feld (Schwerpunkt ~0) bleibt das Verhalten praktisch identisch.

> Hinweis: Diese vier Punkte wurden bereits prototypisch umgesetzt und ließen
> **alle 50 Bestandstests** grün — sie sind risikoarm. Der Prototyp wurde
> zurückgesetzt; implementiere sie sauber neu.

---

## 4. Aufgabe B — Pupil-Aiming (der Kern)

**Idee:** Vor dem eigentlichen (differenzierbaren) Trace pro Feldwinkel eine
**primale** (reine `f64`, nicht-differenzierbare) Vorab-Lösung, die das Bündel
so positioniert, dass es die Blende trifft.

1. **Blende lokalisieren:** erster Surface-Index mit `s.stop == true`; hat das
   System keine Blende oder ist der Feldwinkel 0, ist das Aiming die Identität.
2. **Primaler Trace bis zur Blende** (`primal_pos_at_stop`): einen Einzelstrahl
   in reiner `f64`-Arithmetik durch die Flächen bis zur Blende verfolgen und
   seine transversale (x, y)-Lage an der Blende zurückgeben; `None`, wenn er
   vorher verloren geht (Miss/TIR). **Nicht** über `Dual` — dieser Helfer dient
   nur der Geometrie-Anzielung und darf die Autodiff-Kette nicht berühren.
3. **Hauptstrahl-Anzielung (chief ray):** per Sekanten-/Bisektionsverfahren den
   Eintritts-Offset `ey` des Pupillenzentrumsstrahls so lösen, dass der
   Hauptstrahl an der Blende bei y ≈ 0 landet (x bleibt on-axis 0). ~40
   Iterationen, robuster Fallback bei verlorenem Strahl.
4. **Fächer-Skalierung (marginal aiming):** den Pupillenfächer-Radius mit einem
   Faktor ≤ 1 multiplizieren und ihn so lange verkleinern (z. B. ×0,85 je
   Schritt), bis die vier Extremstrahlen (±x, ±y am Pupillenrand) die Blende
   innerhalb ihres Klarradius erreichen — dann vignettiert der Fächer nicht mehr.
5. **Anwendung:** die gefundenen Offsets/Skalen als **`Dual::constant`**-Eingang
   in die Bündel-Origins einsetzen (genau wie heute die festen Pupillen-Sample-
   Punkte). Dadurch bleibt der Gradient nach den Linsenparametern korrekt — das
   Aiming ist ein fester geometrischer Setup-Schritt pro Auswertung, keine vom
   Design abhängige differenzierbare Größe.

**Wichtige Sorgfaltspunkte:**
- Der primale Schnitt/Refraktions-Code muss die **gleichen Konventionen** wie
  `03_ray.rs` verwenden (Kugelzentrum `z0 + R`, Normale gegen den Strahl, TIR →
  `None`, `T_EPS`/`PLANAR_EPS`). Am besten die Formeln aus `03_ray.rs`
  spiegeln.
- Aiming ist **primal**, der eigentliche Trace bleibt `Dual`. Keine
  Doppelzählung, kein Gradient durchs Aiming.
- Numerische Robustheit: Sekante mit Bracket-Fallback, Iterationslimits,
  degenerierte Nenner abfangen.

---

## 5. Aufgabe C — Tests

1. **Alle Bestandstests müssen grün bleiben** (`cargo test --release`): 41 unit
   + designs + pipeline + doctests. Der Default `[0.0]` garantiert das.
2. Neuer Test: **on-axis-Äquivalenz** — mit `field_angles_deg = [0.0]` ist der
   Loss identisch (bis auf die winzige Zentroid-Verschiebung) zum bisherigen
   `sum(x²+y²)`.
3. Neuer Test: **Pupil-Aiming wirkt** — für das Patent-Design am Weitwinkelende
   mit mehreren Feldwinkeln steigt die Zahl angekommener Strahlen deutlich
   gegenüber on-axis-only (Referenz: ohne Aiming nur 3/75; Ziel: ein großer
   Teil der Feld-/Pupillenstrahlen erreicht das Bild).
4. `source6/tests/zoom_us5146366.rs` reparieren/erweitern: prüfe monotone
   EFL-Zunahme wide→mid→tele und Parfokalität (nahezu konstanter back focus).
   Der bestehende Stub schlägt aktuell am Weitwinkel-EFL fehl — nach Aiming +
   korrektem Fokus muss er bestehen.

---

## 6. Aufgabe D — Patent-Prescriptions finalisieren

1. In `zoom_us5146366_{wide,mid,tele}.toml`:
   - sinnvolle **Feldwinkel** ergänzen (z. B. gestaffelt nach Patent-Halbfeld:
     wide großes 2ω, tele kleines; Werte aus dem Patent 2ω = 45,24°…8,36°
     ableiten, halbes Feld verwenden).
   - eine realistische **Arbeitspupille** wählen (Patent F/2,0 → grid_radius
     0,25 in Patenteinheiten; ggf. etwas kleiner, damit die Blende der
     limitierende Faktor bleibt statt der ersten Fläche).
   - den **letzten Luftspalt (back focus) je Stellung per Gradientenabstieg
     lösen** (nicht der Platzhalter 0,5!), sodass alle drei Stellungen
     scharf und parfokal sind. Verfahren wie in Tutorial 01/02: letzten
     `thickness` als `optimize = ["thickness"]` flaggen, `optics optimize`
     laufen lassen, gelösten Wert eintragen.
2. Verifizieren: `optics trace` (viele Strahlen an, kleiner loss),
   `optics efl` (EFL-Reihe ~1,0/2,5/5,7 im Patentmaßstab, konstanter back
   focus).

---

## 7. Aufgabe E — Tutorial 02 auf das Patent-Design umstellen

1. Die Patent-TOMLs nach `tutorial/02_zoom_teleskop/assets/` übernehmen
   (die bisherigen Eigendesign-TOMLs `zoom_{wide,mid,tele}.toml` ersetzen oder
   klar als „naiv/klein" kennzeichnen — Entscheidung im Bericht begründen).
2. `tolerance_analysis.py` prüfen/anpassen: die Flächennamen der freien
   Variablen und der Toleranzparameter an das neue, 27-flächige Design
   anpassen. Die gradientenbasierte Sensitivitäts-Methodik, das Toleranz-
   Ranking, die FD-Gegenprobe und die DoE-Robustheitsverbesserung bleiben
   strukturell gleich. Reine Standardbibliothek beibehalten.
3. Treiber laufen lassen, Artefakte prüfen (`sensitivity_profile.csv`,
   `tolerance_ranking.csv`, `robustness_before_after.json`, `summary.json`).
4. `Bericht.md` aktualisieren:
   - echtes Patent nennen (US 5,146,366 A, abgelaufen; Quelle:
     https://patents.google.com/patent/US5146366A/en), **quelltreu**, ohne
     lange Wörtliche Übernahmen; Lizenz-/Zitatregeln beachten.
   - den **Zoomfaktor 5,7×** als reale Kennzahl darstellen.
   - **Pupil-Aiming** als neue Solver-Fähigkeit erklären (Laien-tauglich:
     Hauptstrahl trifft Blendenmitte, Fächer füllt die Blende) samt Mermaid-
     Diagramm.
   - ehrlich benennen, was der vereinfachte Tracer weiterhin *nicht* kann
     (z. B. echte Vignettierungs-Kurven, Verzeichnung, sagittale/tangentiale
     Trennung), damit keine Überinterpretation entsteht.

---

## 8. Aufgabe F — Endabnahme & Commit

1. `cd source6 && cargo build --release && cargo test --release` — **alles
   grün**.
2. `cd tutorial/02_zoom_teleskop && python3 tolerance_analysis.py` — exit 0,
   Artefakte erzeugt, Zahlen im Bericht stimmen mit der Ausgabe überein.
3. `results/` und `__pycache__/` bleiben per `.gitignore` ausgeschlossen.
4. **Nur** die zu dieser Aufgabe gehörenden Dateien committen (Tracer-Quellen,
   Patent-Assets, Tests, Tutorial-Dateien). Die nicht verwandte Datei
   `examples/26_onnx/source6/inference.yml` **nicht** anfassen. Aussagekräftige
   Commit-Message.

---

## 9. Randbedingungen (verbindlich)

- **Autodiff nicht brechen:** Aiming primal, eigentlicher Trace `Dual`. Die
  bestehende FD-Gegenprobe (`gradient_matches_finite_differences`) und die
  Tutorial-FD-Validierung müssen bestehen bleiben.
- **Rückwärtskompatibel:** Default `field_angles_deg = [0.0]` reproduziert das
  bisherige Verhalten; keine Bestandstests dürfen brechen.
- **Reine Standardbibliothek** für den Python-Treiber.
- **Quelltreue** bei Patentzahlen; keine erfundenen optischen Werte; Back-Focus
  ist per Solver zu lösen, nicht zu raten.
- **Ehrlichkeit im Bericht** über verbleibende Modellgrenzen des Tracers.

---

## 10. Definition of Done

- [ ] Feldwinkel + Pupil-Aiming im Tracer implementiert, primal-aimend,
      differenzierbar-tracend.
- [ ] Alle `cargo test`-Suites grün (inkl. neuer Aiming-Tests und repariertem
      `zoom_us5146366.rs`).
- [ ] Patent-Zoom tracet wide/mid/tele sauber & parfokal; EFL-Reihe ~5,7×.
- [ ] Tutorial 02 nutzt das Patent-Design; Treiber läuft reproduzierbar; alle
      Artefakte erzeugt; Berichtszahlen stimmen.
- [ ] Bericht erklärt Patent + Pupil-Aiming laientauglich und benennt Grenzen.
- [ ] Sauberer, fokussierter Commit.
