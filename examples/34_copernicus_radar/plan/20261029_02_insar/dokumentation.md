# Architektur- und Konzeptdokumentation: Sentinel-1C IW TOPS-Interferometrie (InSAR) in `source2` (`sar_source2`)

Diese Dokumentation beschreibt von den ersten physikalischen Prinzipien bis zur konkreten GPU-Implementierung (`cuda-oxide` + `cuFFT`), wie der in `../20261029_01_implement/dokumentation.md` vorgestellte Sentinel-1-Level-0-Prozessor `source2` (`sar_source2`) zu einem vollständigen, phasenerhaltenden **TOPS-Interferometrie-Prozessor (InSAR)** erweitert wurde.

Als durchgehendes Fallbeispiel und reales Validierungsszenario dient das **12-Tage-Repeat-Pass-Datensatzpaar** des Satelliten **Sentinel-1C** im Modus **Interferometric Wide Swath (IW, Level-0 RAW, VV-Polarisation)** über **Süd- und Zentral-Taiwan** vom **28. September 2026** (Master) und **10. Oktober 2026** (Slave).

Das Dokument ist so aufgebaut, dass man es sowohl als Lehrbuch-Einführung in die TOPS-Interferometrie als auch als präzises Architektur-Handbuch für den Rust-/CUDA-Code in `source2/src/` lesen kann:
- **Kapitel 1 & 2** erklären anschaulich, warum die Phase eines Radarechos ein sub-zentimetergenaues Maßband für Topographie und Bodenbewegung ist und wie sich die interferometrische Phase aus Orbit-Basislinie, Erdkrümmung (`Flat Earth`), Geländehöhe (`Topography`), Deformation und Atmosphäre zusammensetzt.
- **Kapitel 3** zeigt, warum die im 1-Sekunden-Takt übertragenen Bord-Ephemeriden bei naiver Interpolation künstliche Phasenwellen von über $30\text{ rad}$ erzeugen und wie unser gekoppelter **Position-Geschwindigkeits-Polynomfit (`eval_smooth_orbit_poly`)** sowie die **rechtsschauende Zero-Doppler-WGS-84-Inversion (`solve_zero_doppler_target_ecef`)** dieses Problem bis auf Maschinengenauigkeit lösen.
- **Kapitel 4** widmet sich dem Herzstück von TOPS-InSAR: Warum das elektronische Antennenschwenken ($k_t \approx 4{,}5\dots 6{,}5\text{ kHz/s}$) jede herkömmliche Sub-Pixel-Interpolation zerstören würde und wie unsere **Baseband-Erhaltung** mit gemeinsamer Master-Steuerrate (`focus_tops_burst_insar_cpu` / `focus_tops_burst_insar_gpu`) Phasenkohärenz über alle 36 Bursts garantiert.
- **Kapitel 5 bis 7** begleiten ein Burst-Paar durch die **hybride Sub-Pixel-Koregistrierung**, das kombinierte **GPU-Resampling mit analytischem Flat-Earth-Abzug (`insar_resample_and_ifg_kernel`)**, das **Enhanced-Spectral-Diversity-Debursting (ESD)**, die **phasengradienten-kompensierte Kohärenzschätzung**, das **adaptive Goldstein-Werner-Spektralfilter** und das **zweistufige 2D Phase Unwrapping**.
- **Kapitel 8 & 9** präsentieren die Messergebnisse, Laufzeiten und geophysikalischen Beobachtungen auf der Süd-Taiwan-Szene sowie die Modul-, Test- und CLI-Referenz.

---

## Kapitel 1: Einführung & Motivation — Von der SAR-Amplitude zur Interferometrie (InSAR)

### 1.1 Was steckt im komplexen Pixel eines fokussierten SAR-Bildes?

In der ersten Ausbaustufe von `source2` (`../20261029_01_implement/dokumentation.md`) haben wir aus den rohen CCSDS-Weltraumpaketen ein fokussiertes **Single Look Complex (SLC)**-Bild berechnet und am Ende dessen **Betragsquadrat (Intensität)** $|z(a, r)|^2 = \text{Re}(z)^2 + \text{Im}(z)^2$ als Graustufenbild dargestellt.

Jedes fokussierte Pixel $(a, r)$ einer SAR-Aufnahme ist jedoch eine komplexe Zahl
$$z(a, r) = A(a, r) \cdot \exp\!\big(j\,\phi(a, r)\big) \in \mathbb{C}$$
mit zwei völlig unterschiedlichen Informationskanälen:
1. **Die Amplitude $A(a, r) = |z(a, r)|$** verrät uns, *wie stark* der Boden die Radarwelle zurückstreut (Rauigkeit, Feuchtigkeit, Neigung zum Satelliten, Schiffe, Häuserkanten). Ihre räumliche Auflösung wird durch die Chirp-Bandbreite ($B_{\text{rg}} \approx 42\dots 56\text{ MHz} \implies \Delta r \approx 2{,}7\dots 3{,}5\text{ m}$) und die synthetische Apertur ($\Delta a \approx 14\text{ m}$ bei TOPS) bestimmt.
2. **Die Phase $\phi(a, r) = \arg(z(a, r)) \in [-\pi, +\pi]$** zählt die exakten Wellenberge und Wellentäler der C-Band-Trägerwelle ($f_c = 5{,}405\text{ GHz}$, Wellenlänge $\lambda = \frac{c}{f_c} \approx 5{,}546576\text{ cm}$) auf dem über $800\text{ km}$ langen Weg vom Satelliten zum Boden und wieder zurück:
   $$\phi(a, r) = -\frac{4\pi}{\lambda}\,R(a, r) + \phi_{\text{scat}}(a, r) \pmod{2\pi}$$

Betrachtet man die Phase $\phi(a, r)$ eines *einzelnen* SAR-Bildes, sieht man nur scheinbar wertloses weißes Rauschen zwischen $-\pi$ und $+\pi$. Warum?
- In einer einzigen Auflösungszelle von z. B. $3\text{ m} \times 14\text{ m}$ befinden sich hunderte Einzelstreuer (Steine, Blätter, Dachziegel, Wellenkämme) in zufälligen Mikro-Entfernungen. Ihre Überlagerung erzeugt eine zufällige **Streuphase $\phi_{\text{scat}}(a, r)$** (den bekannten *Speckle*-Effekt).
- Zudem entspricht schon eine Änderung der Schrägentfernung $R$ um eine halbe Wellenlänge ($\frac{\lambda}{2} \approx 2{,}77\text{ cm}$) einer vollen Phasendrehung von $2\pi$ ($360^\circ$). Über $850\text{ km}$ Abstand hat die Welle rund $30{,}6\text{ Millionen}$ Wellenlängen durchlaufen!

```text
Einzelnes SAR-Bild (SLC):                  Interferometrisches Paar (Master M & Slave S):

Satellit M (28.09.2026)                    Satellit M (28.09.) ◄─── Basislinie B ≈ 17,0 m ───► Satellit S (10.10.)
       │                                            \                                               /
       │ R_M = 850.000,000 m                         \  R_M                                    R_S /
       ▼                                              \                                           /
  ┌─────────┐                                          ↘                ┌─────────┐              ↙
  │ Pixel P │  φ_M = -4π/λ·R_M + φ_scat                 ──────────────► │ Pixel P │ ◄────────────
  └─────────┘  (φ_scat ist zufällig!)                                   └─────────┘
                                                        Interferogramm I = M · S* :
                                                        φ_scat kürzt sich exakt heraus!
                                                        Δφ = φ_M - φ_S = +4π/λ · (R_S - R_M)
```

Das Bild ändert sich schlagartig, sobald derselbe Satellit (oder ein Zwillingssatellit) das Gebiet nach einigen Tagen aus nahezu derselben Bahnposition ein zweites Mal aufnimmt (**Repeat-Pass-Interferometrie**):
- Solange sich die Mikro-Anordnung der Rückstreuer am Boden (Gebäude, Felsen, Straßen, trockene Felder) zwischen der ersten Aufnahme (**Master $M$**) und der zweiten Aufnahme (**Slave $S$**) nicht wesentlich verändert hat, ist die zufällige Streuphase in beiden Bildern **identisch**: $\phi_{\text{scat},M}(a, r) \approx \phi_{\text{scat},S}(a, r)$.
- Multiplizieren wir das komplexe Master-Pixel mit dem komplex konjugierten Slave-Pixel,
  $$I(a, r) = M(a, r) \cdot S^*(a, r) = |M|\,|S|\,\exp\!\big(j\,(\phi_M - \phi_S)\big),$$
  so **fällt die unbekannte Streuphase $\phi_{\text{scat}}$ exakt heraus**, und es bleibt die reine geometrische Zwei-Wege-Wegdifferenz auf Bruchteile eines Millimeters genau übrig:
  $$\Delta\phi(a, r) = \phi_M(a, r) - \phi_S(a, r) = -\frac{4\pi}{\lambda} R_M + \frac{4\pi}{\lambda} R_S = +\frac{4\pi}{\lambda}\big(R_S(a, r) - R_M(a, r)\big) \pmod{2\pi}$$

Aus einem verrauschten Phasenfeld wird so ein flächenhaftes geodätisches Präzisionsinstrument, das über eine Schwadbreite von $250\text{ km}$ gleichzeitig das **dreidimensionale Geländerelief** und **millimetergenaue Oberflächenverschiebungen** (Tektonik, Grundwasserabsenkung, Hangrutschungen) sichtbar macht.

---

### 1.2 Das Süd-Taiwan Sentinel-1C Repeat-Pass-Datensatzpaar

Für die Entwicklung und Validierung der InSAR-Pipeline in `source2` nutzen wir ein reales **12-Tage-Repeat-Pass-Paar** des neuen Copernicus-Satelliten **Sentinel-1C** über Süd- und Zentral-Taiwan:

| Parameter | Master-Aufnahme (Referenz) | Slave-Aufnahme (Sekundär) |
| :--- | :--- | :--- |
| **SAFE-Produktkennung** | `S1C_IW_RAW__0SDV_20260928T215149_..._009653_01336F_D2EE.SAFE` | `S1C_IW_RAW__0SDV_20261010T215149_..._009828_013950_CEB1.SAFE` |
| **Rohdaten-Datei (`.dat`)** | `s1c-iw-raw-s-vv-20260928t215149-20260928t215222-009653-01336f.dat` | `s1c-iw-raw-s-vv-20261010t215149-20261010t215222-009828-013950.dat` |
| **Aufnahmezeitraum (UTC)** | `2026-09-28 21:51:49` bis `21:52:22` ($\approx 33\text{ s}$) | `2026-10-10 21:51:49` bis `21:52:22` ($\approx 33\text{ s}$) |
| **Zeitliche Basislinie ($\Delta T$)** | $0\text{ Tage}$ (Referenz) | $+12{,}000\text{ Tage}$ ($1\,036\,800\text{ s}$, genau 175 Orbits später) |
| **Flugbahn / Blickrichtung** | Descending (Nord-Nordost $\to$ Süd-Südwest), **Right-Looking** (nach WNW) | Descending (Nord-Nordost $\to$ Süd-Südwest), **Right-Looking** (nach WNW) |
| **Absoluter / Relativer Orbit** | Abs. Orbit `009653` (Rel. Orbit 69 / 105, Zyklus 59) | Abs. Orbit `009828` (Rel. Orbit 69 / 105, Zyklus 60) |
| **CCSDS-Pakete / Ephemeriden** | $51\,774$ Pakete / $33$ Sub-Comm-Zustandsvektoren ($\Delta t \approx 1\text{ s}$) | $51\,774$ Pakete / $33$ Sub-Comm-Zustandsvektoren ($\Delta t \approx 1\text{ s}$) |
| **Sub-Swaths & Burst-Struktur** | IW1 (`swath=10`), IW2 (`swath=11`), IW3 (`swath=12`), je 12 Bursts | IW1 (`swath=10`), IW2 (`swath=11`), IW3 (`swath=12`), je 12 Bursts |
| **Echo-Dimensionen ($N_{\text{az}} \times N_{\text{rg}}$)** | IW1: $1409 \times 23\,880$, IW2: $1548 \times 24\,164$, IW3: $1410 \times 19\,814$ | Exakt identische Burst-Zeilen- und Spaltenzahlen ($1:1$ synchronisiert) |
| **Slant-Range-Abdeckung** | $799{,}282\text{ km} \dots 962{,}774\text{ km}$ ($\Delta R = 163{,}492\text{ km}$) | Auf das Master-Slant-Range-Gitter koregistriert |

Warum ist gerade Süd-Taiwan ein idealer Härtetest für einen InSAR-Prozessor?
1. **Extremer topographischer Kontrast auf engstem Raum**: Innerhalb von nur $50\text{ km}$ Entfernung steigt das Gelände von $0\text{ m}$ an der Westküste (Tainan, Kaohsiung, Pingtung-Ebene) bis auf fast $4\,000\text{ m}$ im **Zentralgebirge (Yushan / Central Mountain Range)** an.
2. **Scharfe Kohärenzgrenzen**: Die Szene enthält gleichzeitig offenes Meer (**Taiwanstraße** im Westen, **Pazifik** im Südosten mit $\gamma \to 0$), dicht bebaute Millionenstädte und Agrarebenen (**Kaohsiung, Tainan, Chiayi, Pingtung, Penghu-Inseln** mit $\gamma \approx 0{,}6\dots 0{,}9$) sowie dicht bewaldetes subtropisches Hochgebirge ($\gamma \approx 0{,}25\dots 0{,}45$). Jeder Fehler in der Koregistrierung, der Orbit-Geolocation oder dem Phase-Unwrapping fällt an den Küstenlinien und Gebirgskämmen sofort auf.

---

### 1.3 Überblick über die InSAR-Erweiterung in `source2`

Die InSAR-Funktionalität fügt sich nahtlos in die bestehende nummerierte Modulstruktur von `source2/src/` ein, ohne die bestehenden Stripmap/TOPS-Amplitudenpfade zu brechen:

```text
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                     Sentinel-1 TOPS-InSAR-Pipeline in `source2/src/`                     │
├──────────────────────┬───────────────────────────────────────────────────────────────────┤
│ 02_l0.rs & 05_ingest │ Paralleles Level-0-FDBAQ-Entpacken für Master & Slave (je 36      │
│                      │ Bursts in IW1..IW3), SWST-Ausrichtung auf gemeinsames Startgate   │
├──────────────────────┼───────────────────────────────────────────────────────────────────┤
│ 03_ephem.rs          │ `eval_smooth_orbit_poly`: Gekoppelter Least-Squares-Polynomfit    │
│                      │ an ECEF-Position P(t) und Doppler-Geschwindigkeit V(t)            │
├──────────────────────┼───────────────────────────────────────────────────────────────────┤
│ 06_rda.rs & 09_gpu   │ `focus_tops_burst_insar_cpu` / `focus_tops_burst_insar_gpu`:      │
│                      │ Gemeinsames Master-Steering (f_dc_mid, k_t) + Baseband-Erhaltung  │
├──────────────────────┼───────────────────────────────────────────────────────────────────┤
│ 11_insar.rs (Teil 1) │ `compute_orbit_baseline`, `solve_zero_doppler_target_ecef`,       │
│                      │ `project_target_to_slave`, `estimate_burst_geometric_model`       │
├──────────────────────┼───────────────────────────────────────────────────────────────────┤
│ 11_insar.rs (Teil 2) │ `refine_burst_coreg_empirical` (Log-Intensitäts-NCC + 1/20-px     │
│ & 09_gpu.rs          │ Kohärenz-Feinsuche) + GPU `insar_resample_and_ifg_kernel`         │
├──────────────────────┼───────────────────────────────────────────────────────────────────┤
│ 11_insar.rs (Teil 3) │ `deburst_insar_swath` (ESD-Phasenabgleich in Burst-Überlappungen) │
│                      │ & `mosaic_insar_swaths` (4-Sub-Bin-Range-Phasenausrichtung)       │
├──────────────────────┼───────────────────────────────────────────────────────────────────┤
│ 11_insar.rs (Teil 4) │ `compute_coherence_map` (2D-Gradienten-Kompensation),             │
│                      │ `goldstein_filter_interferogram` (32×32 2D-FFT, α = 0.65),        │
│                      │ `unwrap_phase_2d` & `unwrap_phase_2d_poisson` + Farb-PNG-Export   │
└──────────────────────┴───────────────────────────────────────────────────────────────────┘
```

---

## Kapitel 2: Physikalische & Geometrische Grundlagen der SAR-Interferometrie

### 2.1 Zerlegung der interferometrischen Phase

Wenn wir das komplexe Interferogramm $I(a, r) = M(a, r) \cdot S_{\text{interp}}^*(a, r)$ bilden, setzt sich die gemessene Phasendifferenz $\Delta\phi = \arg(I) \in [-\pi, +\pi]$ additiv aus fünf physikalischen Termen zusammen:

$$\Delta\phi = \Delta\phi_{\text{flat}} + \Delta\phi_{\text{topo}} + \Delta\phi_{\text{defo}} + \Delta\phi_{\text{atm}} + \Delta\phi_{\text{noise}} \pmod{2\pi}$$

| Phasenterm | Mathematischer Ausdruck | Physikalische Ursache & Größenordnung (Süd-Taiwan) |
| :--- | :--- | :--- |
| **1. Flat-Earth-Phase ($\Delta\phi_{\text{flat}}$)** | $+\frac{4\pi}{\lambda}\big(R_{S,0}(a,r) - R_{M,0}(a,r)\big) \approx -\frac{4\pi B_\perp}{\lambda R \tan\theta}\,\Delta r$ | Geometrische Wegdifferenz auf dem glatten Referenz-Ellipsoid ($h = 0\text{ m}$) infolge des sich über den Schwad ändernden Blickwinkels $\theta$. **Muss rechnerisch exakt abgezogen werden!** |
| **2. Topographische Phase ($\Delta\phi_{\text{topo}}$)** | $-\frac{4\pi B_\perp}{\lambda R \sin\theta_{\text{inc}}}\,h(a, r)$ | Änderung des Blickwinkels durch die Geländehöhe $h$ über dem Ellipsoid. Bei $B_\perp = -5{,}68\text{ m}$ erzeugt $h = 2915{,}6\text{ m}$ genau einen $2\pi$-Fringe. |
| **3. Deformations-Phase ($\Delta\phi_{\text{defo}}$)** | $+\frac{4\pi}{\lambda}\,\Delta d_{\text{LOS}}(a, r)$ | Reale Bodenbewegung entlang der Sichtlinie (Line-of-Sight) zwischen dem 28.09. und 10.10.2026: **Schon $\Delta d_{\text{LOS}} = 2{,}77\text{ cm}$ erzeugt einen vollen $2\pi$-Zyklus!** |
| **4. Atmosphärische Phase ($\Delta\phi_{\text{atm}}$)** | $+\frac{4\pi}{\lambda}\big(\delta R_{\text{tropo/iono},S} - \delta R_{\text{tropo/iono},M}\big)$ | Unterschiedliche Ausbreitungsverzögerung durch räumlich-zeitliche Schwankungen des troposphärischen Wasserdampfs (typisch $0{,}5\dots 2\text{ rad}$ in den Subtropen). |
| **5. Phasenrauschen ($\Delta\phi_{\text{noise}}$)** | Varianz $\sigma_\phi^2 \approx \frac{1 - |\gamma|^2}{2 L |\gamma|^2}$ | Thermisches Rauschen, zeitliche Dekorrelation (Wasser, Vegetation) und Interpolationsfehler. |

---

### 2.2 Das lokale Orbit-Dreibein und die Zerlegung der 3D-Basislinie

Um die Geometrie zwischen dem Master-Überflug $\mathbf{P}_M(t)$ und dem Slave-Überflug $\mathbf{P}_S(t)$ exakt zu beschreiben, errichten wir in `compute_orbit_baseline` (`source2/src/11_insar.rs`) am Punkt der nächsten Annäherung (*Closest Approach*) ein rechtshändiges lokales Koordinatensystem $(\hat{\mathbf{u}}_r, \hat{\mathbf{u}}_v, \hat{\mathbf{u}}_c)$:

$$\hat{\mathbf{u}}_r = \frac{\mathbf{P}_M}{\|\mathbf{P}_M\|} \quad (\text{radial nach außen / Zenit}), \qquad \hat{\mathbf{u}}_v = \frac{\mathbf{V}_M}{\|\mathbf{V}_M\|} \quad (\text{entlang der Flugbahn}), \qquad \hat{\mathbf{u}}_c = \frac{\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_r}{\|\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_r\|} \quad (\text{Cross-Track rechts})$$

```text
Querschnitt in der Zero-Doppler-Ebene (Blick in Flugrichtung u_v):

                    ▲ u_r (Radial / Zenit)
                    │
                    │     ● Slave S: ΔP = B_radial·u_r + B_cross·u_c
                    │    ╱  (B_radial = +9,19 m, B_cross = -14,30 m)
                    ●───╱───────────────────► u_c (Cross-Track nach rechts)
                 Master M  ╲
                    │       ╲  B_perp = -5,68 m (senkrecht zum Sehstrahl l)
                    │   θ    ╲ ╱
                    │ (33,8°) ╳  B_para = -16,02 m (entlang des Sehstrahls l)
                    │          ╲
                    ▼ -u_r      ╲   Sehstrahl l = -cos(θ)·u_r + sin(θ)·u_c
                 (Nadir)         ╲  (R_mid ≈ 881 km)
                                  ╲
                                   ↘     ▲ Topographie h
          ═══════════════════════════════╩═══════════════════ WGS-84-Ellipsoid
                                      Ziel P
```

Der Verbindungsvektor $\mathbf{B} = \mathbf{P}_S(t_{\text{CA}}) - \mathbf{P}_M(t_M)$ im Punkt der nächsten Annäherung (($\mathbf{P}_S(t_{\text{CA}}) - \mathbf{P}_M(t_M)) \cdot \mathbf{V}_M = 0$) besitzt in dieser Ebene die Komponenten:
$$B_{\text{radial}} = \mathbf{B} \cdot \hat{\mathbf{u}}_r, \qquad B_{\text{cross}} = \mathbf{B} \cdot \hat{\mathbf{u}}_c, \qquad B_{3\text{D}} = \|\mathbf{B}\|_2$$
Für das Radar entscheidend ist die Projektion auf den **Einheits-Sehstrahl (Line-of-Sight)** $\hat{\mathbf{l}} = -\cos\theta\,\hat{\mathbf{u}}_r + \sin\theta\,\hat{\mathbf{u}}_c$ unter dem Look-Winkel $\theta \approx 33{,}8^\circ$ (bzw. dem Einfallswinkel am Boden $\theta_{\text{inc}} = \arcsin\!\left(\frac{\|\mathbf{P}_M\|}{R_E}\sin\theta\right) \approx 38{,}5^\circ$):
- **Parallele Basislinie $B_\parallel$** (in Richtung des Sehstrahls $\hat{\mathbf{l}}$):
  $$B_\parallel = \mathbf{B} \cdot \hat{\mathbf{l}} = -B_{\text{radial}} \cos\theta + B_{\text{cross}} \sin\theta \approx -R_S + R_M$$
- **Senkrechte (effektive) Basislinie $B_\perp$** (senkrecht zum Sehstrahl in der Zero-Doppler-Ebene):
  $$B_\perp = B_{\text{radial}} \sin\theta + B_{\text{cross}} \cos\theta$$

Für unser Süd-Taiwan-Paar (`2026-09-28` vs. `2026-10-10`) liefert `compute_orbit_baseline` in der Szenenmitte ($R_{\text{mid}} \approx 881\text{ km}$):
- **$B_{3\text{D}} = 16{,}999\text{ m}$** (extrem enge Bahnregelung der Sentinel-1C Orbitalröhre!)
- **$B_{\text{radial}} = +9{,}188\text{ m}$**, **$B_{\text{cross}} = -14{,}302\text{ m}$**
- **$B_\parallel = -16{,}022\text{ m}$** (der Slave-Orbit ist entlang der Sichtlinie im Mittel rund $16{,}02\text{ m} \approx 5{,}9\text{ Range-Bins}$ weiter vom Ziel entfernt als der Master-Orbit)
- **$B_\perp = -5{,}679\text{ m}$**
- **Closest-Approach-Zeitversatz**: $\Delta t_{\text{CA}} = (t_{\text{CA}} - 12 \cdot 86\,400\text{ s}) - t_M = +0{,}049482\text{ s} = +49{,}48\text{ ms}$

---

### 2.3 Höhenambiguität $h_{2\pi}$, kritische Basislinie und komplexe Kohärenz $|\gamma|$

#### Die Höhenambiguität $h_{2\pi}$

Wie stark ändert sich die interferometrische Phase $\Delta\phi_{\text{topo}}$, wenn ein Berg um $\Delta h$ über der Küstenebene aufragt? Aus der Differenzierung der Laufweggeometrie folgt:
$$\frac{\partial \Delta\phi_{\text{topo}}}{\partial h} = -\frac{4\pi B_\perp}{\lambda R \sin\theta_{\text{inc}}}$$
Diejenige Höhendifferenz, die genau eine volle Phasenperiode $|\Delta\phi_{\text{topo}}| = 2\pi$ (einen **Interferenzstreifen / Fringe**) erzeugt, heißt **Höhenambiguität $h_{2\pi}$**:
$$h_{2\pi} = \frac{\lambda\,R\,\sin\theta_{\text{inc}}}{2\,|B_\perp|}$$
Setzen wir die gemessenen Werte des Süd-Taiwan-Paares ein ($\lambda = 0{,}0554658\text{ m}$, $R_{\text{mid}} \approx 881\text{ km}$, $\sin\theta_{\text{inc}} \approx 0{,}62$, $|B_\perp| = 5{,}679\text{ m}$), erhalten wir:
$$h_{2\pi} = \mathbf{2\,915{,}62\text{ m / Zyklus}}$$

Diese sehr kleine senkrechte Basislinie ($|B_\perp| = 5{,}68\text{ m}$) hat eine hochinteressante physikalische Konsequenz für unsere Szene:
1. Selbst im extrem steilen **Zentralgebirge Taiwans** (Anstieg von $0\text{ m}$ auf $3\,952\text{ m}$ am Yushan) durchläuft die topographische Phase insgesamt nur rund $1{,}0\dots 1{,}4$ volle $2\pi$-Zyklen!
2. Dadurch treten in den steilen Gebirgstälern **kein geometrisches Fringe-Aliasing** und **keine geometrische Basislinien-Dekorrelation** auf (die kritische Basislinie $B_{\perp,\text{crit}} = \frac{\lambda R \tan\theta_{\text{inc}}}{2 \Delta r} \approx 5{,}1\text{ km}$ wird um fast drei Größenordnungen unterschritten).

#### Der komplexe Kohärenzkoeffizient $\gamma$

Das zentrale Qualitätsmaß jedes Interferogramms ist die **komplexe Kohärenz**:
$$\gamma = \frac{\mathbb{E}\!\left[M \cdot S^*\right]}{\sqrt{\mathbb{E}\!\left[|M|^2\right]\,\mathbb{E}\!\left[|S|^2\right]}}, \qquad |\gamma| \in [0, 1]$$
Sie lässt sich als Produkt mehrerer physikalischer und algorithmischer Faktoren verstehen:
$$|\gamma| = \gamma_{\text{thermal}} \cdot \gamma_{\text{geom}} \cdot \gamma_{\text{temporal}} \cdot \gamma_{\text{vol}} \cdot \gamma_{\text{proc}}$$
- Über dem **Ozean** (Taiwanstraße, Pazifik) ändert sich die Wasseroberfläche in Millisekunden: $\gamma_{\text{temporal}} \to 0$.
- Über **Städten, Straßen, Häfen und trockenen Ebenen** (Tainan, Kaohsiung, Penghu) ist $\gamma_{\text{temporal}} \approx 0{,}8\dots 0{,}95$: Ob wir diese hohe Kohärenz im fertigen Bild tatsächlich erreichen, hängt ausschließlich von unserer **Prozessierungs-Kohärenz $\gamma_{\text{proc}}$** (Orbit-Modellierung, Baseband-Fokussierung, Sub-Pixel-Koregistrierung) ab!

---

## Kapitel 3: Hochpräzise Orbit-Modellierung & Rechtsschauende Zero-Doppler-Geolokation

### 3.1 Warum stückweise Interpolation der 1-Hz-Sub-Commutated-Orbits für InSAR nicht ausreicht

In den Level-0-Rohdaten wird alle 64 Pakete (also etwa im **1-Sekunden-Takt**, $\Delta t \approx 1{,}0\text{ s}$) ein neuer GPS-Zustandsvektor $(\mathbf{P}_k, \mathbf{V}_k)$ über die Sub-Commutated Data Words übertragen (insgesamt 33 Punkte über die 33-Sekunden-Szene).

Für ein reines Amplitudenbild reicht eine stückweise lineare oder lokale Hermite-Interpolation zwischen den 1-Hz-Punkten völlig aus. Für die **Interferometrie** erweist sich eine lokale Interpolation jedoch aus zwei Gründen als fatal:

1. **Der Sehnenfehler (Sagitta) einer gekrümmten Kreisbahn**:
   Der Satellit erfährt im Schwerefeld der Erde eine Zentripetalbeschleunigung von $a_c = \frac{V_s^2}{R_{\text{orb}}} \approx \frac{(7590\text{ m/s})^2}{7070\text{ km}} \approx 8{,}15\text{ m/s}^2$ Richtung Erdmittelpunkt. Würde man zwischen zwei Stützstellen im Abstand $\Delta t = 1\text{ s}$ linear interpolieren, weicht die Sehne in der Intervallmitte um die **Sagitta**
   $$\delta r_{\text{sagitta}} = \frac{1}{8}\,a_c\,\Delta t^2 \approx \frac{1}{8} \cdot 8{,}15\text{ m/s}^2 \cdot (1\text{ s})^2 \approx 1{,}02\text{ m}$$
   von der wahren Bahnkurve ab! In Einheiten der C-Band-Phase entspricht $\delta r \approx 0{,}9\dots 1{,}0\text{ m}$ einem Phasenfehler von:
   $$\delta\phi = \frac{4\pi}{\lambda}\,\delta r \approx \frac{12{,}566}{0{,}0555\text{ m}} \cdot 0{,}9\text{ m} \approx \mathbf{204\text{ rad}} \approx \mathbf{32\text{ volle } 2}\pi\mathbf{\text{-Zyklen!}}$$
2. **1-Hz-Positions-Jitter der Bord-PVT-Lösung**:
   Einzelne im 1-Hz-Takt abgestempelte GPS-Positionsworte $\mathbf{P}_k$ weisen im Level-0-Strom kleine Quantisierungs- und Filter-Sprünge von $\pm 0{,}2\dots 0{,}4\text{ m}$ entlang der Bahn auf, während die aus der Trägerphasen-Dopplermessung stammenden Geschwindigkeitsvektoren $\mathbf{V}_k$ deutlich glatter sind. Ein stückweiser kubischer Hermite-Spline, der exakt durch jeden verrauschten Punkt $\mathbf{P}_k$ gezwungen wird, übersetzt diese Sprünge in unphysikalische 1-Hz-Schwingungen der Bahngeschwindigkeit $\dot{\mathbf{P}}(t)$!

```text
Stückweise Interpolation (lokal):                  Gekoppelter globaler Polynomfit (`eval_smooth_orbit_poly`):

Position P(t)                                      Position P(t) & Geschwindigkeit V(t) = dP/dt
  ▲      ●                   ●                       ▲      ●                   ●
  │    ╱   ╲   1-Hz-Welligkeit!                      │    ───────────────────────────  Glatte Keplersche
  │  ●       ● ──────────── ●                        │  ●       ●               ●      Bahnkurve über 33 s!
  └──────────────────────────────► t (1 s Takt)      └──────────────────────────────► t
```

---

### 3.2 Gekoppelter Position-Geschwindigkeits-Polynomfit (`eval_smooth_orbit_poly` in `03_ephem.rs`)

Ein 33 Sekunden kurzer Bogen einer 98-Minuten-Umlaufbahn ($\approx 0{,}56\%$ eines Orbits, Zentriwinkel $\approx 2^\circ$) ist physikalisch eine extrem glatte analytische Kurve. In `source2/src/03_ephem.rs` (`eval_smooth_orbit_poly`) approximieren wir jede ECEF-Koordinate $(X, Y, Z)$ über der normierten Szenenzeit $u = \frac{t - t_{\text{mid}}}{T_{\text{half}}} \in [-1, +1]$ durch ein **4-parametriges Polynom (kubische Potenzbasis $1, u, u^2, u^3$)**, dessen Koeffizienten $\mathbf{c} = (c_0, c_1, c_2, c_3)^T$ **simultan an die Positionsmessungen $\mathbf{P}_k$ und die skalierten Geschwindigkeitsmessungen $T_{\text{half}}\,\mathbf{V}_k$** im Kleinste-Quadrate-Sinn angepasst werden:

$$\mathbf{P}(u) = c_0 + c_1 u + c_2 u^2 + c_3 u^3, \qquad T_{\text{half}}\,\mathbf{V}(u) = \frac{d\mathbf{P}}{du}(u) = c_1 + 2 c_2 u + 3 c_3 u^2$$

Die Normalgleichungsmatrix $\mathbf{A}^T \mathbf{A} \in \mathbb{R}^{4 \times 4}$ summiert über alle $K = 33$ Ephemeridenpunkte sowohl die Positions-Basisvektoren $\mathbf{b}_{\text{pos}}(u_k) = [1, u_k, u_k^2, u_k^3]^T$ als auch die mit $w_{\text{vel}} = 4{,}0$ gewichteten Ableitungs-Basisvektoren $\mathbf{b}_{\text{vel}}(u_k) = [0, 1, 2 u_k, 3 u_k^2]^T$:

$$\left(\sum_{k=1}^{K} \Big[\mathbf{b}_{\text{pos}}(u_k)\,\mathbf{b}_{\text{pos}}(u_k)^T + w_{\text{vel}}\,\mathbf{b}_{\text{vel}}(u_k)\,\mathbf{b}_{\text{vel}}(u_k)^T\Big]\right) \mathbf{c} = \sum_{k=1}^{K} \Big[\mathbf{b}_{\text{pos}}(u_k)\,\mathbf{P}_k + w_{\text{vel}}\,\mathbf{b}_{\text{vel}}(u_k)\,(T_{\text{half}}\,\mathbf{V}_k)\Big]$$

Dieses kleine $4 \times 4$-System wird mit `solve_sym::<4>` (Gauß-Jordan mit Spaltenpivotisierung in `f64`) gelöst. Der Gewinn ist dreifach:
1. **Keinerlei 1-Hz-Rippel**: Zufällige Paket-Jitters einzelner Stützstellen werden über alle $33 \times 2 = 66$ Beobachtungen pro Raumrichtung herausgemittelt.
2. **Keine Runge-Oszillationen am Rand**: Durch die Hinzunahme der Geschwindigkeits-Beobachtungen $\mathbf{V}_k$ mit Gewicht $w_{\text{vel}} = 4{,}0$ ist die Steigung auch am ersten und letzten Burst der Szene physikalisch fest verankert.
3. **Analytische Konsistenz $\mathbf{V}(t) \equiv \frac{d}{dt}\mathbf{P}(t)$**: Da `interp_vel` exakt die analytische Ableitung desselben Polynoms `interp_pos` auswertet, konvergiert die nachfolgende Newton-Iteration der Zero-Doppler-Projektion quadratisch in wenigen Schritten auf $< 10^{-8}\text{ s}$.

---

### 3.3 Newton-Raphson Zero-Doppler-Inversion auf dem WGS-84-Ellipsoid (`solve_zero_doppler_target_ecef`)

Für ein fokussiertes Master-Pixel zur Azimut-Zero-Doppler-Zeit $t_M$ und Schrägentfernung $R_M$ suchen wir den zugehörigen 3D-Punkt $\mathbf{X} = (X, Y, Z)^T$ auf der Erdoberfläche (WGS-84-Ellipsoid mit Äquatorradius $a = 6\,378\,137{,}0\text{ m}$, Polradius $b = 6\,356\,752{,}314245\text{ m}$ und Referenzhöhe $h_{\text{ellip}} = 0\text{ m}$).

Mathematisch ist $\mathbf{X}$ der eindeutige Schnittpunkt dreier geometrischer Bedingungen (auf der **rechten** Seite der Flugbahn):
1. **Zero-Doppler-Ebene**: $(\mathbf{X} - \mathbf{P}_M) \cdot \mathbf{V}_M = 0$
2. **Range-Kugel**: $\|\mathbf{X} - \mathbf{P}_M\|_2 = R_M$
3. **WGS-84-Ellipsoid**: $F(\mathbf{X}) = \frac{X^2 + Y^2}{(a + h_{\text{ellip}})^2} + \frac{Z^2}{(b + h_{\text{ellip}})^2} - 1 = 0$

Bei der Implementierung dieser Gleichungen auf realen Sentinel-1C-Daten stießen wir auf **zwei subtile, aber entscheidende geometrische Fallstricke**, deren Lösung für das Gelingen der gesamten Koregistrierung ausschlaggebend war:

---

#### Fallstrick 1: Die Nadir-Orthogonalisierung gegen $\hat{\mathbf{u}}_v$ (der `-55 px` Azimut-Geolokations-Fix)

Auf einer perfekt kreisförmigen Bahn um eine kugelförmige Erde steht der Ortsvektor $\mathbf{P}_M$ exakt senkrecht auf dem Geschwindigkeitsvektor $\mathbf{V}_M$ ($\mathbf{P}_M \cdot \mathbf{V}_M = 0$). Auf dem **abgeplatteten WGS-84-Ellipsoid** und einem leicht exzentrischen LEO-Orbit hat der Satellit jedoch eine nichtverschwindende Vertikalgeschwindigkeit $\mathbf{P}_M \cdot \hat{\mathbf{u}}_v \neq 0$!

Würde man naiv $\hat{\mathbf{u}}_r = \mathbf{P}_M / \|\mathbf{P}_M\|$ als Nadir-Achse in der Zero-Doppler-Ebene verwenden und den Sehstrahl als $\hat{\mathbf{l}}(\theta) = \sin\theta\,\hat{\mathbf{u}}_c - \cos\theta\,\hat{\mathbf{u}}_r$ ansetzen, dann hätte der Punkt $\mathbf{X} = \mathbf{P}_M + R_M\,\hat{\mathbf{l}}(\theta)$ plötzlich eine **Komponente in Flugrichtung $\hat{\mathbf{u}}_v$**:
$$(\mathbf{X} - \mathbf{P}_M) \cdot \hat{\mathbf{u}}_v = -R_M \cos\theta\,(\hat{\mathbf{u}}_r \cdot \hat{\mathbf{u}}_v) \neq 0 \quad (\text{mehrere } 100\text{ m Along-Track-Fehler!})$$
Als wir einen solchen Punkt testweise wieder auf denselben Master-Orbit zurückprojizierten (`project_target_to_slave`), landete er um **$-55\text{ Azimut-Pixel}$ ($\approx 770\text{ m}$)** versetzt!

**Die exakte Lösung in `solve_zero_doppler_target_ecef` (`11_insar.rs`, Zeilen 116–131)**:
Wir führen vorab eine **Gram-Schmidt-Orthogonalisierung** des Ortsvektors $\mathbf{P}_M$ gegen die Flugrichtung $\hat{\mathbf{u}}_v = \mathbf{V}_M / \|\mathbf{V}_M\|$ durch:
$$\mathbf{r}_\perp = \mathbf{P}_M - (\mathbf{P}_M \cdot \hat{\mathbf{u}}_v)\,\hat{\mathbf{u}}_v, \qquad \hat{\mathbf{u}}_\perp = \frac{\mathbf{r}_\perp}{\|\mathbf{r}_\perp\|}$$
Da nun nach Konstruktion $\hat{\mathbf{u}}_\perp \cdot \hat{\mathbf{u}}_v \equiv 0$ und $\hat{\mathbf{u}}_c = \hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_\perp \perp \hat{\mathbf{u}}_v$ gilt, liegt für **jeden** Winkel $\theta$ der Sehstrahl
$$\mathbf{X}(\theta) = \mathbf{P}_M + R_M \big(\sin\theta\,\hat{\mathbf{u}}_c - \cos\theta\,\hat{\mathbf{u}}_\perp\big)$$
bis auf Maschinengenauigkeit ($|(\mathbf{X} - \mathbf{P}_M)\cdot \hat{\mathbf{u}}_v| < 10^{-12}$) exakt in der wahren Zero-Doppler-Ebene!

---

#### Fallstrick 2: Das Vorzeichen der Cross-Track-Achse für rechtsschauendes SAR (der `+6 px` Range-Fix)

In welche Richtung zeigt das Kreuzprodukt zweier Einheitsvektoren im 3D-Raum?
- $\hat{\mathbf{u}}_v$ zeigt in Flugrichtung (wie der Daumen oder Zeigefinger nach vorn).
- $\hat{\mathbf{u}}_\perp$ zeigt vom Erdmittelpunkt radial nach **oben** (Zenit).
- Nach der **Rechte-Hand-Regel** zeigt $\hat{\mathbf{u}}_\perp \times \hat{\mathbf{u}}_v$ (Zenit $\times$ Vorwärts) in Flugrichtung gesehen nach **links**, während $\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_\perp$ (Vorwärts $\times$ Zenit) nach **rechts** zeigt!

Da Sentinel-1 eine **rechtsschauende (Right-Looking)** Antenne besitzt, muss zwingend
$$\hat{\mathbf{u}}_c = \frac{\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_\perp}{\|\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_\perp\|}$$
gewählt werden. Ein vertauschtes Vorzeichen projizierte den Zielpunkt auf die linke Seite der Sub-Satellitenspur (in den Pazifik statt in die Taiwanstraße) und kehrte das Vorzeichen von $\Delta R_{\text{geo}} = R_S - R_M$ von $\approx +6\text{ px}$ auf $\approx -6\text{ px}$ um!

Mit der korrekten Basis $(\hat{\mathbf{u}}_\perp, \hat{\mathbf{u}}_c)$ wird das 3D-Schnittproblem auf eine eindimensionale Gleichung $F(\theta) = 0$ für den Look-Winkel $\theta$ reduziert, die in **6 Newton-Raphson-Iterationen** mit der analytischen Ableitung
$$\frac{d\mathbf{X}}{d\theta}(\theta) = R_M \big(\cos\theta\,\hat{\mathbf{u}}_c + \sin\theta\,\hat{\mathbf{u}}_\perp\big), \qquad \frac{dF}{d\theta} = \frac{2 X \dot{X} + 2 Y \dot{Y}}{(a+h_{\text{ellip}})^2} + \frac{2 Z \dot{Z}}{(b+h_{\text{ellip}})^2}$$
auf einen Ellipsoidfehler $|F(\theta)| < 10^{-8}$ gelöst wird. Anschließend bestimmt `project_target_to_slave` über 8 Newton-Schritte
$$t_S^{(k+1)} = t_S^{(k)} + \frac{(\mathbf{X} - \mathbf{P}_S(t_S^{(k)})) \cdot \mathbf{V}_S(t_S^{(k)})}{\|\mathbf{V}_S(t_S^{(k)})\|_2^2}$$
die zugehörige Slave-Zero-Doppler-Zeit $t_S$ und den Slave-Abstand $R_S = \|\mathbf{X} - \mathbf{P}_S(t_S)\|_2$.

---

## Kapitel 4: Phasenerhaltende TOPS-Burst-Fokussierung (Baseband-Erhaltung)

### 4.1 Warum ein reiner Amplituden-TOPS-Prozessor für InSAR modifiziert werden muss

In `06_rda.rs` (`focus_tops_burst_cpu`) und `09_gpu.rs` (`focus_tops_burst_gpu`) nutzt `source2` für die Fokussierung eines TOPS-Bursts das effiziente **Baseband-Scaling-/Deramping-Verfahren**:

1. Nach der Range-Kompression wird das Signal im Azimut-Zeitbereich $t_m = (m - N_{\text{az}}/2)\cdot\text{PRI}$ mit der geschätzten Antennen-Schwenkrate $k_t > 0$ (bei IW typisch $+4\,500\dots +6\,200\text{ Hz/s}$) und dem mittleren Doppler-Zentroid $f_{\text{DC,mid}}$ in das Basisband gedreht (**Deramping**) und zentrisch in ein auf $N_{\text{az,fft}}$ Null-gepaddetes Array eingebettet:
   $$s_{\text{deramp}}(t_m, r) = s_{\text{rc}}(t_m, r) \cdot \exp\!\Big(-j\big(2\pi f_{\text{DC,mid}} t_m + \pi k_t t_m^2\big)\Big)$$
2. Im Azimut-Frequenzbereich $f_a \in [-\text{PRF}/2, +\text{PRF}/2]$ wird zunächst die Range-Cell-Migration (RCMC) korrigiert und danach jedes Range-Bin $r$ mit der **effektiven FM-Rate**
   $$k_{\text{eff}}(r) = k_a(r) - k_t \quad \text{mit} \quad k_a(r) = -\frac{2 V_{\text{eff}}(r)^2}{\lambda R(r)} < 0$$
   komprimiert:
   $$H_{\text{az,TOPS}}(f_a, r) = \exp\!\left(+j \pi \frac{f_a^2}{k_{\text{eff}}(r)}\right)$$

Welche Azimut-Position und welche Phase hat ein Punktziel mit wahrer Zero-Doppler-Zeit $\tau_0$ (relativ zur Burst-Mitte) und Abstand $R_0$ nach der Azimut-iFFT?
Werten wir das Faltungsintegral nach der **Methode der stationären Phase** aus, so ergibt sich ein fundamentales Resultat für TOPS:
1. **Zeitliche Skalierung des fokussierten Gitters**:
   Ein Ziel mit Zero-Doppler-Zeit $\tau_0$ fokussiert nicht bei der Indexzeit $t = \tau_0$, sondern an der um den Faktor $\alpha(r) = \frac{k_a(r)}{k_a(r) - k_t} \in (0{,}25, \, 0{,}40)$ komprimierten Zeitkoordinate (genau dem Zeitpunkt, an dem die elektronisch schwenkende Antennenkeule das Ziel überstrich):
   $$\tau_{\text{foc}}(a) = \left(a - \frac{N_{\text{az,fft}}}{2}\right)\text{PRI} = \frac{k_a(r)}{k_a(r) - k_t}\,\tau_0 = \alpha(r)\,\tau_0 \quad \Longleftrightarrow \quad \Delta t_{\text{foc}} = \frac{\text{PRI}}{\alpha}$$
2. **Analytische Phase am fokussierten Peak**:
   Am Ort des fokussierten Peaks $\tau_{\text{foc}} = \alpha(r)\,\tau_0$ trägt das Signal nach der Azimut-iFFT die Phase:
   $$\phi_{\text{iFFT}}(\tau_{\text{foc}}, R_0) = -\frac{4\pi}{\lambda}\,R_0 - 2\pi f_{\text{DC,mid}}\,(\tau_{\text{foc}} - \tau_0) - \pi\,k_{\text{top}}(R_0)\,\tau_0^2 \qquad \text{mit} \qquad k_{\text{top}}(R_0) = \frac{k_a(R_0)\,k_t}{k_a(R_0) - k_t}$$

---

### 4.2 Das fundamentale TOPS-Resampling-Dilemma

Betrachten wir die effektive quadratische Phasenrate $k_{\text{top}} = \frac{k_a k_t}{k_a - k_t}$ (bzw. bezogen auf die fokussierte Zeit $\tau_{\text{foc}}$ die Rate $\frac{k_{\text{top}}}{\alpha^2} = \frac{k_t (k_a - k_t)}{k_a} \approx -5\,964\text{ Hz/s}$):

```text
Was passiert bei bilinearer Sub-Pixel-Interpolation des Slave-Bursts um Δa = 0,25 Pixel?

Fall A: Mit quadratischer TOPS-Phase exp(-j·π·k_top·τ_0²) am Rand des Bursts (τ_0 ≈ 0,4 s):
  Momentanfrequenz f_inst = k_top · τ_0 ≈ 2.000 Hz  ──►  Phasendrehung Δφ ≈ 3,6 rad pro Pixel (> 200°/px)!
  Lineare Mittelung (1 - fa)·S[a0] + fa·S[a0+1] zweier gegenphasiger Nachbarpixel:
  S[a0] (──►) + S[a0+1] (◄──)  ══►  Destruktive Interferenz (Auslöschung & Phasensprung)!

Fall B: Im glatten Zero-Doppler-Basisband (Baseband-Erhaltung):
  Phase ändert sich zwischen Nachbarpixeln nur um < 0,02 rad (~1°/px)!
  Bilineare Interpolation (1 - fa)·S[a0] + fa·S[a0+1] erhält Amplitude und Phase exakt!
```

Wenn im fokussierten Bild eine schnelle quadratische Phasenrampe steckt (oder wenn man gar nach der Azimut-iFFT ein quadratisches Re-Ramping $\exp(+j\pi k_{\text{top}}\tau_0^2)$ aufmodulieren würde), dreht sich der komplexe Zeiger zwischen zwei benachbarten Azimut-Zeilen $a_0$ und $a_0 + 1$ am Burst-Rand bereits um mehrere Radiant!
- Sobald wir den Slave-Burst bei der Koregistrierung um einen gebrochenen Versatz $\Delta a \approx +0{,}20\dots +0{,}28\text{ Pixel}$ bilinear zwischen Zeile $a_0$ und $a_0+1$ interpolieren, addieren wir zwei komplexe Zahlen mit fast entgegengesetzter Phase – das Signal löscht sich selbst aus ($\gamma_{\text{proc}} \to 0$).
- Noch schlimmer: Würden Master und Slave unabhängig voneinander ihr eigenes $k_{t,M}$ und $k_{t,S}$ per Clutterlock schätzen, hätten beide fokussierten Bursts leicht unterschiedliche Zeitmaßstäbe $\Delta t_{\text{foc},M} \neq \Delta t_{\text{foc},S}$!

---

### 4.3 Unsere Lösung in `06_rda.rs` und `09_gpu.rs`: Gemeinsames Master-Steering & Baseband-Erhaltung

In `process_burst_pair_insar` (`11_insar.rs`, Zeilen 994–1005) sowie `focus_tops_burst_insar_cpu` (`06_rda.rs`) und `focus_tops_burst_insar_gpu` (`09_gpu.rs`) lösen wir beide Probleme gleichzeitig durch zwei strikte Invarianten:

1. **Identische Master-Steering-Parameter (`override_steering: Option<TopsSteering>`)**:
   Für jedes der 36 Burst-Paare $(M_b, S_b)$ wird `estimate_tops_steering` ausschließlich einmal auf dem Master-Burst ausgeführt (`override_steering = None`). Der dabei gewonnene Parameter-Satz `steering = TopsSteering { fdc_mid_hz, kt_hz_s, swept_bw_hz }` wird anschließend via `Some(steering)` unverändert in die Slave-Fokussierung eingespeist:
   ```rust
   // Aus source2/src/11_insar.rs (process_burst_pair_insar):
   let (m_data, n_az_fft, n_rg, steering) = if let Some(gpu) = gpu_opt {
       gpu.focus_tops_burst_insar_gpu(m_blk, true, None)?
   } else {
       focus_tops_burst_insar_cpu(m_blk, true, None)
   };

   let (s_data, s_az_fft, s_rg, _) = if let Some(gpu) = gpu_opt {
       gpu.focus_tops_burst_insar_gpu(s_blk, true, Some(steering))?
   } else {
       focus_tops_burst_insar_cpu(s_blk, true, Some(steering))
   };
   ```
   Dadurch besitzen Master- und Slave-SLC garantiert exakt dieselbe gepaddete FFT-Länge `n_az_fft`, denselben Skalierungsfaktor $\alpha(r) = \frac{k_a(r)}{k_a(r) - k_t}$ und denselben effektiven Zeitschritt `dt_foc = PRI / alpha`.

2. **Fokussierung und Resampling direkt im Basisband + stationärer Phasenkompensations-Kernel**:
   Da Master und Slave wegen der extrem engen Orbit-Synchronisation ($\Delta t_{\text{CA}} - \Delta t_{\text{burst}} \approx +0{,}78\text{ ms} \approx 0{,}2\text{ fokussierte Zeilen}$) mit identischem $k_t$ derampt wurden, verbleiben beide Bursts nach der Azimut-iFFT ohne künstliches Post-iFFT-Re-Ramping direkt im glatten Basisband. Zudem stellt `09_gpu.rs` den dedizierten `cuda-oxide`-Kernel `tops_insar_phase_comp_inplace` bereit, der bei Bedarf den analytischen stationären Phasenterm
   $$\phi_{\text{comp}}(a, r) = 2\pi f_{\text{DC,mid}}\,(\tau_{\text{foc}}(a) - \tau_0(a, r)) + \pi\,k_{\text{top}}(r)\,\tau_0(a, r)^2$$
   in-place im GPU-Speicher kompensiert.

---

## Kapitel 5: Hybride Sub-Pixel-Koregistrierung, Flat-Earth-Demodulation & GPU-Interferogrammbildung

### 5.1 Dreistufiges Koregistrierungsmodell (`estimate_burst_geometric_model` & `refine_burst_coreg_empirical`)

Damit die Phasen von Master $M(a, r)$ und Slave $S(a + \Delta a, r + \Delta r)$ kohärent interferieren, muss das Slave-Bild auf besser als $\frac{1}{10}$ bis $\frac{1}{20}$ Pixel genau auf das Master-Gitter ausgerichtet werden. Gleichzeitig muss die durch die Basislinie auf dem WGS-84-Ellipsoid hervorgerufene **Flat-Earth-Phase** $\phi_{\text{flat}}(a, r) = \frac{4\pi}{\lambda}(R_S(a, r) - R_M(a, r))$ abgezogen werden.

In `11_insar.rs` geschieht dies für jeden Burst durch das parametrische Modell `BurstCoregModel` (relativ zur Burst-Mitte $a_f = a - N_{\text{az,fft}}/2$, $r_f = r - N_{\text{rg}}/2$):
$$\Delta a(a_f, r_f) = a_0 + a_1\,a_f + a_2\,r_f$$
$$\Delta r(a_f, r_f) = b_0 + b_1\,a_f + b_2\,r_f$$
$$\phi_{\text{model}}(a_f, r_f) = \phi_0 + \phi_1\,a_f + \phi_2\,r_f + \phi_3\,r_f^2$$

Die Parameter werden in **drei aufeinander aufbauenden Stufen** bestimmt:

```text
Stufe 1: Analytisches 3D-ECEF-Orbit-Modell (`estimate_burst_geometric_model`)
  • Wertet `solve_zero_doppler_target_ecef` + `project_target_to_slave` auf einem Stützstellen-Kreuz
    (0, 0), (±step_a, 0), (0, ±step_r) um die Burst-Mitte aus.
  • Liefert die affinen Ableitungen (a_1, a_2, b_1, b_2), den Start-Shift (a_0,geo, b_0,geo) sowie
    das quadratische Flat-Earth-Phasenpolynom φ_flat(a_f, r_f) = +4π/λ · (R_S - R_M).
        │
        ▼
Stufe 2: Empirische Grobsuche im ±1,5-Pixel-Fenster (`refine_burst_coreg_empirical`, Δ = 0,25 px)
  • Prüft ein 13 × 13 Gitter um (a_0,geo, b_0,geo) auf dem zentralen Vollapertur-Kern (±16 % von N_az,raw).
  • Kombiniert normierte Kreuzkorrelation der Log-Intensitäten (`eval_icorr`) mit der komplexen
    Multilook-Patch-Kohärenz (`eval_patch_coh` auf 5 × 20 Pixel-Patches) + leichter Tikhonov-Regularisierung
    zum Orbit-Prior (verhindert Fehlsprünge auf Meeres-Speckle!).
        │
        ▼
Stufe 3: 1/20-Pixel-Feinsuche (Δ = 0,05 px) + Lag-2-Autokorrelation der Rest-Phasenrampe
  • Verfeinert (a_0, b_0) auf einem 9 × 9 Feingitter mit 0,05 px Schrittweite.
  • Schätzt etwaige lineare Rest-Phasengradienten (δφ_a, δφ_r) über eine komplexe Lag-2-Autokorrelation
    `c(a+2, r) * conj(c(a, r))` und `c(a, r+2) * conj(c(a, r))` (Lag 2 vermeidet das Korrelations-Übersprechen
    benachbarter bilinear interpolierter Pixel!).
```

Warum stimmt unser geometrisches Orbit-Modell mit der empirischen Kreuzkorrelation so präzise überein?
- Aus der senkrechten/parallelen Basislinie ($B_\parallel = -16{,}022\text{ m}$) ist der Slave-Abstand $R_S$ um rund $16{,}02\text{ m}$ größer als $R_M$.
- Bei einer Range-Pixelgröße von $\Delta r = 2{,}33\text{ m}$ (IW1), $2{,}75\text{ m}$ (IW2) bzw. $3{,}20\text{ m}$ (IW3) entspricht das einem positiven Range-Versatz von:
  $$\Delta r_{\text{IW1}} = \frac{+16{,}02\text{ m}}{2{,}33\text{ m/px}} \approx +6{,}73\text{ px}, \qquad \Delta r_{\text{IW2}} \approx +5{,}94\text{ px}, \qquad \Delta r_{\text{IW3}} \approx +5{,}13\text{ px}$$
  Unsere hybride Schätzung trifft diese physikalischen Werte auf allen 36 Bursts auf wenige Hundertstel Pixel genau!

---

### 5.2 Kombinierter GPU-Kernel für Sub-Pixel-Resampling & Flat-Earth-Abzug (`insar_resample_and_ifg_kernel`)

Sobald das verfeinerte Modell `BurstCoregModel` vorliegt, werden das komplexe Slave-Resampling, die Flat-Earth-Demodulation und die Leistungsberechnung in einem einzigen speicherbandbreiten-optimierten GPU-Pass (`insar_resample_and_ifg_kernel` in `09_gpu.rs`, Zeilen 308–395) ausgeführt:

```rust
// Aus source2/src/09_gpu.rs (insar_resample_and_ifg_kernel):
let s_interp = Complex32::new(
    w00 * s00.re + w01 * s01.re + w10 * s10.re + w11 * s11.re,
    w00 * s00.im + w01 * s01.im + w10 * s10.im + w11 * s11.im,
);
let m_val = master[i as usize];
pm = m_val.re * m_val.re + m_val.im * m_val.im;
ps = s_interp.re * s_interp.re + s_interp.im * s_interp.im;

// Komplexes Produkt M * conj(S_interp)
let prod = Complex32::new(
    m_val.re * s_interp.re + m_val.im * s_interp.im,
    m_val.im * s_interp.re - m_val.re * s_interp.im,
);
// Simultane Flat-Earth- und Rampen-Demodulation exp(-j * phi_model)
let phi_mod = phi0 + dphi_da * af + dphi_dr * rf + dphi_dr2 * rf * rf;
let rot = Complex32::new(phi_mod.cos() as f32, (-phi_mod.sin()) as f32);
ig = cmul(prod, rot);
```

Beachten wir hier das **Vorzeichen der Flat-Earth-Phase**:
- Ein Radarecho mit Zwei-Wege-Laufzeit $\tau = \frac{2 R}{c}$ besitzt nach dem Heruntermischen mit der Trägerfrequenz $f_c$ die Phase $\phi_{\text{SLC}}(R) = -2\pi f_c \tau = -\frac{4\pi}{\lambda} R$.
- Im Interferogramm $I = M \cdot S^*$ beträgt die Phasendifferenz auf dem Ellipsoid daher:
  $$\phi_{\text{FE}} = \phi_M - \phi_S = \left(-\frac{4\pi}{\lambda} R_M\right) - \left(-\frac{4\pi}{\lambda} R_S\right) = +\frac{4\pi}{\lambda}\big(R_S - R_M\big)$$
- Um diese Ellipsoid-Phase abzuziehen, multipliziert der Kernel mit $\exp(-j\,\phi_{\text{FE}}) = \cos(\phi_{\text{FE}}) - j\sin(\phi_{\text{FE}})$.

In `test_insar_resample_and_form_ifg_cpu_gpu_parity` (`tests/integration.rs`) wird das GPU-Ergebnis Pixel für Pixel gegen die CPU-Referenz `resample_and_form_ifg_cpu` geprüft: Die maximale Abweichung über das gesamte Gitter liegt bei $< 10^{-4}$!

---

## Kapitel 6: TOPS-Debursting, Enhanced Spectral Diversity (ESD) & Multi-Swath-Mosaikierung

### 6.1 Burst-Überlappungen und das Doppel-Differenz-Interferogramm (ESD) in `deburst_insar_swath`

In einem TOPS-Sub-Swath (z. B. IW2 mit 12 Bursts) überlappen sich zwei zeitlich aufeinanderfolgende Bursts $b$ und $b+1$ am Boden in einem schmalen Streifen $\Omega_{b, b+1}$ von rund $40\dots 80$ fokussierten Azimut-Zeilen:
- Am **Ende von Burst $b$** blickt die vorwärts geschwenkte Antenne unter einem positiven Schielwinkel (Vorwärts-Blick, hohe positive Doppler-Frequenz $f_{\eta,b} > 0$) auf diesen Bodenstreifen.
- Am **Anfang von Burst $b+1$** (rund $2{,}75\text{ s}$ später, nachdem die Antenne zwischendurch IW3 und IW1 bedient hat) blickt die nach hinten zurückgesetzte Antenne unter einem negativen Schielwinkel (Rückwärts-Blick, hohe negative Doppler-Frequenz $f_{\eta,b+1} < 0$) auf **exakt dieselben Bodenpunkte**!

```text
Globale Azimut-Zeitachse eines Sub-Swaths (12 Bursts):

Burst b (Vorwärts-Blick am Ende):    [═══════════════════════╲
                                                              ╲  Überlappungszone Ω_{b, b+1}:
Burst b+1 (Rückwärts-Blick am Start):                          ╲═══════════════════════]
                                     ──────────────────────────┼─────────────────────────► Azimut a
                                                Doppel-Differenz-Produkt:
                                        Δφ_ESD = arg( Σ_{(a,r) ∈ Ω} I_b(a,r) · I_{b+1}^*(a,r) )
```

Durch diese gewaltige Doppler-Trennung $\Delta f_{\text{ovl}} = f_{\eta,b} - f_{\eta,b+1} \approx 4\,000\dots 5\,000\text{ Hz}$ reagiert die Phasendifferenz zwischen dem Interferogramm $I_b(a, r)$ des vorderen Bursts und dem Interferogramm $I_{b+1}(a, r)$ des nachfolgenden Bursts extrem empfindlich auf kleinste Azimut-Koregistrierungsresiduen oder konstante Phasenoffsets zwischen den Bursts (**Enhanced Spectral Diversity, ESD**).

In `deburst_insar_swath` (`11_insar.rs`, Zeilen 518–685) berechnen wir deshalb vor dem Verschmelzen jedes neuen Bursts $b+1$ mit dem bereits akkumulierten Sub-Swath-Puffer in der Überlappungszone $\Omega_{b, b+1}$ das **komplexe Doppel-Differenz-Produkt**:
$$\Delta\phi_{\text{ESD}} = \arg\!\left(\sum_{(a, r) \in \Omega_{b, b+1}} I_{\text{prev}}(a, r) \cdot \big(I_{\text{curr}}(a, r)\,e^{j\Phi_{\text{cum}}}\big)^*\right)$$
und aktualisieren den kumulierten komplexen Phasenrotator $e^{j\Phi_{\text{cum}}} \leftarrow e^{j\Phi_{\text{cum}}} \cdot e^{j\Delta\phi_{\text{ESD}}}$. Anschließend werden alle drei Kanäle – das komplexe Interferogramm `ifg`, die Master-Leistung `pm` und die Slave-Leistung `ps` – mit einem weichen Kosinus-Cross-Fade
$$w(x) = \frac{1}{2}\big(1 - \cos(\pi x)\big), \qquad x \in [0, 1]$$
phasenstetig und ohne sichtbare Burst-Nähte auf das durchgehende Sub-Swath-Gitter addiert.

---

### 6.2 Nahtlose Multi-Swath-Mosaikierung über IW1, IW2 und IW3 (`mosaic_insar_swaths`)

Nachdem die drei Sub-Swaths IW1, IW2 und IW3 jeweils einzeln deburstet wurden, vereint `mosaic_insar_swaths` (`11_insar.rs`, Zeilen 758–988) sie auf einem gemeinsamen Süd-Taiwan-Multilook-Gitter ($6 \times 24$ Looks $\implies 2518 \times 2925\text{ Pixel}$, Slant-Range $R_{\min} = 799{,}282\text{ km}$ bis $R_{\max} = 962{,}774\text{ km}$).

Dabei sind zwei besondere Herausforderungen der Interferometrie gelöst:

1. **4-Sub-Bin-Range-Phasenausrichtung beim Multilooking (`combine_range_subbins`)**:
   Eine Multilook-Zelle fasst in Range $L_{\text{rg}} = 24$ volle Spalten ($\approx 56\dots 77\text{ m}$ Schrägentfernung) zusammen. Wenn über steilem Gelände in Taiwan eine merkliche Range-Fringe-Rate $\omega_r$ herrscht, würde eine naive komplexe Summe über 24 Spalten die Amplitude durch Phasenrotation innerhalb der Zelle dämpfen!
   Deshalb unterteilt `mosaic_insar_swaths` jede Multilook-Zelle intern in **4 Range-Sub-Bins** $B_0, B_1, B_2, B_3$ (je 6 Spalten), schätzt aus den drei 1-Lag-Produkten $B_1 B_0^* + B_2 B_1^* + B_3 B_2^*$ den lokalen Phasenfortschritt $\Delta\phi_{\text{sub}}$ innerhalb der Zelle und dreht alle 4 Sub-Bins vor der Summation kohärent auf die Zellenmitte zurück:
   $$I_{\text{cell}} = \sum_{k=0}^{3} B_k \cdot \exp\!\big(-j\,(k - 1{,}5)\,\Delta\phi_{\text{sub}}\big)$$
2. **Phasen-Harmonisierung und Kosinus-Blending an den Sub-Swath-Nähten (IW1 $\leftrightarrow$ IW2 $\leftrightarrow$ IW3)**:
   Genau wie an den horizontalen Burst-Grenzen bestimmt `mosaic_insar_swaths` in den vertikalen Überlappungszonen zwischen IW1 und IW2 (bei $R \approx 853\text{ km}$) sowie zwischen IW2 und IW3 (bei $R \approx 905\text{ km}$) die mittlere Phasendifferenz $\Delta\phi_{\text{swath}} = \arg\sum (I_{s-1} \cdot I_s^*)$, richtet die Phase von Sub-Swath $s$ exakt auf Sub-Swath $s-1$ aus und blendet über eine $\pm 1200\text{ m}$ breite Nahtzone kosinusförmig über.

---

## Kapitel 7: Kohärenzschätzung, Goldstein-Filterung & 2D Phase Unwrapping

### 7.1 Warum der naive Boxcar-Kohärenzschätzer im Gebirge versagt – und wie `compute_coherence_map` ihn repariert

Der klassische Boxcar-Kohärenzschätzer über ein $(2 w_a + 1) \times (2 w_r + 1)$-Fenster $W$ (bei uns $5 \times 5$ Multilook-Zellen mit `--coh-win 2`) lautet:
$$|\hat{\gamma}_{\text{naiv}}(a, r)| = \frac{\left|\sum_{(i, j) \in W} I_{\text{ml}}(i, j)\right|}{\sqrt{\sum_{(i, j) \in W} P_{M,\text{ml}}(i, j) \cdot \sum_{(i, j) \in W} P_{S,\text{ml}}(i, j)}}$$

Was passiert mit diesem Schätzer an den steilen Hängen des taiwanesischen Zentralgebirges?
Dort ändert sich die wahre topographische Phase von einer Multilook-Zelle zur nächsten um einen lokalen Gradienten $(\omega_a, \omega_r)$. Über ein $5 \times 5$-Fenster drehen sich die komplexen Zeiger $I_{\text{ml}}(i, j)$ bereits merklich gegeneinander – der Betrag der unkompensierten Vektorsumme $\left|\sum I_{\text{ml}}\right|$ bricht ein, und der naive Schätzer meldet fälschlicherweise eine niedrige Kohärenz, obwohl das Signal rauschfrei ist!

In `compute_coherence_map` (`11_insar.rs`, Zeilen 691–756) führen wir deshalb in jedem Fenster $W$ **vor der Summation eine lokale 2D-Phasengradienten-Schätzung und -Kompensation** durch:
1. Aus den horizontalen und vertikalen 1-Lag-Produkten innerhalb des Fensters $W$ wird die lokale Kreisfrequenz $(\hat{\omega}_a, \hat{\omega}_r)$ geschätzt:
   $$\hat{\omega}_a = \arg\!\left(\sum_{(i,j)\in W} I_{\text{ml}}(i+1, j)\,I_{\text{ml}}^*(i, j)\right), \qquad \hat{\omega}_r = \arg\!\left(\sum_{(i,j)\in W} I_{\text{ml}}(i, j+1)\,I_{\text{ml}}^*(i, j)\right)$$
2. Anschließend wird die lokale Phasenebene im Fenster abgezogen (**Local Deramping**):
   $$S_{\text{deramp}} = \left|\sum_{(i, j) \in W} I_{\text{ml}}(i, j) \cdot \exp\!\Big(-j\big(\hat{\omega}_a (i - a) + \hat{\omega}_r (j - r)\big)\Big)\right|$$
3. Kombiniert mit der Intra-Zellen-Magnitudensumme $S_{\text{mag}} = \sum_{(i,j)\in W} |I_{\text{ml}}(i, j)|$ liefert das geometrische Mittel
   $$\hat{\gamma}(a, r) = \frac{\sqrt{S_{\text{deramp}} \cdot S_{\text{mag}}}}{\sqrt{\sum P_{M,\text{ml}} \cdot \sum P_{S,\text{ml}}}} \in [0, 1]$$
   eine unverzerrte Kohärenzkarte: Auf einer steilen synthetischen 2D-Phasenrampe über mehrere $2\pi$-Zyklen erreicht `compute_coherence_map` im Test `test_insar_coherence_goldstein_and_poisson_phase_unwrap` eine mittlere Kohärenz von **$> 0{,}98$**!

---

### 7.2 Nichtlineare spektrale Goldstein-Werner-Filterung (`goldstein_filter_interferogram`)

Um das Phasenrauschen in mäßig kohärenten Gebieten (Bergwälder, Übergangszonen) zu unterdrücken, ohne die topographischen Interferenzstreifen zu verwischen, implementiert `goldstein_filter_interferogram` (`11_insar.rs`, Zeilen 990–1118) das klassische **adaptive 2D-Frequenzbereichsfilter nach Goldstein & Werner (1998)**:

```text
Überlappende 32 × 32 Patches (Schrittweite 16 px, 2D-Hann-Fenster w(ia, ir)):

  z(a, r) = I(a, r) / |I(a, r)| · w(a, r)
                 │
                 ▼  2D-Radix-2-FFT (32 × 32)
            Z(u, v) ∈ ℂ^{32×32}
                 │
                 ▼  3×3-Box-Glättung des Amplitudenspektrums |Z(u, v)|
            S(u, v) = Smooth_3×3(|Z(u, v)|)
                 │
                 ▼  Nichtlineare Potenz-Gewichtung mit α = 0,65
       Z_filt(u, v) = ( S(u, v) / max S )^α · Z(u, v)
                 │
                 ▼  2D-iFFT (32 × 32) + Hann-gewichtetes Overlap-Add
         I_filt(a, r)
```

Warum wirkt dieses Filter so selektiv?
- Eine echte topographische oder atmosphärische Phasenrampe ist lokal eine harmonische Welle $\exp(j(\omega_a a + \omega_r r))$ und konzentriert ihre gesamte Energie im $32 \times 32$-Spektrum in **einem einzigen schmalen Peak** $(u_0, v_0)$, wo $S(u_0, v_0) / \max S \approx 1$ gilt.
- Unkorreliertes Phasenrauschen verteilt seine Energie dagegen flach über alle $32 \times 32 = 1024$ Frequenzbins ($S(u, v) \ll \max S$) und wird durch den Faktor $(S / \max S)^{\alpha}$ (mit $\alpha = 0{,}65$) stark gedämpft!

Auf der Süd-Taiwan-Szene hebt das Goldstein-Filter die mittlere Szenen-Kohärenz von $0{,}1556$ auf **$0{,}3512$** und das **90. Perzentil (die kohärenten Landgebiete) von $0{,}3622$ auf $0{,}7763$**!

---

### 7.3 Zweistufiges 2D Phase Unwrapping (`unwrap_phase_2d` & `unwrap_phase_2d_poisson`)

Das gefilterte Interferogramm liefert zunächst nur die auf das Hauptwertintervall $[-\pi, +\pi)$ gefaltete (**gewrappte**) Phase $\psi(a, r) = \arg(I_{\text{filt}}(a, r))$. Um daraus die wahre, kontinuierliche Phase $\Phi(a, r) = \psi(a, r) + 2\pi k(a, r)$ ($k \in \mathbb{Z}$) zurückzugewinnen, stellt `11_insar.rs` ein zweistufiges 2D-Unwrapping-Verfahren bereit:

#### Stufe 1: Kohärenzgesteuerte Priority-Flood-Integration (`unwrap_phase_2d`)
1. Der Algorithmus sucht im zentralen Bildbereich automatisch das **Startpixel (`seed_idx`) mit der höchsten Kohärenz** $\gamma(a_0, r_0)$ (in Taiwan mitten in der hochkohärenten Westküstenebene).
2. Eine Max-Priority-Queue (`BinaryHeap<(u32, usize, usize)>`) expandiert die Wellenfront streng in der Reihenfolge der Kantenqualität $q(p, n) = \min\big(\gamma(p), \gamma(n)\big)$.
3. Sobald ein neues Pixel $(a, r)$ aus der Queue entnommen wird, bildet es den mit $\gamma^2$ gewichteten Mittelwert der Vorhersagen **aller bereits entfalteten 4er-Nachbarn** $nb \in \mathcal{N}_4(a, r)$:
   $$\hat{\Phi}_{\text{pred}}(a, r) = \frac{\sum_{nb \in \text{visited}} \gamma(nb)^2 \cdot \Big(\Phi(nb) + \mathcal{W}\big(\psi(a, r) - \psi(nb)\big)\Big)}{\sum_{nb \in \text{visited}} \gamma(nb)^2}$$
   mit dem Wrapping-Operator $\mathcal{W}(\Delta) = \big((\Delta + \pi) \bmod 2\pi\big) - \pi \in [-\pi, +\pi)$, und rundet auf das exakt kongruente Vielfache von $2\pi$:
   $$\Phi(a, r) = \psi(a, r) + 2\pi \cdot \text{round}\!\left(\frac{\hat{\Phi}_{\text{pred}}(a, r) - \psi(a, r)}{2\pi}\right)$$
   Dadurch umfließt der Unwrapper hochkohärente Küstenlinien, Halbinseln und Täler zuerst, bevor dekorrelierte Meeres- oder Hochgebirgszonen überhaupt berührt werden!

#### Stufe 2: Kohärenzgewichteter Poisson-Löser via Preconditioned Conjugate Gradient (`unwrap_phase_2d_poisson`)
Ausgehend von der Priority-Flood-Lösung löst `unwrap_phase_2d_poisson` die diskrete **kohärenzgewichtete 2D-Poisson-Gleichung**:
$$-\nabla \cdot \big(w(a, r)\,\nabla \Phi(a, r)\big) = -\nabla \cdot \Big(w(a, r)\,\mathcal{W}\big(\nabla \psi(a, r)\big)\Big), \qquad w = \min(\gamma_i, \gamma_j)^2$$
mittels **64 Iterationen eines Jacobi-präkonditionierten Conjugate-Gradient-Verfahrens (PCG)**. Zum Abschluss werden beide Lösungen kohärenzgewichtet auf den Mittelwert Null zentriert ($\sum \gamma^2 \Phi = 0$).

---

## Kapitel 8: Ergebnisse & Physikalische Interpretation der Süd-Taiwan-Szene

### 8.1 Quantitative Gesamtergebnisse (`taiwan_insar_insar_summary.json`)

Der vollständige GPU-Lauf über alle **36 Burst-Paare (72 fokussierte TOPS-Bursts à $\approx 4096 \times 24\,000$ Samples)** auf der **NVIDIA RTX A4000 (`sm_86`, 16 GB VRAM)** wurde in **129,39 Sekunden ($\approx 2{,}15\text{ Minuten}$, also nur $1{,}8\text{ s}$ pro Burst inkl. Level-0-Dekodierung, GPU-Fokus, Koregistrierung, Mosaikierung, Goldstein-Filterung und Unwrapping)** abgeschlossen:

| Kategorie | Parameter / Messgröße | Gemessener Wert | Physikalische Bedeutung |
| :--- | :--- | ---: | :--- |
| **Gitter & Abdeckung** | Ausgabegitter (`rows × cols`) | $2\,518 \times 2\,925\text{ px}$ | $6 \times 24$ Multilook ($7{,}365\text{ Mio.}$ Bildpunkte) |
| | Slant-Range-Spanne $[R_{\min}, R_{\max}]$ | $799{,}282 \dots 962{,}774\text{ km}$ | $163{,}492\text{ km}$ Schrägentfernung ($\approx 250\text{ km}$ am Boden) |
| **Orbit & Basislinie** | 3D-Basislinie $B_{3\text{D}}$ | $16{,}999\text{ m}$ | Abstand Master–Slave im Closest Approach |
| | Senkrechte Basislinie $B_\perp$ | $-5{,}679\text{ m}$ | Effektive Basislinie für Topographie |
| | Parallele Basislinie $B_\parallel$ | $-16{,}022\text{ m}$ | Basislinie entlang des Sehstrahls ($\approx -R_S + R_M$) |
| | Radial / Cross-Track ($B_{\text{radial}}, B_{\text{cross}}$) | $+9{,}188\text{ m} \,/\, -14{,}302\text{ m}$ | Zerlegung im lokalen Orbit-Dreibein |
| | Closest-Approach-Offset $\Delta t_{\text{CA}}$ | $+0{,}049482\text{ s}$ | $+49{,}48\text{ ms}$ relativ zu exakt $12 \times 86\,400\text{ s}$ |
| | Höhenambiguität $h_{2\pi}$ | $2\,915{,}62\text{ m}$ | Geländehöhe pro $2\pi$-Interferenzstreifen |
| **Sub-Swath IW1** | Koregistrierung $(\Delta a, \Delta r)$ | $+0{,}202\text{ px}, \, +6{,}732\text{ px}$ | Mittlere Burst-Kohärenz: **$0{,}1643$** (Ostküste & viel Pazifik) |
| **Sub-Swath IW2** | Koregistrierung $(\Delta a, \Delta r)$ | $+0{,}283\text{ px}, \, +5{,}941\text{ px}$ | Mittlere Burst-Kohärenz: **$0{,}3372$** (Zentralgebirge, Pingtung, Tainan, Kaohsiung) |
| **Sub-Swath IW3** | Koregistrierung $(\Delta a, \Delta r)$ | $+0{,}165\text{ px}, \, +5{,}129\text{ px}$ | Mittlere Burst-Kohärenz: **$0{,}2364$** (Westküste, Taiwanstraße, Penghu) |
| **Gesamt-Mosaik** | Kohärenz vor Filterung (Mittel / P90) | $0{,}1556 \,/\, 0{,}3622$ | Inkl. $\approx 45\%$ offener Meeresfläche |
| | **Kohärenz nach Goldstein (Mittel / P90)** | **$0{,}3512 \,/\, 0{,}7763$** | Hohe Phasenstabilität über ganz West- und Süd-Taiwan |
| | Rückstreu-Kontrast $\sigma_{\text{dB}}$ | $5{,}869\text{ dB}$ | Scharfer Land-Meer- und Reliefkontrast |
| | **Gesamtlaufzeit (GPU RTX A4000)** | **$129{,}391\text{ s}$** | Kompletter End-to-End-Durchlauf aus Level-0-`.dat` |

---

### 8.2 Die visuellen InSAR-Produkte und ihre geophysikalische Interpretation

*Hinweis zur Bildorientierung*: Da alle Karten im natürlichen **Radar-Koordinatensystem (Zeilen = Azimut / Flugzeit, Spalten = Slant-Range $R$)** dargestellt sind und Sentinel-1C auf einem **absteigenden Orbit (Descending: von Nord-Nordost nach Süd-Südwest)** nach **rechts (West-Nordwest)** blickt, gilt:
- **Oben $\to$ Unten**: Norden (Zentral-Taiwan / Taichung / Hualien) $\to$ Süden (Hengchun-Halbinsel / Südspitze Taiwans).
- **Links $\to$ Rechts**: Near Range im Osten (**IW1**: Pazifik, Ostküste bei Taitung/Hualien) $\to$ Mid Range in der Mitte (**IW2**: Zentralgebirge, Pingtung-Becken, Kaohsiung, Tainan) $\to$ Far Range im Westen (**IW3**: flache Westküste, Taiwanstraße, Penghu-Archipel).

#### 1. Das 2×2-Gesamtpanel (`taiwan_insar_insar_panel.png`)

![Süd-Taiwan InSAR 4-Panel-Übersicht](taiwan_insar_insar_panel.png)

Das Übersichtspanel (`2932 × 2526 px`) stellt die vier komplementären Beobachtungsgrößen synoptisch gegenüber:
- **Oben links (Kombinierte SAR-Intensität in dB)**: Man erkennt gestochen scharf das stark gegliederte Relief des **taiwanesischen Zentralgebirges** in der Bildmitte, das topographisch flache, intensiv genutzte **Pingtung-Becken** südlich davon, die hell rückstreuenden Metropolregionen **Kaohsiung**, **Tainan** und **Chiayi** an der rechten (westlichen) Küste, die Fischteiche und Lagunen entlang der Küstenlinie sowie rechts außen im dunklen Wasser der Taiwanstraße die Inselgruppe **Penghu (Pescadores)** und zahlreiche punktförmige **Frachtschiffe**.
- **Oben rechts (Interferometrische Kohärenz $|\gamma| \in [0, 1]$, Viridis)**:
  - Der **Pazifik** (links) und die **Taiwanstraße** (rechts) sind tief dunkelviolett ($|\gamma| < 0{,}08$), da das bewegte Meerwasser nach 12 Tagen vollständig dekorreliert ist.
  - Die **urbanen und landwirtschaftlichen Ebenen West- und Süd-Taiwans** (von Chiayi über Tainan und Kaohsiung bis ins Pingtung-Tal) sowie das **Huadong-Längstal** im Osten und die **Penghu-Inseln** leuchten hellgrün bis gelb ($|\gamma| \approx 0{,}65\dots 0{,}92$).
  - Im steilen, dicht mit subtropischem Bergwald bewachsenen **Zentralgebirge** liegt die Kohärenz im Bereich $|\gamma| \approx 0{,}25\dots 0{,}45$ (Volumen- und Winddekorrelation des Blätterdachs über 12 Tage), wobei unbewachsene Felsgrate und Flusstäler als kohärente Bänder hervortreten.
- **Unten links (Goldstein-gefilterte interferometrische Phase $[-\pi, +\pi]$, HSV-Farbrad)**:
  Über dem gesamten Landkorridor von der Westküste bis in das Gebirge hinein zeigt sich ein glattes, zusammenhängendes Phasenfeld ohne jegliche Sprünge an den 12 Burst-Grenzen oder den Nahtstellen IW1/IW2 und IW2/IW3! Entlang des Übergangs von der flachen Küstenebene (gelb/grün) in das Zentralgebirge (türkis/blau/violett) vollzieht die Phase genau den für $h_{2\pi} \approx 2916\text{ m}$ erwarteten topographischen + troposphärischen Phasenanstieg von rund einem $2\pi$-Zyklus.
- **Unten rechts (Kohärenzgewichtet entfaltete Phase / Unwrapped Phase, Coolwarm)**:
  Die entfaltete Phase bildet die großräumige Phasenlandschaft Süd-Taiwans kontinuierlich und frei von künstlichen $2\pi$-Phasenabrissen ab.

---

#### 2. Detailansicht: Interferometrische Kohärenzkarte (`taiwan_insar_insar_coherence.png`)

![Interferometrische Kohärenz Süd-Taiwan](taiwan_insar_insar_coherence.png)

Die Vollauflösungs-Kohärenzkarte (`2925 × 2518 px`) demonstriert eindrucksvoll die Qualität der Baseband-Fokussierung und der Sub-Pixel-Koregistrierung: Selbst feinste Küstenstrukturen (Hafenmole von Kaohsiung, Flussläufe des Gaoping-Flusses im Pingtung-Becken, einzelne Inseln des Penghu-Archipels) heben sich mit scharfem Kontrast vom dekorrelierten Meer ab. Es gibt **keinerlei dunkle horizontale Streifen an den Burst-Übergängen**, was beweist, dass die TOPS-Phase bis in die äußeren Ränder jedes Bursts kohärent erhalten bleibt.

---

#### 3. Detailansicht: Goldstein-gefilterte vs. Entfaltete Phase (`taiwan_insar_insar_phase_filtered.png` & `taiwan_insar_insar_phase_unwrapped.png`)

![Goldstein-gefilterte Phase Süd-Taiwan](taiwan_insar_insar_phase_filtered.png)

![Entfaltete Phase Süd-Taiwan](taiwan_insar_insar_phase_unwrapped.png)

Im direkten Vergleich zwischen dem gewrappten Goldstein-Interferogramm (oben) und der entfalteten Phase (unten) sieht man, wie der kohärenzgesteuerte Priority-Flood-Integrator (`unwrap_phase_2d`) das zusammenhängende Westküsten- und Beckenband als hochkohärentes Rückgrat nutzt und von dort aus stetig in die Gebirgstäler hinein integriert.

---

## Kapitel 9: Zusammenfassung der Code-Architektur, Testabdeckung & CLI-Referenz

### 9.1 Modul-Übersicht des gesamten Crates `sar_source2` (`source2/`)

Mit dem Abschluss der InSAR-Erweiterung umfasst `source2/` insgesamt **8.734 Zeilen Rust- und GPU-Kernel-Code** (vollständig frei von Compiler- und Clippy-Warnungen):

| Datei | Zeilen | Rolle in der Gesamtarchitektur (Stripmap + TOPS + InSAR) |
| :--- | ---: | :--- |
| `src/01_types.rs` | 323 | Kerndatentypen (`Complex32`, `Vec3d`, `RadarMode`, `EchoMeta`, `SwathParams`, `BurstBlock`, `FocusedBurst`) |
| `src/02_l0.rs` | 688 | Eigenständiger 68-Byte-CCSDS-Scanner und `rayon`-paralleler Type-D-FDBAQ-/Bypass-Decoder |
| `src/03_ephem.rs` | 353 | Extraktion der Sub-Commutated-Orbitblöcke, gekoppelter Position-Geschwindigkeits-Polynomfit (`eval_smooth_orbit_poly`) und $V_{\text{eff}}(R)$ |
| `src/04_chirp.rs` | 246 | Nominale Chirp-Replika, Range-Matched-Filter (inkl. eingefaltetem Korrelations-Shift) und $k_a(R)$ |
| `src/05_ingest.rs` | 474 | Demultiplexing nach `swath_number`, Burst-Segmentierung und SWST-Ausrichtung |
| `src/06_rda.rs` | 604 | CPU-RDA (Stripmap), TOPS-Deramp-Fokussierung (`focus_tops_burst_cpu`) und phasenerhaltende InSAR-Fokussierung (`focus_tops_burst_insar_cpu`) |
| `src/07_tdbp.rs` | 128 | Exakte 3D-ECEF Time-Domain Backprojection (CPU-Schiedsrichter) |
| `src/08_cufft.rs` | 123 | Sichere Rust-Wrapper um `cufftPlanMany` und `cufftExecC2C` für Zeilen- und Spalten-Batches |
| `src/09_gpu.rs` | 937 | `#[cuda_oxide::kernel]`-GPU-Kerne für Stripmap, TOPS und InSAR (`tops_insar_phase_comp_inplace`, `insar_resample_and_ifg_kernel`) + `GpuSarContext` |
| `src/10_mosaic.rs` | 894 | Amplituden-Debursting (`deburst_swath`), Multi-Swath-Mosaikierung (`mosaic_swaths`) und dB-PNG/JSON-Export |
| `src/11_insar.rs` | 2.710 | Vollständige InSAR-Pipeline: 3D-Basislinie, WGS-84-Zero-Doppler-Geolokation, hybride Koregistrierung, ESD-Debursting, Multi-Swath-Mosaik, gradientenkompensierte Kohärenz, Goldstein-Filter, 2D-Unwrapping & Farb-PNG-Export |
| `src/main.rs` | 402 | Einheitliches CLI für Stripmap, TOPS-Mosaike und TOPS-InSAR (`--slave <PFAD>`) |
| `tests/integration.rs` | 825 | **10 End-to-End-Integrationstests** (6 Stripmap/TOPS-Basistests + 4 InSAR-Tests) |

---

### 9.2 Die 10 Integrationstests (`tests/integration.rs`)

Alle 10 Integrationstests laufen deterministisch über `cargo oxide test -- -j 1 --release`:

| # | Testfunktion | Geprüfte Eigenschaft & Abnahmekriterium |
| :---: | :--- | :--- |
| 1 | `test_l0_header_and_fdbaq_synthetic_and_real` | Bitgenaues CCSDS-Header-Parsing und FDBAQ-Dekodierung |
| 2 | `test_ephemeris_and_veff_geometry` | Sub-Comm-Orbit-Extraktion, glatter Polynomfit und $V_{\text{eff}} \in [7000, 7300]\text{ m/s}$ |
| 3 | `test_range_compression_point_target` | Chirp-Kompression exakt auf dem Ziel-Bin mit hohem Peak-to-Sidelobe-Abstand |
| 4 | `test_stripmap_rda_cpu_gpu_parity_and_tdbp` | CPU- vs. GPU-RDA-Parität ($< 10^{-3}$) und Übereinstimmung mit 3D-ECEF-TDBP |
| 5 | `test_tops_deramp_focus_point_target_cpu_and_gpu` | Synthetischer TOPS-Burst mit Antennenschwenk $k_t$: CPU- und GPU-Fokus-Parität |
| 6 | `test_deburst_and_multiswath_mosaic_synthetic` | Nahtloses Amplituden-Debursting und Multi-Swath-Mosaicking ohne Lücken |
| **7** | **`test_insar_orbit_baseline_and_zero_doppler_geolocation`** | **Prüft $B_{3\text{D}}, B_{\text{radial}}, B_{\text{cross}}, \Delta t_{\text{CA}}$ auf $< 5\text{ cm}$, WGS-84-Ellipsoid-Fehler $< 10^{-8}$, Zero-Doppler-Orthogonalität $< 10^{-12}$, Rechtsblick-Bedingung $(\mathbf{V}\times\mathbf{P})\cdot\mathbf{L} > 0$ und Selbst-Invertierbarkeit von `project_target_to_slave`** |
| **8** | **`test_insar_resample_and_form_ifg_cpu_gpu_parity`** | **Prüft Sub-Pixel-Resampling + Flat-Earth-Demodulation auf Phasenfehler $< 10^{-3}\text{ rad}$ sowie CPU-↔-GPU-Parität von `insar_resample_and_ifg_kernel` ($< 10^{-4}$)** |
| **9** | **`test_insar_coherence_goldstein_and_poisson_phase_unwrap`** | **Prüft auf steiler 2D-Fringe-Rampe: Gradienten-Kohärenz $> 0{,}98$, Rauschreduktion durch Goldstein-Filter ($< 0{,}65 \times \text{RMSE}_{\text{vorher}}$) und 2D-Phase-Unwrapping ($\text{RMSE} < 10^{-3}\text{ rad}$ für Priority-Flood und PCG-Poisson)** |
| **10** | **`test_insar_real_south_taiwan_single_burst_and_panel_downsample`** | **Realer End-to-End-InSAR-Test auf Burst 5 des Süd-Taiwan-Paares (IW1–IW3) inkl. Prüfung von Basislinie, Kohärenz und Panel-Kompression ($< 10\text{ MB}$)** |

---

### 9.3 CLI-Referenz für den InSAR-Modus

```bash
cd /workspace/src/cl-rust-generator/examples/34_copernicus_radar/source2

# 1. Kompilieren (inkl. NVPTX-GPU-Kernels via cargo-oxide)
cargo oxide build -- -j 1 --release

# 2. Vollständige Süd-Taiwan TOPS-InSAR-Pipeline (alle 36 Burst-Paare über IW1, IW2, IW3) auf der GPU
./target/release/sar_source2 \
  --input ../data/insar/S1C_IW_RAW__0SDV_20260928T215149_20260928T215222_009653_01336F_D2EE.SAFE/s1c-iw-raw-s-vv-20260928t215149-20260928t215222-009653-01336f.dat \
  --slave ../data/insar/S1C_IW_RAW__0SDV_20261010T215149_20261010T215222_009828_013950_CEB1.SAFE/s1c-iw-raw-s-vv-20261010t215149-20261010t215222-009828-013950.dat \
  --out-dir out/insar_taiwan \
  --prefix taiwan_insar \
  --multilook 6,24 \
  --goldstein-alpha 0.65 \
  --coh-win 2

# 3. Schneller Testlauf auf einem einzelnen Burst-Ausschnitt (z. B. Burst 5..7) oder einzelnem Sub-Swath (z. B. IW2 = 11)
./target/release/sar_source2 \
  --input ../data/insar/S1C_IW_RAW__0SDV_20260928T215149_20260928T215222_009653_01336F_D2EE.SAFE/s1c-iw-raw-s-vv-20260928t215149-20260928t215222-009653-01336f.dat \
  --slave ../data/insar/S1C_IW_RAW__0SDV_20261010T215149_20261010T215222_009828_013950_CEB1.SAFE/s1c-iw-raw-s-vv-20261010t215149-20261010t215222-009828-013950.dat \
  --swath 11 \
  --bursts 5,7 \
  --out-dir out/insar_quick \
  --prefix quick_iw2
```

---

## Kapitel 10: Glossar der TOPS-Interferometrie (InSAR)

| Begriff | Anschauliche und mathematische Bedeutung |
| :--- | :--- |
| **Baseband-Erhaltung** | Verzicht auf das quadratische Post-iFFT-Re-Ramping nach der TOPS-Azimut-Fokussierung (bei identischer Master-Schwenkrate $k_t$ für Master und Slave), damit die komplexe Phase bei der Sub-Pixel-Interpolation glatt bleibt. |
| **Basislinie ($\mathbf{B}, B_\perp, B_\parallel$)** | Räumlicher Abstandsvektor zwischen Master- und Slave-Satellitenbahn am Punkt der nächsten Annäherung. $B_\perp$ (senkrecht zum Sehstrahl) bestimmt die Empfindlichkeit für Geländehöhen, $B_\parallel$ (parallel zum Sehstrahl) den mittleren Range-Versatz. |
| **Closest Approach ($t_{\text{CA}}$)** | Zeitpunkt, an dem der Verbindungsvektor vom Bodenpunkt (bzw. Master-Satelliten) zum Satelliten exakt senkrecht auf dessen ECEF-Geschwindigkeitsvektor steht (Zero-Doppler-Bedingung). |
| **ESD (*Enhanced Spectral Diversity*)** | Phasenabgleich in den azimutalen Überlappungszonen benachbarter TOPS-Bursts über das Doppel-Differenz-Interferogramm $I_b \cdot I_{b+1}^*$, das den großen Blickwinkelunterschied zwischen Burst-Ende und Burst-Anfang ausnutzt. |
| **Flat-Earth-Phase ($\Delta\phi_{\text{flat}}$)** | Durch die gekrümmte Referenz-Erdoberfläche (WGS-84-Ellipsoid mit $h = 0\text{ m}$) und die Orbit-Basislinie erzeugte geometrische Phasenrampe $\frac{4\pi}{\lambda}(R_{S,0} - R_{M,0})$, die vor der topographischen Auswertung abgezogen wird. |
| **Fringe (Interferenzstreifen)** | Ein voller Farbzyklus ($-\pi \to +\pi$, also $2\pi\text{ rad} = 360^\circ$) im gewrappten Interferogramm; entspricht einer Zwei-Wege-Wegänderung von einer Wellenlänge $\lambda \approx 5{,}55\text{ cm}$ (bzw. $\lambda/2 \approx 2{,}77\text{ cm}$ in Sichtlinie oder $h_{2\pi}$ an Geländehöhe). |
| **Goldstein-Werner-Filter** | Nichtlineares adaptives Spektralfilter auf überlappenden $32 \times 32$-Patches im 2D-FFT-Raum, das die Spektralkoeffizienten mit der geglätteten Potenz $(S(u,v)/\max S)^\alpha$ gewichtet. |
| **Höhenambiguität ($h_{2\pi}$)** | Geländehöhendifferenz $\frac{\lambda R \sin\theta_{\text{inc}}}{2 |B_\perp|}$, die genau einen $2\pi$-Phasenzyklus erzeugt (beim Süd-Taiwan-Paar $2\,915{,}62\text{ m}$). |
| **Interferogramm ($I = M \cdot S^*$)** | Punktweises Produkt aus dem komplexen Master-SLC $M$ und dem komplex konjugierten, koregistrierten Slave-SLC $S^*$. |
| **Kohärenz ($|\gamma| \in [0, 1]$)** | Normierter komplexer Korrelationskoeffizient zwischen Master und Slave; Maß für die Phasenstabilität und das Signal-zu-Rausch-Verhältnis der interferometrischen Phase. |
| **Koregistrierung** | Sub-pixelgenaue geometrische und empirische Ausrichtung des Slave-SLCs auf das Pixelgitter $(a, r)$ des Master-SLCs. |
| **Phase Unwrapping (Phasenentfaltung)** | Rekonstruktion der wahren, stetigen Phase $\Phi(a, r) \in \mathbb{R}$ aus der nur modulo $2\pi$ bekannten Hauptwertphase $\psi(a, r) \in [-\pi, +\pi)$ durch Addition ganzzahliger Vielfacher von $2\pi$. |
