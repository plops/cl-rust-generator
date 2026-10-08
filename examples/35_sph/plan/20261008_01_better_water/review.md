Die Beobachtung ist **physikalisch und numerisch vollkommen zutreffend**: Das Fluid verhält sich in der aktuellen Implementierung eher wie ein **expansives Gas oder trockener Sand** als wie zusammenhängendes Wasser.

Dafür gibt es mehrere konkrete Ursachen in der mathematischen Formulierung, der Parameterwahl und der Numerik:

---

### 1. Das Hauptproblem: Null Kohäsion / Keine Oberflächenspannung

In `src/03_sph_math.rs` ist die Zustandsgleichung (Equation of State) definiert als:
```rust
pub fn pressure(density: f32, rest_density: f32, stiffness: f32) -> f32 {
    (stiffness * (density - rest_density)).max(0.0)
}
```
* **Nur Abstoßung, keine Anziehung:** Wenn Partikel komprimiert werden ($\rho > \rho_0$), stoßen sie sich ab. Sobald sie sich aber voneinander entfernen ($\rho < \rho_0$), wird der Druck auf **exakt 0 geklemmt**. 
* **Wasser besitzt jedoch Kohäsion und Oberflächenspannung:** In echten Flüssigkeiten ziehen sich Moleküle an den Grenzflächen an. Ohne einen Kohäsions- oder Oberflächenspannungsterm (wie z. B. nach *Müller et al. 2003* via Farbfeld $\nabla^2 c$ oder *Akinci et al. 2013*) gibt es im SPH-Modell **keine Kraft, die Wassertropfen oder freie Oberflächen zusammenhält**.
* Nach einem Aufprall oder Spritzer gibt es nichts, was die Partikel wieder zu einem zusammenhängenden Tropfen oder Schwall bündelt – sie fliegen als Einzelfragmente durch die Domäne.

---

### 2. Akustische CFL-Verletzung: $dt$ ist zu groß für Steifigkeit $k$

In SPH breiten sich Druckwellen mit der numerischen Schallgeschwindigkeit $c_s = \sqrt{\partial P / \partial \rho} = \sqrt{k}$ aus.
* Hier gilt: $k = 2000 \implies c_s = \sqrt{2000} \approx 44{,}7\text{ m/s}$.
* Die **Courant-Friedrichs-Lewy (CFL)-Stabilitätsbedingung** verlangt:
  $$\Delta t \le 0{,}25 \cdot \frac{h}{c_s + v_{\max}}$$
  Mit $h = 0{,}04\text{ m}$ und $v_{\max} \approx 12\text{ m/s}$ ergibt sich ein maximaler stabiler Zeitschritt von:
  $$\Delta t_{\text{CFL}} \approx 0{,}25 \cdot \frac{0{,}04}{44{,}7 + 12} \approx \mathbf{0{,}00017\text{ s}}$$
* In `SimConfig` ist jedoch **`dt = 0.0008 s`** eingestellt – fast **das 5-fache des Stabilitätslimits**.
* **Folge:** Partikel dringen während eines Zeitschritts zu tief in benachbarte Partikel ein. Im nächsten Schritt resultiert das in astronomischen Druckkräften, die Partikel **explosionsartig wegschleudern** ("Partikel-Explosionen").
* Ein klarer Beleg dafür im Code ist das harte Geschwindigkeits-Cap in `k_integrate`:
  ```rust
  let vmax = 12.0;
  if s2 > vmax * vmax { ... }
  ```
  Ohne diesen künstlichen Dämpfer würde die Simulation aufgrund der CFL-Verletzung sofort ins Unendliche explodieren.

---

### 3. Falsches Verhältnis von Glättungslänge $h$ zu Partikelabstand $s$

* Bei $N = 16\,384$ Partikeln im Damm ($0{,}45 \cdot 1{,}6\text{ m} \times 0{,}85 \cdot 1{,}0\text{ m}$) beträgt der Partikelabstand:
  $$s = \sqrt{\frac{0{,}612}{16\,384}} \approx 0{,}0061\text{ m}$$
* Die Glättungslänge ist jedoch fest auf **$h = 0{,}04\text{ m}$** gesetzt.
* Das Verhältnis ist:
  $$\frac{h}{s} \approx 6{,}5$$
* In der Standard-SPH-Literatur wählt man üblicherweise **$h \approx 1{,}2 \cdot s$ bis $2{,}0 \cdot s$**.
* Bei $h / s \approx 6{,}5$ liegen im 2D-Kreis $\pi \cdot (6{,}5)^2 \approx \mathbf{130\text{ Nachbarn}}$ pro Partikel! Da der Poly6-Kernel mit $1/h^8$ und Spiky mit $1/h^5$ skaliert, summieren sich über hunderte Nachbarn enorme Druckkräfte auf, während die Partikel extrem weit voneinander entfernt schon stark wechselwirken.

---

### 4. Rand-Katapult-Effekt an der freien Oberfläche

* An einer freien Oberfläche (wo die Flüssigkeit an Luft/Vakuum grenzt) hat ein Randpartikel nur auf der Innenseite Nachbarn.
* Seine berechnete Dichte beträgt dort nur ca. $\rho \approx 500\text{ kg/m}^3$ (gegenüber $\rho_0 = 1000$).
* Die Partikel eine Schicht weiter innen haben aber $\rho \approx 1000\text{ kg/m}^3$ und damit positiven Druck.
* Der Druckterm $\frac{P_{\text{innen}}}{\rho_{\text{innen}}^2} \nabla W$ erzeugt eine reine Abstoßungskraft nach außen. Die äußerste Schicht Partikel wird kontinuierlich **nach außen wegkatapultiert**, schert ab und zerstäubt.

---

### 5. Reine Punkt-Visualisierung

In `src/07_renderer.rs` wird jedes Partikel als diskretes, 2–6 Pixel breites Quadrat gezeichnet (`draw_rectangle`).
* Es gibt keine Oberflächenrekonstruktion (keine Marching-Squares-Isolinie, kein Metaball-Blurring/Screen-Space Fluid).
* Selbst physikalisch korrekt fließendes Wasser wirkt als Wolke isolierter Punkte für das Auge immer wie Sprühnebel oder Sand.

---

### Wie lässt sich das beheben?

#### A. Sofortmaßnahmen über CLI / Parameter (ohne Code-Änderung)
Das Auseinanderfliegen lässt sich sofort drastisch reduzieren, indem man die Zeitschritt- und Steifigkeitsbalance wieder in das CFL-Fenster bringt:
1. **Zeitschritt verkleinern:**
   `--dt 0.0002` (oder `--substeps 8` in der GUI).
2. **Glättungsradius anpassen:**
   Für $N = 16\,384$ ($s \approx 0{,}006\text{ m}$) einen passenderen Radius wählen, z. B. `--h 0.015`.
3. **Viskosität leicht erhöhen:**
   Die aktuelle Viskosität $\mu = 0{,}1$ dämpft bei $dt = 0{,}0008$ zu schwach gegen die explosiven Druckstöße.

#### B. Code-Verbesserungen (für echtes Wasser-Verhalten)
1. **Oberflächenspannung / künstliche Kohäsion implementieren:**
   Ein einfacher Cohesion-Kernel (nach *Akinci et al. 2013* oder *Tartakovsky*), der Partikel bei Distanzen $r > s$ sanft zueinander zieht, verhindert das Zerstäuben komplett.
2. **Alternative: Tait-Gleichung mit schwachem Unterdruck zulassen:**
   Statt `pressure.max(0.0)` erlaubt man eine moderate negative Zugspannung (z. B. $P = k((\rho/\rho_0)^7 - 1)$ geklemmt auf z. B. $-0{,}2 \cdot k$), was wie ein innerer Zusammenhalt wirkt.
3. **Wandkollision dämpfen:**
   `wall_damping` von $0{,}5$ auf $0{,}1$–$0{,}2$ senken, damit Partikel am Boden flüssig zusammenlaufen, anstatt elastisch wie Flummis abzuprallen.
