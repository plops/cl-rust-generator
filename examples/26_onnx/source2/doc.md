# Echtzeit-Objekterkennung auf dem Desktop mit YOLO, ONNX Runtime und Macroquad

Dieses Programm erfasst fortlaufend einen Bildschirmausschnitt unter X11 (Linux), führt darauf mithilfe eines eingebetteten YOLO-Modells (`yolo26n.onnx`) eine Objekterkennung durch und visualisiert das Live-Bild samt erkannter Bounding Boxes in einem eigenen Fenster.

---

## 1. Verwendete Bibliotheken & Kernkonzepte

* **`macroquad`**: Ein leichtgewichtiges 2D-Game-Framework für Rust. Es übernimmt hier die Fenstererstellung, das Rendern der Desktop-Aufnahme und das Zeichnen der Bounding Boxes.
* **`ort` (ONNX Runtime bindings)**: Führt das vortrainierte neuronale Netz zur Objekterkennung aus.
* **`x11rb`**: Eine reine Rust-Implementierung des X11-Protokolls. Dient zum direkten Auslesen von Bildschirminhalten ohne externe C-Bibliotheken wie `libX11`.
* **Kompilierzeit-Einbettung**: Über `include_bytes!("../yolo26n.onnx")` werden die Modellgewichte direkt in den `.rodata`-Abschnitt der Binärdatei kompiliert. Es wird zur Laufzeit keine externe `.onnx`-Datei auf der Festplatte benötigt.

---

## 2. Detaillierte Code-Erklärung

### 2.1 Initialisierung und Setup

```rust
const SIZE: usize = 640;
const PLANE: usize = SIZE * SIZE;
const MODEL_BYTES: &[u8] = include_bytes!("../yolo26n.onnx");
```
* **Auflösung**: YOLO-Modelle erwarten üblicherweise quadratische Eingabebilder der Größe $640 \times 640$ Pixel.
* `PLANE`: Entspricht der Pixelanzahl einer einzelnen Farbkomponente ($640 \cdot 640 = 409.600$).

In `main()`:
1. **X11-Verbindung**: Stellt die Verbindung zum X-Server her (`x11rb::connect(None)`) und identifiziert das Root-Fenster (den Desktop).
2. **ONNX-Session**: Erstellt eine Inferenz-Sitzung direkt aus dem Arbeitsspeicher (`commit_from_memory(MODEL_BYTES)`).
3. **Puffer-Initialisierung**:
   * `img` & `tex`: Macroquad-Bild- und Texturspeicher zur Darstellung auf der GPU.
   * `input`: Ein flacher `Vec<f32>` mit $3 \times \text{PLANE}$ Elementen für das Tensor-Format **NCHW** (Batch, Channels, Height, Width).

---

### 2.2 Bildschirmaufnahme und Bild-Vorverarbeitung

In jedem Schleifendurchlauf wird die linke obere Ecke des Bildschirms ausgelesen:

```rust
let reply = xproto::get_image(
    &conn,
    ImageFormat::Z_PIXMAP,
    root,
    0, 0,
    SIZE as u16, SIZE as u16,
    u32::MAX,
).unwrap().reply().unwrap();
```

* **Format**: X11 liefert Pixel im Format `Z_PIXMAP` (in der Regel 32-Bit pro Pixel: `BGRA`).
* **NCHW-Layout-Aufteilung**:
  ```rust
  let (r_plane, rest) = input.split_at_mut(PLANE);
  let (g_plane, b_plane) = rest.split_at_mut(PLANE);
  ```
  Neuronalen Netze erwarten Bildkanäle meist getrennt (Planar / NCHW: alle R-Werte, dann alle G-Werte, dann alle B-Werte), während Bildschirme und Texturen Interleaved/Packed (RGBARGBA...) nutzen.
* **Pixel-Konvertierung & Normalisierung**:
  ```rust
  for (i, px) in reply.data.chunks_exact(4).take(PLANE).enumerate() {
      let (b, g, r) = (px[0], px[1], px[2]);
      // ... Kopieren für die Anzeige ...
      r_plane[i] = r as f32 / 255.0;
      g_plane[i] = g as f32 / 255.0;
      b_plane[i] = b as f32 / 255.0;
  }
  ```
  Die Bytewerte ($0 \dots 255$) werden in Fließkommazahlen im Bereich $[0.0, 1.0]$ skaliert.

---

### 2.3 Wie die Modellausgabe geparst wird

Nach Ausführung der Inferenz:

```rust
let outputs = session.run(...).unwrap();
let (_, dets) = outputs[0].try_extract_tensor::<f32>().unwrap();
```

Das Modell liefert ein Array von Detektionen mit Post-Processing/NMS (Non-Maximum Suppression). Die Ausgabedaten werden in 6er-Schritten (`chunks_exact(6)`) geparst:

```rust
for det in dets.chunks_exact(6) {
    if det[4] >= 0.1 {
        draw_rectangle_lines(det[0], det[1], det[2] - det[0], det[3] - det[1], 2.0, RED);
    }
}
```

Jedes Element `det` besteht aus 6 aufeinanderfolgenden `f32`-Werten:
1. `det[0]`: $x_1$ bzw. $x_{\min}$ (Linke Kante der Box in Pixeln)
2. `det[1]`: $y_1$ bzw. $y_{\min}$ (Obere Kante der Box in Pixeln)
3. `det[2]`: $x_2$ bzw. $x_{\max}$ (Rechte Kante der Box in Pixeln)
4. `det[3]`: $y_2$ bzw. $y_{\max}$ (Untere Kante der Box in Pixeln)
5. `det[4]`: **Konfidenzwert / Score** (Wahrscheinlichkeit im Bereich $0.0 \dots 1.0$)
6. `det[5]`: **Klassen-ID** (z. B. `0.0` für Person, `1.0` für Fahrrad nach COCO-Datensatz – wird hier ignoriert)

---

### 2.4 Welche Rechtecke gezeichnet werden

* **Filterbedingung**: `if det[4] >= 0.1`
  * Es werden nur Erkennungen gezeichnet, deren Konfidenz mindestens **10 %** beträgt. Schwächere Detektionen werden verworfen.
* **Berechnung der Abmessungen**:
  Macroquads Funktion `draw_rectangle_lines(x, y, width, height, thickness, color)` erwartet Koordinaten und Dimensionen:
  * **$X$-Position**: `det[0]` ($x_1$)
  * **$Y$-Position**: `det[1]` ($y_1$)
  * **Breite ($w$)**: `det[2] - det[0]` ($x_2 - x_1$)
  * **Höhe ($h$)**: `det[3] - det[1]` ($y_2 - y_1$)
* **Stil**: Rote Umrandung (`RED`) mit einer Linienstärke von `2.0` Pixeln.

---

### 2.5 Was die Bildwiederholrate (Frame Rate) bestimmt

Die resultierende Bildwiederholrate wird durch die langsamste Komponente der synchron ablaufenden Hauptschleife bestimmt:

1. **`next_frame().await` & V-Sync**:
   * Macroquad koppelt die Bildwiederholrate standardmäßig an die vertikale Synchronisation (**V-Sync**) des Monitors (typischerweise 60 Hz bzw. 60 FPS).
   * Selbst wenn der Code extrem schnell wäre, limitiert `next_frame().await` die Obergrenze auf die Bildwiederholrate des Bildschirms.

2. **Inferenzzeit des neuronalen Netzes (`session.run`)**:
   * Der dominierende Engpass. Wird das Modell auf der CPU ausgeführt (Standard-Provider in ONNX Runtime), dauert ein Vorwärtsdurchlauf (Forward Pass) je nach Prozessor meist zwischen **10 und 50 Millisekunden**.
   * Dauert ein Inferenz-Schritt z. B. 33 ms, kann die Anwendung maximal $\approx 30\text{ FPS}$ erreichen.

3. **Synchroner X11-Screen-Grab (`xproto::get_image`)**:
   * Die X11-Anfrage fordert Bilddaten synchron über einen IPC-/Unix-Domain-Socket an. Dies erfordert Kontextwechsel und Datentransfers zwischen X-Server und Client, was pro Frame einige Millisekunden beansprucht.

4. **CPU-Vorverarbeitungsschleife**:
   * Das Umformatieren von $640 \times 640 = 409.600$ Pixeln von `BGRA` nach planar `f32` ist cache-intensiv, bei modernen CPUs jedoch in der Regel im Bereich von $< 2\text{ ms}$ abgeschlossen.

> **Zusammenfassend:** Die Obergrenze wird durch V-Sync vorgegeben (z. B. 60 FPS). In der Praxis wird die tatsächliche Framerate jedoch fast vollständig durch die **Latenz der ONNX-Inferenzzeit** und den **X11-Screenshot-Aufruf** limitiert.
