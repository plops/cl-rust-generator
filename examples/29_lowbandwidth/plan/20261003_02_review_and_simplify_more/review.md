Dein Code ist architektonisch hervorragend. Er ist extrem fokussiert, nutzt Rusts Stärken (Slices, Iteratoren, `serde`) optimal aus und verzichtet auf unnötigen Ballast (wie komplexe `async`-Runtimes für ein simples MVP). 

Trotzdem gibt es genau an den zwei von dir angesprochenen Punkten (OCR-Fallback und AV1-Kacheln) noch Potenzial, um **Code zu sparen** und vor allem die **6kB/s-Grenze** in der Praxis nicht durch Overhead zu sprengen.

Hier ist die Analyse und der beste Weg für dein Ziel:

### 1. OCR-Fallback entfernen (Mehr Minimalismus)
Da 6kB/s (ca. 48 kbit/s) das harte Ziel ist, ist die Textextraktion *essenziell*. Ohne OCR wird der Bildschirm nicht maskiert, und das Senden von reinem AV1-Text würde die Bandbreite sprengen. Ein Fallback auf "Ohne OCR" ist in diesem Szenario also ohnehin nutzlos.

**Wie du Code sparst:**
Du kannst das `enum Ocr` komplett auflösen und stattdessen nur die Struktur behalten.
*   Lösche `--no-ocr` aus der `Config`.
*   Lösche `enum Ocr`. Behalte nur das jetzige `OcrInner` und nenne es `Ocr`.
*   Wenn die Modelle in `Ocr::load` nicht gefunden werden, gib einfach ein hartes `Err("Modelle fehlen")` zurück und lass den Server beenden.

*Ersparnis:* Keine Optionals beim Aufruf, keine Match-Statements im Server-Loop, weniger CLI-Parsing-Code.

---

### 2. Die AV1-Kachel-Strategie (Dein Bauchgefühl war richtig!)
Du fragst, ob das feste 64x64-Raster okay ist oder ob das alte "Zusammenfassen" besser war.
**Antwort: Bei 6kB/s ist das feste 64x64-Raster tödlich.**

**Warum? Der OBU-Header-Overhead.**
Du nutzt AV1-Still-Pictures (Intra-Frames). Jeder AV1-Frame (egal wie klein) benötigt Sequence Header, Frame Header und Tile Group OBUs. Bei `rav1e` sind das selbst bei einem komplett einfarbigen Bild ca. **30 bis 50 Byte purer Overhead**.
*   **Szenario Raster:** Jemand tippt einen Satz. 10 kleine 64x64-Kacheln ändern sich minimal. Du sendest 10 unabhängige AV1-Bilder.
    *Overhead: 10 * 50 Byte = 500 Byte pro Frame.* Bei 10 FPS sind das **5 kB/s nur für Header!** Dein 6kB/s-Budget ist sofort aufgebraucht.
*   **Szenario Zusammenfassen:** AV1 ist exzellent darin, große Flächen zu komprimieren (Blockgrößen bis 128x128). Wenn du die 10 Kacheln zu einem einzigen Rechteck (Bounding Box) zusammenfasst, zahlst du den Header-Overhead nur *einmal* (50 Byte). Die zusätzlichen (unveränderten) Pixel innerhalb des Rechtecks komprimiert AV1 als "Skip-Blöcke" fast auf 0 Byte weg.

#### Der beste (und minimalistischste) Weg: Single Bounding Box
Du musst keine komplizierten "Connected Components" (Inseln) berechnen, um Bandbreite zu sparen. Das bläht den Code auf. Der beste Kompromiss aus *minimalem Code* und *minimaler Datenrate* ist die **globale Bounding Box der Änderungen**.

Statt ein `Vec<Rect>` mit 64x64-Kacheln zurückzugeben, suchst du einfach die äußersten Punkte (`min_x`, `min_y`, `max_x`, `max_y`), an denen sich das Bild geändert hat, und sendest **genau ein** AV1-Bild pro Frame.

**Anpassung im Server (`04_tiles.rs`):**
```rust
/// Findet das kleinstmögliche umschließende Rechteck aller Änderungen.
/// (Muss ein Vielfaches von 2 sein für rav1e, idealerweise mind. 16x16).
pub fn dirty_bbox(prev: Option<&RgbImage>, cur: &RgbImage) -> Option<Rect> {
    let prev = match prev {
        Some(p) => p,
        None => return Some(Rect::new(0, 0, 640, 640)), // Erstes Bild = Vollbild
    };

    let (w, h) = cur.dimensions();
    let (mut x0, mut y0, mut x1, mut y1) = (w, h, 0, 0);

    for y in 0..h {
        for x in 0..w {
            let px_c = cur.get_pixel(x, y);
            let px_p = prev.get_pixel(x, y);
            if px_c != px_p {
                x0 = x0.min(x);
                y0 = y0.min(y);
                x1 = x1.max(x);
                y1 = y1.max(y);
            }
        }
    }

    if x0 > x1 {
        return None; // Keine Änderung
    }

    // Für rav1e müssen Breite/Höhe gerade Zahlen und >= MIN_TILE (16) sein
    let mut bw = (x1 - x0 + 1).max(16);
    let mut bh = (y1 - y0 + 1).max(16);
    if bw % 2 != 0 { bw += 1; }
    if bh % 2 != 0 { bh += 1; }
    
    // Sicherstellen, dass wir nicht über den rechten/unteren Rand ragen
    let bx = if x0 + bw > w { w - bw } else { x0 };
    let by = if y0 + bh > h { h - bh } else { y0 };

    Some(Rect::new(bx as u16, by as u16, bw as u16, bh as u16))
}
```

**Anpassung in der Server-Session (`07_session.rs`):**
Anstatt einer `for r in dirty_tiles(...)`-Schleife hast du nun:
```rust
if let Some(r) = dirty_bbox(prev.as_ref(), &masked) {
    let rgb = crop_rgb(&masked, r);
    // Nur noch EIN Encode-Aufruf pro Frame!
    let data = encode_rgb(&rgb, r.w as usize, r.h as usize, params)?;
    let msg = ServerMsg::Tile { x: r.x, y: r.y, data };
    write_msg(&mut wr, &msg)?;
}
```

**Anpassung im Client (`04_scene.rs`):**
Dein Client muss die feste `TILE`-Konstante (`64`) beim Zeichnen vergessen. Glücklicherweise liefert dein AV1-Decoder (`02_av1.rs`) bereits die tatsächliche Breite und Höhe des dekodierten Bildes im `Rgba`-Struct (`rgba.w` und `rgba.h`)!

Ändere das Blitting so, dass es sich an die empfangene Größe anpasst:
```rust
// Vorher war T fest 64. Jetzt dynamisch:
pub fn blit(&mut self, x: u16, y: u16, rgba: &crate::av1::Rgba) {
    let (x0, y0) = (x as usize, y as usize);
    let (w, h) = (rgba.w, rgba.h);
    if x0 + w > N || y0 + h > N || rgba.data.len() < w * h * 4 {
        return;
    }
    for row in 0..h {
        let s = row * w * 4;
        let d = ((y0 + row) * N + x0) * 4;
        self.canvas[d..d + w * 4].copy_from_slice(&rgba.data[s..s + w * 4]);
    }
    self.dirty = true;
}
```
*(Vergiss nicht, das Event in `03_net.rs` so anzupassen, dass es Breite und Höhe übergibt oder einfach das komplette `Rgba`-Struct).*

### Fazit
1. **Der Code wird durch den Wechsel auf `dirty_bbox` sogar noch einfacher**, weil das `TILE`-Raster komplett entfällt. 
2. **Die Bitrate sinkt dramatisch**, da AV1 größere Rechtecke besser komprimieren kann (Skip-Blöcke für den unveränderten Hintergrund im Rechteck) und du pro Frame nur noch *einmal* Header-Overhead (ca. 50 Byte) sendest statt potenziell *N-mal*. Für 6kB/s ist dieser Schritt fast schon zwingend erforderlich.
