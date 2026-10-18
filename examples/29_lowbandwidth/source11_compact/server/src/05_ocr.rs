//! `05_ocr` — Texterkennung als Ganzes: Detektion + Erkennung + Farben.
//!
//! [`Ocr::text`] orchestriert [`Detector`](crate::detect) und
//! [`Recognizer`](crate::recognize): Boxen finden, je Zeile lesen, Farben
//! samplen. OCR ist Pflicht: ohne Modelle startet der Server nicht (reines
//! AV1-Textbild würde das 6-kB/s-Budget sprengen).

use image::RgbImage;

use lbw_common::{Rect, TextItem};
/// FNV-1a/64 (5 Zeilen Hand-Code, stabil über Prozesse hinweg).
struct Fnv1a64(u64);

impl Fnv1a64 {
    fn new() -> Self {
        Self(0xcbf29ce484222325)
    }
    fn update(&mut self, b: &[u8]) {
        for x in b {
            self.0 ^= u64::from(*x);
            self.0 = self.0.wrapping_mul(0x100000001b3);
        }
    }
    fn finish(&self) -> u64 {
        self.0
    }
}

use crate::detect::{Detector, Provider, clip_to, pad_to_32, px};
use crate::recognize::Recognizer;
use crate::tiles::pad_rect;

/// Erkennungs-Rand je Seite: Detektions-Boxen schneiden Glyphen (z. B.
/// Umlaut-Punkte) haarscharf ab — mit Weißraum liest das Netz deutlich besser
/// (Befund aus `26_onnx`: CER 69→4 %). Per Xvfb/xterm-Sweep bestimmt.
const REC_PAD: u16 = 4;
/// Mindest-Konfidenz, sonst bleibt die Zeile Bildinhalt.
const MIN_TEXT_CONF: f32 = 0.5;
/// Einzelzeichen brauchen deutlich mehr Konfidenz: Der Detektor liefert auf
/// Karten/Icons viele Kleinst-Fragmente (rotierte Labels, Punkte), deren
/// Ratetext den Client zumüllen würde — echte UI-Zeichen („X“, „+“) liest das
/// Netz dagegen mit ≥ 0.9.
const MIN_SINGLE_CONF: f32 = 0.75;
/// Max. ungecachte Erkennungen je Frame (Rechenzeit-Deckel während
/// Bewegung; der Rest kommt in Folgeframes — gecachte Zeilen sind frei).
pub const REC_BUDGET: usize = 32;
/// Max. erkannte Zeilen je Frame (Rechenzeit-Deckel).
const MAX_LINES: usize = 160;
/// Max. Cache-Einträge (danach alles verwerfen — einfache Politik).
const CACHE_CAP: usize = 1024;

/// FNV-1a über Box-Maße + Pixel (Cache-Schlüssel; gleiche Pixel = gleiches
/// Ergebnis — inklusive „Müll"-Urteilen, die sonst jeden Frame neu kämen).
fn hash_rect(img: &RgbImage, r: Rect) -> u64 {
    let mut h = Fnv1a64::new();
    h.update(&r.w.to_le_bytes());
    h.update(&r.h.to_le_bytes());
    let (iw, ih) = (img.width() as usize, img.height() as usize);
    if iw == 0 || ih == 0 {
        return h.finish();
    }
    let x0 = (r.x as usize).min(iw - 1);
    let y0 = (r.y as usize).min(ih - 1);
    let x1 = (x0 + r.w as usize).min(iw);
    let y1 = (y0 + r.h as usize).min(ih);
    for y in y0..y1 {
        for x in x0..x1 {
            h.update(&px(img, x, y));
        }
    }
    h.finish()
}

/// Erkannte Zeile im Cache (Inhalt + Konfidenz + Farben).
#[derive(Clone, Debug, PartialEq)]
struct CachedLine {
    text: String,
    conf: f32,
    fg: [u8; 3],
    bg: [u8; 3],
}

/// Cache für Erkennungsergebnisse: Statische Zeilen kosten nach dem ersten
/// Frame ~0 ms (kein `recognize`, kein `sample_colors`). Auch Negativ-Urteile
/// (leer/unsicher) werden gecacht — sonst kämen Müll-Boxen jeden Frame neu.
#[derive(Default)]
pub struct TextCache {
    map: std::collections::HashMap<u64, CachedLine>,
    hits: u64,
    lookups: u64,
}

impl TextCache {
    fn get(&mut self, key: u64) -> Option<CachedLine> {
        self.lookups += 1;
        let c = self.map.get(&key).cloned();
        if c.is_some() {
            self.hits += 1;
        }
        c
    }

    fn insert(&mut self, key: u64, line: CachedLine) {
        if self.map.len() >= CACHE_CAP {
            self.map.clear();
        }
        self.map.insert(key, line);
    }

    /// (Treffer, Zugriffe) seit Start.
    #[must_use]
    pub fn stats(&self) -> (u64, u64) {
        (self.hits, self.lookups)
    }
}

/// Behält die Zeile (`true`) oder lässt sie Bildinhalt bleiben.
/// Einzelzeichen nur bei hoher Konfidenz (Kleinst-Fragmente filtern).
#[must_use]
pub fn keep_line(text: &str, conf: f32) -> bool {
    if text.is_empty() {
        return false;
    }
    if text.chars().count() <= 1 {
        conf >= MIN_SINGLE_CONF
    } else {
        conf >= MIN_TEXT_CONF
    }
}

/// Max. Breite einer verschmolzenen Zeile: Echte Titel/Labels passen
/// (Titelzeile ~240 px), bandbreite Brücken-Ketten (Karten-Overlays, die per
/// hoher Box Bänder verbinden) würden sonst ganze Reihen zu einer
/// Müll-Zeile verketten.
const MERGE_MAX_W: u16 = 320;

/// Verschmilzt Detektor-Fragmente derselben Zeile („Directi“ + „ons“):
/// gleiche Grundlinie (Mitten-Differenz ≤ halbe Mindesthöhe), Lücke ≤ halbe
/// Mindesthöhe, ähnliche Höhe (keine Band-Brücken über hohe Boxen) und
/// Gesamtbreite ≤ [`MERGE_MAX_W`]. Spalten/Tabs (Lücken ≫ Zeilenhöhe)
/// bleiben getrennt. Fixpunkt-Schleife, damit Ketten („Ca“ + „eck“ + „.“)
/// ganz schließen — die Breiten- und Höhen-Bremse begrenzt den Schaden.
#[must_use]
pub fn merge_rows(mut boxes: Vec<Rect>) -> Vec<Rect> {
    // Band-Ordnung wie der Detektor: Zeilenfragmente liegen benachbart.
    boxes.sort_by_key(|b| (b.y / 16, b.x));
    loop {
        let mut out: Vec<Rect> = Vec::with_capacity(boxes.len());
        let mut changed = false;
        for b in boxes {
            let mut merged = false;
            if let Some(last) = out.last_mut() {
                let (hmin, hmax) = (
                    f32::from(last.h.min(b.h).max(1)),
                    f32::from(last.h.max(b.h).max(1)),
                );
                let cy = f32::from(last.y) + f32::from(last.h) / 2.0;
                let by = f32::from(b.y) + f32::from(b.h) / 2.0;
                let gap = (f32::from(b.x) - f32::from(last.x + last.w)).max(0.0);
                let x2 = (last.x + last.w).max(b.x + b.w);
                if (cy - by).abs() <= hmin / 3.0
                    && gap <= hmin / 2.0
                    && hmax <= 2.5 * hmin
                    && x2 - last.x.min(b.x) <= MERGE_MAX_W
                {
                    let y2 = (last.y + last.h).max(b.y + b.h);
                    last.x = last.x.min(b.x);
                    last.y = last.y.min(b.y);
                    last.w = x2 - last.x;
                    last.h = y2 - last.y;
                    changed = true;
                    merged = true;
                }
            }
            if !merged {
                out.push(b);
            }
        }
        boxes = out;
        if !changed {
            break;
        }
    }
    boxes.sort_by_key(|b| (b.y / 16, b.x));
    boxes
}

/// Schnittfläche zweier Rechtecke in px² (0 ohne Überlappung).
fn overlap_area(a: Rect, b: Rect) -> i32 {
    let (ax, ay) = (u32::from(a.x), u32::from(a.y));
    let (bx, by) = (u32::from(b.x), u32::from(b.y));
    let x0 = ax.max(bx);
    let y0 = ay.max(by);
    let x1 = (ax + u32::from(a.w)).min(bx + u32::from(b.w));
    let y1 = (ay + u32::from(a.h)).min(by + u32::from(b.h));
    (x1.saturating_sub(x0) * y1.saturating_sub(y0)) as i32
}

fn dist2(a: [u8; 3], b: [u8; 3]) -> u32 {
    a.iter()
        .zip(b)
        .map(|(x, y)| u32::from(x.abs_diff(y)).pow(2))
        .sum()
}

fn acc(s: &mut [u32; 3], p: [u8; 3]) {
    for (a, v) in s.iter_mut().zip(p) {
        *a += u32::from(v);
    }
}

/// Dominante Hintergrund- und Schriftfarbe einer Textbox.
/// `bg` = Mittel der häufigsten (auf 4 bit quantisierten) Randfarbe;
/// `fg` = Mittel der Innenpixel mit ≥ 50 % der maximalen Distanz zu `bg`.
#[must_use]
pub fn sample_colors(img: &RgbImage, r: Rect) -> ([u8; 3], [u8; 3]) {
    let (w, h) = (img.width() as usize, img.height() as usize);
    let x0 = (r.x as usize).min(w.saturating_sub(1));
    let y0 = (r.y as usize).min(h.saturating_sub(1));
    let x1 = (x0 + r.w as usize).min(w).saturating_sub(1).max(x0);
    let y1 = (y0 + r.h as usize).min(h).saturating_sub(1).max(y0);
    let mut bins: std::collections::HashMap<u16, ([u32; 3], u32)> = Default::default();
    let mut add = |p: [u8; 3]| {
        let k = (u16::from(p[0] >> 4) << 8) | (u16::from(p[1] >> 4) << 4) | u16::from(p[2] >> 4);
        let e = bins.entry(k).or_default();
        acc(&mut e.0, p);
        e.1 += 1;
    };
    for x in x0..=x1 {
        add(px(img, x, y0));
        add(px(img, x, y1));
    }
    for y in y0..=y1 {
        add(px(img, x0, y));
        add(px(img, x1, y));
    }
    let (sum, n) = bins.values().max_by_key(|(_, n)| *n).copied().unwrap();
    let bg = sum.map(|s| ((s + n / 2) / n) as u8);

    let mut maxd = 0;
    for y in y0..=y1 {
        for x in x0..=x1 {
            maxd = maxd.max(dist2(px(img, x, y), bg));
        }
    }
    if maxd < 30 * 30 {
        // Kaum Kontrast: Schrift in Schwarz/Weiß je nach Helligkeit.
        let luma = u32::from(bg[0]) * 3 + u32::from(bg[1]) * 6 + u32::from(bg[2]);
        return (if luma > 1280 { [0; 3] } else { [255; 3] }, bg);
    }
    let (mut s, mut cnt) = ([0u32; 3], 0u32);
    for y in y0..=y1 {
        for x in x0..=x1 {
            let p = px(img, x, y);
            if dist2(p, bg) * 4 >= maxd {
                acc(&mut s, p);
                cnt += 1;
            }
        }
    }
    (s.map(|v| ((v + cnt / 2) / cnt) as u8), bg)
}

/// Geladene Texterkennung (Pflicht: ohne Modelle kein Serverstart).
pub struct Ocr {
    det: Detector,
    rec: Recognizer,
    cache: TextCache,
    ep: &'static str,
    /// Letzte Inferenz-Zeiten in ms (Detektion, Erkennung gesamt).
    pub last_ms: (f64, f64),
}

impl Ocr {
    /// Lädt `PP-OCRv6_small_{det,rec}.onnx` + `inference.yml` aus `dir`.
    /// Fehlt eine Datei, ist das ein harter Fehler (kein Fallback: ohne
    /// Textmaskierung sprengt AV1-Text das Bandbreiten-Budget).
    /// Loggt den aktiven Execution-Provider auf stderr.
    pub fn load(dir: &str, threads: usize, provider: Provider) -> Result<Self, String> {
        let (det, rec, dict) = (
            format!("{dir}/PP-OCRv6_small_det.onnx"),
            format!("{dir}/PP-OCRv6_small_rec.onnx"),
            format!("{dir}/inference.yml"),
        );
        for p in [&det, &rec, &dict] {
            if !std::path::Path::new(p).exists() {
                return Err(format!("Modell fehlt: {p}"));
            }
        }
        let det = Detector::new(&det, threads, provider)?;
        let rec = Recognizer::new(&rec, &dict, threads)?;
        let ep = if det.ep == rec.ep { det.ep } else { "CUDA+CPU" };
        eprintln!(
            "[ort] execution provider: {ep} (det: {}, rec: {})",
            det.ep, rec.ep
        );
        Ok(Self {
            det,
            rec,
            cache: TextCache::default(),
            ep,
            last_ms: (0.0, 0.0),
        })
    }

    /// Cache-Statistik (Treffer, Zugriffe) seit Start.
    #[must_use]
    pub fn cache_stats(&self) -> (u64, u64) {
        self.cache.stats()
    }

    /// Aktiver Execution-Provider (`"CUDA+CPU"` im Hybrid-Default,
    /// `"CPU"` bei Fallback oder [`Provider::Cpu`]).
    #[must_use]
    pub fn ep(&self) -> &'static str {
        self.ep
    }

    /// Textzeilen mit Farben; unsichere/leere Erkennungen fallen weg
    /// (die bleiben dann Bildinhalt). Das Rechteck ist bereits um [`REC_PAD`]
    /// erweitert — Erkennung, Farben, Maske und Client malen dasselbe.
    /// Das Bild wird für die Detektion auf 32er-Vielfache gepaddet
    /// (1280×720 → 1280×736); Erkennung und Farben laufen auf dem Original.
    pub fn text(&mut self, img: &RgbImage) -> Result<Vec<TextItem>, String> {
        Ok(self.text_full(img, usize::MAX, &[])?.0)
    }

    /// Wie [`Ocr::text`], deckelt aber teure Neuerkennungen auf `max_new`
    /// je Aufruf (der Rest kommt in Folgeframes; Cache-Treffer sind frei).
    /// Misses nahe `hint` (zuletzt gesendete Rechtecke) laufen zuerst —
    /// beim Verschieben konvergiert sichtbarer Text zuerst. Liefert
    /// zusätzlich alle Detektions-Boxen (auch unerkannte) fürs
    /// Still-da-Urteil des Stabilisators.
    pub fn text_full(
        &mut self,
        img: &RgbImage,
        max_new: usize,
        hint: &[Rect],
    ) -> Result<(Vec<TextItem>, Vec<Rect>), String> {
        let mut out = Vec::new();
        let (padded, ow, oh) = pad_to_32(img);
        let t = std::time::Instant::now();
        let boxes = self.det.detect(&padded)?;
        let det_ms = t.elapsed().as_secs_f64() * 1000.0;
        let t = std::time::Instant::now();
        let mut rects: Vec<Rect> = Vec::with_capacity(boxes.len());
        let mut misses: Vec<usize> = Vec::new();
        for r in boxes.into_iter().take(MAX_LINES) {
            let Some(r) = clip_to(r, ow, oh) else {
                continue;
            };
            rects.push(r);
        }
        let merged = merge_rows(rects);
        let mut rects: Vec<Rect> = Vec::with_capacity(merged.len());
        for r in merged {
            rects.push(pad_rect(r, REC_PAD, ow, oh));
        }
        for (i, r) in rects.iter().enumerate() {
            let key = hash_rect(img, *r);
            if let Some(c) = self.cache.get(key) {
                if keep_line(&c.text, c.conf) && !c.text.is_empty() {
                    out.push(TextItem {
                        id: 0, // vergibt der Stabilisator (Inhalts-Matching)
                        rect: *r,
                        fg: c.fg,
                        bg: c.bg,
                        text: c.text,
                    });
                }
            } else {
                misses.push(i);
            }
        }
        // Priorität: erst Nähe zu zuletzt Gesendetem, dann Fläche.
        misses.sort_by_key(|&i| {
            let r = rects[i];
            let area = -(r.w as i32 * r.h as i32);
            let ov = hint.iter().map(|h| overlap_area(r, *h)).max().unwrap_or(0);
            (ov == 0, area)
        });
        for &i in misses.iter().take(max_new) {
            let r = rects[i];
            let key = hash_rect(img, r);
            let (text, conf) = self.rec.recognize(img, r)?;
            let text = text.trim().to_owned();
            if !keep_line(&text, conf) {
                self.cache.insert(
                    key,
                    CachedLine {
                        text: String::new(),
                        conf,
                        fg: [0; 3],
                        bg: [0; 3],
                    },
                );
                continue;
            }
            let (fg, bg) = sample_colors(img, r);
            self.cache.insert(
                key,
                CachedLine {
                    text: text.clone(),
                    conf,
                    fg,
                    bg,
                },
            );
            out.push(TextItem {
                id: 0, // vergibt der Stabilisator (Inhalts-Matching)
                rect: r,
                fg,
                bg,
                text,
            });
        }
        self.last_ms = (det_ms, t.elapsed().as_secs_f64() * 1000.0);
        Ok((out, rects))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capture::solid;
    use crate::detect::Provider;

    #[test]
    fn missing_models_are_an_error() {
        let Err(e) = Ocr::load("/pfad/den/es/nicht/gibt", 1, Provider::Cpu) else {
            panic!("muss scheitern");
        };
        assert!(e.contains("Modell fehlt"), "{e}");
    }

    #[test]
    fn hash_rect_is_stable_and_sensitive() {
        let img = solid(32, 32, [9; 3]);
        let r = Rect::new(4, 4, 16, 16);
        assert_eq!(hash_rect(&img, r), hash_rect(&img, r));
        let mut other = img.clone();
        other.put_pixel(5, 5, image::Rgb([10, 9, 9]));
        assert_ne!(hash_rect(&img, r), hash_rect(&other, r));
        // Verschobenes Fenster ohne das Störpixel → anderer Schlüssel.
        assert_ne!(
            hash_rect(&other, r),
            hash_rect(&other, Rect::new(6, 4, 16, 16))
        );
        // Andere Maße → anderer Schlüssel (steht im Key).
        assert_ne!(hash_rect(&img, r), hash_rect(&img, Rect::new(4, 4, 17, 16)));
    }

    #[test]
    fn merge_rows_joins_fragments_not_columns() {
        // „Directi“ + „ons“: gleiche Zeile, kleine Lücke → eine Box.
        let b = merge_rows(vec![
            Rect::new(437, 560, 60, 22),
            Rect::new(500, 561, 33, 21),
        ]);
        assert_eq!(b.len(), 1);
        assert_eq!(b[0].x, 437);
        assert_eq!(b[0].x + b[0].w, 533);
        // Tabs: gleiche Zeile, große Lücke → getrennt.
        let b = merge_rows(vec![
            Rect::new(661, 455, 85, 21),
            Rect::new(802, 454, 67, 23),
        ]);
        assert_eq!(b.len(), 2);
        // Verschiedene Zeilen → getrennt (auch bei x-Überlappung).
        let b = merge_rows(vec![
            Rect::new(140, 199, 57, 21),
            Rect::new(140, 247, 57, 21),
        ]);
        assert_eq!(b.len(), 2);
        // Kette schließt ganz („Ca“ + „eck“ + Rest).
        let b = merge_rows(vec![
            Rect::new(1244, 27, 33, 23),
            Rect::new(1250, 30, 20, 20),
            Rect::new(1272, 28, 8, 22),
        ]);
        assert_eq!(b.len(), 1);
        // Hohe Box brückt keine Bänder (Karten-Overlay + Fragment).
        let b = merge_rows(vec![
            Rect::new(1233, 246, 35, 46),
            Rect::new(27, 256, 23, 17),
        ]);
        assert_eq!(b.len(), 2);
        // Breiten-Bremse: keine reihenbreiten Müll-Zeilen.
        let b = merge_rows(vec![
            Rect::new(21, 689, 42, 12),
            Rect::new(70, 689, 290, 12),
            Rect::new(367, 689, 290, 12),
        ]);
        assert!(b.len() >= 2);
        assert!(b.iter().all(|r| r.w <= 320));
    }

    #[test]
    fn keep_line_gates_singles_harder() {
        assert!(keep_line("Hallo", 0.5));
        assert!(!keep_line("Hallo", 0.49));
        assert!(!keep_line("", 1.0));
        // Einzelzeichen: erst ab 0.75 (Karten-Fragmente raus, UI-„X“ drin).
        assert!(!keep_line("三", 0.6));
        assert!(keep_line("X", 0.9));
        assert!(!keep_line("X", 0.7));
    }

    #[test]
    fn overlap_area_measures_intersection() {
        assert_eq!(
            overlap_area(Rect::new(0, 0, 10, 10), Rect::new(5, 5, 10, 10)),
            25
        );
        assert_eq!(
            overlap_area(Rect::new(0, 0, 10, 10), Rect::new(10, 0, 10, 10)),
            0
        );
        assert_eq!(
            overlap_area(Rect::new(0, 0, 10, 10), Rect::new(20, 20, 5, 5)),
            0
        );
    }

    #[test]
    fn cache_hits_misses_and_evicts() {
        let mut c = TextCache::default();
        assert_eq!(c.get(1), None);
        let line = CachedLine {
            text: "hi".into(),
            conf: 0.9,
            fg: [0; 3],
            bg: [255; 3],
        };
        c.insert(1, line.clone());
        assert_eq!(c.get(1), Some(line));
        assert_eq!(c.stats(), (1, 2));
        // Überlauf: alles weg (einfache Politik), danach Miss.
        for k in 2..(CACHE_CAP as u64 + 10) {
            c.insert(
                k,
                CachedLine {
                    text: String::new(),
                    conf: 0.0,
                    fg: [0; 3],
                    bg: [0; 3],
                },
            );
        }
        assert_eq!(c.get(1), None);
    }

    #[test]
    fn colors_follow_contrast() {
        // Weiß mit schwarzem Innenstrich: bg weiß, fg dunkel.
        let mut img = solid(16, 16, [255; 3]);
        for x in 4..12 {
            img.put_pixel(x, 8, image::Rgb([0; 3]));
        }
        let (fg, bg) = sample_colors(&img, Rect::new(0, 0, 16, 16));
        assert_eq!(bg, [255; 3]);
        assert!(fg[0] < 128, "{fg:?}");
        // Kontrastlos: Ersatz-Schriftfarbe nach Helligkeit.
        let (fg, _) = sample_colors(&solid(8, 8, [250; 3]), Rect::new(0, 0, 8, 8));
        assert_eq!(fg, [0; 3]);
    }
}
