//! `05_ocr` — Texterkennung als Ganzes: Detektion + Erkennung + Farben.
//!
//! [`Ocr::text`] orchestriert [`Detector`](crate::detect) und
//! [`Recognizer`](crate::recognize): Boxen finden, je Zeile lesen, Farben
//! samplen. OCR ist Pflicht: ohne Modelle startet der Server nicht (reines
//! AV1-Textbild würde das 6-kB/s-Budget sprengen).

use image::RgbImage;

use lbw_common::{Rect, TextItem};
use lbw_log::Fnv1a64;

use crate::detect::{Detector, Provider, clip_to, pad_to_32, px};
use crate::recognize::Recognizer;
use crate::tiles::pad_rect;

/// Erkennungs-Rand je Seite: Detektions-Boxen schneiden Glyphen (z. B.
/// Umlaut-Punkte) haarscharf ab — mit Weißraum liest das Netz deutlich besser
/// (Befund aus `26_onnx`: CER 69→4 %). Per Xvfb/xterm-Sweep bestimmt.
const REC_PAD: u16 = 4;
/// Mindest-Konfidenz, sonst bleibt die Zeile Bildinhalt.
const MIN_TEXT_CONF: f32 = 0.5;
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
        let mut out = Vec::new();
        let (padded, ow, oh) = pad_to_32(img);
        let t = std::time::Instant::now();
        let boxes = self.det.detect(&padded)?;
        let det_ms = t.elapsed().as_secs_f64() * 1000.0;
        let t = std::time::Instant::now();
        for r in boxes.into_iter().take(MAX_LINES) {
            let Some(r) = clip_to(r, ow, oh) else {
                continue;
            };
            let r = pad_rect(r, REC_PAD, ow, oh);
            let key = hash_rect(img, r);
            if let Some(c) = self.cache.get(key) {
                if !c.text.is_empty() && c.conf >= MIN_TEXT_CONF {
                    out.push(TextItem {
                        id: 0, // vergibt die Session (`09_textids`)
                        rect: r,
                        fg: c.fg,
                        bg: c.bg,
                        text: c.text,
                    });
                }
                continue;
            }
            let (text, conf) = self.rec.recognize(img, r)?;
            let text = text.trim().to_owned();
            if text.is_empty() || conf < MIN_TEXT_CONF {
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
                id: 0, // vergibt die Session (`09_textids`, Positions-Matching)
                rect: r,
                fg,
                bg,
                text,
            });
        }
        self.last_ms = (det_ms, t.elapsed().as_secs_f64() * 1000.0);
        Ok(out)
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
