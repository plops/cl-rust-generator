//! Grundtypen: Treemap-Rechteck, Farbe, Dateibaum-Knoten, Byte-Format.

use std::path::PathBuf;

/// Achsenparalleles Rechteck in Pixel-Koordinaten (Ursprung oben links).
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

impl Rect {
    #[must_use]
    pub const fn new(x: f32, y: f32, w: f32, h: f32) -> Self {
        Self { x, y, w, h }
    }

    /// True, wenn der Punkt `(px, py)` innerhalb liegt (Kante inklusive).
    #[must_use]
    pub fn contains(self, px: f32, py: f32) -> bool {
        px >= self.x && px < self.x + self.w && py >= self.y && py < self.y + self.h
    }

    /// Fläche in Quadratpixeln.
    #[must_use]
    pub fn area(self) -> f32 {
        self.w * self.h
    }
}

/// RGB-Farbe, Kanalwerte 0.0..=1.0 (sRGB, wird 1:1 an den Shader gegeben).
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Rgb {
    pub r: f32,
    pub g: f32,
    pub b: f32,
}

impl Rgb {
    #[must_use]
    pub const fn new(r: f32, g: f32, b: f32) -> Self {
        Self { r, g, b }
    }

    /// Aus 8-Bit-Kanälen (0..=255).
    #[must_use]
    pub const fn bytes(r: u8, g: u8, b: u8) -> Self {
        Self {
            r: r as f32 / 255.0,
            g: g as f32 / 255.0,
            b: b as f32 / 255.0,
        }
    }
}

/// Ein Datei- oder Verzeichnisknoten mit Größe, Kindern und Layout-Rechteck.
#[derive(Debug)]
pub struct Node {
    pub path: PathBuf,
    /// Gesamtgröße in Bytes (Datei: eigene Größe; Verzeichnis: Summe der Kinder).
    pub size: u64,
    pub is_dir: bool,
    pub children: Vec<Node>,
    /// Vom Squarify-Layout zugewiesenes Rechteck (Pixel).
    pub rect: Rect,
    pub color: Rgb,
}

/// Formatiert Bytes als `"2.0 KB"` (eine Nachkommastelle, 1024er-Stufen).
pub fn format_bytes(bytes: u64) -> String {
    const UNITS: [&str; 5] = ["B", "KB", "MB", "GB", "TB"];
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit < 4 {
        value /= 1024.0;
        unit += 1;
    }
    format!("{value:.1} {}", UNITS[unit])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_bytes_units() {
        assert_eq!(format_bytes(0), "0.0 B");
        assert_eq!(format_bytes(100), "100.0 B");
        assert_eq!(format_bytes(2048), "2.0 KB");
        assert_eq!(format_bytes(5_242_880), "5.0 MB");
        assert_eq!(format_bytes(1 << 40), "1.0 TB");
    }

    #[test]
    fn rect_contains_edges() {
        let r = Rect::new(10.0, 20.0, 30.0, 40.0);
        assert!(r.contains(10.0, 20.0));
        assert!(r.contains(39.9, 59.9));
        assert!(!r.contains(40.0, 60.0));
        assert!(!r.contains(0.0, 0.0));
        assert!(!r.contains(50.0, 30.0));
    }

    #[test]
    fn rgb_bytes_scale() {
        assert_eq!(Rgb::bytes(255, 0, 0), Rgb::new(1.0, 0.0, 0.0));
        let mid = Rgb::bytes(128, 128, 128);
        assert!((mid.r - 128.0 / 255.0).abs() < f32::EPSILON);
    }
}
