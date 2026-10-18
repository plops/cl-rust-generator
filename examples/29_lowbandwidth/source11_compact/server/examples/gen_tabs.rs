//! Fixture-Generator für `tests/tabs.rs`: zwei browserartige 1280×720-
//! Screenshots (Tab A / Tab B) mit echtem DejaVu-Text. Laufen lassen mit:
//! `cargo run -p lbw-server --example gen_tabs`
//! Schreibt `server/tests/data/tab_a.png` + `tab_b.png` (eingecheckt;
//! Tests lesen nur die PNGs und brauchen keine Schrift).

use image::{Rgb, RgbImage};

const W: u32 = 1280;
const H: u32 = 720;
const FONT_PATH: &str = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";

fn fill(img: &mut RgbImage, x: u32, y: u32, w: u32, h: u32, c: [u8; 3]) {
    for yy in y..(y + h).min(H) {
        for xx in x..(x + w).min(W) {
            img.put_pixel(xx, yy, Rgb(c));
        }
    }
}

/// Zeichnet eine Zeile (linksbündig, Oberkante `y_top`). Die Glyphen-Box
/// wird an der Schriftgröße ausgerichtet; exakte Baseline-Treue ist für
/// OCR-Fixtures egal, Überlappung mit Nachbarzeilen wird durch großzügige
/// Zeilenabstände ausgeschlossen.
fn text(
    img: &mut RgbImage,
    font: &fontdue::Font,
    x: u32,
    y_top: i32,
    px: f32,
    c: [u8; 3],
    s: &str,
) {
    let mut pen = x as f32;
    for ch in s.chars() {
        let (m, bmp) = font.rasterize(ch, px);
        let adv = if m.advance_width > 0.0 {
            m.advance_width
        } else {
            px * 0.6
        };
        if !bmp.is_empty() {
            // Oberkante der Glyphen-Box grob an `px` ausrichten.
            let gy0 = y_top + (px as i32 - m.height as i32).max(0);
            for (i, &a) in bmp.iter().enumerate() {
                if a < 128 {
                    continue;
                }
                let gx = pen as i32 + m.xmin + (i % m.width) as i32;
                let gy = gy0 + (i / m.width) as i32;
                if gx >= 0 && gy >= 0 && (gx as u32) < W && (gy as u32) < H {
                    img.put_pixel(gx as u32, gy as u32, Rgb(c));
                }
            }
        }
        pen += adv;
    }
}

struct Tab<'a> {
    tabs: (&'a str, &'a str),
    active: usize,
    url: &'a str,
    title: &'a str,
    lines: &'a [&'a str],
}

fn render(tab: &Tab, font: &fontdue::Font) -> RgbImage {
    let mut img = RgbImage::new(W, H);
    fill(&mut img, 0, 0, W, H, [255; 3]);
    // Tableiste: beide Titel in beiden Bildern (nur aktiv/inaktiv wechselt).
    fill(&mut img, 0, 0, W, 44, [210, 210, 215]);
    let names = [tab.tabs.0, tab.tabs.1];
    for (i, name) in names.iter().enumerate() {
        let x = 8 + i as u32 * 244;
        let active = i == tab.active;
        fill(
            &mut img,
            x,
            6,
            240,
            38,
            if active {
                [245, 245, 245]
            } else {
                [195, 195, 200]
            },
        );
        text(&mut img, font, x + 14, 14, 20.0, [20; 3], name);
    }
    // Adressleiste.
    fill(&mut img, 0, 44, W, 44, [235, 235, 238]);
    fill(&mut img, 120, 54, 980, 24, [255; 3]);
    text(&mut img, font, 132, 56, 20.0, [40; 3], tab.url);
    // Inhalt.
    text(&mut img, font, 80, 120, 40.0, [10; 3], tab.title);
    for (i, line) in tab.lines.iter().enumerate() {
        text(&mut img, font, 80, 190 + i as i32 * 56, 30.0, [10; 3], line);
    }
    img
}

fn main() -> Result<(), String> {
    let bytes = std::fs::read(FONT_PATH).map_err(|e| format!("{FONT_PATH}: {e}"))?;
    let font = fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default())
        .map_err(|e| format!("Schrift unlesbar: {e:?}"))?;
    // Nur ASCII (OCR-Wörterbuch treu, keine Umlaut-Lotterie).
    let a = Tab {
        tabs: ("Wetterbericht", "Stundenplan"),
        active: 0,
        url: "https://wetter.example/heute",
        title: "Wetter heute",
        lines: &[
            "Sonnig bei zwanzig Grad",
            "Wind aus West schwach",
            "Nachts klar und kalt",
            "Morgen frueh Nebel",
            "Mittags bis vierundzwanzig",
            "Abends Wechselhaft",
            "Nachts Niederschlag",
            "Sicht heute gut",
        ],
    };
    let b = Tab {
        tabs: ("Wetterbericht", "Stundenplan"),
        active: 1,
        url: "https://schule.example/plan",
        title: "Unterricht",
        lines: &[
            "Montag Mathematik",
            "Dienstag Deutsch",
            "Mittwoch Englisch",
            "Donnerstag Physik",
            "Freitag Sport",
            "Pause um zehn Uhr",
            "Mittagessen Mensa",
            "Chor am Nachmittag",
        ],
    };
    let dir = format!("{}/tests/data", env!("CARGO_MANIFEST_DIR"));
    std::fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    render(&a, &font)
        .save(format!("{dir}/tab_a.png"))
        .map_err(|e| e.to_string())?;
    render(&b, &font)
        .save(format!("{dir}/tab_b.png"))
        .map_err(|e| e.to_string())?;
    eprintln!("Fixtures geschrieben nach {dir}/tab_a.png + tab_b.png");
    Ok(())
}
