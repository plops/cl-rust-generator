//! Tab-Wechsel auf echten Bildern (ignored): zwei browserartige Tabs werden
//! per fontdue im Speicher gerendert (DejaVu, keine Blobs im Git;
//! `examples/gen_tabs.rs` schreibt dieselben Bilder als PNG zur Ansicht),
//! echte PP-OCRv6-Modelle (CPU), wiederholte Wechsel A→B→A→B durch den
//! echten Stabilisator, plus Masken-Nachweis (im Tile steht kein lesbarer
//! Text). Laufen lassen mit:
//! `cargo test --release -p lbw-server --test tabs -- --ignored`

use std::collections::HashMap;
use std::time::{Duration, Instant};

use image::{Rgb, RgbImage};
use lbw_common::ServerMsg;
use lbw_server::detect::Provider;
use lbw_server::ocr::Ocr;
use lbw_server::stabilize::Stabilizer;
use lbw_server::tiles::{MASK_PAD, fill_rect, pad_rect};

/// Browserartige Fixtures werden im Speicher gerendert (keine Blobs im Git;
/// `examples/gen_tabs.rs` schreibt dieselben Bilder als PNG zur Ansicht).
/// Braucht eine System-Schrift (DejaVu, Standard auf fast jedem Linux).
const FONT_PATH: &str = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";

fn font() -> fontdue::Font {
    let bytes = std::fs::read(FONT_PATH).expect("DejaVuSans.ttf fehlt");
    fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default()).unwrap()
}

fn fill(img: &mut RgbImage, x: u32, y: u32, w: u32, h: u32, c: [u8; 3]) {
    for yy in y..(y + h).min(720) {
        for xx in x..(x + w).min(1280) {
            img.put_pixel(xx, yy, Rgb(c));
        }
    }
}

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
            let gy0 = y_top + (px as i32 - m.height as i32).max(0);
            for (i, &al) in bmp.iter().enumerate() {
                if al < 128 {
                    continue;
                }
                let gx = pen as i32 + m.xmin + (i % m.width) as i32;
                let gy = gy0 + (i / m.width) as i32;
                if gx >= 0 && gy >= 0 && (gx as u32) < 1280 && (gy as u32) < 720 {
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
    let mut img = RgbImage::new(1280, 720);
    fill(&mut img, 0, 0, 1280, 720, [255; 3]);
    fill(&mut img, 0, 0, 1280, 44, [210, 210, 215]);
    for (i, name) in [tab.tabs.0, tab.tabs.1].iter().enumerate() {
        let x = 8 + i as u32 * 244;
        fill(
            &mut img,
            x,
            6,
            240,
            38,
            if i == tab.active {
                [245, 245, 245]
            } else {
                [195, 195, 200]
            },
        );
        text(&mut img, font, x + 14, 14, 20.0, [20; 3], name);
    }
    fill(&mut img, 0, 44, 1280, 44, [235, 235, 238]);
    fill(&mut img, 120, 54, 980, 24, [255; 3]);
    text(&mut img, font, 132, 56, 20.0, [40; 3], tab.url);
    text(&mut img, font, 80, 120, 40.0, [10; 3], tab.title);
    for (i, line) in tab.lines.iter().enumerate() {
        text(&mut img, font, 80, 190 + i as i32 * 56, 30.0, [10; 3], line);
    }
    img
}

// Nur ASCII (OCR-Wörterbuch treu, keine Umlaut-Lotterie).
const A_LINES: &[&str] = &[
    "Sonnig bei zwanzig Grad",
    "Wind aus West schwach",
    "Nachts klar und kalt",
    "Morgen frueh Nebel",
    "Mittags bis vierundzwanzig",
    "Abends Wechselhaft",
    "Nachts Niederschlag",
    "Sicht heute gut",
];
const B_LINES: &[&str] = &[
    "Montag Mathematik",
    "Dienstag Deutsch",
    "Mittwoch Englisch",
    "Donnerstag Physik",
    "Freitag Sport",
    "Pause um zehn Uhr",
    "Mittagessen Mensa",
    "Chor am Nachmittag",
];

fn tab_a(font: &fontdue::Font) -> RgbImage {
    render(
        &Tab {
            tabs: ("Wetterbericht", "Stundenplan"),
            active: 0,
            url: "https://wetter.example/heute",
            title: "Wetter heute",
            lines: A_LINES,
        },
        font,
    )
}

fn tab_b(font: &fontdue::Font) -> RgbImage {
    render(
        &Tab {
            tabs: ("Wetterbericht", "Stundenplan"),
            active: 1,
            url: "https://schule.example/plan",
            title: "Unterricht",
            lines: B_LINES,
        },
        font,
    )
}

/// Eindeutige Inhaltszeilen je Tab (Titel + 8 Zeilen; Tableiste/URL sind
/// absichtlich ausgenommen — die Leiste steht in beiden Bildern).
const A_UNIQUE: &[&str] = &[
    "Wetter heute",
    "Sonnig bei zwanzig Grad",
    "Wind aus West schwach",
    "Nachts klar und kalt",
    "Morgen frueh Nebel",
    "Mittags bis vierundzwanzig",
    "Abends Wechselhaft",
    "Nachts Niederschlag",
    "Sicht heute gut",
];
const B_UNIQUE: &[&str] = &[
    "Unterricht",
    "Montag Mathematik",
    "Dienstag Deutsch",
    "Mittwoch Englisch",
    "Donnerstag Physik",
    "Freitag Sport",
    "Pause um zehn Uhr",
    "Mittagessen Mensa",
    "Chor am Nachmittag",
];

#[test]
fn unique_matches_rendered_lines() {
    // Fixture-Konsistenz: Die Sets müssen exakt Titel + Zeilen sein.
    assert_eq!(A_UNIQUE[0], "Wetter heute");
    assert_eq!(&A_UNIQUE[1..], A_LINES);
    assert_eq!(B_UNIQUE[0], "Unterricht");
    assert_eq!(&B_UNIQUE[1..], B_LINES);
}

fn texts_of(items: &[lbw_common::TextItem]) -> Vec<&str> {
    items.iter().map(|t| t.text.as_str()).collect()
}

#[test]
#[ignore]
fn tabs_switch_repeatedly_and_mask_hides_text() {
    let dir = format!("{}/../models", env!("CARGO_MANIFEST_DIR"));
    let mut ocr = Ocr::load(&dir, 4, Provider::Cpu).unwrap();
    let f = font();
    let a = tab_a(&f);
    let b = tab_b(&f);
    assert_eq!((a.width(), a.height()), (1280, 720));
    assert_eq!((b.width(), b.height()), (1280, 720));

    // Phase 1: Beide Tabs werden gelesen — jeder seinen Inhalt, nichts vom
    // jeweils anderen.
    let (ta, seen_a) = ocr.text_full(&a, usize::MAX, &[]).unwrap();
    let (tb, seen_b) = ocr.text_full(&b, usize::MAX, &[]).unwrap();
    let (sa, sb) = (texts_of(&ta), texts_of(&tb));

    // Anwesenheit case-insensitiv (das Modell wackelt bei Großbuchstaben
    // wie in "StundenPlan"/"PhYsik" — Worttreue zählt, nicht Case-Treue).
    let lower = |ts: &[&str]| ts.iter().map(|t| t.to_lowercase()).collect::<Vec<_>>();
    let (la, lb) = (lower(&sa), lower(&sb));
    for want in A_UNIQUE {
        assert!(
            la.contains(&want.to_lowercase()),
            "Tab A ohne {want:?}: {sa:?}"
        );
    }
    for want in B_UNIQUE {
        assert!(
            lb.contains(&want.to_lowercase()),
            "Tab B ohne {want:?}: {sb:?}"
        );
    }
    for other in B_UNIQUE {
        assert!(!sa.contains(other), "Tab A zeigt B-Inhalt {other:?}");
    }
    for other in A_UNIQUE {
        assert!(!sb.contains(other), "Tab B zeigt A-Inhalt {other:?}");
    }

    // Phase 2: Wiederholte Wechsel A→B→A→B durch den Stabilisator. Nach
    // jedem Wechsel muss der neue Inhalt da sein, der alte weg — ohne
    // Aufschaukeln (keine Geister, kein Wachstum über Runden).
    let mut st = Stabilizer::new();
    let mut live: HashMap<u64, String> = HashMap::new();
    let mut now = Instant::now();
    let mut first = true;
    for _round in 0..3 {
        for (items, seen, expect, gone) in [
            (&ta, &seen_a, A_UNIQUE, B_UNIQUE),
            (&tb, &seen_b, B_UNIQUE, A_UNIQUE),
        ] {
            for _ in 0..10 {
                now += Duration::from_millis(100);
                for m in st.step(items.clone(), seen, first, now) {
                    first = false;
                    match m {
                        ServerMsg::ClearText => live.clear(),
                        ServerMsg::NewText(t) => {
                            live.insert(t.id, t.text.clone());
                        }
                        ServerMsg::RemoveText(id) => {
                            live.remove(&id);
                        }
                        _ => {}
                    }
                }
            }
            let live_texts: Vec<String> = live.values().map(|t| t.to_lowercase()).collect();
            for want in expect {
                assert!(
                    live_texts.contains(&want.to_lowercase()),
                    "nach Wechsel fehlt {want:?}: {live_texts:?}"
                );
            }
            for old in gone {
                assert!(
                    !live_texts.contains(&old.to_lowercase()),
                    "Geistertext nach Wechsel: {old:?} in {live_texts:?}"
                );
            }
            assert!(
                live.len() <= 20,
                "Zeilenzahl schaukelt sich auf: {}",
                live.len()
            );
        }
    }

    // Phase 3: Masken-Nachweis — exakt wie die Session (gesendete Boxen +
    // Pad mit Hintergrundfarbe füllen), dann echte OCR auf das maskierte
    // Bild: Findet sie nichts mehr, kann die AV1-Kachel keinen lesbaren
    // Text enthalten (Kodieren entfernt nur Details, erfindet keine).
    let mut masked = b.clone();
    for t in &tb {
        let m = pad_rect(t.rect, MASK_PAD, b.width(), b.height());
        fill_rect(&mut masked, m, t.bg);
    }
    let rest = ocr.text(&masked).unwrap();
    assert!(
        rest.is_empty(),
        "maskierte Kachel enthält lesbaren Text: {:?}",
        texts_of(&rest)
    );
}
