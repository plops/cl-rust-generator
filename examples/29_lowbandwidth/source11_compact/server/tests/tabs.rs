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
use lbw_server::ocr::{Ocr, REC_BUDGET};
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

/// Ground-Truth-Zeile: gerenderter Text + exakte Pixel-Box (nur Testcode).
struct GtLine {
    text: String,
    rect: lbw_common::Rect,
}

#[allow(clippy::too_many_arguments)] // Fixture-Helfer (Testcode).
fn text(
    img: &mut RgbImage,
    font: &fontdue::Font,
    x: u32,
    y_top: i32,
    px: f32,
    c: [u8; 3],
    s: &str,
    gt: &mut Vec<GtLine>,
) {
    let mut pen = x as f32;
    let (mut x0, mut y0, mut x1, mut y1) = (1280i32, 720i32, -1i32, -1i32);
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
                    x0 = x0.min(gx);
                    y0 = y0.min(gy);
                    x1 = x1.max(gx);
                    y1 = y1.max(gy);
                }
            }
        }
        pen += adv;
    }
    if x1 >= x0 {
        gt.push(GtLine {
            text: s.to_string(),
            rect: lbw_common::Rect::new(
                x0 as u16,
                y0 as u16,
                (x1 - x0 + 1) as u16,
                (y1 - y0 + 1) as u16,
            ),
        });
    }
}

struct Tab<'a> {
    tabs: (&'a str, &'a str),
    active: usize,
    url: &'a str,
    title: &'a str,
    lines: &'a [&'a str],
}

/// Rendert den Tab; `dy` verschiebt Titel+Inhalt (Scroll-Simulation,
/// Leiste/URL bleiben). Liefert Bild + Ground-Truth-Boxen aller Zeilen.
fn render(tab: &Tab, font: &fontdue::Font, dy: i32) -> (RgbImage, Vec<GtLine>) {
    let mut img = RgbImage::new(1280, 720);
    let mut gt = Vec::new();
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
        text(&mut img, font, x + 14, 14, 20.0, [20; 3], name, &mut gt);
    }
    fill(&mut img, 0, 44, 1280, 44, [235, 235, 238]);
    fill(&mut img, 120, 54, 980, 24, [255; 3]);
    text(&mut img, font, 132, 56, 20.0, [40; 3], tab.url, &mut gt);
    text(
        &mut img,
        font,
        80,
        120 + dy,
        40.0,
        [10; 3],
        tab.title,
        &mut gt,
    );
    for (i, line) in tab.lines.iter().enumerate() {
        text(
            &mut img,
            font,
            80,
            190 + i as i32 * 56 + dy,
            30.0,
            [10; 3],
            line,
            &mut gt,
        );
    }
    (img, gt)
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

fn tab_a(font: &fontdue::Font, dy: i32) -> (RgbImage, Vec<GtLine>) {
    render(
        &Tab {
            tabs: ("Wetterbericht", "Stundenplan"),
            active: 0,
            url: "https://wetter.example/heute",
            title: "Wetter heute",
            lines: A_LINES,
        },
        font,
        dy,
    )
}

fn tab_b(font: &fontdue::Font, dy: i32) -> (RgbImage, Vec<GtLine>) {
    render(
        &Tab {
            tabs: ("Wetterbericht", "Stundenplan"),
            active: 1,
            url: "https://schule.example/plan",
            title: "Unterricht",
            lines: B_LINES,
        },
        font,
        dy,
    )
}

fn tab_c(font: &fontdue::Font) -> (RgbImage, Vec<GtLine>) {
    // Dicht: 40 Zeilen in 14 px (Budget-Grenze 32 wird kalt gerissen) plus
    // Einzelzeichen (hartes keep_line-Gatter in source11).
    let mut lines: Vec<&str> = vec![
        "Apfel", "Birne", "Couch", "Dach", "Eimer", "Fenster", "Garten", "Haus", "Igel", "Jacke",
        "Kaffee", "Lampe", "Maus", "Nagel", "Ofen", "Pferd", "Quelle", "Radio", "Sofa", "Tasse",
        "Uhr", "Vogel", "Wald", "Zucker", "Anker", "Besen", "Dose", "Ecke", "Flasche", "Gabel",
        "Hobel", "Insel", "Kamm", "Loeffel", "Messer", "Nuss", "X", "Q", "7", "+",
    ];
    assert_eq!(lines.len(), 40);
    let mut img = RgbImage::new(1280, 720);
    let mut gt = Vec::new();
    fill(&mut img, 0, 0, 1280, 720, [255; 3]);
    let mut y = 20;
    for line in lines.drain(..) {
        text(&mut img, font, 60, y, 14.0, [10; 3], line, &mut gt);
        y += 15;
    }
    (img, gt)
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

/// Überlappung zweier Boxen in px² (Testcode).
fn overlap_area(a: lbw_common::Rect, b: lbw_common::Rect) -> i64 {
    let x0 = u32::from(a.x).max(u32::from(b.x));
    let y0 = u32::from(a.y).max(u32::from(b.y));
    let x1 = (u32::from(a.x) + u32::from(a.w)).min(u32::from(b.x) + u32::from(b.w));
    let y1 = (u32::from(a.y) + u32::from(a.h)).min(u32::from(b.y) + u32::from(b.h));
    i64::from(x1.saturating_sub(x0)) * i64::from(y1.saturating_sub(y0))
}

/// Ground-Truth-Trefferquote: Für jede erwartete Zeile muss es ein
/// OCR-Ergebnis mit gleichem Text (case-insensitiv) und ≥ halber
/// Box-Überlappung geben. Liefert die vermissten Zeilen (Messgröße!).
fn content_misses(items: &[lbw_common::TextItem], gt: &[GtLine], wants: &[&str]) -> Vec<String> {
    let mut out = Vec::new();
    for want in wants {
        let g = gt
            .iter()
            .find(|g| g.text.to_lowercase() == want.to_lowercase());
        let Some(g) = g else {
            out.push(format!("{want} (nicht gerendert)"));
            continue;
        };
        let area = i64::from(g.rect.w) * i64::from(g.rect.h);
        let found = items.iter().any(|t| {
            t.text.to_lowercase() == g.text.to_lowercase()
                && overlap_area(t.rect, g.rect) * 2 >= area
        });
        if !found {
            out.push(want.to_string());
        }
    }
    out
}

#[test]
#[ignore]
fn tabs_switch_repeatedly_and_mask_hides_text() {
    let dir = format!("{}/../models", env!("CARGO_MANIFEST_DIR"));
    let mut ocr = Ocr::load(&dir, 4, Provider::Cpu).unwrap();
    let f = font();
    let (a, _gt_a) = tab_a(&f, 0);
    let (b, _gt_b) = tab_b(&f, 0);
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

/// Ground Truth: Wie viele gerenderte Inhaltszeilen erkennt die
/// Session-Pipeline (Budget + Stabilisator), und was passiert beim Scrollen
/// (Inhalt wandert in 8-px-Schritten, Leiste steht)? Misst die Anzahl der
/// nicht gefundenen Textvorkommnisse — strikt, ohne Toleranz.
#[test]
#[ignore]
fn groundtruth_recall_and_scroll() {
    let dir = format!("{}/../models", env!("CARGO_MANIFEST_DIR"));
    let mut ocr = Ocr::load(&dir, 4, Provider::Cpu).unwrap();
    let f = font();

    // Phase A: Kalte Session-Konvergenz auf stehendem Bild (wie serve_client:
    // REC_BUDGET/Frame, Hint aus Vorframe). Jede Iteration meldet Stand.
    let (img_a, gt_a) = tab_a(&f, 0);
    let mut hint: Vec<lbw_common::Rect> = Vec::new();
    let mut converged = None;
    for iter in 1..=12 {
        let (items, _seen) = ocr.text_full(&img_a, REC_BUDGET, &hint).unwrap();
        hint = items.iter().map(|t| t.rect).collect();
        let miss = content_misses(&items, &gt_a, A_UNIQUE);
        eprintln!(
            "[gt] iter {iter}: {}/{} Zeilen, vermisst: {miss:?}",
            A_UNIQUE.len() - miss.len(),
            A_UNIQUE.len()
        );
        if miss.is_empty() {
            converged = Some(iter);
            break;
        }
    }
    assert!(
        converged.is_some(),
        "kalte Session konvergiert nicht auf vollen Recall"
    );

    // Phase B: Scrollen — Inhalt wandert, dann Stillstand. Pro Schritt wird
    // der OCR-Recall gemessen; der Nachrichtenstrom muss aus Moves bestehen
    // (kein Neu-/Weg-Spam bei reiner Verschiebung).
    let mut st = Stabilizer::new();
    let mut now = Instant::now();
    let mut first = true;
    let (mut news, mut moves, mut removes) = (0, 0, 0);
    let mut live: HashMap<u64, lbw_common::TextItem> = HashMap::new();
    // Schritt 0 trägt den Erstbestand ein (ClearText + NewTexts zählen
    // nicht als Scroll-Spam); erst danach wird der Strom bewertet.
    let (img0, _) = tab_a(&f, 0);
    let (items0, seen0) = ocr.text_full(&img0, REC_BUDGET, &[]).unwrap();
    now += Duration::from_millis(100);
    for m in st.step(items0, &seen0, first, now) {
        first = false;
        if let ServerMsg::NewText(t) = m {
            live.insert(t.id, t);
        }
    }
    for step in 1..=7 {
        // Nach unten scrollen: keine Kollision mit der Leiste (echte Browser
        // clippen darunterliegenden Inhalt; das Fixture übermalt nicht).
        let (img, gt) = tab_a(&f, 8 * step);
        let (items, seen) = ocr.text_full(&img, REC_BUDGET, &[]).unwrap();
        let miss = content_misses(&items, &gt, A_UNIQUE);
        eprintln!("[gt] scroll dy=+{}: vermisst: {miss:?}", 8 * step);
        assert!(
            miss.is_empty(),
            "Scroll-Schritt dy=+{}: Zeilen verloren: {miss:?}",
            8 * step
        );
        now += Duration::from_millis(100);
        for m in st.step(items, &seen, first, now) {
            first = false;
            match m {
                ServerMsg::ClearText => live.clear(),
                ServerMsg::NewText(t) => {
                    news += 1;
                    live.insert(t.id, t);
                }
                ServerMsg::MoveText { id, rect } => {
                    moves += 1;
                    if let Some(e) = live.get_mut(&id) {
                        e.rect = rect;
                    }
                }
                ServerMsg::RemoveText(id) => {
                    removes += 1;
                    live.remove(&id);
                }
                _ => {}
            }
        }
    }
    // Stillstand: letzte Position wiederholen, bis alles bestätigt.
    let (img_end, _) = tab_a(&f, 56);
    for _ in 0..10 {
        let (items, seen) = ocr.text_full(&img_end, REC_BUDGET, &[]).unwrap();
        now += Duration::from_millis(100);
        for m in st.step(items, &seen, false, now) {
            match m {
                ServerMsg::ClearText => live.clear(),
                ServerMsg::NewText(t) => {
                    news += 1;
                    live.insert(t.id, t);
                }
                ServerMsg::MoveText { id, rect } => {
                    moves += 1;
                    if let Some(e) = live.get_mut(&id) {
                        e.rect = rect;
                    }
                }
                ServerMsg::RemoveText(id) => {
                    removes += 1;
                    live.remove(&id);
                }
                _ => {}
            }
        }
    }
    eprintln!("[gt] scroll-strom: neu={news} bewegt={moves} weg={removes}");
    assert_eq!(removes, 0, "reines Scrollen darf nichts entfernen");
    assert_eq!(news, 0, "reines Scrollen darf nichts Neues melden");
    assert!(moves > 0, "Scrollen muss als Moves ankommen");
    let live_texts: Vec<String> = live.values().map(|t| t.text.to_lowercase()).collect();
    for want in A_UNIQUE {
        assert!(
            live_texts.contains(&want.to_lowercase()),
            "nach Scroll fehlt {want:?}: {live_texts:?}"
        );
    }

    // Phase C: Maske aus den Session-Live-Boxen (nicht Roh-OCR) — was die
    // Session nicht als Text führt, wird nicht ausmaskiert. OCR auf dem
    // maskierten Bild muss leer sein, sonst steht Text in der Kachel.
    let mut masked = img_end.clone();
    for t in live.values() {
        let m = pad_rect(t.rect, MASK_PAD, masked.width(), masked.height());
        fill_rect(&mut masked, m, t.bg);
    }
    let rest = ocr.text(&masked).unwrap();
    assert!(
        rest.is_empty(),
        "Kachel enthält Text aus nicht geführten Zeilen: {:?}",
        texts_of(&rest)
    );
}

/// Dichter Tab (40 Zeilen, 14 px, Einzelzeichen): Misst, ob die kalte
/// Session trotz REC_BUDGET auf vollen Recall konvergiert — und welche
/// Vorkommnisse fehlen.
#[test]
#[ignore]
fn groundtruth_dense_recall() {
    let dir = format!("{}/../models", env!("CARGO_MANIFEST_DIR"));
    let mut ocr = Ocr::load(&dir, 4, Provider::Cpu).unwrap();
    let f = font();
    let (img, gt) = tab_c(&f);
    let wants: Vec<&str> = gt.iter().map(|g| g.text.as_str()).collect();
    // Dokumentiertes Modell-Inventar (beide Generationen gleich, s.
    // `groundtruth_loss_stage`): Diese Vorkommnisse liefert die Pipeline
    // grundsätzlich nicht — alles darüber hinaus ist eine Regression.
    const KNOWN_MISS: &[&str] = &["Igel", "Flasche", "Gabel", "X", "Q", "7", "+"];
    let mut hint: Vec<lbw_common::Rect> = Vec::new();
    let mut last_miss = Vec::new();
    for iter in 1..=15 {
        let (items, _seen) = ocr.text_full(&img, REC_BUDGET, &hint).unwrap();
        hint = items.iter().map(|t| t.rect).collect();
        last_miss = content_misses(&items, &gt, &wants);
        eprintln!(
            "[gt-dicht] iter {iter}: {}/{} Zeilen, vermisst: {last_miss:?}",
            wants.len() - last_miss.len(),
            wants.len()
        );
        if last_miss.iter().all(|m| KNOWN_MISS.contains(&m.as_str())) {
            break;
        }
    }
    // Konvergenz-Nachweis: Nach spätestens 15 Frames darf nichts
    // Unbekanntes mehr fehlen (Budget-Deckel heilt über Frames, Stillstand
    // trägt Cache) — alles darüber hinaus ist eine Regression.
    for m in &last_miss {
        assert!(
            KNOWN_MISS.contains(&m.as_str()),
            "neues nicht-erkanntes Vorkommnis (Regression?): {m:?} in {last_miss:?}"
        );
    }
}
/// Verluststufen-Kontrolle (Diagnose, ignored): Meldet, für welche
/// GT-Zeilen schon der Detektor keine Box liefert (Modell-Limit, beide
/// Generationen gleich). Das Inventar ist dokumentiert, nicht geraten:
/// Nur die beiden winzigen Zeichen "7"/"+" sind detektorblind; alles
/// andere muss mindestens eine Box bekommen (auch falsch gelesene
/// Zeilen werden als Box geführt und maskiert).
#[test]
#[ignore]
fn groundtruth_loss_stage() {
    let dir = format!("{}/../models", env!("CARGO_MANIFEST_DIR"));
    let mut det = lbw_server::detect::Detector::new(
        &format!("{dir}/PP-OCRv6_small_det.onnx"),
        4,
        Provider::Cpu,
    )
    .unwrap();
    let f = font();
    let (img, gt) = tab_c(&f);
    let (padded, _, _) = lbw_server::detect::pad_to_32(&img);
    let boxes = det.detect(&padded).unwrap();
    eprintln!("[verlust] Detektor-Boxen: {}", boxes.len());
    let mut blind = Vec::new();
    for g in &gt {
        let area = i64::from(g.rect.w) * i64::from(g.rect.h);
        let hit = boxes.iter().any(|b| overlap_area(*b, g.rect) * 2 >= area);
        if !hit {
            blind.push(g.text.clone());
        }
    }
    eprintln!("[verlust] detektorblind: {blind:?}");
    for b in &blind {
        assert!(
            ["7", "+"].contains(&b.as_str()),
            "neue Detektor-Blindheit: {b:?} (bisher nur 7/+ bekannt)"
        );
    }
}
