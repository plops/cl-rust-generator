//! Render-Lage-Prüfung: Text-Glyphen müssen in ihrer Box sitzen.
//! Braucht Display + GL — nur via `scripts/render_check.sh` (Xvfb) laufen
//! lassen. Panic = Glyphen außerhalb der Box (z. B. Baseline statt Oberkante).

use lbw_client::app::draw_text_item;
use lbw_common::{Rect, TextItem};
use macroquad::prelude::*;

fn window_conf() -> Conf {
    Conf {
        window_title: "render_probe".into(),
        window_width: 800,
        window_height: 600,
        ..Default::default()
    }
}

fn red_item() -> TextItem {
    TextItem {
        id: 1,
        rect: Rect::new(100, 200, 300, 40),
        fg: [255, 0, 0],
        bg: [0, 0, 0],
        // Versalien + Unterlängen + Umlaut + Ziffern: volle Vertikal-Ausdehnung.
        text: "AgHITZÄpfqy 019".into(),
    }
}

fn is_ink(p: &[u8]) -> bool {
    p[0] > 40 && p[1] < 40 && p[2] < 40
}

#[macroquad::main(window_conf)]
async fn main() {
    let t = red_item();
    for _ in 0..2 {
        clear_background(BLACK);
        draw_text_item(&t);
        next_frame().await;
    }
    // Letzter Frame: grabben VOR dem Present (Backbuffer).
    clear_background(BLACK);
    draw_text_item(&t);
    let img = get_screen_data();
    // get_screen_data liefert GL-Orientierung (Zeile 0 = UNTEN, verifiziert
    // per Marker-Rechteck) → in Screen-Zeilen spiegeln.
    let flip = |y_img: usize| img.height as usize - 1 - y_img;
    assert_eq!((img.width as u16, img.height as u16), (800, 600));
    let (w, bytes) = (img.width as usize, &img.bytes);
    let (x0, x1) = (95usize, 405usize);
    let (top, bottom) = (t.rect.y as usize, (t.rect.y + t.rect.h) as usize);
    let mut ink_rows = Vec::new();
    let mut ink_n = 0;
    for y in 0..img.height as usize {
        let mut row_ink = false;
        for x in x0..x1.min(w) {
            let i = (y * w + x) * 4;
            if is_ink(&bytes[i..i + 4]) {
                row_ink = true;
                ink_n += 1;
            }
        }
        if row_ink {
            ink_rows.push(y);
        }
    }
    assert!(ink_n > 100, "Text wurde gar nicht gerendert?");
    let min_y = flip(*ink_rows.iter().max().unwrap());
    let max_y = flip(*ink_rows.iter().min().unwrap());
    assert!(
        min_y >= top,
        "Glyphen stehen über der Box: oberste Zeile {min_y}, Box ab {top}"
    );
    assert!(
        max_y < bottom,
        "Glyphen ragen unter die Box: unterste Zeile {max_y}, Box bis {bottom}"
    );
    println!("render_probe: PASS (Ink-Zeilen {min_y}–{max_y} in Box {top}–{bottom})");
}
