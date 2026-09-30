//! main.rs — nur Verdrahtung.

use std::path::Path;
use unicode_ocr::{lang, render};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.first().map(String::as_str) == Some("render-test") && args.len() >= 3 {
        let l = &lang::LANGS[lang::by_code(&args[1]).expect("unknown language")];
        let px = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(32);
        let mut r = render::Raster::load(None).expect("font");
        let lines: Vec<String> = l.pangrams.iter().map(|s| s.to_string()).collect();
        let c = r.render(&lines, px, l.rtl);
        render::write_ppm(Path::new(&args[2]), &c.rgba, render::CANVAS, render::CANVAS)
            .expect("write ppm");
        for g in &c.lines {
            println!("{:?} {}", g.rect, g.text);
        }
    } else {
        eprintln!("usage: unicode_ocr render-test <lang> <out.ppm> [px]");
        std::process::exit(2);
    }
}
