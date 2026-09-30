//! main.rs — nur Verdrahtung: Befehl parsen → Fenster, bench, Testbild.

use unicode_ocr::{bench, cli, lang, render, ui_loop};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match cli::parse(&args) {
        Ok(cli::Cmd::Window) => {
            if let Err(e) = ui_loop::run_window() {
                eprintln!("error: {e}");
                std::process::exit(1);
            }
        }
        Ok(cli::Cmd::Bench(b)) => match bench::run(&b) {
            Ok(md) => print!("{md}"),
            Err(e) => {
                eprintln!("error: {e}");
                std::process::exit(1);
            }
        },
        Ok(cli::Cmd::RenderTest { lang, out, px }) => {
            let l = &lang::LANGS[lang];
            let mut r = render::Raster::load(None).unwrap_or_else(|e| {
                eprintln!("error: {e}");
                std::process::exit(1);
            });
            let lines: Vec<String> = l.pangrams.iter().map(|s| s.to_string()).collect();
            let c = r.render(&lines, px, l.rtl);
            if let Err(e) = render::write_ppm(&out, &c.rgba, render::CANVAS, render::CANVAS) {
                eprintln!("error: {}: {e}", out.display());
                std::process::exit(1);
            }
            for g in &c.lines {
                println!("{:?} {}", g.rect, g.text);
            }
        }
        Ok(cli::Cmd::Help) => println!("{}", cli::usage()),
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(2);
        }
    }
}
