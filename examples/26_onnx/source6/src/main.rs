//! main.rs — Deklaration + Verdrahtung (S6). Exit: 0/1/2 (Ende/X11/Konfig).

#[path = "00_args.rs"]
mod args;

#[path = "01_view.rs"]
mod view;

#[path = "02_capture.rs"]
mod capture;

#[path = "03_detect.rs"]
mod detect;

#[path = "04_recognize.rs"]
mod recognize;

#[path = "05_input.rs"]
mod input;

#[path = "06_config.rs"]
mod config;

#[path = "07_match.rs"]
mod mtch;

#[path = "08_rules.rs"]
mod rules;

#[path = "09_tui.rs"]
mod tui;

#[path = "10_canvas.rs"]
mod canvas;

use std::time::{Duration, Instant};

use crossterm::event::{self, Event};
use x11rb::connection::Connection;

use args::parse_args;
use canvas::{TuiGuard, render_frame};
use capture::{
    ConvertPath, bgra_to_rgba, convert_path, prepare_native, resize_nearest_planar, screen_size,
    try_capture_roi, view_changed,
};
use detect::Detector;
use input::{DrySink, X11Input};
use recognize::Recognizer;
use rules::{Automation, BoxHit, Sink};
use tui::{BatchDisplay, Dashboard, FrameDisplay, KeyAction, TableRow, TuiDisplay};
use view::{MODEL_SIZE, ROI_STEPS, Screen, View};

struct App {
    view: View,
    detector: Detector,
    recognizer: Recognizer,
    automation: Automation,
}

fn run_loop<C: Connection>(
    conn: &C,
    root: u32,
    scr: Screen,
    mut app: App,
    sink: &mut dyn Sink,
    display: &mut impl FrameDisplay,
) -> Result<i32, String> {
    let mut rgba = vec![0u8; 4 * app.view.size as usize * app.view.size as usize];
    let mut prev_bytes: Vec<u8> = Vec::new();
    let mut prev_view = app.view;
    let mut hits: Vec<BoxHit> = Vec::new();
    let mut det_ms = 0.0;
    let mut rec_ms = 0.0;
    let mut paused_once = false;

    loop {
        // Nur im Terminal-Modus: Tasten lesen (Batch läuft ohne Tty).
        while display.interactive()
            && event::poll(Duration::from_millis(5)).map_err(|e| format!("Terminal: {e}"))?
        {
            let Event::Key(k) = event::read().map_err(|e| format!("Terminal: {e}"))? else {
                continue;
            };
            match tui::map_key(k.code, k.modifiers) {
                KeyAction::Quit => return Ok(0),
                KeyAction::Pan(dx, dy) => {
                    app.view.pan(dx, dy, scr);
                }
                KeyAction::ZoomIn => {
                    app.view.zoom_in(scr);
                }
                KeyAction::ZoomOut => {
                    app.view.zoom_out(scr);
                }
                KeyAction::ToggleAutomation => {
                    let on = !app.automation.is_enabled();
                    app.automation.set_enabled(on);
                }
                KeyAction::None => {}
            }
        }

        // ROI-Wechsel invalidiert Caches + pausiert die Automation einen
        // Zyklus (kein Klick auf veraltete Boxen).
        if view_changed(&prev_view, &app.view) {
            prev_bytes.clear();
            hits.clear();
            paused_once = true;
            if app.view.size != prev_view.size {
                rgba = vec![0u8; 4 * app.view.size as usize * app.view.size as usize];
            }
            prev_view = app.view;
        }

        let bgra = match try_capture_roi(conn, root, &app.view) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("X11-Capture fehlgeschlagen: {e}");
                return Ok(1);
            }
        };
        let frame_changed = bgra != prev_bytes;
        if frame_changed {
            match convert_path(&app.view) {
                ConvertPath::Native => {
                    prepare_native(&bgra, &mut app.detector.det_input, &mut rgba);
                }
                ConvertPath::Scaled => {
                    resize_nearest_planar(
                        &bgra,
                        app.view.size as usize,
                        &mut app.detector.det_input,
                        MODEL_SIZE as usize,
                    );
                    bgra_to_rgba(&bgra, &mut rgba);
                }
            }

            let t0 = Instant::now();
            let mut boxes = app.detector.detect();
            det_ms = t0.elapsed().as_secs_f64() * 1000.0;

            let t1 = Instant::now();
            app.recognizer
                .recognize(&rgba, &mut boxes, app.view.size as usize);
            rec_ms = t1.elapsed().as_secs_f64() * 1000.0;

            hits.clear();
            for b in &boxes {
                let rect = app.view.to_screen_rect(b.x, b.y, b.w, b.h, scr);
                hits.push(BoxHit {
                    text: b.text.clone(),
                    rect,
                });
            }
            if paused_once {
                paused_once = false;
            } else {
                let logged = app.automation.log().len();
                app.automation.evaluate(&hits, (scr.w, scr.h), sink);
                // Gefeuerte Aktionen auch auf stderr: sichtbar ohne
                // Dashboard (z. B. --dry-run-Diagnose, Batch-Modus).
                for line in &app.automation.log()[logged..] {
                    eprintln!("{line}");
                }
            }
            prev_bytes = bgra;
        }

        let rows: Vec<TableRow> = hits
            .iter()
            .map(|h| TableRow {
                rect: h.rect,
                text: h.text.clone(),
            })
            .collect();
        let dash = Dashboard {
            roi: (app.view.x, app.view.y, app.view.size),
            screen: (scr.w, scr.h),
            changed: frame_changed,
            automation: app.automation.is_enabled(),
            det_ms,
            rec_ms,
            skipped: sink.skipped(),
            rows: &rows,
            log: app.automation.log(),
        };
        let term = crossterm::terminal::size().unwrap_or((80, 24));
        let text = render_frame(&dash, display.spatial(), term);
        display.show(&text, frame_changed)?;
        if display.done() {
            return Ok(0);
        }
        std::thread::sleep(Duration::from_millis(30));
    }
}

fn run() -> Result<i32, String> {
    let args = parse_args()?;
    let cfg = config::load_from_file(&args.rules_path).map_err(|e| {
        eprintln!("Konfig-Fehler: {e}");
        e
    })?;

    let (conn, screen_idx) = x11rb::connect(None).map_err(|e| format!("kein X11: {e}"))?;
    let root = conn.setup().roots[screen_idx].root;
    let size = screen_size(&conn, screen_idx);
    let scr = Screen {
        w: size.w,
        h: size.h,
    };

    if !ROI_STEPS.contains(&cfg.pan.default_size) {
        eprintln!("Hinweis: default_size ungültig — starte mit 640.");
    }
    let view = View::from_pan(
        cfg.pan.default_size,
        cfg.pan.step_divisor,
        cfg.pan.step_min_px,
    );

    let mut app = App {
        view,
        detector: Detector::new(),
        recognizer: Recognizer::new(),
        automation: Automation::from_config(&cfg),
    };
    // `-a`/`--auto`: Automation startet scharf — kein Tastendruck nötig
    // (für Skript-Läufe, die pro TOML-Regel einen frischen Prozess starten).
    if args.auto_start {
        app.automation.set_enabled(true);
        eprintln!("Hinweis: Automation per -a/--auto scharf geschaltet.");
    }

    let mut dry = DrySink;
    let mut live;
    let sink: &mut dyn Sink = if args.dry_run {
        eprintln!("Hinweis: --dry-run — keine echten Klicks/Tasten.");
        &mut dry
    } else {
        live = X11Input::new(&conn, screen_idx).map_err(|e| format!("XTEST-Init: {e}"))?;
        &mut live
    };

    if let Some(n) = args.headless_frames {
        let mut display = BatchDisplay {
            remaining: n.min(1000) as usize,
        };
        run_loop(&conn, root, scr, app, sink, &mut display)
    } else {
        let _guard = TuiGuard::enter().map_err(|e| format!("Terminal: {e}"))?;
        let mut display = TuiDisplay::new();
        run_loop(&conn, root, scr, app, sink, &mut display)
    }
}

fn main() {
    let code = match run() {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Fehler: {e}");
            let prefix = [
                "kann ",
                "TOML-Fehler",
                "Schema-Fehler",
                "Ungültige Konfiguration",
                "unbekanntes",
                "--rules",
                "--headless",
            ];
            std::process::exit(if prefix.iter().any(|p| e.starts_with(p)) {
                2
            } else {
                1
            });
        }
    };
    std::process::exit(code);
}
