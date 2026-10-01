//! Treemap-Disk-Visualisierer (wgpu + winit, Vulkan-only).
//!
//! Nur Verdrahtung: CLI-Einstiegspunkt, Event-Loop, Exit-Codes.
//! Exit-Codes: 0 = ok, 1 = kein Verzeichnis, 2 = Fenster-/GPU-Fehler.

use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use treemap::App;
use winit::event_loop::EventLoop;

fn main() -> ExitCode {
    let raw = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));
    let target = fs::canonicalize(&raw).unwrap_or(raw);
    if !target.is_dir() {
        eprintln!("error: not a directory: {}", target.display());
        return ExitCode::from(1);
    }
    let event_loop = match EventLoop::new() {
        Ok(event_loop) => event_loop,
        Err(err) => {
            eprintln!("error: event loop: {err:?}");
            return ExitCode::from(2);
        }
    };
    let proxy = event_loop.create_proxy();
    let mut app = App::new(target, proxy);
    if let Err(err) = event_loop.run_app(&mut app) {
        eprintln!("error: event loop: {err:?}");
        return ExitCode::from(2);
    }
    match app.take_error() {
        Some(err) => {
            eprintln!("error: {err}");
            ExitCode::from(2)
        }
        None => ExitCode::SUCCESS,
    }
}
