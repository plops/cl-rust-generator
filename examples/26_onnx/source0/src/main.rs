//! main.rs — nur CLI-Parsing + Verdrahtung (kein Verhalten).
//!
//! Ablauf: Flags → Monitor + Region → Modell → je nach Modus ein
//! annotierter Frame (`--save-frame`), Dauerlauf ohne Fenster
//! (`--headless`) oder Live-Fenster (pixels + fps-Pacer).

use anyhow::Result;
use clap::Parser;
use ort::session::Session;
use source0::{
    blit_nearest, capture_clamped, clamp_region, draw_detections, frame_period, infer_image,
    load_session, monitor_geom, rgba_image_to_rgb, run_window, save_frame_png, select_monitor,
    zoomed_size, Args, ClampedRegion,
};
use std::time::Instant;

/// Ein Frame: capturen → Inferenz → Overlay. Gibt die RGBA-Bytes der
/// Capture-Groesse plus Detektionszahl zurueck (fuer Log/Timing).
fn process_frame(
    session: &mut Session,
    mon: &xcap::Monitor,
    region: ClampedRegion,
    args: &Args,
) -> Result<(Vec<u8>, usize)> {
    let img = capture_clamped(mon, region)?;
    let (w, h) = img.dimensions();
    let rgb = rgba_image_to_rgb(&img);
    let t0 = Instant::now();
    let dets = infer_image(session, rgb.as_raw(), w, h, args.conf, args.nms)?;
    let infer_ms = t0.elapsed().as_secs_f64() * 1000.0;
    let mut frame = img.into_raw();
    draw_detections(&mut frame, w, h, &dets);
    eprintln!(
        "frame {w}x{h}: {} detections, infer {infer_ms:.1} ms",
        dets.len()
    );
    Ok((frame, dets.len()))
}

fn main() -> Result<()> {
    let args = Args::parse();
    args.validate()?;

    let mon = select_monitor(args.monitor)?;
    let geom = monitor_geom(&mon)?;
    let region = clamp_region(args.x, args.y, args.w, args.h, geom)?;
    eprintln!(
        "capture: monitor {} ({}x{}), region {:?}",
        args.monitor,
        geom.w,
        geom.h,
        (region.x, region.y, region.w, region.h)
    );

    let mut session = load_session(&args.model)?;
    eprintln!("model: {}", args.model);

    if let Some(path) = &args.save_frame {
        let (frame, n) = process_frame(&mut session, &mon, region, &args)?;
        let (dw, dh) = zoomed_size(region.w, region.h, args.zoom, args.win_w, args.win_h)?;
        let mut out = vec![0u8; (dw as usize) * (dh as usize) * 4];
        blit_nearest(&frame, region.w, region.h, &mut out, dw, dh)?;
        save_frame_png(path, &out, dw, dh)?;
        eprintln!("saved {} ({dw}x{dh}, {n} detections)", path.display());
        return Ok(());
    }

    if args.headless {
        let period = frame_period(args.effective_fps());
        loop {
            let t0 = Instant::now();
            process_frame(&mut session, &mon, region, &args)?;
            let wait = period.saturating_sub(t0.elapsed());
            std::thread::sleep(wait);
        }
    }

    let (dw, dh) = zoomed_size(region.w, region.h, args.zoom, args.win_w, args.win_h)?;
    eprintln!("view: {dw}x{dh} @ {} fps", args.effective_fps());
    run_window(
        region.w,
        region.h,
        dw,
        dh,
        "x11_yolo_viewer",
        args.effective_fps(),
        move |src| {
            let (frame, _) = process_frame(&mut session, &mon, region, &args)?;
            src.copy_from_slice(&frame);
            Ok(())
        },
    )
}
