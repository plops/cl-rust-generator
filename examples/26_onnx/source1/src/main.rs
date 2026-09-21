//! main.rs — nur CLI-Parsing + Verdrahtung (kein Verhalten).
//!
//! Ablauf wie source0: Flags → Screen + Region → Modell → je nach Modus
//! annotierter Frame (`--save-frame`), Dauerlauf ohne Fenster
//! (`--headless`) oder Live-Fenster (macroquad + fps-Pacer, via
//! `Window::from_config`, damit die Groesse aus den Flags kommt).

use anyhow::Result;
use clap::Parser;
use macroquad::prelude::Conf;
use ort::session::Session;
use source1::{
    Args, Capturer, ClampedRegion, blit_nearest, clamp_region, draw_detections, frame_period,
    infer_image, load_session, rgba_image_to_rgb, run_window_mq, save_frame_png, zoomed_size,
};
use std::time::Instant;

/// Ein Frame: capturen → Inferenz → Overlay. RGBA-Bytes der
/// Capture-Groesse plus Detektionszahl (fuer Log/Timing).
fn process_frame(
    session: &mut Session,
    cap: &Capturer,
    region: ClampedRegion,
    args: &Args,
) -> Result<(Vec<u8>, usize)> {
    let img = cap.capture(region)?;
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

    let cap = Capturer::connect(args.monitor)?;
    let geom = cap.geom();
    let region = clamp_region(args.x, args.y, args.w, args.h, geom)?;
    eprintln!(
        "capture: screen {} ({}x{}), region {:?}",
        args.monitor,
        geom.w,
        geom.h,
        (region.x, region.y, region.w, region.h)
    );

    let mut session = load_session(&args.model)?;
    eprintln!("model: {}", args.model);

    if let Some(path) = &args.save_frame {
        let (frame, n) = process_frame(&mut session, &cap, region, &args)?;
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
            process_frame(&mut session, &cap, region, &args)?;
            std::thread::sleep(period.saturating_sub(t0.elapsed()));
        }
    }

    let (dw, dh) = zoomed_size(region.w, region.h, args.zoom, args.win_w, args.win_h)?;
    let fps = args.effective_fps();
    eprintln!("view: {dw}x{dh} @ {fps} fps");
    let conf = Conf {
        window_title: "x11_rb_mq_viewer".to_string(),
        window_width: dw as i32,
        window_height: dh as i32,
        ..Default::default()
    };
    macroquad::Window::from_config(conf, async move {
        if let Err(e) = run_window_mq(region.w, region.h, dw, dh, fps, |src| {
            let (frame, _) = process_frame(&mut session, &cap, region, &args)?;
            src.copy_from_slice(&frame);
            Ok(())
        })
        .await
        {
            eprintln!("viewer error: {e:#}");
        }
    });
    Ok(())
}
