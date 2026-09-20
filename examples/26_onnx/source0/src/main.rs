//! main.rs — nur CLI-Parsing + Verdrahtung (kein Verhalten).
//!
//! S0: Flags parsen, validieren, effektive Konfiguration melden.
//! Capture/Inferenz/View kommen in S1–S4 dazu.

use anyhow::Result;
use clap::Parser;
use source0::Args;

fn main() -> Result<()> {
    let args = Args::parse();
    args.validate()?;
    println!(
        "region=({},{}) {}x{} monitor={} zoom={} fps={} conf={} nms={} headless={} model={}",
        args.x,
        args.y,
        args.w,
        args.h,
        args.monitor,
        args.zoom,
        args.effective_fps(),
        args.conf,
        args.nms,
        args.headless,
        args.model,
    );
    if let Some(path) = &args.save_frame {
        println!("save-frame={}", path.display());
    }
    Ok(())
}
