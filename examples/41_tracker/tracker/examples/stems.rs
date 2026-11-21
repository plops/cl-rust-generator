//! Render each channel of a demo solo to `<out>/<demo>_chN.wav` (sound-design tool).
//! `cargo run --release --example stems -- dnb /tmp/stems 12`

use dnb_tracker::engine::Engine;
use dnb_tracker::protocol::Command;
use dnb_tracker::types::CHANNELS;
use dnb_tracker::{demo_songs, wav};
use std::path::PathBuf;

fn main() -> anyhow::Result<()> {
    let mut a = std::env::args().skip(1);
    let demo = a.next().unwrap_or_else(|| "dnb".into());
    let out = PathBuf::from(a.next().unwrap_or_else(|| ".".into()));
    let secs: f32 = a.next().map_or(Ok(12.0), |s| s.parse())?;
    std::fs::create_dir_all(&out)?;
    for solo in 0..CHANNELS {
        let song = demo_songs::by_name(&demo).expect("demo name");
        let mut e = Engine::new(Box::new(song), 48000.0);
        for ch in (0..CHANNELS).filter(|&c| c != solo) {
            e.apply(Command::SetMute {
                ch: ch as u8,
                mute: true,
            });
        }
        e.play(0, None);
        let mut buf = vec![0.0; (secs * 48000.0) as usize * 2];
        e.render(&mut buf);
        if buf.iter().any(|v| v.abs() > 1e-4) {
            let p = out.join(format!("{demo}_ch{solo}.wav"));
            wav::write_wav(&p, &buf, 48000)?;
            println!("{}", p.display());
        }
    }
    Ok(())
}
