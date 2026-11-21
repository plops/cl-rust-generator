//! CLI-Verdrahtung: Argumente lesen, Song laden, Audio starten, UI oder Render.

use anyhow::{Context, Result, bail};
use dnb_tracker::{audio_out, demo_songs, protocol, types::Song, wav};
use std::path::PathBuf;

const USAGE: &str = "usage: dnb-tracker [--demo dnb|phonk|empty] [--device NAME]
                   [--list-devices] [--play] [--render OUT.wav] [--seconds N]";

#[derive(Default)]
struct Args {
    demo: Option<String>,
    device: Option<String>,
    list: bool,
    play: bool,
    render: Option<PathBuf>,
    seconds: Option<f32>,
}

fn parse_args() -> Result<Args> {
    let mut a = Args::default();
    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        let mut val = || it.next().with_context(|| format!("{arg} needs a value"));
        match arg.as_str() {
            "--demo" => a.demo = Some(val()?),
            "--device" => a.device = Some(val()?),
            "--list-devices" => a.list = true,
            "--play" => a.play = true,
            "--render" => a.render = Some(val()?.into()),
            "--seconds" => a.seconds = Some(val()?.parse()?),
            "-h" | "--help" => {
                println!("{USAGE}");
                std::process::exit(0);
            }
            _ => bail!("unknown argument {arg}\n{USAGE}"),
        }
    }
    Ok(a)
}

fn main() -> Result<()> {
    let args = parse_args()?;
    if args.list {
        for d in audio_out::list_devices()? {
            println!("{d}");
        }
        return Ok(());
    }
    let demo = args.demo.as_deref().unwrap_or("dnb");
    let song: Song = demo_songs::by_name(demo).with_context(|| format!("unknown demo {demo}"))?;
    if let Some(out) = &args.render {
        let sr = 48000;
        let data = wav::render(song, args.seconds.unwrap_or(30.0), sr);
        wav::write_wav(out, &data, sr)?;
        println!("wrote {}", out.display());
        return Ok(());
    }
    let (mut ui, audio) = protocol::link();
    let out = audio_out::start(song, audio, args.device.as_deref())?;
    eprintln!("playing on {} @ {} Hz", out.device, out.sample_rate);
    ui.send(protocol::Command::Play {
        order_pos: 0,
        loop_pattern: None,
    });
    let secs = args.seconds.unwrap_or(f32::INFINITY);
    let start = std::time::Instant::now();
    while start.elapsed().as_secs_f32() < secs {
        std::thread::sleep(std::time::Duration::from_millis(50));
        ui.poll();
    }
    Ok(())
}
