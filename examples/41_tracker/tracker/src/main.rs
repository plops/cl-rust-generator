//! CLI-Verdrahtung: Argumente lesen, Song laden, Audio starten, dann TUI,
//! Headless-Wiedergabe oder Offline-Render.

use anyhow::{Context, Result, bail};
use dnb_tracker::{audio_out, demo_songs, protocol, song_io, ui_runner, ui_state::App, wav};
use std::path::PathBuf;

const USAGE: &str = "usage: dnb-tracker [SONG.txt] [--demo dnb|phonk|empty] [--device NAME]
                   [--list-devices] [--play] [--render OUT.wav] [--seconds N]

  SONG.txt        load (if it exists) and save target for Ctrl+S (default song.txt)
  --demo NAME     start from a built-in song instead of a file (default dnb)
  --device NAME   substring of an output device from --list-devices
  --play          play without TUI (for --seconds, default forever)
  --render FILE   write a 48 kHz WAV of --seconds (default 30) and exit";

#[derive(Default)]
struct Args {
    song: Option<PathBuf>,
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
            s if !s.starts_with('-') && a.song.is_none() => a.song = Some(s.into()),
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
    let path = args.song.clone().unwrap_or_else(|| "song.txt".into());
    let song = match (&args.demo, args.song.as_ref().filter(|p| p.exists())) {
        (None, Some(p)) => {
            let (song, warnings) = song_io::load(p)?;
            warnings.iter().for_each(|w| eprintln!("warning: {w}"));
            song
        }
        (demo, _) => {
            let name = demo.as_deref().unwrap_or("dnb");
            demo_songs::by_name(name).with_context(|| format!("unknown demo {name}"))?
        }
    };
    if let Some(out) = &args.render {
        let sr = 48000;
        let data = wav::render(song, args.seconds.unwrap_or(30.0), sr);
        wav::write_wav(out, &data, sr)?;
        println!("wrote {}", out.display());
        return Ok(());
    }
    let (mut ui, audio) = protocol::link();
    let out = audio_out::start(song.clone(), audio, args.device.as_deref())?;
    eprintln!("audio: {} @ {} Hz", out.device, out.sample_rate);
    if !args.play {
        return ui_runner::run(App::new(song, path), ui);
    }
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
    eprintln!("audio errors/xruns: {}", audio_out::error_count());
    Ok(())
}
