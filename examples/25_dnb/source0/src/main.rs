//! main.rs — nur CLI-Parsing + Verdrahtung (kein Verhalten).

use anyhow::Result;
use clap::Parser;
use source0::{list_devices, play_live, render_bars, write_wav, Preset};

#[derive(Parser, Debug)]
#[command(name = "dark_dnb_synth", about = "174 BPM dark DnB synth (ALSA)")]
struct Args {
    /// Tempo in BPM (ueberschreibt Preset)
    #[arg(long)]
    bpm: Option<f64>,
    /// Takte, 0 = Stille rendern (ueberschreibt Preset)
    #[arg(long)]
    bars: Option<u32>,
    /// Master-Gain in dB, clamp +6 (ueberschreibt Preset)
    #[arg(long)]
    gain_db: Option<f32>,
    /// Offline nach WAV rendern statt live zu spielen
    #[arg(long)]
    render_wav: Option<String>,
    /// ALSA-Geraete auflisten und beenden
    #[arg(long)]
    list_devices: bool,
    /// Preset-TOML laden (bpm/bars/gain_db, CLI ueberschreibt nicht)
    #[arg(long)]
    preset: Option<String>,
    /// Aktuelle Einstellungen als Preset-TOML speichern und beenden
    #[arg(long)]
    save_preset: Option<String>,
    /// Default-Preset als TOML auf stdout und beenden
    #[arg(long)]
    dump_preset: bool,
    /// 60-s-Framezaehl-Report (erwartet vs. gerendert) und beenden
    #[arg(long)]
    calibrate: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if args.list_devices {
        for d in list_devices()? {
            println!("{d}");
        }
        return Ok(());
    }
    if args.dump_preset {
        print!("{}", toml::to_string_pretty(&Preset::default())?);
        return Ok(());
    }
    // Aufloesung: Default <- Preset <- explizite CLI-Flags.
    let mut preset = Preset::default();
    if let Some(path) = &args.preset {
        preset = Preset::load(path)?;
    }
    let bpm = args.bpm.unwrap_or(preset.bpm);
    let bars = args.bars.unwrap_or(preset.bars);
    let gain_db = args.gain_db.unwrap_or(preset.gain_db);
    if let Some(path) = &args.save_preset {
        Preset {
            ver: source0::PRESET_VER,
            bpm,
            bars,
            gain_db,
        }
        .save(path)?;
        println!("saved preset -> {path}");
        return Ok(());
    }
    if args.calibrate {
        let sample_rate = 44100.0;
        let frames = render_bars(bpm, 52, sample_rate);
        let expected = (source0::bar_secs(bpm) * sample_rate).round() as u64 * 52;
        println!("expected {expected} frames, rendered {}", frames.len());
        if frames.len() as u64 != expected {
            anyhow::bail!("calibration mismatch");
        }
        return Ok(());
    }
    let sample_rate = 44100.0;
    if let Some(path) = args.render_wav {
        let frames = render_bars(bpm, bars, sample_rate);
        write_wav(&path, &frames, sample_rate as u32)?;
        println!("rendered {} frames -> {path}", frames.len());
        return Ok(());
    }
    println!("Playing dark DnB: {bpm} BPM, {bars} bars");
    play_live(bpm, bars, gain_db)
}
