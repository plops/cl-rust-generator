//! `lbw-replay` — Headless-Replay einer `.lbwlog`-Aufzeichnung.
//!
//! Füttert alle Server→Client-Nachrichten durch den echten AV1-`Decoder` und
//! die echte `Scene` (kein Fenster, kein Server nötig) und meldet Zähler plus
//! Canvas-Hash (Determinismus-Nachweis: zwei Läufe → gleicher Hash).
//! Client→Server-Eingaben werden nicht abgespielt (reine Anzeige-Wiedergabe).

use std::io::{BufWriter, Write};

use clap::Parser;
use lbw_client::av1::Decoder;
use lbw_client::net::Event;
use lbw_client::scene::Scene;
use lbw_common::framing::decode_server_logged;
use lbw_common::{HEIGHT, ServerMsg, WIDTH};
use lbw_log::fnv1a64;
use lbw_log::io::load_lenient;
use lbw_log::record::{Dir, LogRecord};
use lbw_log::stats::log_version;

#[derive(Parser)]
#[command(
    name = "lbw-replay",
    version,
    about = "Spielt .lbwlog headless wieder ab"
)]
struct Args {
    /// Aufzeichnung (Server- oder Client-Log).
    file: String,
    /// Canvas nach jeder Kachel als PPM in dieses Verzeichnis schreiben.
    #[arg(long)]
    ppm: Option<String>,
    /// Mono-Abstände der Aufnahme einhalten (Echtzeit, max. 2 s je Sprung).
    #[arg(long)]
    realtime: bool,
}

fn write_ppm(path: &str, rgba: &[u8]) -> std::io::Result<()> {
    let mut f = BufWriter::new(std::fs::File::create(path)?);
    write!(f, "P6\n{WIDTH} {HEIGHT}\n255\n")?;
    for px in rgba.as_chunks::<4>().0 {
        f.write_all(&px[..3])?;
    }
    Ok(())
}

fn main() {
    let args = Args::parse();
    let (records, torn) = match load_lenient(&args.file) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("lbw-replay: {e}");
            std::process::exit(1);
        }
    };
    if torn {
        eprintln!("{}: WARNUNG — zerrissener Schluss, Teilstand", args.file);
    }
    if let Some(d) = &args.ppm
        && let Err(e) = std::fs::create_dir_all(d)
    {
        eprintln!("lbw-replay: {d}: {e}");
        std::process::exit(1);
    }
    let mut dec = match Decoder::new() {
        Ok(d) => d,
        Err(e) => {
            eprintln!("lbw-replay: rav1d: {e}");
            std::process::exit(1);
        }
    };
    let mut scene = Scene::new();
    let version = log_version(&records);
    let (mut tiles, mut texts, mut decode_err, mut msg_err, mut ppm_n) =
        (0u32, 0u32, 0u32, 0u32, 0u32);
    let mut next_id = 1u64; // für v1-Logs (AddText ohne id → frische IDs)
    let mut last_mono: Option<u64> = None;
    for r in &records {
        if let LogRecord::Msg {
            stamp, dir, body, ..
        } = r
        {
            if args.realtime {
                if let Some(t0) = last_mono {
                    let dt = stamp.mono_us.saturating_sub(t0).min(2_000_000);
                    std::thread::sleep(std::time::Duration::from_micros(dt));
                }
                last_mono = Some(stamp.mono_us);
            }
            if *dir != Dir::SrvToCli {
                continue;
            }
            let msg: ServerMsg = match decode_server_logged(body, version) {
                Ok(m) => m,
                Err(e) => {
                    msg_err += 1;
                    eprintln!("lbw-replay: Protokoll: {e}");
                    continue;
                }
            };
            match msg {
                ServerMsg::Hello => scene.apply(Event::Connected),
                ServerMsg::ClearText => scene.apply(Event::ClearText),
                ServerMsg::AddText(mut t) => {
                    texts += 1;
                    if t.id == 0 {
                        t.id = next_id;
                        next_id += 1;
                    }
                    scene.apply(Event::AddText(t));
                }
                ServerMsg::RemoveText(id) => scene.apply(Event::RemoveText(id)),
                ServerMsg::Tile { x, y, data } => match dec.decode(&data) {
                    Ok(rgba) => {
                        tiles += 1;
                        scene.apply(Event::Tile {
                            x,
                            y,
                            w: rgba.w,
                            h: rgba.h,
                            rgba: rgba.data,
                            bytes: data.len(),
                        });
                        if let Some(d) = &args.ppm {
                            ppm_n += 1;
                            let p = format!("{d}/frame-{ppm_n:05}.ppm");
                            if let Err(e) = write_ppm(&p, &scene.canvas) {
                                eprintln!("lbw-replay: {p}: {e}");
                                std::process::exit(1);
                            }
                        }
                    }
                    Err(e) => {
                        decode_err += 1;
                        eprintln!("lbw-replay: AV1: {e}");
                    }
                },
            }
        }
    }
    println!(
        "replay: {} Kacheln, {} Texte, canvas {:016x} ({} Fehler: {} msg, {} av1)",
        tiles,
        texts,
        fnv1a64(&scene.canvas),
        msg_err + decode_err,
        msg_err,
        decode_err
    );
}
