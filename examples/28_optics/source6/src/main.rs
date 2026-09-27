//! optics CLI - argument parsing plus dispatch only (no behavior).

use optics::{
    AppState, OpticalSetup, RayEnd, back_focal_z, descend, efl, get_var, image_z, load_toml,
    loss_for, run_tui, to_json, to_toml, trace_system, variables,
};
use std::fs;

const HELP: &str = "optics - differentiable ray tracer
usage:
  optics trace    --config <toml>                 trace bundle, print spots
  optics efl      --config <toml>                 effective focal length (mm)
  optics optimize --config <toml> [--iters N] [--lr F] [--write-back <toml>]
  optics export   --config <toml> --out <json>     three.js system.json
  optics tui      [--config <toml>]                telemetry (q/Esc quits)
  optics --help";

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if let Err(msg) = run(&args) {
        eprintln!("optics: {msg}");
        std::process::exit(1);
    }
}

/// Value of `--name <value>`, if present.
fn flag(args: &[String], name: &str) -> Option<String> {
    args.windows(2).find(|w| w[0] == name).map(|w| w[1].clone())
}

fn load_config(args: &[String]) -> Result<OpticalSetup, String> {
    let path = flag(args, "--config").unwrap_or_else(|| "assets/sample.toml".into());
    let text = fs::read_to_string(&path).map_err(|e| format!("read {path}: {e}"))?;
    load_toml(&text).map_err(|e| format!("parse {path}: {e}"))
}

fn run(args: &[String]) -> Result<(), String> {
    if args.is_empty() || args[0] == "--help" || args[0] == "-h" || args[0] == "help" {
        println!("{HELP}");
        return Ok(());
    }
    match args[0].as_str() {
        "trace" => {
            let setup = load_config(args)?;
            let paths = trace_system(&setup.surfaces, &setup);
            let arrived = paths.iter().filter(|p| p.end == RayEnd::Image).count();
            println!(
                "rays: {arrived}/{} arrived, loss = {:.6}",
                paths.len(),
                loss_for(&setup)
            );
            for (i, p) in paths.iter().enumerate() {
                match p.image() {
                    Some(img) => println!(
                        "ray {i}: {:?} x={:.4} y={:.4} z={:.4} l={}",
                        p.end, img.x.v, img.y.v, img.z.v, p.wavelength
                    ),
                    None => println!("ray {i}: {:?} l={}", p.end, p.wavelength),
                }
            }
            Ok(())
        }
        "efl" => {
            let setup = load_config(args)?;
            match (efl(&setup), back_focal_z(&setup)) {
                (Some(f), Some(b)) => {
                    let img = image_z(&setup.surfaces);
                    println!("EFL = {f:.4} mm");
                    println!("back focus z = {b:.4} mm (image plane {img:.4})");
                    println!("defocus = {:.4} mm", img - b);
                    Ok(())
                }
                _ => Err("EFL not computable (marginal ray lost)".into()),
            }
        }
        "optimize" => {
            let mut setup = load_config(args)?;
            if let Some(n) = flag(args, "--iters") {
                setup.optimize.iters = n.parse().map_err(|_| "bad --iters".to_string())?;
            }
            if let Some(lr) = flag(args, "--lr") {
                setup.optimize.learning_rate = lr.parse().map_err(|_| "bad --lr".to_string())?;
            }
            let vars = variables(&setup)?;
            if vars.is_empty() {
                return Err("no optimize variables in config".into());
            }
            let (opt, history) = descend(&setup)?;
            let first = history.first().unwrap_or(&f64::NAN);
            let last = history.last().unwrap_or(&f64::NAN);
            println!(
                "loss: {first:.6} -> {last:.6} ({} iters)",
                history.len().saturating_sub(1)
            );
            for w in &vars {
                println!(
                    "{}[{}] = {:.6}",
                    opt.surfaces[w.surface].name,
                    w.key.key(),
                    get_var(&opt.surfaces, *w)
                );
            }
            if let Some(path) = flag(args, "--write-back") {
                let text = to_toml(&opt).map_err(|e| e.to_string())?;
                fs::write(&path, text).map_err(|e| format!("write {path}: {e}"))?;
            }
            Ok(())
        }
        "export" => {
            let setup = load_config(args)?;
            let out = flag(args, "--out").ok_or("missing --out <file>".to_string())?;
            let paths = trace_system(&setup.surfaces, &setup);
            let nseg: usize = paths.iter().map(|p| p.points.len().saturating_sub(1)).sum();
            fs::write(&out, to_json(&setup, &paths)).map_err(|e| format!("write {out}: {e}"))?;
            println!("wrote {out} ({nseg} segments)");
            Ok(())
        }
        "tui" => {
            let setup = load_config(args)?;
            let (opt, history) = descend(&setup).unwrap_or_else(|_| {
                let h = vec![loss_for(&setup)];
                (setup.clone(), h)
            });
            let vars = variables(&setup).unwrap_or_default();
            let state = AppState {
                vars: vars
                    .iter()
                    .map(|w| {
                        (
                            format!("{}[{}]", opt.surfaces[w.surface].name, w.key.key()),
                            get_var(&opt.surfaces, *w),
                        )
                    })
                    .collect(),
                loss_history: history,
                status: format!("{} surfaces", setup.surfaces.len()),
            };
            run_tui(&state).map_err(|e| e.to_string())
        }
        other => Err(format!("unknown command '{other}' (see --help)")),
    }
}
