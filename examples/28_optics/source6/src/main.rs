//! optics CLI - argument parsing plus dispatch only (no behavior).

use optics::{
    AppState, OpticalSetup, RayEnd, back_focal_z, descend, efl, get_var, gradient, image_z,
    load_toml, loss_for, run_tui, to_json, to_toml, trace_system, variables,
};
use std::fs;

const HELP: &str = "optics - differentiable ray tracer
usage:
  optics trace       --config <toml>              trace bundle, print spots
  optics efl         --config <toml>              effective focal length (mm)
  optics optimize    --config <toml> [--iters N] [--lr F] [--write-back <toml>]
  optics sensitivity --config <toml>              exact dLoss/dp per optimize var
  optics export      --config <toml> --out <json>  three.js system.json
  optics tui         [--config <toml>]             telemetry (q/Esc quits)
  optics --help";

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if let Err(msg) = run(&args) {
        eprintln!("optics: {msg}");
        std::process::exit(1);
    }
}

/// Value of `--name <value>`, if present.
fn flag(args: &[String], name: &str) -> Result<Option<String>, String> {
    let Some(index) = args.iter().position(|arg| arg == name) else {
        return Ok(None);
    };
    let value = args
        .get(index + 1)
        .filter(|value| !value.starts_with("--"));
    value
        .cloned()
        .map(Some)
        .ok_or_else(|| format!("missing value for {name}"))
}

fn load_config(args: &[String]) -> Result<OpticalSetup, String> {
    let path = flag(args, "--config")?.unwrap_or_else(|| "assets/sample.toml".into());
    let text = fs::read_to_string(&path).map_err(|e| format!("read {path}: {e}"))?;
    load_toml(&text).map_err(|e| format!("parse {path}: {e}"))
}

fn run(args: &[String]) -> Result<(), String> {
    if args.is_empty() || args[0] == "--help" || args[0] == "-h" || args[0] == "help" {
        println!("{HELP}");
        return Ok(());
    }
    let allowed: &[&str] = match args[0].as_str() {
        "trace" | "efl" | "sensitivity" | "tui" => &["--config"],
        "optimize" => &["--config", "--iters", "--lr", "--write-back"],
        "export" => &["--config", "--out"],
        other => return Err(format!("unknown command '{other}' (see --help)")),
    };
    for pair in args[1..].chunks(2) {
        let name = &pair[0];
        if !allowed.contains(&name.as_str()) {
            return Err(format!("unknown option '{name}' (see --help)"));
        }
        if pair.get(1).is_none_or(|value| value.starts_with("--")) {
            return Err(format!("missing value for {name}"));
        }
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
            if let Some(n) = flag(args, "--iters")? {
                setup.optimize.iters = n.parse().map_err(|_| "bad --iters".to_string())?;
            }
            if let Some(lr) = flag(args, "--lr")? {
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
            if let Some(path) = flag(args, "--write-back")? {
                let text = to_toml(&opt).map_err(|e| e.to_string())?;
                fs::write(&path, text).map_err(|e| format!("write {path}: {e}"))?;
            }
            Ok(())
        }
        "sensitivity" => {
            let setup = load_config(args)?;
            let vars = variables(&setup)?;
            if vars.is_empty() {
                return Err("no optimize variables in config \
                            (flag tolerance parameters with `optimize = [...]`)"
                    .into());
            }
            let grad = gradient(&setup, &vars);
            let loss = loss_for(&setup);
            // High precision: downstream finite-difference cross-checks need
            // far more than the 6 decimals used for human-facing trace output.
            println!("loss = {loss:.12e}");
            // One line per variable: name, key, value and exact dLoss/dp.
            // `sensitivity` is the raw gradient; downstream tools rank by
            // its magnitude to find the tightest tolerances.
            for (w, s) in vars.iter().zip(grad.iter()) {
                println!(
                    "sensitivity {} {} value={:.6} dloss_dp={:.9e}",
                    setup.surfaces[w.surface].name,
                    w.key.key(),
                    get_var(&setup.surfaces, *w),
                    s
                );
            }
            Ok(())
        }
        "export" => {
            let setup = load_config(args)?;
            let out = flag(args, "--out")?.ok_or("missing --out <file>".to_string())?;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn args(values: &[&str]) -> Vec<String> {
        values.iter().map(|value| (*value).to_string()).collect()
    }

    #[test]
    fn flag_distinguishes_absent_and_present_values() {
        assert_eq!(flag(&args(&["trace"]), "--config"), Ok(None));
        assert_eq!(
            flag(&args(&["trace", "--config", "lens.toml"]), "--config"),
            Ok(Some("lens.toml".into()))
        );
        assert_eq!(
            flag(&args(&["optimize", "--lr", "-0.1"]), "--lr"),
            Ok(Some("-0.1".into()))
        );
    }

    #[test]
    fn flag_rejects_missing_values_and_following_options() {
        for name in ["--config", "--iters", "--lr", "--write-back", "--out"] {
            let expected = Err(format!("missing value for {name}"));
            assert_eq!(flag(&args(&["optimize", name]), name), expected);
            assert_eq!(
                flag(&args(&["optimize", name, "--config", "lens.toml"]), name),
                expected
            );
        }
    }

    #[test]
    fn missing_config_does_not_fall_back_to_sample() {
        assert_eq!(
            run(&args(&["trace", "--config"])),
            Err("missing value for --config".into())
        );
    }

    #[test]
    fn unknown_options_are_rejected_before_dispatch() {
        for command in ["trace", "efl", "optimize", "sensitivity", "export", "tui"] {
            assert_eq!(
                run(&args(&[command, "--confg", "lens.toml"])),
                Err("unknown option '--confg' (see --help)".into())
            );
        }
        assert_eq!(
            run(&args(&["trace", "--iters", "1"])),
            Err("unknown option '--iters' (see --help)".into())
        );
    }

    #[test]
    fn documented_options_succeed() {
        assert_eq!(
            run(&args(&["trace", "--config", "assets/sample.toml"])),
            Ok(())
        );
        assert_eq!(
            run(&args(&[
                "optimize", "--config", "assets/sample.toml", "--iters", "0", "--lr", "0.1",
            ])),
            Ok(())
        );
    }

    #[test]
    fn negative_numeric_values_reach_numeric_validation() {
        assert_eq!(
            run(&args(&["optimize", "--lr", "-0.1"])),
            Err("learning_rate must be finite and positive".into())
        );
        assert_eq!(
            run(&args(&["optimize", "--iters", "-1"])),
            Err("bad --iters".into())
        );
    }

    #[test]
    fn missing_values_are_rejected_before_dispatch() {
        assert_eq!(
            run(&args(&["optimize", "--write-back"])),
            Err("missing value for --write-back".into())
        );
        assert_eq!(
            run(&args(&["optimize", "--lr", "--iters", "1"])),
            Err("missing value for --lr".into())
        );
    }
}
