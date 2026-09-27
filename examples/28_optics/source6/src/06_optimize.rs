//! RMS spot-size objective with seeded forward-mode gradients.
//!
//! Loss is `sum(x_i^2 + y_i^2)` over arrived image points (all
//! wavelengths). Each gradient component comes from one trace seeded with
//! that variable as [`Dual::variable`]: with the image point `(x, y)` as
//! dual numbers, `dLoss/dp = sum(2*(x*dx/dp + y*dy/dp))` is just the `.d`
//! field of the accumulated loss. [`descend`] runs plain gradient descent
//! and returns the loss history for the TUI graph.

use crate::dual::Dual;
use crate::system::{OpticalSetup, Surface};
use crate::trace::{DualSurface, IntersectionResult, layout_for, trace_dual};

/// Optimizable parameter kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKey {
    Radius,
    Thickness,
    Material,
}

impl VarKey {
    /// Canonical TOML key.
    #[must_use]
    pub fn key(self) -> &'static str {
        match self {
            VarKey::Radius => "radius",
            VarKey::Thickness => "thickness",
            VarKey::Material => "material",
        }
    }
}

/// One optimization variable: surface index plus parameter kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Var {
    pub surface: usize,
    pub key: VarKey,
}

/// Collect optimization variables; errors on unknown keys.
pub fn variables(setup: &OpticalSetup) -> Result<Vec<Var>, String> {
    let mut out = Vec::new();
    for (i, s) in setup.surfaces.iter().enumerate() {
        for key in &s.optimize {
            let k = match key.as_str() {
                "radius" => VarKey::Radius,
                "thickness" => VarKey::Thickness,
                "material" => VarKey::Material,
                other => {
                    return Err(format!(
                        "surface {i} ({}): unknown optimize key '{other}' \
                         (use 'radius', 'thickness' or 'material')",
                        s.name
                    ));
                }
            };
            out.push(Var { surface: i, key: k });
        }
    }
    Ok(out)
}

/// Read one variable value.
#[must_use]
pub fn get_var(surfaces: &[Surface], var: Var) -> f64 {
    let s = &surfaces[var.surface];
    match var.key {
        VarKey::Radius => s.radius,
        VarKey::Thickness => s.thickness,
        VarKey::Material => s.material,
    }
}

/// Write one variable value.
pub fn set_var(surfaces: &mut [Surface], var: Var, value: f64) {
    let s = &mut surfaces[var.surface];
    match var.key {
        VarKey::Radius => s.radius = value,
        VarKey::Thickness => s.thickness = value,
        VarKey::Material => s.material = value,
    }
}

/// Spot loss over arrived rays (value + seeded derivative).
#[must_use]
pub fn spot_loss(paths: &[IntersectionResult]) -> Dual {
    let mut loss = Dual::constant(0.0);
    for p in paths {
        if let Some(img) = p.image() {
            loss = loss + img.x * img.x + img.y * img.y;
        }
    }
    loss
}

/// Loss value of a setup at all wavelengths.
#[must_use]
pub fn loss_for(setup: &OpticalSetup) -> f64 {
    spot_loss(&crate::trace::trace_system(&setup.surfaces, setup)).v
}

/// Layout with `vars[seed]` as the single seeded variable at `lambda_um`.
fn seeded_layout(
    setup: &OpticalSetup,
    vars: &[Var],
    seed: usize,
    lambda_um: f64,
) -> (Vec<DualSurface>, Dual) {
    let (mut surfs, _) = layout_for(&setup.surfaces, lambda_um);
    let w = vars[seed];
    // Re-seed: layout_for used constants; patch the seeded entry.
    let mut z = Dual::constant(0.0);
    for (i, (s, ds)) in setup.surfaces.iter().zip(surfs.iter_mut()).enumerate() {
        ds.vertex = z;
        if w.surface == i && w.key == VarKey::Radius {
            ds.radius = Dual::variable(s.radius);
        }
        if w.surface == i && w.key == VarKey::Material {
            let b = s.cauchy_b / (lambda_um * lambda_um);
            ds.material = Dual::variable(s.material) + Dual::constant(b);
        }
        let thick = if w.surface == i && w.key == VarKey::Thickness {
            Dual::variable(s.thickness)
        } else {
            Dual::constant(s.thickness)
        };
        z = z + thick;
    }
    (surfs, z)
}

/// Exact gradient, one seeded trace per variable (summed over wavelengths).
#[must_use]
pub fn gradient(setup: &OpticalSetup, vars: &[Var]) -> Vec<f64> {
    (0..vars.len())
        .map(|k| {
            let mut g = 0.0;
            for lambda in &setup.source.wavelengths {
                let (surfs, image) = seeded_layout(setup, vars, k, *lambda);
                g += spot_loss(&trace_dual(&surfs, setup, image, *lambda)).d;
            }
            g
        })
        .collect()
}

/// Gradient descent over the setup's `optimize` variables.
/// Returns the updated setup plus the loss history (initial first).
pub fn descend(setup: &OpticalSetup) -> Result<(OpticalSetup, Vec<f64>), String> {
    let vars = variables(setup)?;
    let mut cur = setup.clone();
    let mut history = vec![loss_for(&cur)];
    for _ in 0..setup.optimize.iters {
        let g = gradient(&cur, &vars);
        for (w, step) in vars.iter().zip(g.iter()) {
            let v = get_var(&cur.surfaces, *w) - cur.optimize.learning_rate * step;
            set_var(&mut cur.surfaces, *w, v);
        }
        history.push(loss_for(&cur));
    }
    Ok((cur, history))
}

/// Serialize a setup (optimized parameters) back to TOML.
pub fn to_toml(setup: &OpticalSetup) -> Result<String, toml::ser::Error> {
    toml::to_string(setup)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::system::load_toml;

    const SAMPLE: &str = include_str!("../assets/sample.toml");

    fn sample() -> OpticalSetup {
        load_toml(SAMPLE).expect("sample must parse")
    }

    #[test]
    fn loss_is_finite_positive() {
        let setup = sample();
        let loss = loss_for(&setup);
        assert!(loss.is_finite() && loss > 0.0, "loss = {loss}");
    }

    #[test]
    fn gradient_matches_finite_differences() {
        let setup = sample();
        let vars = variables(&setup).expect("vars");
        assert_eq!(vars.len(), 1);
        let analytic = gradient(&setup, &vars)[0];
        let e = 1e-6;
        let mut plus = setup.clone();
        let mut minus = setup.clone();
        let base = get_var(&setup.surfaces, vars[0]);
        set_var(&mut plus.surfaces, vars[0], base + e);
        set_var(&mut minus.surfaces, vars[0], base - e);
        let numeric = (loss_for(&plus) - loss_for(&minus)) / (2.0 * e);
        let tol = 1e-4 * numeric.abs().max(1.0);
        assert!(
            (analytic - numeric).abs() < tol,
            "analytic = {analytic}, numeric = {numeric}"
        );
    }

    #[test]
    fn material_gradient_matches_finite_differences() {
        let mut setup = sample();
        setup.surfaces[0].optimize = vec!["material".to_string()];
        let vars = variables(&setup).expect("vars");
        let analytic = gradient(&setup, &vars)[0];
        let e = 1e-7;
        let mut plus = setup.clone();
        let mut minus = setup.clone();
        let base = get_var(&setup.surfaces, vars[0]);
        set_var(&mut plus.surfaces, vars[0], base + e);
        set_var(&mut minus.surfaces, vars[0], base - e);
        let numeric = (loss_for(&plus) - loss_for(&minus)) / (2.0 * e);
        let tol = 1e-3 * numeric.abs().max(1.0);
        assert!(
            (analytic - numeric).abs() < tol,
            "analytic = {analytic}, numeric = {numeric}"
        );
    }

    #[test]
    fn descend_lowers_loss() {
        let setup = sample();
        let (opt, history) = descend(&setup).expect("descent");
        assert_eq!(history.len(), setup.optimize.iters + 1);
        assert!(
            history.last().unwrap() < history.first().unwrap(),
            "history = {history:?}"
        );
        assert!(
            (opt.surfaces[0].radius - setup.surfaces[0].radius).abs() > 0.0,
            "radius must move"
        );
    }

    #[test]
    fn unknown_optimize_key_errors() {
        let setup = load_toml(
            "[source]\n[[surfaces]]\nname = \"S\"\nradius = 1.0\nthickness = 1.0\n\
             optimize = [\"focal\"]\n",
        )
        .expect("must parse");
        assert!(variables(&setup).is_err());
    }

    #[test]
    fn write_back_round_trips() {
        let setup = sample();
        let text = to_toml(&setup).expect("serialize");
        let back = load_toml(&text).expect("reparse");
        assert!((back.surfaces[0].radius - 50.0).abs() < 1e-12);
        assert_eq!(back.surfaces[0].optimize, vec!["radius".to_string()]);
    }
}
