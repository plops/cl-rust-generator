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
///
/// Rays are grouped by (wavelength, field angle); the loss is the sum of
/// squared distances of each arrived ray from the centroid of its group. An
/// off-axis field lands beside the axis by design, so this measures spot
/// *size* (aberration), not field position. For a single on-axis field the
/// centroid is ~0 and this reduces to the legacy `sum(x^2 + y^2)`.
#[must_use]
pub fn spot_loss(paths: &[IntersectionResult]) -> Dual {
    use std::collections::BTreeMap;
    // Group arrived rays by (wavelength bits, field bits).
    let mut groups: BTreeMap<(u64, u64), Vec<usize>> = BTreeMap::new();
    for (i, p) in paths.iter().enumerate() {
        if p.image().is_some() {
            let key = (p.wavelength.to_bits(), p.field_deg.to_bits());
            groups.entry(key).or_default().push(i);
        }
    }
    let mut loss = Dual::constant(0.0);
    for idxs in groups.values() {
        let n = idxs.len() as f64;
        if n == 0.0 {
            continue;
        }
        // Group centroid (Dual, so gradients flow through it).
        let mut cx = Dual::constant(0.0);
        let mut cy = Dual::constant(0.0);
        for &i in idxs {
            let img = paths[i].image().expect("grouped ray arrived");
            cx = cx + img.x;
            cy = cy + img.y;
        }
        cx = cx / n;
        cy = cy / n;
        for &i in idxs {
            let img = paths[i].image().expect("grouped ray arrived");
            let dx = img.x - cx;
            let dy = img.y - cy;
            loss = loss + dx * dx + dy * dy;
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

/// Convergence tolerance: stop when `|loss - prev_loss|` falls below this.
pub const CONVERGENCE_TOL: f64 = 1e-12;

/// Gradient descent over the setup's `optimize` variables.
///
/// Returns the updated setup plus the loss history (initial loss first).
/// The loop stops early once the loss change between iterations drops below
/// [`CONVERGENCE_TOL`]. Each step is guarded: if a full step would *increase*
/// the loss (learning rate too large for the local curvature), the step is
/// halved up to a few times, and if it still fails to improve, descent stops
/// with the last improving parameters rather than diverging.
///
/// # Examples
///
/// ```
/// use optics::{load_toml, descend};
///
/// let setup = load_toml(
///     "[source]\n\
///      ray_count = 10\n\
///      grid_radius = 5.0\n\
///      [optimize]\n\
///      learning_rate = 0.001\n\
///      iters = 20\n\
///      [[surfaces]]\n\
///      name = \"Front\"\n\
///      radius = 50.0\n\
///      thickness = 5.0\n\
///      material = 1.5168\n\
///      optimize = [\"radius\"]\n\
///      [[surfaces]]\n\
///      name = \"Back\"\n\
///      radius = -100.0\n\
///      thickness = 40.0\n",
/// )
/// .unwrap();
///
/// let (optimized, history) = descend(&setup).expect("has optimize vars");
/// // The loss history never increases and ends below where it started.
/// assert!(history.windows(2).all(|w| w[1] <= w[0]));
/// assert!(history.last().unwrap() < history.first().unwrap());
/// // The optimized radius has moved away from its initial value.
/// assert_ne!(optimized.surfaces[0].radius, setup.surfaces[0].radius);
/// ```
pub fn descend(setup: &OpticalSetup) -> Result<(OpticalSetup, Vec<f64>), String> {
    let vars = variables(setup)?;
    let mut cur = setup.clone();
    let mut loss = loss_for(&cur);
    let mut history = vec![loss];
    let base_lr = cur.optimize.learning_rate;
    for _ in 0..setup.optimize.iters {
        let g = gradient(&cur, &vars);
        // Backtracking line search: shrink the step until it improves.
        let mut lr = base_lr;
        let mut stepped = false;
        for _ in 0..8 {
            let mut trial = cur.clone();
            for (w, step) in vars.iter().zip(g.iter()) {
                let v = get_var(&trial.surfaces, *w) - lr * step;
                set_var(&mut trial.surfaces, *w, v);
            }
            let trial_loss = loss_for(&trial);
            if trial_loss.is_finite() && trial_loss < loss {
                cur = trial;
                history.push(trial_loss);
                let improved = loss - trial_loss;
                loss = trial_loss;
                stepped = true;
                if improved < CONVERGENCE_TOL {
                    return Ok((cur, history));
                }
                break;
            }
            lr *= 0.5;
        }
        if !stepped {
            // No step size improved the loss: at a local minimum or the
            // gradient is uninformative. Stop with the best parameters.
            break;
        }
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
        // Backtracking + early-stop may end before `iters`, but never after.
        assert!(history.len() >= 2);
        assert!(history.len() <= setup.optimize.iters + 1);
        // Loss history is monotonically non-increasing (guarded steps only
        // ever accept an improving trial).
        assert!(
            history.windows(2).all(|w| w[1] <= w[0]),
            "history not monotone: {history:?}"
        );
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
    fn descend_does_not_diverge_with_large_lr() {
        // A wildly oversized learning rate would blow up plain descent;
        // the backtracking guard must keep the loss non-increasing.
        let mut setup = sample();
        setup.optimize.learning_rate = 1e6;
        let (_opt, history) = descend(&setup).expect("descent");
        assert!(
            history.iter().all(|l| l.is_finite()),
            "history has non-finite loss: {history:?}"
        );
        assert!(
            history.last().unwrap() <= history.first().unwrap(),
            "loss increased: {history:?}"
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

    #[test]
    fn on_axis_loss_matches_legacy_sum_of_squares() {
        // With a single on-axis field AND a symmetric (full-grid) bundle the
        // group centroid is ~0, so the new group-centroid loss equals the
        // legacy sum(x^2 + y^2). This guards backward compatibility of the
        // loss definition on the symmetric case the plan calls out.
        // ray_count = 9 fills a 3x3 grid exactly (no asymmetric truncation).
        let setup = load_toml(
            "[source]\nray_count = 9\ngrid_radius = 5.0\n\
             [[surfaces]]\nname = \"L1\"\nradius = 50.0\nthickness = 5.0\nmaterial = 1.5168\n\
             [[surfaces]]\nname = \"L2\"\nradius = -100.0\nthickness = 40.0\n",
        )
        .expect("parse");
        let paths = crate::trace::trace_system(&setup.surfaces, &setup);
        let legacy: f64 = paths
            .iter()
            .filter_map(|p| p.image())
            .map(|img| img.x.v * img.x.v + img.y.v * img.y.v)
            .sum();
        let grouped = spot_loss(&paths).v;
        assert!(
            (legacy - grouped).abs() < 1e-9,
            "legacy {legacy} vs grouped {grouped}"
        );
    }
}
