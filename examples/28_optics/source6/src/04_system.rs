//! System description: TOML config, surface layout and ray bundle.
//!
//! Conventions (prompt gaps, fixed here): surface vertices accumulate from
//! `thickness` (`z0[0] = 0`); the last `thickness` is the gap to the image
//! plane; each surface's `material` is the index *after* the surface (first
//! segment travels in air, `n = 1`); rays start collimated along `+z` from
//! `z0[0] - 10` on a grid over the pupil. `material` defaults to air when
//! omitted (patent tables only list glass). Pupil radius is `grid_radius`
//! if given, else `aperture_diameter / 2`, else a built-in default.
//! Dispersion is Cauchy: `n(l) = material + cauchy_b / l^2` (`l` in um,
//! `material` is `n_d`); `cauchy_b = 0` disables it per surface.

use serde::{Deserialize, Serialize};

/// d-line reference wavelength (um) used when no wavelengths are given.
pub const D_LINE: f64 = 0.5876;

/// Fallback pupil radius when the source specifies neither grid nor aperture.
pub const DEFAULT_PUPIL_RADIUS: f64 = 5.0;

fn default_ray_count() -> usize {
    10
}

fn default_learning_rate() -> f64 {
    0.001
}

fn default_iters() -> usize {
    20
}

fn default_wavelengths() -> Vec<f64> {
    vec![D_LINE]
}

fn air() -> f64 {
    1.0
}

/// Ray source: pupil sampling plus wavelength list.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceCfg {
    /// Ray count sampled over the pupil grid.
    #[serde(default = "default_ray_count")]
    pub ray_count: usize,
    /// Pupil radius (takes precedence over `aperture_diameter`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub grid_radius: Option<f64>,
    /// Full clear aperture; pupil radius is half of it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub aperture_diameter: Option<f64>,
    /// Wavelengths in um, traced independently (polychromatic loss).
    #[serde(default = "default_wavelengths")]
    pub wavelengths: Vec<f64>,
}

/// One optical surface; `radius = 0` is a plane.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Surface {
    /// Human-readable name.
    pub name: String,
    /// Radius of curvature (`0` = plane).
    pub radius: f64,
    /// Gap to the next vertex (last one: gap to the image plane).
    pub thickness: f64,
    /// Index after the surface (`n_d` when dispersive); air if omitted.
    #[serde(default = "air")]
    pub material: f64,
    /// Optimization variables: `radius`, `thickness`, `material`.
    #[serde(default)]
    pub optimize: Vec<String>,
    /// Clear aperture for profiles/stop; defaults to the pupil.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub diameter: Option<f64>,
    /// Cauchy B (um^2); `0` disables dispersion.
    #[serde(default)]
    pub cauchy_b: f64,
    /// Aperture stop: blocks rays beyond the pupil radius here.
    #[serde(default)]
    pub stop: bool,
}

/// Gradient-descent hyperparameters.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizeCfg {
    /// Step size per iteration.
    #[serde(default = "default_learning_rate")]
    pub learning_rate: f64,
    /// Descent iterations.
    #[serde(default = "default_iters")]
    pub iters: usize,
}

impl Default for OptimizeCfg {
    fn default() -> Self {
        Self {
            learning_rate: default_learning_rate(),
            iters: default_iters(),
        }
    }
}

/// Whole setup as parsed from TOML.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpticalSetup {
    pub source: SourceCfg,
    pub surfaces: Vec<Surface>,
    #[serde(default)]
    pub optimize: OptimizeCfg,
}

/// Parse a TOML document into a setup.
///
/// # Examples
///
/// ```
/// use optics::load_toml;
///
/// let setup = load_toml(
///     "[source]\n\
///      ray_count = 4\n\
///      grid_radius = 5.0\n\
///      [[surfaces]]\n\
///      name = \"Front\"\n\
///      radius = 50.0\n\
///      thickness = 5.0\n\
///      material = 1.5168\n\
///      [[surfaces]]\n\
///      name = \"Back\"\n\
///      radius = -100.0\n\
///      thickness = 40.0\n",
/// )
/// .expect("valid config");
/// assert_eq!(setup.surfaces.len(), 2);
/// assert_eq!(setup.source.ray_count, 4);
/// ```
pub fn load_toml(text: &str) -> Result<OpticalSetup, toml::de::Error> {
    toml::from_str(text)
}

/// Effective pupil radius from the source description.
#[must_use]
pub fn pupil_radius(source: &SourceCfg) -> f64 {
    if let Some(r) = source.grid_radius {
        return r;
    }
    if let Some(d) = source.aperture_diameter {
        return d / 2.0;
    }
    DEFAULT_PUPIL_RADIUS
}

/// Vertex z positions accumulated from thicknesses.
#[must_use]
pub fn vertices(surfaces: &[Surface]) -> Vec<f64> {
    let mut z = 0.0;
    let mut out = Vec::with_capacity(surfaces.len());
    for s in surfaces {
        out.push(z);
        z += s.thickness;
    }
    out
}

/// Image-plane z (last vertex plus last thickness; `0` when empty).
#[must_use]
pub fn image_z(surfaces: &[Surface]) -> f64 {
    let mut z = 0.0;
    for s in surfaces {
        z += s.thickness;
    }
    z
}

/// Pupil grid points, `ceil(sqrt(n))^2` truncated to `ray_count`.
#[must_use]
pub fn bundle(source: &SourceCfg) -> Vec<(f64, f64)> {
    if source.ray_count == 0 {
        return Vec::new();
    }
    let side = (source.ray_count as f64).sqrt().ceil() as usize;
    let side = side.max(1);
    let r = pupil_radius(source);
    let mut pts = Vec::with_capacity(side * side);
    for j in 0..side {
        for i in 0..side {
            let pick = |k: usize| {
                if side == 1 {
                    0.0
                } else {
                    -r + 2.0 * r * (k as f64) / ((side - 1) as f64)
                }
            };
            pts.push((pick(i), pick(j)));
        }
    }
    pts.truncate(source.ray_count);
    pts
}

/// Cauchy index of a surface at wavelength `lambda_um`.
#[must_use]
pub fn index_at(surface: &Surface, lambda_um: f64) -> f64 {
    surface.material + surface.cauchy_b / (lambda_um * lambda_um)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = include_str!("../assets/sample.toml");

    #[test]
    fn parses_prompt_example() {
        let setup = load_toml(SAMPLE).expect("sample must parse");
        assert_eq!(setup.source.ray_count, 10);
        assert_eq!(setup.surfaces.len(), 2);
        assert_eq!(setup.surfaces[0].name, "Front Element");
        assert!((setup.surfaces[0].radius - 50.0).abs() < 1e-12);
        assert_eq!(setup.surfaces[0].optimize, vec!["radius".to_string()]);
        assert!(setup.surfaces[1].optimize.is_empty());
    }

    #[test]
    fn vertex_layout_and_image_plane() {
        let setup = load_toml(SAMPLE).expect("sample must parse");
        assert_eq!(vertices(&setup.surfaces), vec![0.0, 5.0]);
        assert!((image_z(&setup.surfaces) - 45.0).abs() < 1e-12);
    }

    #[test]
    fn material_defaults_to_air() {
        let setup = load_toml(
            "[source]\n\
             [[surfaces]]\nname = \"L1 Back\"\nradius = -400.0\nthickness = 10.0\n",
        )
        .expect("must parse");
        assert!((setup.surfaces[0].material - 1.0).abs() < 1e-12);
        assert!(!setup.surfaces[0].stop);
    }

    #[test]
    fn aperture_source_variant() {
        let setup = load_toml(
            "[source]\nray_count = 1\naperture_diameter = 10.0\n\
             [[surfaces]]\nname = \"Stop\"\nradius = 0.0\nthickness = 15.0\n",
        )
        .expect("must parse");
        assert!((pupil_radius(&setup.source) - 5.0).abs() < 1e-12);
        assert_eq!(bundle(&setup.source), vec![(0.0, 0.0)]);
    }

    #[test]
    fn bundle_covers_pupil() {
        let setup = load_toml(SAMPLE).expect("sample must parse");
        let pts = bundle(&setup.source);
        assert_eq!(pts.len(), 10);
        for (x, y) in pts {
            assert!(x.abs() <= 5.0 + 1e-12 && y.abs() <= 5.0 + 1e-12);
        }
    }

    #[test]
    fn cauchy_dispersion_matches_bk7() {
        // N-BK7-ish: B = 0.0042 um^2 gives nF - nC ~ 0.008.
        let s = Surface {
            name: "t".into(),
            radius: 50.0,
            thickness: 5.0,
            material: 1.5168,
            optimize: vec![],
            diameter: None,
            cauchy_b: 0.0042,
            stop: false,
        };
        let d = index_at(&s, 0.4861) - index_at(&s, 0.6563);
        assert!((d - 0.008).abs() < 0.001);
    }
}
