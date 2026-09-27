//! Versioned JSON export for the Three.js viewer.
//!
//! Schema `version = 1`: lens profiles as `[r, z]` polylines (feed into
//! `THREE.LatheGeometry` after revolving) and ray paths as consecutive
//! point pairs (directly consumable by `THREE.LineSegments`). Paths are
//! grouped by wavelength in configuration order; the `wavelengths` echo
//! lets the viewer color them. Sag: `z(r) = z0 + R - sgn(R)*sqrt(R^2-r^2)`.

use crate::optimize::{get_var, spot_loss, variables};
use crate::system::{OpticalSetup, pupil_radius, vertices};
use crate::trace::IntersectionResult;
use serde::{Deserialize, Serialize};

/// Current schema version (bump on breaking changes).
pub const SCHEMA_VERSION: u32 = 1;

/// Radial samples per lens profile.
pub const PROFILE_SAMPLES: usize = 25;

/// Optimized variable snapshot.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportVar {
    pub surface: String,
    pub key: String,
    pub value: f64,
}

/// Lens profile polyline for one surface.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportSurface {
    pub name: String,
    pub vertex_z: f64,
    pub radius: f64,
    /// `[r, z]` points from axis to rim.
    pub profile: Vec<[f64; 2]>,
}

/// Whole export document.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemJson {
    pub version: u32,
    pub loss: f64,
    pub wavelengths: Vec<f64>,
    pub variables: Vec<ExportVar>,
    pub surfaces: Vec<ExportSurface>,
    /// Consecutive `[p0, p1]` pairs grouped by wavelength.
    pub segments: Vec<[[f64; 3]; 2]>,
}

/// Sag height at radius `r` (flat when `R ~ 0`).
#[must_use]
pub fn sag(radius: f64, vertex: f64, r: f64) -> f64 {
    if radius.abs() < 1e-12 {
        return vertex;
    }
    vertex + radius - radius.signum() * (radius * radius - r * r).sqrt()
}

/// Profile points clamped to `0.9*|R|` (keeps `sqrt` real).
#[must_use]
pub fn profile_points(radius: f64, vertex: f64, semi: f64) -> Vec<[f64; 2]> {
    let rmax = if radius.abs() < 1e-12 {
        semi
    } else {
        semi.min(0.9 * radius.abs())
    };
    (0..PROFILE_SAMPLES)
        .map(|i| {
            let r = rmax * (i as f64) / ((PROFILE_SAMPLES - 1) as f64);
            [r, sag(radius, vertex, r)]
        })
        .collect()
}

/// Build the export document.
#[must_use]
pub fn to_document(setup: &OpticalSetup, paths: &[IntersectionResult]) -> SystemJson {
    let z = vertices(&setup.surfaces);
    let pupil = pupil_radius(&setup.source);
    let surfaces = setup
        .surfaces
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let semi = s.diameter.unwrap_or(2.0 * pupil) / 2.0;
            ExportSurface {
                name: s.name.clone(),
                vertex_z: z[i],
                radius: s.radius,
                profile: profile_points(s.radius, z[i], semi),
            }
        })
        .collect();
    let mut segments = Vec::new();
    for p in paths {
        for w in p.points.windows(2) {
            segments.push([w[0].values(), w[1].values()]);
        }
    }
    let mut vars = Vec::new();
    if let Ok(list) = variables(setup) {
        for w in list {
            vars.push(ExportVar {
                surface: setup.surfaces[w.surface].name.clone(),
                key: w.key.key().to_string(),
                value: get_var(&setup.surfaces, w),
            });
        }
    }
    SystemJson {
        version: SCHEMA_VERSION,
        loss: spot_loss(paths).v,
        wavelengths: setup.source.wavelengths.clone(),
        variables: vars,
        surfaces,
        segments,
    }
}

/// Pretty JSON document (serialization is infallible for this schema).
#[must_use]
pub fn to_json(setup: &OpticalSetup, paths: &[IntersectionResult]) -> String {
    serde_json::to_string_pretty(&to_document(setup, paths)).expect("SystemJson serializes")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::system::load_toml;
    use crate::trace::{RayEnd, trace_system};

    const SAMPLE: &str = include_str!("../assets/sample.toml");

    #[test]
    fn json_round_trips_with_version() {
        let setup = load_toml(SAMPLE).expect("sample must parse");
        let paths = trace_system(&setup.surfaces, &setup);
        let text = to_json(&setup, &paths);
        let back: SystemJson = serde_json::from_str(&text).expect("must reparse");
        assert_eq!(back.version, 1);
        assert_eq!(back.surfaces.len(), 2);
        assert_eq!(back.variables.len(), 1);
        assert!(!back.segments.is_empty());
    }

    #[test]
    fn profile_lies_on_sphere() {
        let setup = load_toml(SAMPLE).expect("sample must parse");
        let paths = trace_system(&setup.surfaces, &setup);
        let doc = to_document(&setup, &paths);
        // Surface 0: R = 50, center z = 50: r^2 + (z-50)^2 = 2500.
        for [r, z] in &doc.surfaces[0].profile {
            let res = r * r + (z - 50.0) * (z - 50.0) - 2500.0;
            assert!(res.abs() < 1e-9, "residual = {res}");
        }
    }

    #[test]
    fn segments_are_continuous_paths() {
        let setup = load_toml(SAMPLE).expect("sample must parse");
        let paths = trace_system(&setup.surfaces, &setup);
        assert!(paths.iter().all(|p| p.end == RayEnd::Image));
        // Each path contributes points-1 segments.
        let doc = to_document(&setup, &paths);
        let want: usize = paths.iter().map(|p| p.points.len() - 1).sum();
        assert_eq!(doc.segments.len(), want);
    }
}
