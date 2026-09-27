//! Sequential ray tracing through the surface array.
//!
//! [`trace_system`] matches the prompt signature and traces the source
//! bundle independently at each configured wavelength (polychromatic spot
//! diagrams fall out naturally). Materials are [`Dual`], so `material`
//! gradients flow through Snell's law. Surfaces flagged `stop` vignette
//! rays beyond the pupil radius. [`trace_ray`] traces one arbitrary ray
//! (used by the benchmark tests and [`efl`]).

use crate::dual::Dual;
use crate::linalg::{Point3, Vec3};
use crate::ray::{Ray, intersect_plane_z, intersect_surface, point_at, refract};
use crate::system::{D_LINE, OpticalSetup, Surface, bundle, index_at, pupil_radius};

/// Surface with differentiable parameters at one wavelength.
#[derive(Debug, Clone)]
pub struct DualSurface {
    /// Radius of curvature (`0` = plane).
    pub radius: Dual,
    /// Vertex z.
    pub vertex: Dual,
    /// Index after the surface at this wavelength.
    pub material: Dual,
    /// Vignette rays beyond the pupil radius here.
    pub stop: bool,
}

/// How a ray trace ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RayEnd {
    /// Reached the image plane.
    Image,
    /// Stopped by total internal reflection.
    Tir,
    /// Missed a surface or the image plane.
    Missed,
    /// Blocked by an aperture stop.
    Vignetted,
}

/// Full segment path of one traced ray.
#[derive(Debug, Clone)]
pub struct IntersectionResult {
    /// Origin, surface hits and (on [`RayEnd::Image`]) the image point.
    pub points: Vec<Point3>,
    /// How the trace ended.
    pub end: RayEnd,
    /// Wavelength (um) this path was traced at.
    pub wavelength: f64,
}

impl IntersectionResult {
    /// Image point, if the ray arrived.
    #[must_use]
    pub fn image(&self) -> Option<&Point3> {
        if matches!(self.end, RayEnd::Image) {
            self.points.last()
        } else {
            None
        }
    }
}

/// Trace one ray through `surfs` onto the `image_z` plane.
pub fn trace_ray(
    surfs: &[DualSurface],
    ray: &Ray,
    image_z: Dual,
    pupil_r: f64,
    wavelength: f64,
) -> IntersectionResult {
    let mut ray = *ray;
    let mut points = vec![ray.origin];
    let mut n1 = Dual::constant(1.0);
    for s in surfs {
        let Some(hit) = intersect_surface(&ray, s.vertex, s.radius) else {
            return IntersectionResult {
                points,
                end: RayEnd::Missed,
                wavelength,
            };
        };
        if s.stop {
            let r2 = hit.point.x * hit.point.x + hit.point.y * hit.point.y;
            if r2.v > pupil_r * pupil_r {
                points.push(hit.point);
                return IntersectionResult {
                    points,
                    end: RayEnd::Vignetted,
                    wavelength,
                };
            }
        }
        let Some(dir) = refract(&ray.direction, &hit.normal, n1, s.material) else {
            points.push(hit.point);
            return IntersectionResult {
                points,
                end: RayEnd::Tir,
                wavelength,
            };
        };
        points.push(hit.point);
        ray = Ray {
            origin: hit.point,
            direction: dir,
        };
        n1 = s.material;
    }
    match intersect_plane_z(&ray, image_z) {
        Some(t) => {
            points.push(point_at(&ray, t));
            IntersectionResult {
                points,
                end: RayEnd::Image,
                wavelength,
            }
        }
        None => IntersectionResult {
            points,
            end: RayEnd::Missed,
            wavelength,
        },
    }
}

/// Trace the source bundle through constant-seed surfaces.
pub fn trace_dual(
    surfs: &[DualSurface],
    setup: &OpticalSetup,
    image_z: Dual,
    wavelength: f64,
) -> Vec<IntersectionResult> {
    let start_z = surfs
        .first()
        .map(|s| s.vertex - Dual::constant(10.0))
        .unwrap_or(Dual::constant(-10.0));
    let pupil_r = pupil_radius(&setup.source);
    bundle(&setup.source)
        .into_iter()
        .map(|(x0, y0)| {
            let ray = Ray {
                origin: Point3::new(Dual::constant(x0), Dual::constant(y0), start_z),
                direction: Vec3::constant(0.0, 0.0, 1.0),
            };
            trace_ray(surfs, &ray, image_z, pupil_r, wavelength)
        })
        .collect()
}

/// Layout with all-constant seeds at one wavelength; returns surfaces plus
/// the image-plane z.
pub fn layout_for(surfaces: &[Surface], lambda_um: f64) -> (Vec<DualSurface>, Dual) {
    let mut z = Dual::constant(0.0);
    let mut out = Vec::with_capacity(surfaces.len());
    for s in surfaces {
        out.push(DualSurface {
            radius: Dual::constant(s.radius),
            vertex: z,
            material: Dual::constant(index_at(s, lambda_um)),
            stop: s.stop,
        });
        z = z + Dual::constant(s.thickness);
    }
    (out, z)
}

/// Trace the bundle through `surfaces` at every configured wavelength.
/// `setup` provides source, vertices context and image gaps.
pub fn trace_system(surfaces: &[Surface], setup: &OpticalSetup) -> Vec<IntersectionResult> {
    let mut paths = Vec::new();
    for lambda in &setup.source.wavelengths {
        let (surfs, image) = layout_for(surfaces, *lambda);
        paths.extend(trace_dual(&surfs, setup, image, *lambda));
    }
    paths
}

/// Marginal-ray height for paraxial analysis (EFL, back focus).
pub const MARGINAL_H: f64 = 0.1;

/// Marginal ray (parallel input at [`MARGINAL_H`]) propagated through all
/// surfaces at the reference wavelength (nearest the d-line).
/// `None` when it vignettes, TIRs or misses.
fn marginal_ray(setup: &OpticalSetup) -> Option<Ray> {
    let mut lambda = D_LINE;
    let mut best = f64::INFINITY;
    for w in &setup.source.wavelengths {
        let d = (w - D_LINE).abs();
        if d < best {
            best = d;
            lambda = *w;
        }
    }
    let (surfs, _) = layout_for(&setup.surfaces, lambda);
    let start_z = surfs.first().map(|s| s.vertex.v - 10.0).unwrap_or(-10.0);
    let mut ray = Ray {
        origin: Point3::constant(0.0, MARGINAL_H, start_z),
        direction: Vec3::constant(0.0, 0.0, 1.0),
    };
    let pupil_r = pupil_radius(&setup.source);
    let mut n1 = Dual::constant(1.0);
    for s in &surfs {
        let hit = intersect_surface(&ray, s.vertex, s.radius)?;
        if s.stop {
            let r2 = hit.point.x.v * hit.point.x.v + hit.point.y.v * hit.point.y.v;
            if r2 > pupil_r * pupil_r {
                return None;
            }
        }
        let dir = refract(&ray.direction, &hit.normal, n1, s.material)?;
        ray = Ray {
            origin: hit.point,
            direction: dir,
        };
        n1 = s.material;
    }
    Some(ray)
}

/// Effective focal length: `EFL = h / |u'|` with marginal-ray output
/// slope `u' = dy/dz`. `None` when the marginal ray is lost.
pub fn efl(setup: &OpticalSetup) -> Option<f64> {
    let ray = marginal_ray(setup)?;
    let slope = (ray.direction.y / ray.direction.z).v.abs();
    if slope < 1e-15 {
        return None;
    }
    Some(MARGINAL_H / slope)
}

/// Back focal point: axis crossing of the exiting marginal ray
/// (negative = virtual focus of a diverging system).
pub fn back_focal_z(setup: &OpticalSetup) -> Option<f64> {
    let ray = marginal_ray(setup)?;
    let dy = ray.direction.y.v;
    if dy.abs() < 1e-15 {
        return None;
    }
    Some(ray.origin.z.v + (-ray.origin.y.v / dy) * ray.direction.z.v)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::system::load_toml;

    const SAMPLE: &str = include_str!("../assets/sample.toml");

    fn sample() -> OpticalSetup {
        load_toml(SAMPLE).expect("parse")
    }

    #[test]
    fn bundle_reaches_image_plane() {
        let setup = sample();
        let paths = trace_system(&setup.surfaces, &setup);
        assert_eq!(paths.len(), 10);
        for p in &paths {
            assert_eq!(p.end, RayEnd::Image);
            let img = p.image().expect("image point");
            assert!((img.z.v - 45.0).abs() < 1e-9);
            assert_eq!(p.points.len(), 4);
        }
    }

    #[test]
    fn stop_vignettes_outer_rays() {
        let mut setup = sample();
        setup.source.grid_radius = Some(5.0);
        setup.surfaces[0].stop = true;
        let paths = trace_system(&setup.surfaces, &setup);
        // Corner rays of the 4x4 grid over +-5 sit beyond r = 5.
        assert!(paths.iter().any(|p| p.end == RayEnd::Vignetted));
        assert!(paths.iter().any(|p| p.end == RayEnd::Image));
    }

    #[test]
    fn polychromatic_trace_tags_wavelengths() {
        let mut setup = sample();
        setup.source.wavelengths = vec![0.4861, 0.5876, 0.6563];
        let paths = trace_system(&setup.surfaces, &setup);
        assert_eq!(paths.len(), 30);
        assert!((paths[0].wavelength - 0.4861).abs() < 1e-12);
        assert!((paths[29].wavelength - 0.6563).abs() < 1e-12);
    }

    #[test]
    fn efl_matches_gullstrand() {
        // Symmetric biconvex, R = +-100, d = 2, n = 1.5:
        // Gullstrand gives f = 100.33 (thin approx 100.0).
        let setup = load_toml(
            "[source]\nray_count = 1\ngrid_radius = 1.0\n\
             [[surfaces]]\nname = \"L1\"\nradius = 100.0\nthickness = 2.0\nmaterial = 1.5\n\
             [[surfaces]]\nname = \"L2\"\nradius = -100.0\nthickness = 90.0\n",
        )
        .expect("must parse");
        let f = efl(&setup).expect("EFL computable");
        assert!((f - 100.33).abs() < 0.05, "EFL = {f}, want ~100.33");
    }
}
