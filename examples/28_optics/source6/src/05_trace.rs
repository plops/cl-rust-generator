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
use crate::system::{D_LINE, OpticalSetup, Surface, SourceCfg, bundle, index_at, pupil_radius};
use crate::ray::{PLANAR_EPS, T_EPS};

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
    /// Clear radius for the stop vignette check. `None` falls back to the
    /// entrance pupil radius (legacy behavior). Set from `Surface.diameter`.
    pub clear_r: Option<f64>,
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
    /// Field angle (degrees) this path was traced at.
    pub field_deg: f64,
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
                field_deg: 0.0,
            };
        };
        if s.stop {
            let r2 = hit.point.x * hit.point.x + hit.point.y * hit.point.y;
            let clear = s.clear_r.unwrap_or(pupil_r);
            if r2.v > clear * clear {
                points.push(hit.point);
                return IntersectionResult {
                    points,
                    end: RayEnd::Vignetted,
                    wavelength,
                    field_deg: 0.0,
                };
            }
        }
        let Some(dir) = refract(&ray.direction, &hit.normal, n1, s.material) else {
            points.push(hit.point);
            return IntersectionResult {
                points,
                end: RayEnd::Tir,
                wavelength,
                field_deg: 0.0,
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
                field_deg: 0.0,
            }
        }
        None => IntersectionResult {
            points,
            end: RayEnd::Missed,
            wavelength,
            field_deg: 0.0,
        },
    }
}

/// Result of primal pupil-aiming for one field angle.
#[derive(Debug, Clone, Copy)]
pub struct PupilAim {
    /// Chief-ray entry offset in y (patent/lens units) at the start plane.
    pub chief_ey: f64,
    /// Marginal-fan scale (<= 1) that keeps the extreme rays inside the stop.
    pub fan_scale: f64,
}

impl PupilAim {
    /// Identity aiming: on-axis, full fan.
    const IDENTITY: PupilAim = PupilAim {
        chief_ey: 0.0,
        fan_scale: 1.0,
    };
}

/// Primal (`f64`, non-differentiable) ray for the aiming pre-solve. It mirrors
/// the conventions of [`crate::ray`] exactly (sphere center `z0 + R`, normal
/// opposing the ray, TIR -> `None`) but stays in plain `f64` so it never
/// touches the autodiff chain.
#[derive(Clone, Copy)]
struct PRay {
    ox: f64,
    oy: f64,
    oz: f64,
    dx: f64,
    dy: f64,
    dz: f64,
}

/// Primal spherical/planar intersection, mirroring [`intersect_surface`].
/// Returns `(hit point, unit normal opposing the ray)`.
fn primal_intersect(r: &PRay, vertex_z: f64, radius: f64) -> Option<(f64, f64, f64, [f64; 3])> {
    if radius.abs() < PLANAR_EPS {
        if r.dz.abs() < 1e-12 {
            return None;
        }
        let t = (vertex_z - r.oz) / r.dz;
        if t < T_EPS {
            return None;
        }
        let p = [r.ox + r.dx * t, r.oy + r.dy * t, r.oz + r.dz * t];
        let mut n = [0.0, 0.0, 1.0];
        if n[0] * r.dx + n[1] * r.dy + n[2] * r.dz > 0.0 {
            n = [0.0, 0.0, -1.0];
        }
        return Some((p[0], p[1], p[2], n));
    }
    let cz = vertex_z + radius;
    let ocx = r.ox;
    let ocy = r.oy;
    let ocz = r.oz - cz;
    let a = r.dx * r.dx + r.dy * r.dy + r.dz * r.dz;
    let b = 2.0 * (ocx * r.dx + ocy * r.dy + ocz * r.dz);
    let c = ocx * ocx + ocy * ocy + ocz * ocz - radius * radius;
    let disc = b * b - 4.0 * a * c;
    if disc < 0.0 || a.abs() < 1e-18 {
        return None;
    }
    let root = disc.sqrt();
    let two_a = 2.0 * a;
    let t0 = (-b - root) / two_a;
    let t1 = (-b + root) / two_a;
    let t = if t0 > T_EPS {
        t0
    } else if t1 > T_EPS {
        t1
    } else {
        return None;
    };
    let p = [r.ox + r.dx * t, r.oy + r.dy * t, r.oz + r.dz * t];
    let mut n = [p[0] - 0.0, p[1] - 0.0, p[2] - cz];
    let len = (n[0] * n[0] + n[1] * n[1] + n[2] * n[2]).sqrt();
    if len < 1e-18 {
        return None;
    }
    n = [n[0] / len, n[1] / len, n[2] / len];
    if n[0] * r.dx + n[1] * r.dy + n[2] * r.dz > 0.0 {
        n = [-n[0], -n[1], -n[2]];
    }
    Some((p[0], p[1], p[2], n))
}

/// Primal Snell refraction, mirroring [`refract`]. `None` on TIR.
fn primal_refract(d: [f64; 3], n: [f64; 3], n1: f64, n2: f64) -> Option<[f64; 3]> {
    let mu = n1 / n2;
    let cos1 = -(n[0] * d[0] + n[1] * d[1] + n[2] * d[2]);
    let k = 1.0 - mu * mu * (1.0 - cos1 * cos1);
    if k < 0.0 {
        return None;
    }
    let f = mu * cos1 - k.sqrt();
    let out = [
        d[0] * mu + n[0] * f,
        d[1] * mu + n[1] * f,
        d[2] * mu + n[2] * f,
    ];
    let len = (out[0] * out[0] + out[1] * out[1] + out[2] * out[2]).sqrt();
    if len < 1e-18 {
        return None;
    }
    Some([out[0] / len, out[1] / len, out[2] / len])
}

/// Primal trace of one ray up to (and stopping at) surface `stop_idx`.
/// Returns the transverse `(x, y)` position at that surface, or `None` if the
/// ray is lost (miss/TIR) before reaching it.
fn primal_pos_at_stop(surfs: &[DualSurface], r0: PRay, stop_idx: usize) -> Option<(f64, f64)> {
    let mut r = r0;
    let mut n1 = 1.0;
    for (i, s) in surfs.iter().enumerate() {
        let (px, py, pz, nrm) = primal_intersect(&r, s.vertex.v, s.radius.v)?;
        if i == stop_idx {
            return Some((px, py));
        }
        let dir = primal_refract([r.dx, r.dy, r.dz], nrm, n1, s.material.v)?;
        r = PRay {
            ox: px,
            oy: py,
            oz: pz,
            dx: dir[0],
            dy: dir[1],
            dz: dir[2],
        };
        n1 = s.material.v;
    }
    None
}

/// Primal pupil-aiming for one field angle.
///
/// 1. Locate the aperture stop (first surface with `stop == true`). No stop or
///    on-axis field -> identity.
/// 2. Chief-ray aiming: secant/bisection on the entry offset `ey` so the pupil-
///    center ray lands at y ~ 0 on the stop.
/// 3. Marginal fan scaling: shrink the fan radius until the four extreme rays
///    (+/-x, +/-y at the pupil rim) reach the stop within its clear radius.
fn aim_pupil(
    surfs: &[DualSurface],
    source: &SourceCfg,
    start_z: f64,
    field_deg: f64,
    pupil_r: f64,
) -> PupilAim {
    let stop_idx = match surfs.iter().position(|s| s.stop) {
        Some(i) => i,
        None => return PupilAim::IDENTITY,
    };
    if field_deg.abs() < 1e-9 {
        return PupilAim::IDENTITY;
    }
    let alpha = field_deg.to_radians();
    let (sin_a, cos_a) = (alpha.sin(), alpha.cos());
    let chief = |ey: f64| -> Option<f64> {
        let r = PRay {
            ox: 0.0,
            oy: ey,
            oz: start_z,
            dx: 0.0,
            dy: sin_a,
            dz: cos_a,
        };
        primal_pos_at_stop(surfs, r, stop_idx).map(|(_, y)| y)
    };

    // --- Step 2: chief-ray aiming via bracketed bisection. ---
    // The chief ray must land at y ~ 0 on the stop. A naive secant can jump to
    // a spurious far-off-axis crossing, so instead sample a grid of entry
    // offsets, pick the sign-change bracket nearest ey = 0, and bisect it.
    // This selects the physically meaningful (near-paraxial) chief ray.
    let span = pupil_r.max(0.1) * 60.0;
    let n_scan = 240usize;
    // Collect (ey, y_at_stop) samples over the search span.
    let mut samples: Vec<(f64, f64)> = Vec::with_capacity(n_scan + 1);
    for k in 0..=n_scan {
        let ey = -span + 2.0 * span * (k as f64) / (n_scan as f64);
        if let Some(y) = chief(ey) {
            samples.push((ey, y));
        }
    }
    let mut chief_ey = 0.0;
    // Find the sign-change bracket whose midpoint offset is closest to 0.
    let mut best_bracket: Option<(f64, f64, f64, f64)> = None; // (a, fa, b, fb)
    let mut best_key = f64::INFINITY;
    for w in samples.windows(2) {
        let (a, fa) = w[0];
        let (b, fb) = w[1];
        if fa == 0.0 {
            // Exact hit at a.
            if a.abs() < best_key {
                best_key = a.abs();
                best_bracket = Some((a, fa, a, fa));
            }
        }
        if fa * fb < 0.0 {
            let key = ((a + b) * 0.5).abs();
            if key < best_key {
                best_key = key;
                best_bracket = Some((a, fa, b, fb));
            }
        }
    }
    if let Some((mut a, mut fa, mut b, mut fb)) = best_bracket {
        if (a - b).abs() < 1e-15 {
            chief_ey = a;
        } else {
            for _ in 0..60 {
                let m = 0.5 * (a + b);
                let fm = match chief(m) {
                    Some(y) => y,
                    None => break,
                };
                if fm.abs() < 1e-10 {
                    a = m;
                    b = m;
                    break;
                }
                if fa * fm < 0.0 {
                    b = m;
                    fb = fm;
                } else {
                    a = m;
                    fa = fm;
                }
            }
            let _ = fb;
            chief_ey = 0.5 * (a + b);
        }
    } else if let Some(&(ey, _)) = samples
        .iter()
        .min_by(|x, y| x.1.abs().partial_cmp(&y.1.abs()).unwrap())
    {
        // No sign change reachable: take the offset with smallest |y|.
        chief_ey = ey;
    }

    // --- Step 3: marginal fan scaling. ---
    // Clear radius at the stop: its own clear radius if given, else pupil.
    let stop_clear = surfs[stop_idx]
        .clear_r
        .or_else(|| source.aperture_diameter.map(|d| d / 2.0))
        .unwrap_or(pupil_r)
        .max(pupil_r);
    let extreme = |scale: f64| -> bool {
        for (ex, ey) in [
            (pupil_r * scale, 0.0),
            (-pupil_r * scale, 0.0),
            (0.0, pupil_r * scale),
            (0.0, -pupil_r * scale),
        ] {
            let r = PRay {
                ox: ex,
                oy: ey + chief_ey,
                oz: start_z,
                dx: 0.0,
                dy: sin_a,
                dz: cos_a,
            };
            match primal_pos_at_stop(surfs, r, stop_idx) {
                Some((x, y)) => {
                    if (x * x + y * y).sqrt() > stop_clear {
                        return false;
                    }
                }
                None => return false,
            }
        }
        true
    };
    let mut fan_scale = 1.0;
    for _ in 0..24 {
        if extreme(fan_scale) {
            break;
        }
        fan_scale *= 0.85;
    }

    PupilAim {
        chief_ey,
        fan_scale,
    }
}

/// Trace the source bundle through constant-seed surfaces.
///
/// For each configured field angle the input bundle is tilted in the y-z
/// plane and pupil-aimed (see [`aim_pupil`]) so oblique field bundles pass
/// through the aperture stop instead of vignetting. The aiming is a purely
/// primal (`f64`) geometric setup step: the offsets/scales it finds enter
/// the bundle as [`Dual::constant`], so the autodiff chain w.r.t. lens
/// parameters is untouched.
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
    let base = bundle(&setup.source);
    let mut out = Vec::with_capacity(base.len() * setup.source.field_angles_deg.len());
    for &field_deg in &setup.source.field_angles_deg {
        let alpha = field_deg.to_radians();
        let (sin_a, cos_a) = (alpha.sin(), alpha.cos());
        // Primal pupil-aiming for this field: chief-ray entry offset plus a
        // marginal fan scale that keeps the extreme rays inside the stop.
        let aim = aim_pupil(surfs, &setup.source, start_z.v, field_deg, pupil_r);
        for (x0, y0) in &base {
            // Tilted collimated bundle: origin shifted so the ray still
            // enters near the first vertex, plus the chief-ray aiming offset.
            let ox = *x0 * aim.fan_scale;
            let oy = *y0 * aim.fan_scale + aim.chief_ey;
            let ray = Ray {
                origin: Point3::new(
                    Dual::constant(ox),
                    Dual::constant(oy),
                    start_z,
                ),
                direction: Vec3::constant(0.0, sin_a, cos_a),
            };
            let mut res = trace_ray(surfs, &ray, image_z, pupil_r, wavelength);
            res.field_deg = field_deg;
            out.push(res);
        }
    }
    out
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
            clear_r: s.diameter.map(|d| d / 2.0),
        });
        z = z + Dual::constant(s.thickness);
    }
    (out, z)
}

/// Trace the bundle through `surfaces` at every configured wavelength.
/// `setup` provides source, vertices context and image gaps.
///
/// # Examples
///
/// ```
/// use optics::{load_toml, trace_system, RayEnd};
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
/// .unwrap();
///
/// let paths = trace_system(&setup.surfaces, &setup);
/// assert_eq!(paths.len(), 4); // one path per bundle ray, single wavelength
/// assert!(paths.iter().all(|p| p.end == RayEnd::Image));
/// ```
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
            let clear = s.clear_r.unwrap_or(pupil_r);
            if r2 > clear * clear {
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
///
/// # Examples
///
/// ```
/// use optics::{load_toml, efl};
///
/// // Symmetric biconvex, R = +/-100, d = 2, n = 1.5: EFL ~ 100.33 mm.
/// let setup = load_toml(
///     "[source]\n\
///      ray_count = 1\n\
///      grid_radius = 1.0\n\
///      [[surfaces]]\n\
///      name = \"L1\"\n\
///      radius = 100.0\n\
///      thickness = 2.0\n\
///      material = 1.5\n\
///      [[surfaces]]\n\
///      name = \"L2\"\n\
///      radius = -100.0\n\
///      thickness = 90.0\n",
/// )
/// .unwrap();
///
/// let f = efl(&setup).expect("marginal ray reaches image");
/// assert!((f - 100.33).abs() < 0.05, "EFL = {f}");
/// ```
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

    #[test]
    fn default_field_angle_is_on_axis_only() {
        // The serde default `[0.0]` must reproduce the legacy single-field
        // trace exactly: same ray count, all on-axis (field_deg == 0).
        let setup = sample();
        assert_eq!(setup.source.field_angles_deg, vec![0.0]);
        let paths = trace_system(&setup.surfaces, &setup);
        assert_eq!(paths.len(), 10);
        assert!(paths.iter().all(|p| p.field_deg == 0.0));
    }

    #[test]
    fn on_axis_bundle_is_unshifted_by_aiming() {
        // With a single on-axis field, aiming is the identity. For a
        // symmetric full-grid bundle the traced image points are symmetric
        // about the axis, so the group centroid stays on the optical axis --
        // exactly as before field angles existed.
        let setup = load_toml(
            "[source]\nray_count = 9\ngrid_radius = 5.0\n\
             [[surfaces]]\nname = \"L1\"\nradius = 50.0\nthickness = 5.0\nmaterial = 1.5168\n\
             [[surfaces]]\nname = \"L2\"\nradius = -100.0\nthickness = 40.0\n",
        )
        .expect("parse");
        let paths = trace_system(&setup.surfaces, &setup);
        let mut sx = 0.0;
        let mut sy = 0.0;
        let mut n = 0.0;
        for p in &paths {
            if let Some(img) = p.image() {
                sx += img.x.v;
                sy += img.y.v;
                n += 1.0;
            }
        }
        assert!(n > 0.0);
        // Symmetric on-axis bundle -> centroid on the optical axis.
        assert!((sx / n).abs() < 1e-9 && (sy / n).abs() < 1e-9);
    }

    #[test]
    fn pupil_aiming_recovers_off_axis_rays() {
        // A field bundle through a system with a real aperture stop would
        // vignette almost entirely without pupil-aiming. With aiming, the
        // oblique bundle is steered onto the stop and reaches the image.
        // Two surfaces + a downstream stop with a finite clear aperture.
        let cfg = "[source]\n\
             ray_count = 9\n\
             grid_radius = 1.0\n\
             field_angles_deg = [0.0, 8.0]\n\
             [[surfaces]]\n\
             name = \"Front\"\nradius = 40.0\nthickness = 6.0\nmaterial = 1.5168\n\
             [[surfaces]]\n\
             name = \"Back\"\nradius = -40.0\nthickness = 20.0\n\
             [[surfaces]]\n\
             name = \"Stop\"\nradius = 0.0\nthickness = 40.0\ndiameter = 3.0\nstop = true\n";
        let setup = load_toml(cfg).expect("parse");
        let paths = trace_system(&setup.surfaces, &setup);
        let off: Vec<_> = paths.iter().filter(|p| p.field_deg == 8.0).collect();
        let arrived = off.iter().filter(|p| p.end == RayEnd::Image).count();
        // The aimed off-axis bundle should largely reach the image.
        assert!(
            arrived >= off.len() / 2,
            "aimed off-axis rays arrived {arrived}/{}",
            off.len()
        );
    }
}
