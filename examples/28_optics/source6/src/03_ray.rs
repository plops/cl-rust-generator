//! Rays, surface intersection and Snell refraction (all [`Dual`]-valued).
//!
//! Spherical surfaces solve `||o + t*d - c||^2 = R^2` with center
//! `c = (0, 0, z0 + R)`; a radius of `0.0` denotes a planar surface and is
//! intersected as the plane `z = z0`. Normals always oppose the incident
//! ray. Refraction returns `None` on total internal reflection (TIR) instead
//! of producing NaNs.

use crate::dual::Dual;
use crate::linalg::{Point3, Vec3};

/// Threshold below which `|R|` counts as a planar surface.
pub const PLANAR_EPS: f64 = 1e-12;

/// Minimum positive hit distance (self-intersection guard).
pub const T_EPS: f64 = 1e-9;

/// Ray with [`Dual`] origin and (normalized) direction.
#[derive(Clone, Copy, Debug)]
pub struct Ray {
    pub origin: Point3,
    pub direction: Vec3,
}

/// Surface hit: distance, point and unit normal (opposing the ray).
#[derive(Clone, Copy, Debug)]
pub struct Hit {
    pub t: Dual,
    pub point: Point3,
    pub normal: Vec3,
}

/// Point along the ray at distance `t`.
#[must_use]
pub fn point_at(ray: &Ray, t: Dual) -> Point3 {
    ray.origin + ray.direction * t
}

/// Intersect the plane `z = z0`; `None` for parallel/missed rays.
pub fn intersect_plane_z(ray: &Ray, z0: Dual) -> Option<Dual> {
    if ray.direction.z.v.abs() < 1e-12 {
        return None;
    }
    let t = (z0 - ray.origin.z) / ray.direction.z;
    if t.v < T_EPS { None } else { Some(t) }
}

/// Unit plane normal opposing the incident direction.
fn plane_normal(dir: &Vec3) -> Vec3 {
    let n = Vec3::constant(0.0, 0.0, 1.0);
    if n.dot(*dir).v > 0.0 { -n } else { n }
}

/// Intersect a spherical surface (`vertex_z`, `radius`) or a plane when
/// `radius == 0`. Returns the nearest positive hit, `None` on miss.
pub fn intersect_surface(ray: &Ray, vertex_z: Dual, radius: Dual) -> Option<Hit> {
    if radius.v.abs() < PLANAR_EPS {
        let t = intersect_plane_z(ray, vertex_z)?;
        return Some(Hit {
            t,
            point: point_at(ray, t),
            normal: plane_normal(&ray.direction),
        });
    }
    let center = Point3::new(Dual::constant(0.0), Dual::constant(0.0), vertex_z + radius);
    let oc = ray.origin - center;
    let a = ray.direction.dot(ray.direction);
    let b = 2.0 * oc.dot(ray.direction);
    let c = oc.dot(oc) - radius * radius;
    let disc = b * b - 4.0 * a * c;
    if disc.v < 0.0 {
        return None;
    }
    let two_a = 2.0 * a;
    if two_a.v.abs() < 1e-18 {
        return None;
    }
    let root = disc.sqrt();
    let t0 = (-b - root) / two_a;
    let t1 = (-b + root) / two_a;
    let t = if t0.v > T_EPS {
        t0
    } else if t1.v > T_EPS {
        t1
    } else {
        return None;
    };
    let point = point_at(ray, t);
    let mut normal = (point - center).normalize();
    if normal.dot(ray.direction).v > 0.0 {
        normal = -normal;
    }
    Some(Hit { t, point, normal })
}

/// Vector-form Snell law; `None` on total internal reflection.
///
/// The result is renormalized: the inputs are unit-length in exact
/// arithmetic, but successive refractions accumulate floating-point
/// magnitude drift that would otherwise bias `direction.z`-based plane
/// intersection and the marginal-ray slope used for EFL.
pub fn refract(dir: &Vec3, normal: &Vec3, n1: Dual, n2: Dual) -> Option<Vec3> {
    let mu = n1 / n2;
    let cos1 = -normal.dot(*dir);
    let one = Dual::constant(1.0);
    let k = one - mu * mu * (one - cos1 * cos1);
    if k.v < 0.0 {
        return None;
    }
    Some((*dir * mu + *normal * (mu * cos1 - k.sqrt())).normalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn axial_ray(x: f64, y: f64) -> Ray {
        Ray {
            origin: Point3::constant(x, y, -10.0),
            direction: Vec3::constant(0.0, 0.0, 1.0),
        }
    }

    fn close(a: f64, b: f64, tol: f64) -> bool {
        (a - b).abs() < tol
    }

    #[test]
    fn axial_sphere_hit_distance() {
        // Vertex at z = 0 (center 50, R = 50): ray from z = -10 hits at t = 10.
        let ray = axial_ray(0.0, 0.0);
        let hit =
            intersect_surface(&ray, Dual::constant(0.0), Dual::constant(50.0)).expect("must hit");
        assert!(close(hit.t.v, 10.0, 1e-9));
        assert!(close(hit.point.z.v, 0.0, 1e-9));
        let n = hit.normal.values();
        assert!(close(n[0], 0.0, 1e-12) && close(n[2], -1.0, 1e-12));
    }

    #[test]
    fn off_axis_hit_and_negative_radius() {
        let ray = axial_ray(5.0, 0.0);
        let hit =
            intersect_surface(&ray, Dual::constant(0.0), Dual::constant(50.0)).expect("must hit");
        assert!(close(hit.t.v, 60.0 - 2475.0f64.sqrt(), 1e-9));
        // R = -100, vertex z = 5: on-axis hit 15 mm past z = -10.
        let ray = axial_ray(0.0, 0.0);
        let hit =
            intersect_surface(&ray, Dual::constant(5.0), Dual::constant(-100.0)).expect("must hit");
        assert!(close(hit.t.v, 15.0, 1e-9));
        assert!(close(hit.point.z.v, 5.0, 1e-9));
    }

    #[test]
    fn sphere_miss_returns_none() {
        let ray = axial_ray(60.0, 0.0);
        assert!(intersect_surface(&ray, Dual::constant(0.0), Dual::constant(50.0)).is_none());
    }

    #[test]
    fn planar_surface_hit() {
        let ray = axial_ray(5.0, -3.0);
        let hit = intersect_surface(&ray, Dual::constant(7.0), Dual::constant(0.0))
            .expect("plane must hit");
        assert!(close(hit.t.v, 17.0, 1e-9));
        assert!(close(hit.normal.values()[2], -1.0, 1e-12));
    }

    #[test]
    fn normal_incidence_refraction_is_identity() {
        let dir = Vec3::constant(0.0, 0.0, 1.0);
        let n = Vec3::constant(0.0, 0.0, -1.0);
        let out = refract(&dir, &n, Dual::constant(1.0), Dual::constant(1.5168))
            .expect("no TIR at normal incidence");
        let v = out.values();
        assert!(close(v[0], 0.0, 1e-12) && close(v[2], 1.0, 1e-12));
    }

    #[test]
    fn refracted_direction_is_unit_length() {
        // Oblique incidence at an air/glass boundary: the bent ray must
        // stay unit-length so downstream z-slope math is unbiased.
        let a = 30.0f64.to_radians();
        let dir = Vec3::constant(a.sin(), 0.0, a.cos());
        let n = Vec3::constant(0.0, 0.0, -1.0);
        let out = refract(&dir, &n, Dual::constant(1.0), Dual::constant(1.5168))
            .expect("no TIR at 30 deg");
        assert!(close(out.norm().v, 1.0, 1e-12), "norm = {}", out.norm().v);
    }

    #[test]
    fn tir_returns_none() {
        // 80 deg inside n = 1.5 glass against air: beyond critical angle.
        let a = 80.0f64.to_radians();
        let dir = Vec3::constant(a.sin(), 0.0, a.cos());
        let n = Vec3::constant(0.0, 0.0, -1.0);
        assert!(refract(&dir, &n, Dual::constant(1.5), Dual::constant(1.0)).is_none());
    }

    #[test]
    fn derivative_flows_through_hit_distance() {
        // Off-axis: on-axis hit distance is exactly R-independent
        // (every sphere passes through its vertex), so seed off-axis.
        let ray = axial_ray(5.0, 0.0);
        let hit =
            intersect_surface(&ray, Dual::constant(0.0), Dual::variable(50.0)).expect("must hit");
        assert!(hit.t.d.is_finite() && hit.t.d.abs() > 0.0);
    }
}
