//! Minimal 3D vector/point types over [`Dual`].
//!
//! Hand-rolled instead of `nalgebra`: `dot`/`norm`/`normalize` plus the
//! operator set the tracer needs fit in ~60 lines and keep the dependency
//! tree lean. All components are [`Dual`], so derivatives propagate through
//! every geometric calculation untouched.

use crate::dual::Dual;
use std::ops::{Add, Div, Mul, Neg, Sub};

/// Free 3-vector (directions, normals, offsets).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Vec3 {
    pub x: Dual,
    pub y: Dual,
    pub z: Dual,
}

/// 3D point (ray origins, hit positions).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point3 {
    pub x: Dual,
    pub y: Dual,
    pub z: Dual,
}

impl Vec3 {
    /// Full constructor from [`Dual`] components.
    #[must_use]
    pub fn new(x: Dual, y: Dual, z: Dual) -> Self {
        Self { x, y, z }
    }

    /// Constant vector (zero derivatives).
    #[must_use]
    pub fn constant(x: f64, y: f64, z: f64) -> Self {
        Self {
            x: Dual::constant(x),
            y: Dual::constant(y),
            z: Dual::constant(z),
        }
    }

    /// Dot product (derivatives follow the product rule per component).
    #[must_use]
    pub fn dot(self, o: Vec3) -> Dual {
        self.x * o.x + self.y * o.y + self.z * o.z
    }

    /// Squared norm.
    #[must_use]
    pub fn norm2(self) -> Dual {
        self.dot(self)
    }

    /// Euclidean norm.
    #[must_use]
    pub fn norm(self) -> Dual {
        self.norm2().sqrt()
    }

    /// Unit vector along `self`.
    #[must_use]
    pub fn normalize(self) -> Vec3 {
        self / self.norm()
    }

    /// Primal values only (for export / assertions).
    #[must_use]
    pub fn values(self) -> [f64; 3] {
        [self.x.v, self.y.v, self.z.v]
    }
}

impl Point3 {
    /// Full constructor from [`Dual`] components.
    #[must_use]
    pub fn new(x: Dual, y: Dual, z: Dual) -> Self {
        Self { x, y, z }
    }

    /// Constant point (zero derivatives).
    #[must_use]
    pub fn constant(x: f64, y: f64, z: f64) -> Self {
        Self {
            x: Dual::constant(x),
            y: Dual::constant(y),
            z: Dual::constant(z),
        }
    }

    /// Primal values only (for export / assertions).
    #[must_use]
    pub fn values(self) -> [f64; 3] {
        [self.x.v, self.y.v, self.z.v]
    }
}

impl Add<Vec3> for Vec3 {
    type Output = Vec3;
    fn add(self, o: Vec3) -> Vec3 {
        Vec3::new(self.x + o.x, self.y + o.y, self.z + o.z)
    }
}

impl Sub<Vec3> for Vec3 {
    type Output = Vec3;
    fn sub(self, o: Vec3) -> Vec3 {
        Vec3::new(self.x - o.x, self.y - o.y, self.z - o.z)
    }
}

impl Neg for Vec3 {
    type Output = Vec3;
    fn neg(self) -> Vec3 {
        Vec3::new(-self.x, -self.y, -self.z)
    }
}

impl Mul<Dual> for Vec3 {
    type Output = Vec3;
    fn mul(self, s: Dual) -> Vec3 {
        Vec3::new(self.x * s, self.y * s, self.z * s)
    }
}

impl Mul<Vec3> for Dual {
    type Output = Vec3;
    fn mul(self, v: Vec3) -> Vec3 {
        v * self
    }
}

impl Mul<f64> for Vec3 {
    type Output = Vec3;
    fn mul(self, s: f64) -> Vec3 {
        self * Dual::constant(s)
    }
}

impl Div<Dual> for Vec3 {
    type Output = Vec3;
    fn div(self, s: Dual) -> Vec3 {
        Vec3::new(self.x / s, self.y / s, self.z / s)
    }
}

impl Add<Vec3> for Point3 {
    type Output = Point3;
    fn add(self, v: Vec3) -> Point3 {
        Point3::new(self.x + v.x, self.y + v.y, self.z + v.z)
    }
}

impl Sub<Vec3> for Point3 {
    type Output = Point3;
    fn sub(self, v: Vec3) -> Point3 {
        Point3::new(self.x - v.x, self.y - v.y, self.z - v.z)
    }
}

impl Sub<Point3> for Point3 {
    type Output = Vec3;
    fn sub(self, p: Point3) -> Vec3 {
        Vec3::new(self.x - p.x, self.y - p.y, self.z - p.z)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn close(a: [f64; 3], b: [f64; 3]) -> bool {
        a.iter().zip(b.iter()).all(|(x, y)| (x - y).abs() < 1e-12)
    }

    #[test]
    fn dot_product_value() {
        let a = Vec3::constant(1.0, 2.0, 3.0);
        let b = Vec3::constant(4.0, -5.0, 6.0);
        let d = a.dot(b);
        assert!((d.v - 12.0).abs() < 1e-12 && d.d.abs() < 1e-12);
    }

    #[test]
    fn norm_and_normalize() {
        let a = Vec3::constant(3.0, 4.0, 0.0);
        assert!((a.norm().v - 5.0).abs() < 1e-12);
        let u = a.normalize();
        assert!(close(u.values(), [0.6, 0.8, 0.0]));
        assert!((u.norm().v - 1.0).abs() < 1e-12);
    }

    #[test]
    fn derivatives_flow_through_dot() {
        // d/da_x (a . b) = b_x = 4.
        let a = Vec3::new(
            Dual::variable(1.0),
            Dual::constant(2.0),
            Dual::constant(3.0),
        );
        let b = Vec3::constant(4.0, 5.0, 6.0);
        let d = a.dot(b);
        assert!((d.v - 32.0).abs() < 1e-12 && (d.d - 4.0).abs() < 1e-12);
    }

    #[test]
    fn point_arithmetic() {
        let p = Point3::constant(1.0, 2.0, 3.0);
        let v = Vec3::constant(4.0, 5.0, 6.0);
        assert!(close((p + v).values(), [5.0, 7.0, 9.0]));
        assert!(close((p - v).values(), [-3.0, -3.0, -3.0]));
        assert!(close((p + v - p).values(), [4.0, 5.0, 6.0]));
        assert!(close((-v).values(), [-4.0, -5.0, -6.0]));
    }
}
