//! Forward-mode automatic differentiation via dual numbers.
//!
//! A dual number `v + eps*d` (with `eps^2 = 0`) pairs each value `v` with the
//! exact partial derivative `d` of that value with respect to exactly one
//! seeded input. Mark the parameter under study with [`Dual::variable`]
//! (seed `d = 1`) and every other input with [`Dual::constant`] (`d = 0`);
//! each operator then applies its calculus rule (`+`: sum, `*`: product,
//! `/`: quotient, ...), so the derivative ripples from a lens parameter
//! (radius, thickness, material) through intersection and refraction math
//! down to the image-plane error vector. The spot-loss gradient is read off
//! the `.d` field directly - no finite differences. One seed per variable:
//! re-trace once per optimized parameter (see the `optimize` module).

use std::ops::{Add, Div, Mul, Neg, Sub};

/// Dual number: value `v` plus derivative `d` w.r.t. the seeded input.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Dual {
    /// Primal value.
    pub v: f64,
    /// Exact partial derivative w.r.t. the seeded variable.
    pub d: f64,
}

/// Implement one binary op for all (`Dual`/`&Dual`/`f64`) operand combos.
/// `$v`/`$d` are the value/derivative expressions over `(a, da, b, db)`.
macro_rules! dual_binop {
    ($trait:ident::$meth:ident($va:ident,$da:ident,$vb:ident,$db:ident) => $v:expr,$d:expr) => {
        impl $trait<Dual> for Dual {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: Dual) -> Dual {
                let ($va, $da) = (self.v, self.d);
                let ($vb, $db) = (rhs.v, rhs.d);
                Dual { v: $v, d: $d }
            }
        }
        impl $trait<&Dual> for Dual {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: &Dual) -> Dual {
                self.$meth(*rhs)
            }
        }
        impl $trait<Dual> for &Dual {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: Dual) -> Dual {
                (*self).$meth(rhs)
            }
        }
        impl $trait<&Dual> for &Dual {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: &Dual) -> Dual {
                (*self).$meth(*rhs)
            }
        }
        impl $trait<f64> for Dual {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: f64) -> Dual {
                self.$meth(Dual::constant(rhs))
            }
        }
        impl $trait<f64> for &Dual {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: f64) -> Dual {
                (*self).$meth(rhs)
            }
        }
        impl $trait<Dual> for f64 {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: Dual) -> Dual {
                Dual::constant(self).$meth(rhs)
            }
        }
        impl $trait<&Dual> for f64 {
            type Output = Dual;
            #[inline]
            fn $meth(self, rhs: &Dual) -> Dual {
                Dual::constant(self).$meth(*rhs)
            }
        }
    };
}

dual_binop!(Add::add(a,da,b,db) => a + b, da + db);
dual_binop!(Sub::sub(a,da,b,db) => a - b, da - db);
dual_binop!(Mul::mul(a,da,b,db) => a * b, da * b + a * db);
dual_binop!(Div::div(a,da,b,db) => a / b, (da * b - a * db) / (b * b));

impl Neg for Dual {
    type Output = Dual;
    #[inline]
    fn neg(self) -> Dual {
        Dual {
            v: -self.v,
            d: -self.d,
        }
    }
}

impl Neg for &Dual {
    type Output = Dual;
    #[inline]
    fn neg(self) -> Dual {
        -*self
    }
}

impl Dual {
    /// Full constructor (value + explicit derivative seed).
    #[must_use]
    pub fn new(v: f64, d: f64) -> Self {
        Self { v, d }
    }

    /// Constant input: carries no derivative.
    #[must_use]
    pub fn constant(v: f64) -> Self {
        Self { v, d: 0.0 }
    }

    /// Seeded optimization variable: `d(input)/d(input) = 1`.
    #[must_use]
    pub fn variable(v: f64) -> Self {
        Self { v, d: 1.0 }
    }

    /// Square root; derivative guarded to 0 at `v == 0`.
    #[must_use]
    pub fn sqrt(self) -> Self {
        let s = self.v.sqrt();
        Self {
            v: s,
            d: if s == 0.0 { 0.0 } else { self.d / (2.0 * s) },
        }
    }

    /// Sine with chain rule.
    #[must_use]
    pub fn sin(self) -> Self {
        Self {
            v: self.v.sin(),
            d: self.d * self.v.cos(),
        }
    }

    /// Cosine with chain rule.
    #[must_use]
    pub fn cos(self) -> Self {
        Self {
            v: self.v.cos(),
            d: -self.d * self.v.sin(),
        }
    }

    /// Exponential with chain rule.
    #[must_use]
    pub fn exp(self) -> Self {
        let e = self.v.exp();
        Self {
            v: e,
            d: self.d * e,
        }
    }

    /// Power with chain rule; derivative guarded to 0 at `v == 0`.
    #[must_use]
    pub fn powf(self, n: f64) -> Self {
        Self {
            v: self.v.powf(n),
            d: if self.v == 0.0 {
                0.0
            } else {
                self.d * n * self.v.powf(n - 1.0)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_dual(got: Dual, v: f64, d: f64) {
        assert!(
            (got.v - v).abs() < 1e-12 && (got.d - d).abs() < 1e-12,
            "got {got:?}, want v={v} d={d}"
        );
    }

    #[test]
    fn constructors_seed_correctly() {
        assert_dual(Dual::new(3.0, 2.0), 3.0, 2.0);
        assert_dual(Dual::constant(3.0), 3.0, 0.0);
        assert_dual(Dual::variable(3.0), 3.0, 1.0);
    }

    #[test]
    #[allow(clippy::op_ref)] // ref-operand impls are exercised on purpose
    fn add_sub_sum_rule_all_combos() {
        let a = Dual::new(3.0, 2.0);
        let b = Dual::new(1.0, 4.0);
        assert_dual(a + b, 4.0, 6.0);
        assert_dual(a - b, 2.0, -2.0);
        assert_dual(&a + &b, 4.0, 6.0);
        assert_dual(a + &b, 4.0, 6.0);
        assert_dual(&a + b, 4.0, 6.0);
        assert_dual(a + 10.0, 13.0, 2.0);
        assert_dual(10.0 + a, 13.0, 2.0);
        assert_dual(&a + 10.0, 13.0, 2.0);
        assert_dual(10.0 + &a, 13.0, 2.0);
    }

    #[test]
    #[allow(clippy::op_ref)] // ref-operand impls are exercised on purpose
    fn mul_product_rule() {
        let a = Dual::new(3.0, 2.0);
        let b = Dual::new(4.0, 5.0);
        assert_dual(a * b, 12.0, 23.0);
        assert_dual(&a * &b, 12.0, 23.0);
        assert_dual(a * 2.0, 6.0, 4.0);
        assert_dual(2.0 * a, 6.0, 4.0);
    }

    #[test]
    #[allow(clippy::op_ref)] // ref-operand impls are exercised on purpose
    fn div_quotient_rule() {
        let a = Dual::new(3.0, 2.0);
        let b = Dual::new(4.0, 5.0);
        assert_dual(a / b, 0.75, -0.4375);
        assert_dual(&a / &b, 0.75, -0.4375);
        assert_dual(a / 2.0, 1.5, 1.0);
        assert_dual(12.0 / a, 4.0, -24.0 / 9.0);
    }

    #[test]
    fn neg_both_forms() {
        let a = Dual::new(3.0, 2.0);
        assert_dual(-a, -3.0, -2.0);
        assert_dual(-&a, -3.0, -2.0);
    }

    #[test]
    fn elementary_functions_chain_rule() {
        assert_dual(Dual::variable(4.0).sqrt(), 2.0, 0.25);
        assert_dual(Dual::new(0.0, 5.0).sqrt(), 0.0, 0.0);
        assert_dual(Dual::variable(0.0).sin(), 0.0, 1.0);
        assert_dual(Dual::variable(3.0).powf(2.0), 9.0, 6.0);
        assert_dual(Dual::variable(0.0).exp(), 1.0, 1.0);
        let c = Dual::variable(0.0).cos();
        assert!((c.v - 1.0).abs() < 1e-12 && c.d.abs() < 1e-12);
    }

    #[test]
    fn chained_expression_matches_hand_derivative() {
        // f(x) = (x^2 + 1)^2 at x = 3: f = 100, f' = 4x^3 + 4x = 120.
        let x = Dual::variable(3.0);
        let y = x * x + 1.0;
        assert_dual(y * y, 100.0, 120.0);
    }
}
