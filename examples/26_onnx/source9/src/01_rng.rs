//! `01_rng` — SplitMix64: kleiner, schneller, deterministischer Zufall.
//!
//! Ersetzt die `rand`-Crate (Qualität reicht für Textproben völlig).

/// Deterministischer Zufallsgenerator (SplitMix64).
#[derive(Clone, Debug)]
pub struct Rng(u64);

impl Rng {
    /// Neuer Generator mit festem Seed.
    #[must_use]
    pub fn new(seed: u64) -> Self {
        Self(seed)
    }

    /// Nächste 64-Bit-Zahl.
    pub fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }

    /// Gleichverteilt in `0..n` (`n > 0`).
    pub fn below(&mut self, n: usize) -> usize {
        debug_assert!(n > 0);
        ((u128::from(self.next_u64()) * n as u128) >> 64) as usize
    }

    /// Gleichverteilt in `lo..=hi`.
    pub fn range(&mut self, lo: usize, hi: usize) -> usize {
        lo + self.below(hi - lo + 1)
    }

    /// Zufälliges Element (Slice darf nicht leer sein).
    pub fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        &items[self.below(items.len())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_seed_same_sequence() {
        let (mut a, mut b) = (Rng::new(7), Rng::new(7));
        for _ in 0..100 {
            assert_eq!(a.next_u64(), b.next_u64());
        }
        assert_ne!(Rng::new(1).next_u64(), Rng::new(2).next_u64());
    }

    #[test]
    fn below_and_range_stay_in_bounds_and_cover() {
        let mut r = Rng::new(42);
        let mut seen = [false; 5];
        for _ in 0..1000 {
            let v = r.below(5);
            seen[v] = true;
            let w = r.range(2, 8);
            assert!((2..=8).contains(&w));
        }
        assert!(seen.iter().all(|&s| s));
    }
}
