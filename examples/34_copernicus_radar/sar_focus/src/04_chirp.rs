//! Idealer Sende-Chirp (Pflicht-Replika, SSFocus-Formel).
//!
//! `s(t) = exp(2jπ(φ₁t + φ₂t²))` mit `φ₁ = TXPSF + TXPRR·TXPL/2`,
//! `φ₂ = TXPRR/2`, `t ∈ [-TXPL/2, +TXPL/2]`. Die physikalisch schönere
//! Replika aus den 520 Kalibrierpulsen ist bewusst Out-of-Scope (s. Plan).

use crate::types::Complex32;

/// Chirp-Parameter in SI-Einheiten.
#[derive(Clone, Copy, Debug)]
pub struct ChirpParams {
    pub txpsf_hz: f64,
    pub txpl_s: f64,
    pub txprr_hz_s: f64,
    pub fs_hz: f64,
}

/// Replika-Länge in Samples: `int(TXPL·fs)` (SSFocus).
pub fn num_tx_samples(p: &ChirpParams) -> usize {
    (p.txpl_s * p.fs_hz) as usize
}

/// Ideale Chirp-Replika (in `f64` gerechnet, als `f32` abgelegt).
pub fn replica(p: &ChirpParams) -> Vec<Complex32> {
    let n = num_tx_samples(p);
    let phi1 = p.txpsf_hz + p.txprr_hz_s * p.txpl_s / 2.0;
    let phi2 = p.txprr_hz_s / 2.0;
    (0..n)
        .map(|i| {
            // linspace(-TXPL/2, +TXPL/2, n) — exakt wie SSFocus.
            let t = -p.txpl_s / 2.0 + p.txpl_s * i as f64 / (n - 1) as f64;
            let phase = 2.0 * std::f64::consts::PI * (phi1 * t + phi2 * t * t);
            Complex32::new(phase.cos() as f32, phase.sin() as f32)
        })
        .collect()
}

/// Einbett-Start der Replika in der Range-Zeile der Länge `n`:
/// `ceil((n−ntx)/2)−1` (SSFocus-Indexarithmetik, dort `index_start`).
pub fn embed_start(n: usize, ntx: usize) -> usize {
    ((n.saturating_sub(ntx) as f64 / 2.0).ceil() as usize).saturating_sub(1)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s6() -> ChirpParams {
        ChirpParams {
            txpsf_hz: 21.1e6,
            txpl_s: 51.102e-6,
            txprr_hz_s: -826.4e9,
            fs_hz: 46_918_402.8,
        }
    }

    #[test]
    fn replika_laenge_und_norm() {
        let p = s6();
        // int(TXPL·fs) = int(51,102 µs · 46,918 MHz) = 2397 Samples.
        let n = num_tx_samples(&p);
        assert_eq!(n, (p.txpl_s * p.fs_hz) as usize);
        assert_eq!(n, 2397);
        let r = replica(&p);
        assert_eq!(r.len(), n);
        // Einheitsbetrag (reiner Phasen-Chirp).
        for c in &r {
            assert!((c.norm() - 1.0).abs() < 1e-6, "|c| = {}", c.norm());
        }
    }

    #[test]
    fn replika_werte_gegen_explizite_formel() {
        // Unabhängige Gegenprobe: Phase direkt aus t, φ₁, φ₂.
        let p = s6();
        let r = replica(&p);
        let n = r.len();
        let phi1 = p.txpsf_hz + p.txprr_hz_s * p.txpl_s / 2.0;
        let phi2 = p.txprr_hz_s / 2.0;
        for &i in &[0usize, 1, n / 4, n / 2, 3 * n / 4, n - 1] {
            let t = -p.txpl_s / 2.0 + p.txpl_s * i as f64 / (n - 1) as f64;
            let ph = 2.0 * std::f64::consts::PI * (phi1 * t + phi2 * t * t);
            assert!((r[i].re as f64 - ph.cos()).abs() < 1e-6, "i={i}");
            assert!((r[i].im as f64 - ph.sin()).abs() < 1e-6, "i={i}");
        }
    }

    #[test]
    fn einbettung_wie_ssfocus() {
        // N=19950, ntx=2397 → Start ceil(17553/2)−1 = 8776.
        assert_eq!(embed_start(19950, 2397), 8776);
        // Replika passt vollständig in die Zeile.
        assert!(8776 + 2397 <= 19950);
        // Winzige Zeile: Start klemmt bei 0.
        assert_eq!(embed_start(10, 100), 0);
    }
}
