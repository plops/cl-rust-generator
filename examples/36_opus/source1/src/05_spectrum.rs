//! STFT: Hann-Fenster, Radix-2-FFT, Log-Binning (alles `std`-only).
//!
//! Ein Hop = 960 Samples (20 ms @48 kHz) → eine Spektrum-Spalte mit
//! 256 log-Bins (50 Hz–20 kHz), Pegel in dB.

/// FFT-Laenge (muss 2er-Potenz sein).
pub const FFT_SIZE: usize = 1024;
/// Hop zwischen Spalten (20 ms @48 kHz).
pub const HOP: usize = 960;
/// Zahl der Log-Bins pro Spalte.
pub const BINS: usize = 256;
/// Untere/obere Bin-Grenze in Hz.
pub const FMIN: f32 = 50.0;
/// Siehe `FMIN`.
pub const FMAX: f32 = 20_000.0;
/// Sample-Rate (Opus-intern immer 48 kHz).
pub const RATE: f32 = 48_000.0;
/// Dynamik-Untergrenze in dB (alles darunter = Boden).
pub const DB_FLOOR: f32 = -72.0;

/// Hann-Fenster der Laenge `n`.
pub fn hann(n: usize) -> Vec<f32> {
    (0..n)
        .map(|i| {
            0.5 - 0.5
                * (2.0 * std::f32::consts::PI * i as f32 / n as f32).cos()
        })
        .collect()
}

/// In-place Radix-2-FFT (`re`/`im` gleich lang, 2er-Potenz).
///
/// Panics bei ungleicher Laenge oder Nicht-2er-Potenz (Programmierfehler).
pub fn fft(re: &mut [f32], im: &mut [f32]) {
    let n = re.len();
    assert_eq!(n, im.len(), "re/im ungleich lang");
    assert!(n.is_power_of_two(), "keine 2er-Potenz: {n}");
    // Bit-Reversal-Permutation.
    let mut j = 0;
    for i in 1..n {
        let mut bit = n >> 1;
        while j & bit != 0 {
            j &= !bit;
            bit >>= 1;
        }
        j |= bit;
        if i < j {
            re.swap(i, j);
            im.swap(i, j);
        }
    }
    // Cooley-Tukey-Schleifen.
    let mut len = 2;
    while len <= n {
        let ang = -2.0 * std::f32::consts::PI / len as f32;
        let (wr, wi) = (ang.cos(), ang.sin());
        let half = len / 2;
        for start in (0..n).step_by(len) {
            let (mut cr, mut ci) = (1.0f32, 0.0f32);
            for k in 0..half {
                let a = start + k;
                let b = a + half;
                let tr = cr * re[b] - ci * im[b];
                let ti = cr * im[b] + ci * re[b];
                re[b] = re[a] - tr;
                im[b] = im[a] - ti;
                re[a] += tr;
                im[a] += ti;
                (cr, ci) = (cr * wr - ci * wi, cr * wi + ci * wr);
            }
        }
        len <<= 1;
    }
}

/// Log-Frequenzachse: FFT-Bin-Indizes je Log-Bin (Maximum-Pooling).
#[derive(Debug, Clone)]
pub struct LogAxis {
    /// `(start, end)` FFT-Bins je Log-Bin (exklusiv).
    pub ranges: Vec<(usize, usize)>,
}

impl LogAxis {
    /// Baut die Achse fuer `fft_size` bei Rate `RATE`.
    #[must_use]
    pub fn new(fft_size: usize) -> Self {
        let max_bin = fft_size / 2;
        let edges = |b: usize| {
            let f = FMIN * (FMAX / FMIN).powf(b as f32 / BINS as f32);
            (f * fft_size as f32 / RATE).round() as usize
        };
        let mut ranges = Vec::with_capacity(BINS);
        for b in 0..BINS {
            let start = edges(b).min(max_bin);
            let end = edges(b + 1).clamp(start + 1, max_bin + 1);
            ranges.push((start, end));
        }
        Self { ranges }
    }
}

/// Eine Spalte: Fenster → FFT → Betraege → Log-Bins (dB).
pub fn column(
    samples: &[f32],
    window: &[f32],
    axis: &LogAxis,
) -> [f32; BINS] {
    debug_assert_eq!(window.len(), FFT_SIZE);
    let mut re = [0.0f32; FFT_SIZE];
    let mut im = [0.0f32; FFT_SIZE];
    let n = samples.len().min(FFT_SIZE);
    for i in 0..n {
        re[i] = samples[i] * window[i];
    }
    fft(&mut re, &mut im);
    // Referenz: Vollaussteuerungs-Sinus (Amplitude 1) = 0 dB.
    // Hann-koharente Verstaerkung 0.5, FFT-Spitze N/2 → N/4.
    let reference = FFT_SIZE as f32 / 4.0;
    let mut out = [DB_FLOOR; BINS];
    for (bin, (start, end)) in axis.ranges.iter().enumerate() {
        let mut peak = 0.0f32;
        for k in *start..*end {
            let mag = (re[k] * re[k] + im[k] * im[k]).sqrt();
            peak = peak.max(mag);
        }
        let db = 20.0 * (peak / reference + 1e-7).log10();
        out[bin] = db.max(DB_FLOOR);
    }
    out
}

/// STFT ueber alles PCM: Hop `HOP`, letzte Spalte null-aufgefuellt.
#[must_use]
pub fn stft(pcm: &[f32]) -> Vec<[f32; BINS]> {
    let window = hann(FFT_SIZE);
    let axis = LogAxis::new(FFT_SIZE);
    let hops = pcm.len().div_ceil(HOP).max(1);
    let mut cols = Vec::with_capacity(hops);
    for h in 0..hops {
        let start = h * HOP;
        let slice = if start < pcm.len() {
            &pcm[start..]
        } else {
            &[]
        };
        cols.push(column(slice, &window, &axis));
    }
    cols
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Naive DFT als unabhaengiges Orakel (nur Tests).
    fn dft(x: &[f32]) -> Vec<(f32, f32)> {
        let n = x.len() as f32;
        (0..x.len())
            .map(|k| {
                let mut re = 0.0;
                let mut im = 0.0;
                for (i, &v) in x.iter().enumerate() {
                    let a = -2.0 * std::f32::consts::PI * k as f32 * i as f32 / n;
                    re += v * a.cos();
                    im += v * a.sin();
                }
                (re, im)
            })
            .collect()
    }

    #[test]
    fn fft_matches_naive_dft() {
        // Deterministisches Pseudo-Signal (kein RNG nötig).
        let x: Vec<f32> =
            (0..64).map(|i| ((i * 37) % 11) as f32 / 11.0 - 0.5).collect();
        let mut re = x.clone();
        let mut im = vec![0.0f32; 64];
        fft(&mut re, &mut im);
        for (k, (dr, di)) in dft(&x).iter().enumerate() {
            assert!((re[k] - dr).abs() < 1e-4, "re[{k}]");
            assert!((im[k] - di).abs() < 1e-4, "im[{k}]");
        }
    }

    #[test]
    fn sine_peak_lands_in_right_bin() {
        let w = hann(FFT_SIZE);
        let axis = LogAxis::new(FFT_SIZE);
        let pcm: Vec<f32> = (0..FFT_SIZE)
            .map(|i| {
                (2.0 * std::f32::consts::PI * 440.0 * i as f32 / RATE).sin()
            })
            .collect();
        let col = column(&pcm, &w, &axis);
        let peak = col
            .iter()
            .enumerate()
            .max_by(|a, b| a.1.total_cmp(b.1))
            .unwrap()
            .0;
        // Bin-Mitte ~440 Hz: 256*log(8.8)/log(400) ≈ 93.
        assert!((80..105).contains(&peak), "peak {peak}");
        assert!(col[peak] > -12.0, "peak pegel {}", col[peak]);
        assert!(col[10] < -40.0, "rauschen {}", col[10]);
    }

    #[test]
    fn silence_hits_floor_and_parseval_holds() {
        let w = hann(FFT_SIZE);
        let axis = LogAxis::new(FFT_SIZE);
        let col = column(&vec![0.0; FFT_SIZE], &w, &axis);
        assert!(col.iter().all(|&v| v == DB_FLOOR));
        // Parseval: Fenster-Energie = Spektrum-Energie/N (±5 %).
        let x: Vec<f32> =
            (0..FFT_SIZE).map(|i| ((i * 13) % 7) as f32 / 7.0).collect();
        let time_e: f32 =
            x.iter().zip(&w).map(|(s, h)| (s * h).powi(2)).sum();
        let mut re: Vec<f32> =
            x.iter().zip(&w).map(|(s, h)| s * h).collect();
        let mut im = vec![0.0f32; FFT_SIZE];
        fft(&mut re, &mut im);
        let freq_e: f32 =
            re.iter().zip(&im).map(|(r, i)| r * r + i * i).sum::<f32>()
                / FFT_SIZE as f32;
        assert!((time_e - freq_e).abs() / time_e < 0.05);
    }

    #[test]
    fn log_axis_covers_audio_band_monotonically() {
        let axis = LogAxis::new(FFT_SIZE);
        assert_eq!(axis.ranges.len(), BINS);
        assert!(axis.ranges.windows(2).all(|w| w[0].1 <= w[1].1));
        // Oberkante = 20 kHz in FFT-Bins: 20000*1024/48000 ≈ 427.
        assert_eq!(axis.ranges[BINS - 1].1, 427);
        assert_eq!(axis.ranges[0].0, 1); // Unterkante ≈ 50 Hz.
    }
}
