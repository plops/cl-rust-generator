//! 01_clock: 174-BPM-Master-Clock aus reinem Frame-Zaehler.
//! Keine Abhaengigkeit von CPAL/fundsp — voll unit-testbar.

/// 16th-Note-Triggerfrequenz in Hz.
pub fn sixteenth_hz(bpm: f64) -> f64 {
    (bpm / 60.0) * 4.0
}

/// Dauer eines Takts (4/4) in Sekunden.
pub fn bar_secs(bpm: f64) -> f64 {
    60.0 / bpm * 4.0
}

/// Frames pro 16th-Step bei gegebener Sample-Rate.
pub fn frames_per_step(bpm: f64, sample_rate: f64) -> u64 {
    (sample_rate / sixteenth_hz(bpm)).round() as u64
}

/// Step-Index (0..15) aus globalem Frame-Zaehler.
pub fn step_index(frame: u64, bpm: f64, sample_rate: f64) -> usize {
    ((frame / frames_per_step(bpm, sample_rate).max(1)) % 16) as usize
}

/// Takt-Index aus globalem Frame-Zaehler.
pub fn bar_index(frame: u64, bpm: f64, sample_rate: f64) -> u32 {
    let frames_per_bar = (bar_secs(bpm) * sample_rate).round() as u64;
    (frame / frames_per_bar.max(1)) as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sixteenth_of_174_is_11_6_hz() {
        assert!((sixteenth_hz(174.0) - 11.6).abs() < 1e-9);
    }

    #[test]
    fn bar_of_174_is_1379_ms() {
        assert!((bar_secs(174.0) - 1.3793).abs() < 1e-3);
    }

    #[test]
    fn step_zero_at_frame_zero() {
        assert_eq!(step_index(0, 174.0, 44100.0), 0);
    }

    #[test]
    fn bar_zero_at_frame_zero() {
        assert_eq!(bar_index(0, 174.0, 44100.0), 0);
    }
}
