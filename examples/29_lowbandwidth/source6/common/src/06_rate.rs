//! `06_rate` — Token-Bucket (Sendebudget) und Ratenmesser.
//!
//! Zeit wird explizit übergeben (`Instant`), dadurch sind beide Typen mit
//! künstlicher Zeit deterministisch testbar.

use std::time::{Duration, Instant};

/// Token-Bucket: `rate` Byte/s, höchstens `burst` Byte angespart.
#[derive(Clone, Debug)]
pub struct TokenBucket {
    rate: f64,
    burst: f64,
    tokens: f64,
    last: Instant,
}

impl TokenBucket {
    #[must_use]
    pub fn new(rate: u32, burst: u32, now: Instant) -> Self {
        Self {
            rate: f64::from(rate.max(1)),
            burst: f64::from(burst.max(1)),
            tokens: f64::from(burst.max(1)),
            last: now,
        }
    }

    fn refill(&mut self, now: Instant) {
        let dt = now.saturating_duration_since(self.last).as_secs_f64();
        self.tokens = (self.tokens + dt * self.rate).min(self.burst);
        self.last = now;
    }

    /// Wartezeit bis `n` Byte gesendet werden dürfen (0 = sofort).
    pub fn wait(&mut self, n: usize, now: Instant) -> Duration {
        self.refill(now);
        // Pakete größer als der Burst dürfen den Bucket ins Minus ziehen,
        // sobald er voll ist — sonst würden sie nie gesendet.
        let need = (n as f64).min(self.burst);
        if self.tokens >= need {
            Duration::ZERO
        } else {
            Duration::from_secs_f64((need - self.tokens) / self.rate)
        }
    }

    /// Verbucht `n` gesendete Byte.
    pub fn take(&mut self, n: usize, now: Instant) {
        self.refill(now);
        self.tokens -= n as f64;
    }

    #[must_use]
    pub fn rate(&self) -> u32 {
        self.rate as u32
    }
}

/// Gleitender Mittelwert der Byte-Rate über ein Zeitfenster.
#[derive(Clone, Debug)]
pub struct RateMeter {
    window: Duration,
    samples: std::collections::VecDeque<(Instant, usize)>,
}

impl RateMeter {
    #[must_use]
    pub fn new(window: Duration) -> Self {
        Self {
            window,
            samples: Default::default(),
        }
    }

    pub fn add(&mut self, n: usize, now: Instant) {
        self.samples.push_back((now, n));
        while let Some(&(t, _)) = self.samples.front() {
            if now.saturating_duration_since(t) > self.window {
                self.samples.pop_front();
            } else {
                break;
            }
        }
    }

    /// Byte/s über das Fenster (bis `now`).
    #[must_use]
    pub fn rate(&self, now: Instant) -> f64 {
        let total: usize = self
            .samples
            .iter()
            .filter(|(t, _)| now.saturating_duration_since(*t) <= self.window)
            .map(|(_, n)| n)
            .sum();
        total as f64 / self.window.as_secs_f64()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bucket_limits_long_term_rate() {
        let t0 = Instant::now();
        let mut b = TokenBucket::new(6000, 1000, t0);
        let (mut now, mut sent) = (t0, 0usize);
        // 10 s simulieren: 500-Byte-Pakete senden, sobald erlaubt.
        while now < t0 + Duration::from_secs(10) {
            let w = b.wait(500, now);
            now += w;
            b.take(500, now);
            sent += 500;
        }
        let rate = sent as f64 / 10.0;
        assert!((5900.0..=6200.0).contains(&rate), "{rate}");
    }

    #[test]
    fn oversized_packet_waits_for_full_bucket_only() {
        let t0 = Instant::now();
        let mut b = TokenBucket::new(1000, 500, t0);
        assert_eq!(b.wait(2000, t0), Duration::ZERO);
        b.take(2000, t0); // -1500
        let w = b.wait(100, t0);
        assert!((w.as_secs_f64() - 1.6).abs() < 1e-6, "{w:?}");
    }

    #[test]
    fn meter_averages_over_window() {
        let t0 = Instant::now();
        let mut m = RateMeter::new(Duration::from_secs(2));
        for i in 0..40 {
            m.add(300, t0 + Duration::from_millis(100 * i));
        }
        let r = m.rate(t0 + Duration::from_millis(3900));
        assert!((r - 3000.0).abs() < 200.0, "{r}");
        assert_eq!(m.rate(t0 + Duration::from_secs(60)), 0.0);
    }
}
