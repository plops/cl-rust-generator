//! `11_pace` — Leaky-Bucket-Pacing für den 6-kB/s-Kanal.
//!
//! Der Server enkoidiert schneller, als der Kanal abträgt (Beobachtung:
//! ~37 kB/s in einen 6-kB/s-Tunnel → ~2 MB SSH-Queue → Bunches +
//! verhungernde Eingaben). Der `Pacer` dosiert deshalb jede Kachel auf
//! Budget-Rate: `pace(n)` liefert die Wartezeit, die `n` Bytes kosten,
//! abzüglich in Idle-Phasen angesparten Guthabens (Cap: 1 s, damit Stille
//! keinen Riesen-Burst freigibt). Texte buchen ohne zu warten
//! (`note`): Sie überholen Kacheln grundsätzlich, der nächste Tile
//! trägt ihre Schuld ab — im Mittel bleibt die Summe im Budget.

use std::time::{Duration, Instant};

/// Sendepacing nach Budget-Rate (reine Rechenfunktion + Uhr, keine IO).
#[derive(Debug)]
pub struct Pacer {
    budget_bps: f64,
    allowance: f64,
    max_allowance: f64,
    last: Instant,
}

impl Pacer {
    /// `budget_bps`: Kanalrate in Byte/s (0 wird auf 1 geklemmt —
    /// Stillstand statt Division durch null; die Config lehnt 0 ab).
    #[must_use]
    pub fn new(budget_bps: u32) -> Self {
        let budget = budget_bps.max(1) as f64;
        Self {
            budget_bps: budget,
            allowance: budget, // voll starten: Hello + Sync gehen sofort raus
            max_allowance: budget,
            last: Instant::now(),
        }
    }

    /// Wartezeit für `nbytes` (bucht ab; Anrufer schläft, dann sendet).
    pub fn pace(&mut self, nbytes: usize) -> Duration {
        self.pace_at(nbytes, Instant::now())
    }

    /// Wie [`Pacer::pace`], aber mit expliziter Uhr (deterministische Tests).
    pub fn pace_at(&mut self, nbytes: usize, now: Instant) -> Duration {
        let elapsed = now.saturating_duration_since(self.last).as_secs_f64();
        self.last = now;
        self.allowance = (self.allowance + elapsed * self.budget_bps).min(self.max_allowance);
        self.allowance -= nbytes as f64;
        if self.allowance >= 0.0 {
            Duration::ZERO
        } else {
            Duration::from_secs_f64(-self.allowance / self.budget_bps)
        }
    }

    /// Bucht `nbytes` ohne Wartezeit (Vorfahrtspur für Texte/Hello:
    /// Kacheln holen die Schuld danach auf).
    pub fn note(&mut self, nbytes: usize) {
        self.note_at(nbytes, Instant::now());
    }

    /// Wie [`Pacer::note`], mit expliziter Uhr (Tests).
    pub fn note_at(&mut self, nbytes: usize, now: Instant) {
        let _ = self.pace_at(nbytes, now);
    }
}

/// Schläft `pacer.pace(nbytes)` in ≤50-ms-Scheiben; `done()` bricht ab
/// (Reader-Thread weg = Client tot). `true` = gesendet werden darf.
pub fn wait_paced(pacer: &mut Pacer, nbytes: usize, done: &dyn Fn() -> bool) -> bool {
    let mut wait = pacer.pace(nbytes);
    while !wait.is_zero() {
        if done() {
            return false;
        }
        let step = wait.min(Duration::from_millis(50));
        std::thread::sleep(step);
        wait -= step;
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pacer_at(budget: u32, t0: Instant) -> Pacer {
        let mut p = Pacer::new(budget);
        p.last = t0;
        p.allowance = budget.max(1) as f64;
        p
    }

    #[test]
    fn first_send_goes_immediately() {
        let t0 = Instant::now();
        let mut p = pacer_at(6000, t0);
        assert_eq!(p.pace_at(100, t0), Duration::ZERO);
    }

    #[test]
    fn burst_costs_channel_time() {
        let t0 = Instant::now();
        let mut p = pacer_at(6000, t0);
        // Volles Budget auf einmal: Restguthaben 0, kein Warten ...
        assert_eq!(p.pace_at(6000, t0), Duration::ZERO);
        // ... das nächste Byte kostet 1/6000 s.
        let w = p.pace_at(1, t0);
        assert!((w.as_secs_f64() - 1.0 / 6000.0).abs() < 1e-9, "{w:?}");
        // 13-kB-Kachel (0614er-Max) kostet ~2,2 s Kanalzeit.
        let mut q = pacer_at(6000, t0);
        let w = q.pace_at(13628, t0);
        assert!((w.as_secs_f64() - (13628.0 - 6000.0) / 6000.0).abs() < 1e-6);
    }

    #[test]
    fn idle_earns_capped_credit() {
        let t0 = Instant::now();
        let mut p = pacer_at(6000, t0);
        // 10 s Stille → trotzdem nur 1 s Guthaben (kein Riesen-Burst).
        assert_eq!(
            p.pace_at(6000, t0 + Duration::from_secs(10)),
            Duration::ZERO
        );
        assert!(p.pace_at(1, t0 + Duration::from_secs(10)) > Duration::ZERO);
        // 0,5 s Stille → halbes Budget Guthaben.
        let mut q = pacer_at(6000, t0);
        assert_eq!(
            q.pace_at(3000, t0 + Duration::from_millis(500)),
            Duration::ZERO
        );
    }

    #[test]
    fn zero_bytes_wait_nothing() {
        let t0 = Instant::now();
        let mut p = pacer_at(6000, t0);
        assert_eq!(p.pace_at(0, t0), Duration::ZERO);
        assert_eq!(p.pace_at(6000, t0), Duration::ZERO); // nichts verbraucht
    }

    #[test]
    fn small_text_costs_single_digit_ms() {
        let t0 = Instant::now();
        let mut p = pacer_at(6000, t0);
        p.note_at(95, t0); // längstes 0614er-Text-Delta, Vorfahrt ohne Warten
        let w = p.pace_at(95, t0); // und selbst gebucht kostet es < 20 ms
        assert!(w < Duration::from_millis(20), "{w:?}");
    }

    #[test]
    fn noted_texts_delay_next_tile() {
        let t0 = Instant::now();
        let mut p = pacer_at(100, t0);
        p.note_at(100, t0); // Text verbraucht das ganze Budget ...
        let w = p.pace_at(100, t0); // ... Tile wartet die volle Sekunde
        assert!((w.as_secs_f64() - 1.0).abs() < 1e-6, "{w:?}");
    }

    #[test]
    fn wait_paced_aborts_without_sleeping() {
        let t0 = Instant::now();
        let mut p = pacer_at(1, t0); // 1 B/s: jedes Byte wartet ewig ...
        let t = Instant::now();
        assert!(!wait_paced(&mut p, 6000, &|| true)); // ... aber Abbruch zählt
        assert!(t.elapsed() < Duration::from_secs(1));
    }
}
