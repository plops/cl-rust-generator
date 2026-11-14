//! 00_perf — optionale Performance-Instrumentierung (`--features perf-metrics`).
//!
//! Genau eine Zustaendigkeit: phasenweise Laufzeit- und Counter-Messung fuer
//! Review-Cycles. Im Standard-Build (`default = []`) sind alle Zaehler und
//! Timer `#[inline(always)]`-No-Ops: null Overhead im Release. Mit
//! `--features perf-metrics` werden echte Atomics + Nanosekunden-Akkus
//! verdrahtet; `perf_report()` liefert den Phasen-Report fuer `--perf-report`.

#[cfg(feature = "perf-metrics")]
use std::sync::atomic::{AtomicU64, Ordering};

/// Aggregierte Zaehler + Phasen-Zeiten (Snapshot nach `..._and_reset`).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PerfStats {
    /// `RepoIndex::load`-Wandzeit (ns).
    pub index_load_ns: u64,
    /// `is_masked`-Aufrufe (Index-Laden).
    pub index_mask_checks: u64,
    /// `Dep::parse` in `is_masked` (Index-Laden).
    pub index_dep_parses: u64,
    /// `effective_defaults`-Aufrufe.
    pub effective_use_calls: u64,
    /// `effective_defaults`-Zeit (ns).
    pub effective_use_ns: u64,
    /// `package_use_for`-Aufrufe.
    pub package_use_match_calls: u64,
    /// `resolve_with_options`-Aufrufe (inkl. Kaskaden-Runden).
    pub resolve_calls: u64,
    /// `resolve_with_options`-Zeit (ns).
    pub resolve_ns: u64,
    /// `resolve_build_extra`-Laeufe.
    pub build_extra_calls: u64,
    /// `resolve_build_extra`-Zeit (ns).
    pub build_extra_ns: u64,
    /// `resolve_with_cascade_opts`-Aufrufe.
    pub cascade_calls: u64,
    /// Kaskaden-Fixpunkt-Runden.
    pub cascade_rounds: u64,
    /// Kaskaden-Zeit (ns).
    pub cascade_ns: u64,
    /// Delta-Simulationen (`flag_delta`/`flag_delta_with_base`).
    pub delta_runs: u64,
    /// Exakte No-Op-Skips (Delta ohne Simulation, Phase 3).
    pub delta_noop_skips: u64,
    /// Delta-Simulations-Zeit (ns).
    pub delta_ns: u64,
    /// `big_levers`-Laeufe.
    pub big_levers_runs: u64,
    /// Exakte No-Op-Skips (Big Levers ohne Simulation, Phase 3).
    pub big_levers_noop_skips: u64,
    /// `big_levers`-Zeit (ns).
    pub big_levers_ns: u64,
    /// Delta-Cache-Treffer (TUI).
    pub delta_cache_hits: u64,
    /// Delta-Cache-Fehlschlaege (TUI).
    pub delta_cache_misses: u64,
}

#[cfg(feature = "perf-metrics")]
static INDEX_LOAD_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static INDEX_MASK_CHECKS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static INDEX_DEP_PARSES: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static EFFECTIVE_USE_CALLS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static EFFECTIVE_USE_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static PACKAGE_USE_MATCH_CALLS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static RESOLVE_CALLS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static RESOLVE_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static BUILD_EXTRA_CALLS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static BUILD_EXTRA_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static CASCADE_CALLS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static CASCADE_ROUNDS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static CASCADE_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static DELTA_RUNS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static DELTA_NOOP_SKIPS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static DELTA_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static BIG_LEVERS_RUNS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static BIG_LEVERS_NOOP_SKIPS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static BIG_LEVERS_NS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static DELTA_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
#[cfg(feature = "perf-metrics")]
static DELTA_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);

/// Phasen-Timer: misst bis `Drop` und addiert auf den Phasen-Slot.
///
/// Ohne Feature ein leeres Inline-No-Op (null Overhead).
pub struct ScopeTimer {
    #[cfg(feature = "perf-metrics")]
    start: std::time::Instant,
    #[cfg(feature = "perf-metrics")]
    slot: &'static AtomicU64,
}

impl ScopeTimer {
    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn index_load() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &INDEX_LOAD_NS,
        }
    }

    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn effective_use() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &EFFECTIVE_USE_NS,
        }
    }

    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn resolve() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &RESOLVE_NS,
        }
    }

    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn build_extra() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &BUILD_EXTRA_NS,
        }
    }

    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn cascade() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &CASCADE_NS,
        }
    }

    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn delta() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &DELTA_NS,
        }
    }

    /// Timer-Konstruktoren je Phase (Aufrufstelle bleibt in beiden
    /// Konfigurationen identisch).
    #[inline(always)]
    pub fn big_levers() -> Self {
        Self {
            #[cfg(feature = "perf-metrics")]
            start: std::time::Instant::now(),
            #[cfg(feature = "perf-metrics")]
            slot: &BIG_LEVERS_NS,
        }
    }
}

impl Drop for ScopeTimer {
    #[inline(always)]
    fn drop(&mut self) {
        #[cfg(feature = "perf-metrics")]
        {
            let ns = self.start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64;
            self.slot.fetch_add(ns, Ordering::Relaxed);
        }
    }
}

/// Zaehler erhoehen (`resolve_with_options`-Lauf).
#[inline(always)]
pub fn perf_count_resolve() {
    #[cfg(feature = "perf-metrics")]
    RESOLVE_CALLS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (Build-Extra-Lauf).
#[inline(always)]
pub fn perf_count_build_extra() {
    #[cfg(feature = "perf-metrics")]
    BUILD_EXTRA_CALLS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (`resolve_with_cascade_opts`-Aufruf).
#[inline(always)]
pub fn perf_count_cascade() {
    #[cfg(feature = "perf-metrics")]
    CASCADE_CALLS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (eine Kaskaden-Fixpunkt-Runde).
#[inline(always)]
pub fn perf_count_cascade_round() {
    #[cfg(feature = "perf-metrics")]
    CASCADE_ROUNDS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (Delta-Simulation).
#[inline(always)]
pub fn perf_count_delta() {
    #[cfg(feature = "perf-metrics")]
    DELTA_RUNS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (exakter Delta-No-Op-Skip, Phase 3).
#[inline(always)]
pub fn perf_count_delta_noop_skip() {
    #[cfg(feature = "perf-metrics")]
    DELTA_NOOP_SKIPS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (`big_levers`-Lauf).
#[inline(always)]
pub fn perf_count_big_levers() {
    #[cfg(feature = "perf-metrics")]
    BIG_LEVERS_RUNS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (exakter Big-Levers-No-Op-Skip, Phase 3).
#[inline(always)]
pub fn perf_count_big_levers_noop_skip() {
    #[cfg(feature = "perf-metrics")]
    BIG_LEVERS_NOOP_SKIPS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (`effective_defaults`-Aufruf).
#[inline(always)]
pub fn perf_count_effective_use() {
    #[cfg(feature = "perf-metrics")]
    EFFECTIVE_USE_CALLS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (`package_use_for`-Aufruf).
#[inline(always)]
pub fn perf_count_package_use_match() {
    #[cfg(feature = "perf-metrics")]
    PACKAGE_USE_MATCH_CALLS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (`is_masked`-Aufruf beim Index-Laden).
#[inline(always)]
pub fn perf_count_mask_check() {
    #[cfg(feature = "perf-metrics")]
    INDEX_MASK_CHECKS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (`Dep::parse` in `is_masked`).
#[inline(always)]
pub fn perf_count_dep_parse() {
    #[cfg(feature = "perf-metrics")]
    INDEX_DEP_PARSES.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (Delta-Cache-Treffer, TUI).
#[inline(always)]
pub fn perf_count_cache_hit() {
    #[cfg(feature = "perf-metrics")]
    DELTA_CACHE_HITS.fetch_add(1, Ordering::Relaxed);
}

/// Zaehler erhoehen (Delta-Cache-Fehlschlag, TUI).
#[inline(always)]
pub fn perf_count_cache_miss() {
    #[cfg(feature = "perf-metrics")]
    DELTA_CACHE_MISSES.fetch_add(1, Ordering::Relaxed);
}

/// Einen Zaehler lesen (nur in Snapshot-Funktionen verwenden).
#[cfg(feature = "perf-metrics")]
#[inline(always)]
fn read(slot: &'static AtomicU64) -> u64 {
    slot.load(Ordering::Relaxed)
}

/// Alle Zaehler auf Null (nur in Snapshot-Funktionen verwenden).
#[cfg(feature = "perf-metrics")]
fn reset_all() {
    INDEX_LOAD_NS.store(0, Ordering::Relaxed);
    INDEX_MASK_CHECKS.store(0, Ordering::Relaxed);
    INDEX_DEP_PARSES.store(0, Ordering::Relaxed);
    EFFECTIVE_USE_CALLS.store(0, Ordering::Relaxed);
    EFFECTIVE_USE_NS.store(0, Ordering::Relaxed);
    PACKAGE_USE_MATCH_CALLS.store(0, Ordering::Relaxed);
    RESOLVE_CALLS.store(0, Ordering::Relaxed);
    RESOLVE_NS.store(0, Ordering::Relaxed);
    BUILD_EXTRA_CALLS.store(0, Ordering::Relaxed);
    BUILD_EXTRA_NS.store(0, Ordering::Relaxed);
    CASCADE_CALLS.store(0, Ordering::Relaxed);
    CASCADE_ROUNDS.store(0, Ordering::Relaxed);
    CASCADE_NS.store(0, Ordering::Relaxed);
    DELTA_RUNS.store(0, Ordering::Relaxed);
    DELTA_NOOP_SKIPS.store(0, Ordering::Relaxed);
    DELTA_NS.store(0, Ordering::Relaxed);
    BIG_LEVERS_RUNS.store(0, Ordering::Relaxed);
    BIG_LEVERS_NOOP_SKIPS.store(0, Ordering::Relaxed);
    BIG_LEVERS_NS.store(0, Ordering::Relaxed);
    DELTA_CACHE_HITS.store(0, Ordering::Relaxed);
    DELTA_CACHE_MISSES.store(0, Ordering::Relaxed);
}

/// Reiner Snapshot (ohne Reset).
pub fn perf_snapshot() -> PerfStats {
    #[cfg(feature = "perf-metrics")]
    {
        PerfStats {
            index_load_ns: read(&INDEX_LOAD_NS),
            index_mask_checks: read(&INDEX_MASK_CHECKS),
            index_dep_parses: read(&INDEX_DEP_PARSES),
            effective_use_calls: read(&EFFECTIVE_USE_CALLS),
            effective_use_ns: read(&EFFECTIVE_USE_NS),
            package_use_match_calls: read(&PACKAGE_USE_MATCH_CALLS),
            resolve_calls: read(&RESOLVE_CALLS),
            resolve_ns: read(&RESOLVE_NS),
            build_extra_calls: read(&BUILD_EXTRA_CALLS),
            build_extra_ns: read(&BUILD_EXTRA_NS),
            cascade_calls: read(&CASCADE_CALLS),
            cascade_rounds: read(&CASCADE_ROUNDS),
            cascade_ns: read(&CASCADE_NS),
            delta_runs: read(&DELTA_RUNS),
            delta_noop_skips: read(&DELTA_NOOP_SKIPS),
            delta_ns: read(&DELTA_NS),
            big_levers_runs: read(&BIG_LEVERS_RUNS),
            big_levers_noop_skips: read(&BIG_LEVERS_NOOP_SKIPS),
            big_levers_ns: read(&BIG_LEVERS_NS),
            delta_cache_hits: read(&DELTA_CACHE_HITS),
            delta_cache_misses: read(&DELTA_CACHE_MISSES),
        }
    }
    #[cfg(not(feature = "perf-metrics"))]
    {
        PerfStats::default()
    }
}

/// Snapshot + Reset (fuer `tests/perf_system.rs`).
pub fn perf_snapshot_and_reset() -> PerfStats {
    let s = perf_snapshot();
    #[cfg(feature = "perf-metrics")]
    reset_all();
    s
}

/// Ob das `perf-metrics`-Feature einkompiliert ist.
#[inline(always)]
pub fn perf_enabled() -> bool {
    cfg!(feature = "perf-metrics")
}

/// Phasen-Report aus einem Snapshot formatieren (ohne Reset).
pub fn perf_format(s: &PerfStats) -> String {
    #[cfg(feature = "perf-metrics")]
    {
        let ms = |ns: u64| format!("{:.1}", ns as f64 / 1_000_000.0);
        format!(
            "perf-report (Phasen: ms / Anzahl):\n\
             index_load      {:>10} ms\n\
             effective_use   {:>10} ms / {} calls\n\
             package_use     match_calls={}\n\
             resolve         {:>10} ms / {} calls\n\
             build_extra     {:>10} ms / {} calls\n\
             cascade         {:>10} ms / {} calls / {} rounds\n\
             delta           {:>10} ms / {} runs / {} noop_skips\n\
             big_levers      {:>10} ms / {} runs / {} noop_skips\n\
             index           mask_checks={} dep_parses={}\n\
             delta_cache     hits={} misses={}",
            ms(s.index_load_ns),
            ms(s.effective_use_ns),
            s.effective_use_calls,
            s.package_use_match_calls,
            ms(s.resolve_ns),
            s.resolve_calls,
            ms(s.build_extra_ns),
            s.build_extra_calls,
            ms(s.cascade_ns),
            s.cascade_calls,
            s.cascade_rounds,
            ms(s.delta_ns),
            s.delta_runs,
            s.delta_noop_skips,
            ms(s.big_levers_ns),
            s.big_levers_runs,
            s.big_levers_noop_skips,
            s.index_mask_checks,
            s.index_dep_parses,
            s.delta_cache_hits,
            s.delta_cache_misses,
        )
    }
    #[cfg(not(feature = "perf-metrics"))]
    {
        let _ = s;
        "Hinweis: Perf-Metriken brauchen `--features perf-metrics` (Standard-Build misst nichts)."
            .to_string()
    }
}

/// Phasen-Report als Text (fuer `--perf-report`): Snapshot + Reset.
pub fn perf_report() -> String {
    let s = perf_snapshot_and_reset();
    perf_format(&s)
}
