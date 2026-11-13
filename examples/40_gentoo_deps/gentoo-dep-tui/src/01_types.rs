//! 01_types — Basis-IDs, Groessen, USE-Zustaende, Fehler.
//!
//! Genau eine Zustaendigkeit: alle Kerndatentypen der Engine.
//! Integer-IDs (`PkgId`, `FlagId`) statt Strings machen den Graphen
//! evaluations-schnell (wenige Mikrosekunden pro Reachability-Lauf).

use std::collections::HashMap;
use std::fmt;

use thiserror::Error;

/// Kompakte Paket-ID (Index in Engine-Vektoren).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PkgId(pub u32);

/// Kompakte USE-Flag-ID (Index in Engine-Vektoren).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FlagId(pub u32);

/// Groessenmetriken eines Pakets bzw. Deltas (Bytes, vorzeichenbehaftet bei Deltas).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SizeMetrics {
    /// Komprimierte Binpkg-Groesse (Binhost-`SIZE`, Runtime-Proxy).
    pub binpkg_bytes: i64,
    /// Herunterzuladende Quell-Daten (`Manifest`-DIST-Summe).
    pub download_bytes: i64,
}

impl SizeMetrics {
    /// Nullmetrik.
    pub fn zero() -> Self {
        Self::default()
    }

    /// Komponentenweise Addition (saturierend).
    pub fn checked_add(self, other: Self) -> Self {
        Self {
            binpkg_bytes: self.binpkg_bytes.saturating_add(other.binpkg_bytes),
            download_bytes: self.download_bytes.saturating_add(other.download_bytes),
        }
    }
}

/// Effektiver USE-Flag-Zustand eines Pakets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UseState {
    /// Flag ist an (`+flag`).
    On,
    /// Flag ist aus (`-flag`).
    Off,
    /// Nicht explizit gesetzt (faellt auf IUSE-Default/Profil zurueck).
    Unset,
}

/// Eindeutiger Paketschluessel `kategorie/paket`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PkgKey(pub String);

impl PkgKey {
    /// Neuer Schluessel aus Kategorie und Paketname.
    pub fn new(category: &str, package: &str) -> Self {
        Self(format!("{category}/{package}"))
    }
}

impl fmt::Display for PkgKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Abhaengigkeitsklasse: Runtime vs. Buildtime (strikte Trennung!).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepClass {
    /// `RDEPEND` (+ `PDEPEND`): landet im finalen Container.
    Runtime,
    /// `BDEPEND`/`DEPEND`: nur zur Bauzeit noetig (Multi-Stage spart sie).
    Build,
}

/// Uebersprungene Abhaengigkeit (Warnung statt Abbruch, z. B. alter Slot-Pin).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SkippedDep {
    /// Das problematische Atom.
    pub atom: String,
    /// Wer es verlangt (`world` oder Paket).
    pub required_by: String,
    /// Grund (keine Version, kein Slot, ...).
    pub reason: String,
}

/// Ein Eintrag einer Blame-Kette: wer zieht wen ueber welches Flag rein.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlameLink {
    /// Das benoetigte (transitive) Paket.
    pub package: String,
    /// Das Paket, das es direkt referenziert.
    pub required_by: String,
    /// Ausloesendes USE-Flag auf `required_by` (`None` = unbedingt).
    pub via_flag: Option<String>,
}

/// Ein erkannter Widerspruch (z. B. `+foo` vs. `-foo`) mit Aufloesungen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConflictInfo {
    /// Betroffenes Paket (`kategorie/paket`).
    pub package: String,
    /// Menschenlesbare Ursachenbeschreibung.
    pub message: String,
    /// Verursacher-Kette (Paketnamen).
    pub caused_by: Vec<String>,
    /// Gueltige Aufloesungs-Kandidaten (Quick-Switch).
    pub options: Vec<QuickSwitch>,
}

/// Eine umschaltbare Aufloesung eines Konflikts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QuickSwitch {
    /// Anzeigename, z. B. `"libX[+foo] erzwingen"`.
    pub label: String,
    /// Zu setzende Flags `(paket, flag, an?)`.
    pub changes: Vec<(String, String, bool)>,
}

/// Eine `|| ( a b )`-Entscheidung im Graphen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlternativeChoice {
    /// Eindeutige Gruppen-ID (Paket + Zaehler).
    pub group_id: String,
    /// Paket, dessen `RDEPEND` die Gruppe enthaelt.
    pub owner: String,
    /// Kandidaten-Atome in Reihenfolge.
    pub candidates: Vec<String>,
    /// Aktuell gewaehlter Index.
    pub selected: usize,
}

/// Marginal-Delta eines USE-Flag-Toggles.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FlagDelta {
    /// Paket, zu dem das Flag gehoert.
    pub package: String,
    /// Flag-Name.
    pub flag: String,
    /// Aktueller Zustand vor dem Toggle.
    pub current: bool,
    /// Groessenaenderung beim Umschalten (Binpkg + Download).
    pub delta: SizeMetrics,
    /// Kaskaden-Hinweis (erzwungene weitere Flags).
    pub cascade_note: Option<String>,
    /// Gesetzter Konflikt durch diesen Toggle.
    pub conflict: Option<ConflictInfo>,
    /// Neu hinzukommende Pakete (`cat/pkg`, sortiert).
    pub added_packages: Vec<String>,
    /// Entfallende Pakete (`cat/pkg`, sortiert).
    pub removed_packages: Vec<String>,
    /// Ob Binhost-Groessen verfuegbar waren (`false` → `bin: n/a`).
    pub has_binhost_data: bool,
}

/// Instrumentierungs-Zaehler fuer Systemtests (T01/T02).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PerfStats {
    /// Anzahl `resolve()`-Aufrufe (inkl. Kaskaden-Runden).
    pub resolve_calls: u64,
    /// Anzahl `resolve_build_extra()`-Laeufe.
    pub build_extra_calls: u64,
    /// Anzahl Kaskaden-Fixpunkt-Durchlaeufe.
    pub cascade_rounds: u64,
    /// Anzahl Delta-Simulationen (`flag_delta`/`big_levers`).
    pub delta_runs: u64,
}

static PERF_RESOLVE: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static PERF_BUILD_EXTRA: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static PERF_CASCADE: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static PERF_DELTA: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Zaehler erhoehen (no-op Kosten, `Relaxed`-Ordering reicht).
pub fn perf_count_resolve() {
    PERF_RESOLVE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Zaehler erhoehen (Build-Extra-Lauf).
pub fn perf_count_build_extra() {
    PERF_BUILD_EXTRA.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Zaehler erhoehen (Kaskaden-Durchlauf).
pub fn perf_count_cascade() {
    PERF_CASCADE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Zaehler erhoehen (Delta-Simulation).
pub fn perf_count_delta() {
    PERF_DELTA.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Snapshot + Reset (fuer `tests/perf_system.rs`).
pub fn perf_snapshot_and_reset() -> PerfStats {
    PerfStats {
        resolve_calls: PERF_RESOLVE.swap(0, std::sync::atomic::Ordering::Relaxed),
        build_extra_calls: PERF_BUILD_EXTRA.swap(0, std::sync::atomic::Ordering::Relaxed),
        cascade_rounds: PERF_CASCADE.swap(0, std::sync::atomic::Ordering::Relaxed),
        delta_runs: PERF_DELTA.swap(0, std::sync::atomic::Ordering::Relaxed),
    }
}

/// Aggregierter globaler Hebel ueber alle Pakete.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BigLever {
    /// Flag-Name.
    pub flag: String,
    /// Anzahl betroffener Pakete.
    pub packages: usize,
    /// Gesamt-Delta bei globaler Deaktivierung.
    pub total: SizeMetrics,
    /// Anzahl dadurch entstehender Konflikte.
    pub conflicts: usize,
}

/// Strukturierte Engine-Fehler.
#[derive(Debug, Error)]
pub enum EngineError {
    /// Ein-/Ausgabefehler mit Pfadkontext.
    #[error("io error at {path}: {source}")]
    Io {
        /// Betroffener Pfad.
        path: String,
        /// Ursache.
        #[source]
        source: std::io::Error,
    },
    /// Parse-Fehler mit Kontext.
    #[error("parse error in {context}: {message}")]
    Parse {
        /// Wo (Datei/Feld).
        context: String,
        /// Was.
        message: String,
    },
    /// Unaufloesbare Abhaengigkeit.
    #[error("unresolvable dependency {atom} required by {required_by}")]
    Unresolvable {
        /// Das problematische Atom.
        atom: String,
        /// Wer es verlangt.
        required_by: String,
    },
    /// Ungueltige Konfiguration.
    #[error("invalid configuration: {0}")]
    Config(String),
}

/// Ergebnis-Typ der Engine.
pub type EngineResult<T> = Result<T, EngineError>;

/// Bidirektionale ID-Tabelle String <-> u32 (fuer `PkgId`/`FlagId`).
#[derive(Debug, Default)]
pub struct IdTable {
    to_id: HashMap<String, u32>,
    to_name: Vec<String>,
}

impl IdTable {
    /// Leere Tabelle.
    pub fn new() -> Self {
        Self::default()
    }

    /// Gibt die ID zurueck (legt neu an bei Bedarf).
    pub fn intern(&mut self, name: &str) -> u32 {
        if let Some(id) = self.to_id.get(name) {
            return *id;
        }
        let id = self.to_name.len() as u32;
        self.to_name.push(name.to_string());
        self.to_id.insert(name.to_string(), id);
        id
    }

    /// Loest eine ID auf (`None` bei ungueltig).
    pub fn resolve(&self, id: u32) -> Option<&str> {
        self.to_name.get(id as usize).map(String::as_str)
    }

    /// Anzahl Eintraege.
    pub fn len(&self) -> usize {
        self.to_name.len()
    }

    /// Ob leer.
    pub fn is_empty(&self) -> bool {
        self.to_name.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_table_intern_resolve_roundtrip() {
        let mut t = IdTable::new();
        let a = t.intern("sys-kernel/linux-headers");
        let b = t.intern("dev-libs/openssl");
        let a2 = t.intern("sys-kernel/linux-headers");
        assert_eq!(a, a2);
        assert_ne!(a, b);
        assert_eq!(t.resolve(a), Some("sys-kernel/linux-headers"));
        assert_eq!(t.resolve(999), None);
    }

    #[test]
    fn size_metrics_saturating_add() {
        let a = SizeMetrics {
            binpkg_bytes: i64::MAX,
            download_bytes: 10,
        };
        let b = SizeMetrics {
            binpkg_bytes: 5,
            download_bytes: -3,
        };
        let c = a.checked_add(b);
        assert_eq!(c.binpkg_bytes, i64::MAX);
        assert_eq!(c.download_bytes, 7);
    }

    #[test]
    fn pkg_key_display() {
        assert_eq!(
            PkgKey::new("dev-libs", "openssl").to_string(),
            "dev-libs/openssl"
        );
    }

    #[test]
    fn conflict_info_options_preserved() {
        let c = ConflictInfo {
            package: "dev-libs/libX".into(),
            message: "+foo vs -foo".into(),
            caused_by: vec!["app-misc/a".into()],
            options: vec![QuickSwitch {
                label: "opt".into(),
                changes: vec![("dev-libs/libX".into(), "foo".into(), true)],
            }],
        };
        assert_eq!(c.options.len(), 1);
    }
}
