//! 01_types — Basis-IDs, Groessen, Fehler.
//!
//! Genau eine Zustaendigkeit: alle Kerndatentypen der Engine.
//! Die kompakte Paket-ID (`PkgId`) adressiert Engine-Vektoren ohne
//! String-Lookups (ungenutzte `FlagId`/`IdTable` bewusst entfernt,
//! review3 Task 3.3).

use thiserror::Error;

/// Kompakte Paket-ID (Index in Engine-Vektoren).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PkgId(pub u32);

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

// Hinweis: Performance-Instrumentierung (`PerfStats`, Zaehler, `ScopeTimer`)
// liegt zentral in `crate::mod_00_perf` (review3 Phase 2, Feature
// `perf-metrics`); hier bleibt kein unkonditionaler Atomic zurueck.

/// Aggregierter globaler Hebel ueber alle Pakete.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BigLever {
    /// Flag-Name.
    pub flag: String,
    /// Anzahl betroffener Pakete.
    pub packages: usize,
    /// Betroffene Pakete (`cat/pkg`, sortiert, review3 Task 4.3).
    pub affected_packages: Vec<String>,
    /// Gesamt-Delta bei globaler Deaktivierung.
    pub total: SizeMetrics,
    /// Anzahl dadurch entstehender Konflikte.
    pub conflicts: usize,
    /// Neu hinzukommende Pakete (`cat/pkg`, sortiert, review3 Task 4.3).
    pub added_packages: Vec<String>,
    /// Entfallende Pakete (`cat/pkg`, sortiert, review3 Task 4.3).
    pub removed_packages: Vec<String>,
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

/// Welt-/Plan-Atom auf `cat/pkg` kuerzen (zentral, review3 Task 1.3).
///
/// `Dep::parse` versteht Operatoren (`>=`), Versionen, Slots (`:0`, `:0/1`)
/// und USE-Abhaengigkeiten (`[foo]`); nicht parsbare Atome werden lexikalisch
/// an Slot-/USE-Suffixen gekuerzt statt fallengelassen.
pub fn atom_cpn(atom: &str) -> String {
    let atom = atom.split_whitespace().next().unwrap_or(atom);
    if let Ok(dep) = portage_atom::Dep::parse(atom) {
        return dep.cpn.to_string();
    }
    let base = atom.split(':').next().unwrap_or(atom);
    let base = base.split('[').next().unwrap_or(base);
    base.trim_start_matches(['=', '<', '>', '~']).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn atom_cpn_strips_operators_versions_slots_and_use_deps() {
        // Zentraler Kuerrzer (review3 Task 1.3).
        assert_eq!(atom_cpn("app-misc/jq"), "app-misc/jq");
        assert_eq!(atom_cpn(">=dev-libs/openssl-3.0"), "dev-libs/openssl");
        assert_eq!(
            atom_cpn("=sys-kernel/gentoo-sources-6.6.7"),
            "sys-kernel/gentoo-sources"
        );
        assert_eq!(atom_cpn("app-editors/emacs:0"), "app-editors/emacs");
        assert_eq!(atom_cpn("dev-libs/libx:0/1"), "dev-libs/libx");
        assert_eq!(atom_cpn("dev-libs/libx[foo]"), "dev-libs/libx");
        assert_eq!(atom_cpn("dev-libs/libx:0[foo=]"), "dev-libs/libx");
        assert_eq!(atom_cpn("~media-video/ffmpeg-7.0"), "media-video/ffmpeg");
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
